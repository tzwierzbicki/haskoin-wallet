{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

-- | The commands on the wallets and their portfolios
-- ("Haskoin.Wallet.Database.Portfolio"): newportfolio and recover, which
-- add them from a secret the user types, checkmnemonic, which only checks
-- a secret, mnemonicsheet, switch, the listing of portfolios, and
-- deleteportfolio and deletewallet.
--
-- A secret is a mnemonic, the shares of a backup or a master private key,
-- asked for on the terminal ("Haskoin.Wallet.Cli.Prompt"). hw keeps none
-- of it: a command derives the public keys of the accounts and forgets
-- the rest. importwallet and exportwallet are in
-- "Haskoin.Wallet.Commands.WalletFile".
module Haskoin.Wallet.Commands.Portfolio
  ( cmdNewPortfolio,
    cmdMnemonicSheet,
    cmdRecover,
    cmdCheckMnemonic,
    cmdSwitch,
    cmdDeletePortfolio,
    cmdDeleteWallet,
    portfoliosListing,
  )
where

import Control.Monad (forM, guard, join, when)
import Control.Monad.Except (ExceptT (..), catchError, runExceptT, throwError)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Class (lift)
import Data.Bifunctor (first)
import Data.Foldable (for_)
import Data.List (genericTake)
import Data.Maybe (fromMaybe, isJust, isNothing, listToMaybe)
import Data.String.Conversions (cs)
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Time (LocalTime (..), ZonedTime (..), getZonedTime)
import Haskoin
import Haskoin.Wallet.Cli.Listing
import Haskoin.Wallet.Cli.Parser
import Haskoin.Wallet.Cli.PrettyPrinter (orList)
import Haskoin.Wallet.Cli.Prompt
import Haskoin.Wallet.Cli.Response
import Haskoin.Wallet.Cli.Wrap (warn)
import Haskoin.Wallet.Commands.Answer (viewOfPortfolio, walletHiddenUse)
import Haskoin.Wallet.Commands.Backup (backUpBeforeLosing)
import Haskoin.Wallet.Commands.Deletion (confirmDeletion, deletePortfolioNotice, deleteWalletNotice)
import Haskoin.Wallet.Commands.Run (runDBResponse, runResponse)
import Haskoin.Wallet.Commands.Sync (shownSyncs, terminalWatcher)
import Haskoin.Wallet.Config
import Haskoin.Wallet.Database
import Haskoin.Wallet.Database.Account
import Haskoin.Wallet.Database.Portfolio
import Haskoin.Wallet.Database.RawTx (dropOrphanRawTxs)
import Haskoin.Wallet.FileIO
import Haskoin.Wallet.Keys
import Haskoin.Wallet.Keys.Entropy
import Haskoin.Wallet.Keys.Shares (genShares, thresholdNeedsShares)
import Haskoin.Wallet.Report.MnemonicSheet (SheetKind (..), SheetWallet (..), sheetKindOf, sheetOptions, sheetPdf, walletSheetFile)
import Haskoin.Wallet.Store
import Haskoin.Wallet.Sync
import Haskoin.Wallet.Sync.Status (StatusLine (..), checkingHealth, withStatusLine)
import Haskoin.Wallet.Util
import Numeric.Natural (Natural)
import System.Directory (removeFile)
import System.FilePath (takeDirectory)

-- | Make a new mnemonic, or the shares of a new backup, with its wallet
-- and the portfolio 0 of it. Or, with --existing, ask for a mnemonic the
-- user has and add a portfolio to its wallet, the one with its
-- fingerprint ('seedWallet'), at the lowest free index, or create the
-- wallet with its portfolio 0. A portfolio is created whole, with every
-- account ('Haskoin.Wallet.Keys.portfolioAccounts'). The first portfolio
-- of a database becomes the active one, and the answer says which
-- portfolio is active (@newPortfolioAnswer@).
--
-- A mnemonic typed with another passphrase is another seed, which hw
-- cannot tell from a mnemonic the user brings to this database: both are
-- of no wallet here. So --existing creates the wallet either way, and
-- warns when the database had wallets, since the user may have meant one
-- of them (@newWalletWarning@). With --match the user says that the
-- wallet is here, and a seed of no wallet is refused (@unmatchedRefusal@).
cmdNewPortfolio :: Config -> NewPortfolio -> IO Response
cmdNewPortfolio cfg opts
  | opts.match && not opts.existing =
      return $ ResponseError "--match goes with --existing: it refuses a mnemonic that is of no wallet of the database"
  | opts.existing = runDBResponse cfg $ do
      -- Each of these would do nothing here. Refused, it does not let
      -- the user believe it did something.
      when (opts.dice || isJust opts.shares || isJust opts.threshold || isJust opts.entropy || opts.oneLine || opts.showMasterKey) $
        throwError "--entropy, --dice, --shares, --threshold, --one-line and --show-master-key are for a new mnemonic: they do not go with --existing"
      had <- lift $ map (walletIndex . snd) <$> getWallets
      -- No mnemonic can match, so none is asked for
      when (opts.match && null had) $
        throwError "There is no wallet yet for a mnemonic to match: nothing was created. Without --match, newportfolio --existing creates the wallet of the mnemonic"
      secret <- askNewSeedSecret terminal
      seed <- liftEither $ secretSeed secret
      heldM <- seedWallet seed
      when (opts.match && isNothing heldM) $ throwError (unmatchedRefusal secret)
      -- A wallet created beside others may be a mistake
      let warning created = newWalletWarning secret had (refWallet created) <$ guard (isNothing heldM && not (null had))
      newPortfolioAnswer cfg Nothing Nothing warning $ do
        held <- maybe (lift (insertWallet (seedFingerprint seed))) return heldM
        index <- lift $ nextPortfolioIndex seed (fst held)
        createPortfolio seed held index
  | otherwise = do
      -- The secret is made and its passphrase asked before the database
      -- is held: the user may take a while
      made <- runExceptT newSecret
      case made of
        Left err -> return (ResponseError (cs err))
        Right (shown, secret) -> runDBResponse cfg $ do
          seed <- liftEither $ secretSeed secret
          -- Shown with the words, once, and only when asked for
          masterKeyM <- liftEither $ if opts.showMasterKey then Just <$> masterKeyText secret else Right Nothing
          -- A new mnemonic is a new wallet. The same fingerprint in the
          -- database would mean the same seed came out twice.
          clash <- lift $ walletWithFingerprint (seedFingerprint seed)
          for_ clash $ \(_, wallet) ->
            throwError $
              "The new mnemonic has the fingerprint of " <> cs (walletName (walletIndex wallet)) <> ": nothing was created. Run the command again for another mnemonic"
          newPortfolioAnswer cfg (Just shown) masterKeyM (const Nothing) $ do
            held <- lift $ insertWallet (seedFingerprint seed)
            createPortfolio seed held 0
  where
    sources =
      Sources
        { readSystem = systemEntropy,
          askDice = if opts.dice then Just (askDiceEntropy terminal) else Nothing,
          say = tell terminal
        }
    -- A mnemonic of 12 words, unless --entropy says otherwise
    entropyBytes = fromMaybe defaultEntropyBytes opts.entropy
    -- The new secret as the answer shows it, and with its passphrase as
    -- the wallet is derived from it
    newSecret :: ExceptT String IO (NewSecret, SeedSecret)
    newSecret = case opts.shares of
      Nothing -> do
        when (isJust opts.threshold) $ throwError thresholdNeedsShares
        (source, mnemonic) <- genMnemonic sources entropyBytes
        secret <- askNewPassphraseOf terminal (MnemonicWords mnemonic)
        return (NewMnemonic source (Text.words mnemonic), secret)
      Just count -> do
        (source, threshold, shares) <- genShares sources entropyBytes count opts.threshold
        backup <- liftEither $ backupOfShares shares
        secret <- askNewPassphraseOf terminal (BackupShares backup)
        return (NewShares source (fromIntegral threshold) (Text.words <$> shares), secret)

-- | The answer of newportfolio: the secret it made, if any, its master
-- private key when --show-master-key asks for it, the portfolio the
-- action creates, the active portfolio after it, which is the new one
-- when the database had none, the warning that goes with the portfolio
-- created, if any, and the sheet to write a new secret on
-- (@writeWalletSheet@)
newPortfolioAnswer :: Config -> Maybe NewSecret -> Maybe Text -> (PortfolioRef -> Maybe Text) -> ExceptT String (DB IO) Portfolio -> ExceptT String (DB IO) Response
newPortfolioAnswer cfg shownM masterKeyM warningOf create = do
  (portfolio, activeM) <- withActiveAfter create
  -- The database has a portfolio now, so it has an active one
  active <- liftMaybe noWallets activeM
  view <- viewOfPortfolio cfg portfolio
  -- Last, so that a refusal above leaves no sheet of a wallet that was
  -- not created
  sheetM <- liftIO $ join <$> forM shownM (writeWalletSheet cfg view . sheetOfSecret)
  return $ ResponseNewPortfolio shownM masterKeyM view active (warningOf (viewRef view)) sheetM

-- | The sheet that has the lines of a new secret: those of its words, or
-- a page for each of its shares
sheetOfSecret :: NewSecret -> SheetKind
sheetOfSecret = \case
  NewMnemonic _ ws -> MnemonicSheet (length ws)
  NewShares _ threshold shares -> SharesSheet (fromIntegral threshold) (length shares) (maybe 0 length (listToMaybe shares))

-- | Write the sheet of the wallet newportfolio created, to print and to
-- write its new secret on by hand, and answer with the file
-- ("Haskoin.Wallet.Report.MnemonicSheet"). The sheet has the fingerprint
-- of the wallet and the day on it, and no word of the secret. It goes
-- into the directory mnemonic-sheets next to the database
-- ('walletSheetFile'), and takes the place of a sheet left there under
-- the same name.
--
-- The user did not ask for the sheet, and the secret is made already,
-- with dice rolls perhaps. So a sheet that cannot be written is told on
-- stderr, with the command that writes one, and stops nothing: the
-- answer then names no sheet.
--
-- Only a new secret gets a sheet. With --existing the user typed the
-- words from a backup they hold, and recover and importwallet create
-- wallets that were written down long ago.
writeWalletSheet :: Config -> PortfolioView -> SheetKind -> IO (Maybe FilePath)
writeWalletSheet cfg view kind = do
  now <- getZonedTime
  let fingerprint = viewFingerprint view
      file = walletSheetFile (databaseFile cfg) (refWallet (viewRef view)) fingerprint
      wallet = SheetWallet fingerprint (localDay (zonedTimeToLocalTime now))
  written <- runExceptT $ do
    liftEitherIO $ first (couldNotWrite file) <$> systemReason (privateDirectory (takeDirectory file))
    liftEitherIO $ replaceFileBytes file (sheetPdf now (Just wallet) kind)
  case written of
    Right () -> return (Just file)
    Left reason ->
      Nothing
        <$ warn
          ( "The sheet of the new wallet was not written. "
              <> reason
              <> ". The command itself is done. To write a sheet without the fingerprint, run: "
              <> unwords (["hw", "mnemonicsheet", "FILENAME"] <> sheetOptions kind)
          )

-- | Remove the sheet hw wrote for a wallet, when it is there
-- (@writeWalletSheet@): deletewallet does, so that the directory holds
-- no sheet of a wallet the user deleted. The sheet is blank, and
-- mnemonicsheet writes another. A sheet that cannot be removed stops
-- nothing.
removeWalletSheet :: Config -> DBWallet -> IO ()
removeWalletSheet cfg wallet =
  ignoringFailure . removeFile $ walletSheetFile (databaseFile cfg) (walletIndex wallet) (walletFingerprint wallet)

-- | Write a sheet to a new PDF file, to print it and to write a mnemonic
-- on it by hand, or the shares of a backup, and answer with the file
-- ("Haskoin.Wallet.Report.MnemonicSheet"). The options are those of
-- newportfolio that decide what the secret looks like, and they are
-- refused as newportfolio refuses them ('sheetKindOf').
--
-- The command reads no database: the sheet is of no wallet, and is
-- blank. So it runs before the first wallet is made, which is when a
-- sheet is needed.
--
-- The file is written whole, and a path that is taken is refused, as
-- for every file hw writes.
cmdMnemonicSheet :: MnemonicSheetOptions -> IO Response
cmdMnemonicSheet opts =
  runResponse $ do
    kind <- liftEither $ sheetKindOf opts.entropy opts.shares opts.threshold
    checkFileToWrite opts.file
    now <- liftIO getZonedTime
    liftEitherIO $ writeFileBytes opts.file (sheetPdf now Nothing kind)
    return $ ResponseFile opts.file

-- | What newportfolio --existing says of a secret that gives the keys of
-- no wallet of the database
ofNoWallet :: SeedSecret -> String
ofNoWallet = \case
  MnemonicSecret _ -> "This mnemonic and passphrase are those of no wallet here"
  SharesSecret _ _ -> "These shares and passphrase are those of no wallet here"
  MasterKeySecret _ -> "This master private key is that of no wallet here"

-- | The warning of newportfolio --existing when it created the wallet of
-- the secret beside the wallets the database had, given their indexes and
-- the index of the new one. The same typo of a passphrase, typed twice,
-- gives a wallet of a secret the user does not know they typed, and funds
-- sent to it can be spent with that typo only. The answer would otherwise
-- read as the one of a portfolio added, and propose to switch to it. So
-- it names the wallets the user may have meant and the command that
-- deletes the new one, which holds nothing yet.
newWalletWarning :: SeedSecret -> [Natural] -> Natural -> Text
newWalletWarning secret had created =
  cs $
    ofNoWallet secret
      <> (": a new " <> name created <> " was created. If you meant ")
      <> (orList (name <$> had) <> ", " <> differed <> ": delete ")
      <> (name created <> " with hw deletewallet " <> show created)
  where
    name = cs . walletName
    differed = case secret of
      MnemonicSecret _ -> "the mnemonic or the passphrase was not the same"
      SharesSecret _ _ -> "the shares or the passphrase were not the same"
      MasterKeySecret _ -> "the key was not the same"

-- | Why newportfolio --existing --match creates nothing from a secret
-- that gives the keys of no wallet of the database
unmatchedRefusal :: SeedSecret -> String
unmatchedRefusal secret = ofNoWallet secret <> ": nothing was created. Without --match, newportfolio --existing creates a new wallet"

-- | Recover the portfolios of a seed that were paid, from its mnemonic or
-- the shares of its backup: see "Haskoin.Wallet.Sync". The search covers
-- every network of a portfolio, the test networks included, and the
-- hidden ones: a portfolio paid on a hidden network alone is still found
-- and created, and its network is up to date the day the user shows it.
-- The status line names each network as it is searched. The answer keeps
-- to the networks hw shows ('onShownNetworks'): it names no hidden network
-- as searched or left out, and gives none of the syncs or of the funds
-- found there. A store that fails on a hidden network alone does not make
-- the command exit with an error.
--
-- One thing is told of the hidden networks: which of them each portfolio
-- of the wallet is used on ('walletHiddenUse'), with the command that
-- shows them. recover runs once, and its answer is where the user learns
-- what the mnemonic holds. Without it, a portfolio that was paid on a
-- hidden network alone is recovered, has nothing on any network of the
-- answer, and the funds stay unseen, on Bitcoin Cash for the user who
-- hid it. In a database without any portfolio,
-- the first one created becomes the active one, and the answer says which
-- portfolio is active.
--
-- The store of each network is asked whether it is up before the mnemonic
-- is typed, which the line tells. Once the host could not be reached, the
-- stores of the networks after are not asked, and are down
-- ('servedNetworks'). A network the host does not serve is named and left
-- out, and so is one whose store is down, which also makes the command
-- exit with an error once the others are searched: a portfolio that was
-- paid on that network alone is not found. When no network can be
-- searched, the command fails before the mnemonic: the computer is
-- offline, or the host is wrong. A store that fails later, during the
-- search, the syncs or the search of the funds at other paths, is named
-- the same way: its network is searched no further, and the others are,
-- and the wallet found is kept ('storeFailureApart'). The answer names as
-- searched the networks that were searched in full.
--
-- The funds of the seed at the accounts hw does not hold are told, not
-- taken. When nothing at all was found, the answer says to check the
-- passphrase (@passphraseHint@).
cmdRecover :: Config -> Recover -> IO Response
cmdRecover cfg opts =
  withStatusLine $ \line ->
    runDBResponse cfg $ do
      storeOn <- liftIO $ webStores cfg
      let watch = terminalWatcher line
      -- An offline computer hears it before the mnemonic is typed
      served <- liftIO $ servedNetworks (checkingHealth (watch.status "recover") storeOn) portfolioNetworks
      -- With no network to search, the refusal names the shown ones
      liftEither $ requireSomeNetwork (configHost cfg) served {servedNot = onShownNetworks cfg id (servedNot served)}
      -- The question for the mnemonic starts on a clean line
      liftIO $ eraseStatus line
      let searched = servedUp served
      activeBefore <- lift activePortfolioId
      -- Typed once: a mistyped mnemonic finds no paid portfolio
      secret <- askSeedSecret terminal
      seed <- liftEither $ secretSeed secret
      recovered <- recoverSeed cfg storeOn searched seed opts.portfolioGap watch
      -- The networks whose store has not failed yet
      let upAfter failed = [net | net <- searched, net `notElem` (fst <$> failed)]
          failedInRecovery = recoveredDown recovered
      -- The funds at other paths, on each network whose store is still up
      fundsSearched <- forM (upAfter failedInRecovery) $ \net ->
        storeFailureApart (storeOn net) $ \store -> map (net,) <$> foreignFunds cfg store net seed opts.portfolioGap watch
      let elsewhere = concat [funds | Right funds <- fundsSearched]
          failed = failedInRecovery <> [(net, failure) | (net, Left failure) <- zip (upAfter failedInRecovery) fundsSearched]
      active <- activeAfterFrom activeBefore
      usedOnHidden <- maybe (return []) (walletHiddenUse cfg) (recoveredWallet recovered)
      -- The answer keeps to the networks hw shows
      return $
        ResponseRecover
          (onShownNetworks cfg id (upAfter failed))
          opts.portfolioGap
          (recoveredWallet recovered)
          (recoveredPortfolios recovered)
          (shownSyncs cfg (recoveredSyncs recovered))
          (onShownNetworks cfg fst elsewhere)
          (onShownNetworks cfg id (servedNot served))
          [(net, cs failure) | (net, failure) <- onShownNetworks cfg fst (servedDown served <> failed)]
          active
          (if isNothing (recoveredWallet recovered) && null elsewhere then passphraseHint secret else Nothing)
          usedOnHidden

-- | What recover adds when it found nothing of a secret that takes a
-- passphrase: no portfolio, and no funds at the paths hw does not hold.
-- A mistyped passphrase is the usual reason for an empty answer from a
-- mnemonic that was paid: it gives the seed of another wallet, which was
-- never paid. Without the hint the answer reads as "the funds are gone".
-- A master private key takes no passphrase, and gets none.
passphraseHint :: SeedSecret -> Maybe Text
passphraseHint = \case
  MnemonicSecret _ -> Just (hintFor "this mnemonic")
  SharesSecret _ _ -> Just (hintFor "this backup")
  MasterKeySecret _ -> Nothing
  where
    hintFor secret = "If " <> secret <> " was paid, check the passphrase: another passphrase, or none, gives another wallet."

-- | Check a mnemonic and its passphrase, the shares of a backup, or a
-- master private key, against the active portfolio: they match when they give the key of
-- every account the portfolio has. Nothing is written. An answer that
-- they do not match makes hw exit with an error ('responseFailed').
--
-- A secret that does not match may be that of another wallet of the
-- database, which the answer then names (@otherWalletOf@). A user with a
-- sheet of words and several wallets would otherwise switch to each
-- wallet in turn, and read the first answer as a bad backup.
--
-- With --show-master-key, the answer has the master private key of the
-- wallet too: the way to get the key of a wallet that newportfolio made
-- without that option. Only a secret that matches gets its key. A
-- passphrase typed with a mistake gives the key of another wallet, which
-- the user would keep as the key of this one and find out when it signs
-- nothing.
cmdCheckMnemonic :: Config -> CheckMnemonic -> IO Response
cmdCheckMnemonic cfg opts =
  runDBResponse cfg $ do
    portfolio <- requireActivePortfolio
    accounts <- lift $ portfolioAccountRows portfolio.portfolioId
    secret <- askSeedSecret terminal
    seed <- liftEither $ secretSeed secret
    matched <- liftIO $ and <$> mapM (seedDerives seed portfolio . snd) accounts
    otherM <- if matched then return Nothing else otherWalletOf seed
    let (typed, theyMatch) = case secret of
          MnemonicSecret _ -> ("The mnemonic and passphrase", "they match ")
          SharesSecret _ _ -> ("The shares and passphrase", "they match ")
          MasterKeySecret _ -> ("The master private key", "it matches ")
        said =
          typed
            <> (if matched then " matched " else " did not match ")
            <> cs (portfolioText portfolio)
            <> maybe "" (\other -> ": " <> theyMatch <> cs (walletName other)) otherM
    masterKeyM <- liftEither $ if opts.showMasterKey && matched then Just <$> masterKeyText secret else Right Nothing
    return $ ResponseChecked (portfolioRef portfolio) matched said masterKeyM

-- | The wallet of the database that a seed is of, by its index: what
-- checkmnemonic names when a secret does not match the active portfolio.
-- A wallet that has the fingerprint of the seed and other keys is not the
-- wallet of that seed ('seedWallet').
otherWalletOf :: PublicSeed -> ExceptT String (DB IO) (Maybe Natural)
otherWalletOf seed = (fmap (walletIndex . snd) <$> seedWallet seed) `catchError` \_ -> return Nothing

-- | Make the portfolio --wallet and --portfolio name the active one, the
-- one the commands work in from now on, and show it ('switchTo').
-- --wallet alone names portfolio0 of that wallet.
cmdSwitch :: Config -> Maybe Natural -> Maybe Natural -> IO Response
cmdSwitch cfg walletM portfolioM =
  runDBResponse cfg $ do
    portfolio <- switchTo walletM portfolioM
    ResponseSwitched <$> viewOfPortfolio cfg portfolio

-- | Delete a portfolio of the wallet of the active portfolio that was
-- never used: not portfolio 0, not the active one, the last of its
-- wallet, without any activity, and without any address given out
-- ('portfolioHasActivity'). The portfolio is named by its index. The
-- active portfolio is refused rather than replaced by another one: which
-- one the user works in next is theirs to say, with switch.
--
-- Once every rule is passed, the user is asked, unless --yes
-- ('confirmDeletion'). The database and its labels are then copied
-- ('backUpBeforeLosing'). A portfolio that is refused, or that the user
-- keeps, leaves no copy.
cmdDeletePortfolio :: Config -> Natural -> Bool -> IO Response
cmdDeletePortfolio cfg index yes =
  runDBResponse cfg $ do
    active <- requireActivePortfolio
    portfolio <- portfolioNamed (active.portfolioWalletId, active.portfolioWallet) index
    let name = portfolioText portfolio
    when (index == 0) $
      throwError "portfolio0 is never deleted: a wallet always keeps it, and a new one would be the same portfolio, with the same keys. deletewallet deletes a whole wallet"
    when (portfolio.portfolioId == active.portfolioId) $
      throwError (name <> " is the active portfolio: switch to another portfolio first")
    others <- lift $ getPortfolios portfolio.portfolioWalletId
    let highest = maximum (index : (portfolioIndex . snd <$> others))
    when (index /= highest) $
      throwError (name <> " is not the last portfolio of its wallet: " <> cs (portfolioName highest) <> " goes first")
    used <- lift $ portfolioHasActivity portfolio.portfolioId
    when used $
      throwError (name <> " has had activity or gave out an address, so it is kept: a payment to it would go unseen, and recover would find it again. deletewallet deletes a whole wallet")
    confirmDeletion yes (deletePortfolioNotice cfg portfolio) ("Delete " <> name <> "?")
    backup <- backUpBeforeLosing cfg "Nothing was deleted"
    lift $ deletePortfolio portfolio.portfolioId
    return $ ResponseDeleted (cs name <> " has been deleted") backup

-- | Delete a wallet with all of its portfolios, whatever they hold. The
-- wallet is named by its index. The wallet of the active portfolio is
-- refused while another wallet exists: which one the user works in next
-- is theirs to say, with switch. The last wallet goes with the active
-- portfolio, and the next portfolio created becomes active, as in a new
-- database.
--
-- The user is asked first, unless --yes ('confirmDeletion'). The
-- question shows the description and the fingerprint of the wallet, so
-- that a mistyped index is caught there. The command once took the
-- fingerprint as an argument for that. The user had to look it up in
-- portfolios --verbose, and the question now shows it instead.
--
-- The database and its labels are then copied ('backUpBeforeLosing').
-- The copy keeps what nothing else brings back: the labels of the
-- wallet, and its pending transactions. A wallet that is refused, or
-- that the user keeps, leaves no copy.
--
-- The sheet newportfolio wrote for the mnemonic of the wallet goes with
-- it (@removeWalletSheet@).
cmdDeleteWallet :: Config -> Natural -> Bool -> IO Response
cmdDeleteWallet cfg index yes =
  runDBResponse cfg $ do
    (walletId, wallet) <- walletNamed index
    let name = cs (walletName index) :: String
    others <- filter ((/= walletId) . fst) <$> lift getWallets
    activeM <- activePortfolio
    for_ activeM $ \active ->
      when (active.portfolioWalletId == walletId && not (null others)) $
        throwError (name <> " holds the active portfolio, " <> portfolioText active <> ": switch to a portfolio of another wallet first")
    portfolios <- lift $ getPortfolios walletId
    confirmDeletion yes (deleteWalletNotice cfg wallet (length portfolios)) ("Delete " <> name <> "?")
    backup <- backUpBeforeLosing cfg "Nothing was deleted"
    lift $ deleteWallet walletId
    -- The raw transactions its coins came from are of no use any more
    _ <- dropOrphanRawTxs
    liftIO $ removeWalletSheet cfg wallet
    let said =
          cs name
            <> " has been deleted, with all of its portfolios"
            <> (if null others then ". The database has no wallet left: the first portfolio created next becomes the active one." else "")
    return $ ResponseDeleted said backup

-- | Every portfolio, each with what it can spend now on each network hw
-- shows and whether it is the active one, in the order of the wallets and
-- of the portfolios of each: a portfolio names the one to go on after
portfoliosListing :: Config -> ExceptT String (DB IO) Sections
portfoliosListing cfg =
  return . oneSection Nothing $
    Listing
      { listingStart = Nothing,
        listingRows = \afterM countM -> do
          portfolios <- lift getAllPortfolios
          let following = case afterM of
                Nothing -> portfolios
                Just cursor -> drop 1 $ dropWhile ((/= cursor) . portfolioRef) portfolios
          mapM (viewOfPortfolio cfg) $ maybe id genericTake countM following,
        listingCursor = Right . viewRef,
        listingAnswer = ResponsePortfolios,
        -- Every chunk reads every portfolio anyway, and the table of
        -- portfolios takes the width of its columns from all of them
        listingWhole = True,
        listingShownEmpty = False
      }
