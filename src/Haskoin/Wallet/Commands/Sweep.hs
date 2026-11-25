{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Sweeps: preparesweep, signsweep and sweep, which take the coins of
-- private keys from outside the wallet, into the wallet or anywhere else
-- ("Haskoin.Wallet.Tx.Sweep"). A sweep is prepared and signed like a
-- payment, with the helpers of "Haskoin.Wallet.Commands.Payment" and of
-- "Haskoin.Wallet.Commands.Signing". It is signed with the private keys
-- of its coins, where a payment is signed with the mnemonic.
module Haskoin.Wallet.Commands.Sweep
  ( cmdPrepareSweep,
    cmdSweep,
    cmdSignSweep,
  )
where

import Control.Applicative ((<|>))
import Control.Monad (forM, forM_, unless, when, (<=<))
import Control.Monad.Except (ExceptT (..), catchError, throwError, withExceptT)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Class (lift)
import Data.Foldable (for_)
import Data.List (intersect, nub)
import Data.Maybe (isJust, isNothing, mapMaybe, maybeToList)
import Data.Time (getCurrentTime)
import Haskoin
import qualified Haskoin.Store.Data as Store
import Haskoin.Wallet.Amounts (AmountUnit)
import Haskoin.Wallet.Cli.Parser
import Haskoin.Wallet.Cli.PrettyPrinter (renderOn, sweepDoc, sweepDustReportDoc)
import Haskoin.Wallet.Cli.Prompt
import Haskoin.Wallet.Cli.Response
import Haskoin.Wallet.Commands.Answer (seenFromPlace, txAnswer)
import Haskoin.Wallet.Commands.Payment (chooseFeeRate, coinsWithDust, confirmFeeRate, dustOptions, storePrepared, withLabel)
import Haskoin.Wallet.Commands.Run (runDBResponse)
import Haskoin.Wallet.Commands.Scope (poolAmong, shownAmong)
import Haskoin.Wallet.Commands.Send (sendJustSigned, sendTxRead)
import Haskoin.Wallet.Commands.Signing (SignedFile (..), keepSigned, refuseForPrivateKeys, showToSign, signedFileOf, transactionToSign, writeSigned)
import Haskoin.Wallet.Commands.TxFile (TxFileRead (..), fileNetwork, writeOutput)
import Haskoin.Wallet.Config
import Haskoin.Wallet.Database
import Haskoin.Wallet.Database.Account
import Haskoin.Wallet.Database.Address
import Haskoin.Wallet.Database.PendingTx
import Haskoin.Wallet.Database.Portfolio
import Haskoin.Wallet.Database.TxInfo
import Haskoin.Wallet.FileIO
import Haskoin.Wallet.Keys
import Haskoin.Wallet.Store
import Haskoin.Wallet.Tx.Dust (SizedCoin (coin), dustQuestion)
import Haskoin.Wallet.Tx.LockTime (isStale, lockTimeFor)
import Haskoin.Wallet.Tx.Recipient (Share, checkSharing, checkAddressReads, readRecipient, recipientAddress)
import Haskoin.Wallet.Tx.SignData
import Haskoin.Wallet.Tx.Signing (heldPoolOf, labelledTxFile, noneHeld, parseOutsideTxSignData)
import Haskoin.Wallet.Tx.Sweep (SweepCoins (coins, withTokens), UnsweptCoin, UnsweptReason (..), addressesWithoutCoin, buildSweepSignData, fetchSweepCoins, signTxWithKeys, sizedSweepCoins, sweepDustReport, sweepableAddress, sweepingOnly, sweptAddresses, unsweptCoins)
-- The addresses of a private key, where message signing has those of a
-- public key under the same name
import qualified Haskoin.Wallet.Tx.Sweep as Sweep (keyAddresses)
import Haskoin.Wallet.Util
import Numeric.Natural (Natural)
import System.IO (stderr)
import System.Random (initStdGen)

-- | Sweep coins to the destinations: the coins at the given addresses,
-- typed or in a file, at the addresses of the given private keys, and the
-- coins given by outpoint. The keys are not needed here, only to sign: a
-- sweep can be prepared from addresses or coins alone. A sweep that pays
-- an address of the wallet is of the portfolio of that address, on its
-- network, and waits among its pending transactions to be signed by its
-- nosigHash (@sweepPlace@). One that pays no address of the wallet
-- involves no portfolio: hw is then a utility, which writes the sweep to
-- a file, on the --network given, to be signed from there. A destination
-- is an address, or an address and its amount, as for preparesendall. The
-- coins worth less than their fee are swept or left as the user answers,
-- or as --spend-dust or --leave-dust says, as for preparesendall too
-- ('coinsWithDust'). A sweep from an address of the wallet is refused
-- (@refuseOwnAddresses@).
cmdPrepareSweep :: Config -> AmountUnit -> PrepareSweep -> IO Response
cmdPrepareSweep cfg unit opts =
  runDBResponse cfg $ do
    -- Before the keys are typed, not after
    for_ opts.output checkFileToWrite
    (net, poolM) <- sweepPlace cfg opts
    when (isNothing poolM && isNothing opts.output) $
      throwError "The sweep does not involve this wallet: give an --output file to write it to"
    storeOn <- liftIO $ webStores cfg
    built <- buildSweep cfg (storeOn net) unit opts opts.keys net poolM
    info <- keepBuiltSweep net poolM opts.output built
    placeM <- mapM poolPlace poolM
    activeM <- if isJust poolM then activePortfolioRef else return Nothing
    return $ ResponseSweep placeM net info built.builtEmpty built.builtUnswept activeM

-- | Sweep the coins of private keys in one command: prepare the sweep as
-- preparesweep does, sign it as signsweep does, and send it as broadcast
-- does. Each step is that of its command (@buildSweep@ and
-- @keepBuiltSweep@, @signPoolSweep@ or 'signTxWithKeys', 'sendTxRead'), so
-- the sweep and what the wallet holds of it afterwards are the same as
-- with the three commands run one after the other.
--
-- The command is to a sweep what send is to a payment
-- (@prepareSignSend@), and is for keys the user is willing to give to an
-- online computer. It differs from send in when it asks. send shows the
-- payment, and the mnemonic typed after it is the confirmation. Here the
-- keys come first: they say which coins the sweep takes, so there is no
-- sweep to show before them. The sweep is shown once they are read, as
-- preparesweep answers with it, and a question takes the place of the
-- confirmation, unless --yes. The sweep is shown with --yes too: the
-- answer is its txid alone, and the dust coins a sweep leaves must not
-- go unsaid.
--
-- Every coin swept is of a key given: the options name no address and no
-- coin, which preparesweep takes for a sweep signed elsewhere.
--
-- What a failure leaves is what it leaves of a payment of send. Before
-- the signature, nothing is kept: a refusal and a no undo the pending
-- sweep. A sweep that pays a portfolio is kept signed before it is handed
-- to the network ('sendJustSigned'). One that pays no portfolio is never
-- kept, as with the three commands, which hold it in files alone: the
-- user has the keys, and runs the command again.
--
-- The store is asked whether it is well before the keys are read
-- (@buildSweep@).
cmdSweep :: Config -> AmountUnit -> PrepareSweep -> IO Response
cmdSweep cfg unit opts =
  runDBResponse cfg $ do
    (net, poolM) <- sweepPlace cfg opts
    storeOn <- liftIO $ webStores cfg
    -- As preparesweep --key-file: pending and unsigned in the portfolio
    -- it pays. Without a file, the keys are typed, as for signsweep.
    built <- buildSweep cfg (storeOn net) unit opts (opts.keys <|> Just KeysOnTerminal) net poolM
    info <- keepBuiltSweep net poolM Nothing built
    confirm net poolM info built
    case poolM of
      Just pool -> do
        -- As signsweep NOSIGHASH
        let pendingHash = nosigHash (txSignDataTx built.builtSweep)
            signing = signingPending pendingHash
        (_, file) <- transactionToSign cfg Nothing signing
        _ <- signPoolSweep signing NoSignedFile pool file built.builtKeys
        -- As broadcast TXID --yes
        sendJustSigned cfg storeOn unit pendingHash
      Nothing -> do
        -- As signsweep --input and broadcast --input --yes, without the
        -- files
        (signed, _) <- liftEither $ signTxWithKeys net built.builtSweep [] noneHeld built.builtKeys
        sendTxRead cfg storeOn unit True (Just net) (TxFileRead (plainTxFile (Just net) signed) Nothing)
          `catchError` (throwError . (<> ". The wallet keeps nothing of a sweep that pays none of its portfolios: run sweep again to send it"))
  where
    -- The sweep as preparesweep answers with it, then the question. On
    -- stderr, as every question, to keep the --json answer clean.
    confirm :: Network -> Maybe Pool -> TxInfo -> BuiltSweep -> ExceptT String (DB IO) ()
    confirm net poolM info built = do
      seenM <- mapM (seenFromPlace <=< poolPlace) poolM
      naming <- lift walletNaming
      liftIO $ renderOn stderr =<< sweepDoc naming unit seenM net info built.builtEmpty built.builtUnswept
      unless opts.yes $ do
        liftIO $ tell terminal ""
        agreed <- askYes terminal (ScriptOption "--yes") "Sign this sweep and send it to the network? This cannot be undone."
        unless agreed $ throwError "The sweep was not signed, and nothing was sent"

-- | The network of a sweep, and the portfolio it pays, if any. A sweep
-- that pays an address of the wallet is of the portfolio of that address,
-- on its network: hw finds it from the destinations, and takes the active
-- portfolio when they are of several ('poolAmong'). Destinations of the
-- wallet on two networks are refused, since a sweep is on one network. A
-- sweep that pays no address of the wallet is of no portfolio, and its
-- --network must be given: the default network of the configuration does
-- not apply. The keys, the coins and the legacy addresses of Bitcoin Cash
-- read the same on Bitcoin, so a wrong guess would sweep the coins of the
-- other chain.
sweepPlace :: Config -> PrepareSweep -> ExceptT String (DB IO) (Network, Maybe Pool)
sweepPlace cfg opts = do
  -- The portfolio of a destination is found by its address, an amount
  -- after it or not, before the network to read the amounts on is known
  everyHolder <- accountsHolding (\net -> mapMaybe (recipientAddress net) opts.sweepTo)
  -- The destinations the wallet has on hidden networks alone are
  -- refused: read as outside the wallet, the sweep would pay an account
  -- of it that no answer shows
  holders <- shownAmong cfg "The sweep pays an address of the wallet that" (\(_, _, acc) -> accountNetwork acc) everyHolder
  case nub [accountPool acc | (_, _, acc) <- holders] of
    [] -> do
      -- A destination that reads on no network is a typo, of an address
      -- of the wallet perhaps: said here, before a --network and an
      -- --output are asked for a sweep that was never meant to leave it
      when (isNothing opts.network) $ liftEither (mapM_ (checkAddressReads allNets) opts.sweepTo)
      net <-
        liftMaybe
          "Give the --network of a sweep that pays no portfolio of the wallet: the default-network of the configuration file does not apply to it"
          opts.network
      return (net, Nothing)
    paid -> do
      -- The addresses of the wallet it pays tell its portfolio and its
      -- network
      networks <- sweepNetworks
      pool <- poolAmong opts.network "The destinations of the sweep" [p | p <- paid, p.poolNetwork `elem` networks]
      return (pool.poolNetwork, Just pool)
  where
    -- The networks a sweep that pays the wallet can be on: those that hold
    -- every destination the wallet holds. A sweep is on one network, and
    -- the destinations of the wallet on two networks are refused. The
    -- default network must not choose between them: a Bitcoin Cash
    -- address in its legacy form reads on Bitcoin too, and the sweep would
    -- pay it there, where no account of the wallet watches it. Test
    -- networks that share their keys hold the same addresses, and
    -- 'poolAmong' takes the one of --network, or asks for it.
    -- Only the networks hw shows count.
    sweepNetworks = do
      heldOn <- forM opts.sweepTo $ \typed -> do
        holding <- accountsHolding (\net -> maybeToList (recipientAddress net typed))
        return $ onShownNetworks cfg id (nub [accountNetwork acc | (_, _, acc) <- holding])
      let held = filter (not . null) heldOn
          common = case held of
            [] -> []
            some : others -> foldr intersect some others
      when (null common) $
        throwError ("The sweep pays addresses of the wallet on " <> networksText (nub (concat held)) <> ": a sweep is on one network")
      return common

-- | A sweep just built, not kept yet
data BuiltSweep = BuiltSweep
  { -- | The sweep, unsigned
    builtSweep :: !TxSignData,
    -- | The private keys read, if the command read any
    builtKeys :: ![SecKey],
    -- | The addresses given to sweep from that hold no coin
    -- ('addressesWithoutCoin')
    builtEmpty :: ![Address],
    -- | The dust coins the sweep leaves at their addresses
    builtUnswept :: ![UnsweptCoin]
  }

-- | Build a sweep on its network from the options, for the pool it pays,
-- if any: the first step of preparesweep and of sweep. The keys are read
-- from the source given, if any: preparesweep can do without them, and
-- sweep cannot.
--
-- Every refusal that needs no coin comes before the keys are read, and so
-- does the question about a high fee rate. The store is asked whether it
-- is well before both: a store that is down must not cost the user keys
-- typed for nothing, and the advice of the fee rates, to give a
-- --fee-rate, does not help a sweep, whose coins come from the store.
-- The question about the dust coins comes after the keys, which tell the
-- coins.
buildSweep :: Config -> Store -> AmountUnit -> PrepareSweep -> Maybe KeySource -> Network -> Maybe Pool -> ExceptT String (DB IO) BuiltSweep
buildSweep cfg store unit opts keySource net poolM = do
  typed <- liftEither $ mapM sweptAddress opts.sweepFrom
  listed <- maybe (return []) addressesOfFile opts.addressFile
  sweepTo <- liftEither $ mapM (readRecipient net unit) opts.sweepTo
  liftEither $ checkSharing sweepTo
  refuseOwnAddresses net (typed <> listed)
  -- A store that is down stops the command here: before the fee rate,
  -- which a --fee-rate could give without it, and before the keys
  fromStore $ withExceptT (<> ". A sweep reads its coins from the server, and cannot be prepared without it") (checkHealth store)
  feeByte <- chooseFeeRate cfg net store 1 opts.feeRate
  confirmFeeRate opts.yes feeByte
  keys <- maybe (return []) (readSecKeys terminal net anyKeys) keySource
  found <- fetchSweepCoins net store poolM keys (typed <> listed) opts.coins (fst <$> sweepTo)
  -- The coins of --coin and of the keys tell their addresses only now.
  -- The builder counts on this refusal: it reads no coin as the wallet's.
  refuseOwnAddresses net (mapMaybe (.address) found.coins)
  (swept, unswept) <- leavingDust feeByte sweepTo found
  -- The lock time is drawn from the best block of the store, which the
  -- coins come from: a sweep is prepared online, whatever the wallet
  -- knows of the chain. A store behind the chain gives none.
  best <- fromStore $ storeBestBlock store
  now <- liftIO getCurrentTime
  gen <- liftIO initStdGen
  let heightM = if isStale now (Just best.header.timestamp) then Nothing else Just best.height
      (lockTime, buildGen) = lockTimeFor heightM gen
  tsd <- withLabel opts.label <$> liftEither (buildSweepSignData net buildGen swept sweepTo feeByte lockTime)
  return
    BuiltSweep
      { builtSweep = tsd,
        builtKeys = keys,
        builtEmpty = addressesWithoutCoin (typed <> listed) found,
        builtUnswept = unsweptCoins HoldsTokens found.withTokens <> unswept
      }
  where
    -- The coins to sweep among those found, with or without the dust
    -- coins ('coinsWithDust'), and the dust coins left. The question is
    -- about every coin found, those of --coin too: the user who names a
    -- coin may not know what its input costs. The report and the question
    -- go to stderr, to keep the --json answer clean.
    leavingDust :: Natural -> [(Address, Share)] -> SweepCoins -> ExceptT String (DB IO) (SweepCoins, [UnsweptCoin])
    leavingDust feeByte sweepTo found = do
      sized <- liftEither $ sizedSweepCoins net found
      swept <- coinsWithDust opts.dustCoins feeByte sweepTo id (askAboutDust feeByte sweepTo) sized
      return (sweepingOnly ((.coin) <$> swept) found, unsweptCoins WorthLessThanItsFee ((.coin) <$> filter (`notElem` swept) sized))
    askAboutDust feeByte sweepTo worth dust = do
      let report = sweepDustReport feeByte sweepTo worth dust
      liftIO $ renderOn stderr (sweepDustReportDoc net unit report)
      askYes terminal dustOptions (dustQuestion report)
    -- An address of --sweep-from. A private key in its place is the
    -- mistake of a user with a paper wallet, who is told where keys go.
    sweptAddress typed
      | hasSecKeyForm typed = Left (secKeyNotAddress <> ". Give the keys with --key-file, or the address of the key with --sweep-from")
      | otherwise = sweepableAddress net =<< textToAddrE net typed
    -- The addresses of the --address-file, read on the network of the
    -- sweep
    addressesOfFile file = liftEither . (mapM (sweepableAddress net) <=< parseAddresses net) =<< liftEitherIO (readTextFile file)

-- | Keep a sweep just built as preparesweep leaves it, and give its
-- summary as the answer shows it. One that pays a portfolio is pending in
-- it, unsigned, and in the --output file when there is one
-- ('storePrepared'). One that pays none is in its --output file alone:
-- the wallet does not hold it, so nothing keeps the time it was prepared,
-- and the summary takes the time of now, when the file is written.
keepBuiltSweep :: Network -> Maybe Pool -> Maybe FilePath -> BuiltSweep -> ExceptT String (DB IO) TxInfo
keepBuiltSweep net poolM output built = case poolM of
  Just pool -> storePrepared pool Nothing [] output built.builtSweep
  Nothing -> do
    info <- lift . fillContacts net =<< liftEither (parseOutsideTxSignData net built.builtSweep)
    writeOutput Nothing net output info built.builtSweep
    now <- liftIO getCurrentTime
    return $ case info.txInfoStatus of
      Pending pending -> info {txInfoStatus = Pending pending {pendingCreated = Just now}}
      OnChain _ -> info

-- | Refuse a sweep from addresses that the wallet holds on the network of
-- the sweep, in any portfolio ('heldOnNetwork'). A sweep is signed by
-- signsweep with private keys from outside the wallet, and the user has
-- none for an address of the wallet: the next step that preparesweep
-- prints could not be done, and the sweep, kept pending when it pays the
-- wallet too, would lock the coins it spends. sendall and preparetx do
-- the same job, with the rules of a payment and the mnemonic to sign.
-- Keeping the sweep and pointing to signtx was the other way: it would
-- leave two ways to do one thing. The refusal names every such address,
-- and where the wallet has them.
refuseOwnAddresses :: Network -> [Address] -> ExceptT String (DB IO) ()
refuseOwnAddresses net addrs = do
  own <- lift $ heldOnNetwork net addrs
  unless (null own) $ do
    places <- mapM poolPlace (nub (snd <$> own))
    let named = intercalateAnd (addrToString net . fst <$> own)
        subject = case own of
          [_] -> "The address " <> named <> " is"
          _ -> "The addresses " <> named <> " are"
    throwError $
      subject
        <> " of this wallet, in "
        <> intercalateAnd (placeText <$> places)
        <> ". A sweep is for coins from outside the wallet, which signsweep signs with their private keys. To move coins of this wallet, use sendall or preparetx: --spend-from chooses the coins of an address."

-- | Sign a sweep with private keys: a pending transaction of the wallet,
-- by its nosigHash, or one in a file. A sweep outside the wallet is signed
-- on the network its file names, or on that of --network or of the
-- configuration when the file names none, from its file to another, or in
-- its place.
cmdSignSweep :: Config -> AmountUnit -> Signing -> Maybe KeySource -> Maybe Network -> IO Response
cmdSignSweep cfg unit signing keysM givenM =
  runDBResponse cfg $ do
    (poolM, file@(TxFileRead txFile@(TxFile netM tsd _ _) sourceM)) <- transactionToSign cfg givenM signing
    forM_ poolM $ \pool -> refuseForPrivateKeys signing pool tsd
    net <- case poolM of
      Just pool -> return pool.poolNetwork
      Nothing -> commandNetwork cfg <$> fileNetwork givenM netM
    -- A key file that does not read stops the command here, before the
    -- sweep is shown: its error would come below a screen of transaction
    fromFile <- case keysM of
      Just source@(KeysInFile _) -> Just <$> keysOn net tsd source
      _ -> return Nothing
    signedFile <- signedFileOf signing
    -- Shown before the keys are asked for, as signtx shows it
    showToSign unit signing net poolM txFile
    -- Without a file, the keys are typed: signing cannot do without them
    keys <- maybe (keysOn net tsd KeysOnTerminal) return fromFile
    case poolM of
      Just pool -> txAnswer pool =<< signPoolSweep signing signedFile pool file keys
      Nothing -> do
        (signed, bare) <- liftEither $ signTxWithKeys net tsd [] noneHeld keys
        -- No address of the sweep is the wallet's: none has a label
        writeSigned Nothing sourceM signedFile (labelledTxFile net bare signed)
        ResponseOutsideTx net <$> lift (fillContacts net bare)
  where
    -- The coins of the sweep tell when the keys typed are complete
    keysOn net tsd source = do
      coins <- liftEither $ sweptAddresses net tsd
      readSecKeys terminal net (keysOfCoins (Sweep.keyAddresses net) coins) source

-- | Sign a sweep that pays a portfolio with the private keys of its
-- coins, and keep it signed ('keepSigned'): what signsweep does of such a
-- sweep once it has the keys, and the second step of sweep. The accounts
-- of the portfolio tell which outputs are its own.
signPoolSweep :: Signing -> SignedFile -> Pool -> TxFileRead -> [SecKey] -> ExceptT String (DB IO) TxInfo
signPoolSweep signing signedFile pool file@(TxFileRead txFile _) keys = do
  accounts <- lift $ poolAccounts pool
  keysOfPool <- lift $ poolKeys pool
  (signed, info) <- liftEither $ signTxWithKeys pool.poolNetwork (txFileData txFile) keysOfPool (heldPoolOf (snd <$> accounts)) keys
  keepSigned signing signedFile pool file signed info
