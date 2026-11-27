{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Preparing a payment: preparetx, preparesendall and fees. A payment is
-- built from the coins of the active portfolio on a network
-- ("Haskoin.Wallet.Tx.Building", "Haskoin.Wallet.Tx.SendAll") and kept
-- unsigned among the pending transactions, which locks its coins (the
-- type t'Prepared').
--
-- The questions before a payment are asked here: a fee rate that looks
-- like a typo ('confirmFeeRate'), a wallet that is behind the chain
-- ('confirmBehind'), coins of different tags spent together
-- ('confirmMixing'), and the dust coins of a payment of everything
-- ('coinsWithDust'). send and sendall start with the same steps
-- ('preparePayment', 'prepareSendAll'). bumpfee and the sweeps ask the
-- same questions, and keep what they prepare the same way
-- ('storePrepared').
module Haskoin.Wallet.Commands.Payment
  ( withLabel,
    storePrepared,
    confirmFeeRate,
    behindTheChain,
    confirmBehind,
    confirmMixing,
    chooseFeeRate,
    cmdFees,
    Prepared (..),
    preparedAnswer,
    cmdPrepareTx,
    preparePayment,
    cmdPrepareSendAll,
    prepareSendAll,
    dustOptions,
    coinsWithDust,
  )
where

import Control.Monad (filterM, forM, unless, void, when)
import Control.Monad.Except (ExceptT (..), catchError, throwError, withExceptT)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Class (lift)
import Data.Either (isLeft)
import Data.Foldable (for_)
import Data.List (intercalate, nub, partition)
import Data.Maybe (fromMaybe, isJust, maybeToList)
import Data.String.Conversions (cs)
import Data.Text (Text)
import Data.Time (getCurrentTime)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import Data.Word (Word64)
import Haskoin
import qualified Haskoin.Store.Data as Store
import Haskoin.Wallet.Amounts (AmountUnit, UnitChoice, paymentUnit, showAmount, showUnit, unitOn)
import Haskoin.Wallet.Cli.Command
import Haskoin.Wallet.Cli.PrettyPrinter.Base (agoText, renderOn)
import Haskoin.Wallet.Cli.PrettyPrinter.Payment (dustReportDoc, feeRatesDoc, mixingDoc)
import Haskoin.Wallet.Cli.Prompt
import Haskoin.Wallet.Cli.Response
import Haskoin.Wallet.Commands.Run (runDBResponse, runResponse)
import Haskoin.Wallet.Commands.Scope (shownAmong)
import Haskoin.Wallet.Commands.ShortId (walletCoinOf)
import Haskoin.Wallet.Commands.TxFile (writeOutput)
import Haskoin.Wallet.Config
import Haskoin.Wallet.Database
import Haskoin.Wallet.Database.Account
import Haskoin.Wallet.Database.Address
import Haskoin.Wallet.Database.BestBlock (BestBlock (..), getBest)
import Haskoin.Wallet.Database.Coin
import Haskoin.Wallet.Database.PendingTx
import Haskoin.Wallet.Database.Portfolio
import Haskoin.Wallet.Database.Tag
import Haskoin.Wallet.Database.TxInfo
import Haskoin.Wallet.FileIO
import Haskoin.Wallet.Keys
import Haskoin.Wallet.ShortId
import Haskoin.Wallet.Store
import Haskoin.Wallet.Tags (Tag, tagText)
import Haskoin.Wallet.Tx.Building (ChangeTo (..), CoinControl (..), PreparedPayment (..), buildTxSignData)
import Haskoin.Wallet.Tx.CoinSelection (coinKind, coinsDoNotCover)
import Haskoin.Wallet.Tx.Dust (DustReport (dustCoins), SizedCoin (coin), dustQuestion, isDustCoin, spendingAmounts)
import Haskoin.Wallet.Tx.FeeRates
import Haskoin.Wallet.Tx.LockTime (isStale, lockTimeFor)
import Haskoin.Wallet.Tx.Mixing (SpentGroup (..), joinsOutsideTags, spentGroups)
import Haskoin.Wallet.Tx.Recipient (Share, checkChangeApart, checkPaidOnce, checkSharing, readPayment, readRecipient)
import Haskoin.Wallet.Tx.SendAll (buildSendAllSignData, noCoinsToSend, sendAllDustReport, walletSized)
import Haskoin.Wallet.Tx.SignData
import Haskoin.Wallet.Tx.Size (checkFeeRate, highFeeRate)
import Haskoin.Wallet.Util
import Numeric.Natural (Natural)
import System.IO (stderr)
import System.Random (initStdGen)

-- | The signing data of a transaction just prepared, with the label of
-- --label, if given
withLabel :: Maybe Text -> TxSignData -> TxSignData
withLabel labelM tsd = tsd {txSignDataLabel = fromMaybe noLabel labelM}

-- | Keep a transaction of the pool just prepared, among its pending
-- transactions and in the --output file when there is one, and give its
-- summary. The address --change-back named, if any, may be
-- used or held by another pending transaction ('importPreparedTx'). The
-- tags are those the payment named with --spend-from-tag, whose locks it
-- opens.
--
-- The summary is the one of the transaction as the wallet now holds it
-- ('pendingSummary'), so that the answer shows the time it was created,
-- as pendingtxs and show do from then on. The signing data alone has no
-- time, and the answer started with "time unknown" before.
storePrepared :: Pool -> Maybe Address -> [Tag] -> Maybe FilePath -> TxSignData -> ExceptT String (DB IO) TxInfo
storePrepared pool changeBackM opened output tsd = do
  pendingHash <- importPreparedTx pool changeBackM opened tsd
  info <- pendingSummary =<< requirePendingTx pendingHash
  writeOutput (Just pool) pool.poolNetwork output info tsd
  return info

-- | Ask before preparing a transaction at a fee rate above 'highFeeRate',
-- unless --yes: a rate that high is more often a typo than a choice. A fee
-- rate no transaction can be built with is refused first, without a
-- question.
confirmFeeRate :: Bool -> Natural -> ExceptT String (DB IO) ()
confirmFeeRate yes feeByte = do
  liftEither $ checkFeeRate feeByte
  when (feeByte > highFeeRate && not yes) $ do
    accepted <-
      askYes terminal (ScriptOption "--yes") $
        "The fee rate of " <> show feeByte <> " satoshis per virtual byte is above " <> show highFeeRate
          <> ", the most hw takes without asking. Prepare the transaction anyway?"
    unless accepted $ throwError "The transaction was not prepared: give a lower --fee-rate"

-- | Whether the wallet is behind the chain on the network: its best block,
-- as the last sync left it, was mined more than eight hours ago
-- ('isStale'), or at a time the wallet does not know. Left what to tell
-- the user then, since a sync may also find coins spent or received since.
-- It is a sentence for each line of the screen: the fact, the block that
-- shows it, and what a sync may change. On one line they ran past the
-- width of a terminal, with the question after them.
-- Right the height of the best block otherwise, which the lock time of a
-- new transaction is drawn from ('lockTimeFor'), or none before the first
-- sync: a wallet that never synced has no coins to spend, and no height to
-- be behind.
behindTheChain :: Network -> ExceptT String (DB IO) (Either [String] (Maybe BlockHeight))
behindTheChain net = do
  bestM <- getBest net
  now <- liftIO getCurrentTime
  return $ case bestM of
    Nothing -> Right Nothing
    Just best
      | Just time <- best.blockTime,
        not (isStale now (Just time)) ->
          Right (Just best.blockHeight)
      | Just time <- best.blockTime ->
          Left
            [ "The wallet is behind the chain.",
              "Its best block, " <> show best.blockHeight <> ", was mined " <> agoText now (posixSecondsToUTCTime (fromIntegral time)) <> ".",
              syncMayFind
            ]
      | otherwise ->
          Left
            [ "The wallet may be behind the chain.",
              "It does not know when its best block, " <> show best.blockHeight <> ", was mined.",
              syncMayFind
            ]
  where
    syncMayFind = "A sync may find coins spent or received since."

-- | Ask whether to prepare a transaction while the wallet is behind the
-- chain, told why, unless --yes. The transaction then gets a lock time of
-- 0, as Bitcoin Core and Electrum give one then. A no stops the command:
-- the alternative is to sync first.
--
-- Each sentence of the reason has a line of its own, and the question
-- comes on the last line, where the answer is typed. The dust coins of
-- sendall are told before their question in the same way.
confirmBehind :: Bool -> [String] -> ExceptT String (DB IO) ()
confirmBehind yes why =
  unless yes $ do
    liftIO $ mapM_ (tell terminal) why
    agreed <- askYes terminal (ScriptOption "--yes") "Prepare the transaction anyway, with a lock time of 0?"
    unless agreed $ throwError "The transaction was not prepared: run sync first"

-- | Ask before a transaction of the pool is kept that joins groups of
-- coins the user keeps apart with tags, unless --yes: the coins it spends
-- do not all have the same tags, as a kyc coin and a p2p coin
-- ("Haskoin.Wallet.Tx.Mixing"). The coin selection picks coins by their
-- value and knows nothing of tags, so the user learns it here, while the
-- coins can still be chosen otherwise.
--
-- The groups are told first, each on a line, then the question on the
-- last line, as given, with what the command fails with after a no. The
-- transaction is given as it was built, before anything is kept: a no
-- leaves no pending transaction, no change address taken and no --output
-- file. The command calls it after its other questions, since the coins
-- are only known once the transaction is built, which the answers to
-- those decide.
--
-- The rule given says whether the groups are asked about: 'joinsOutsideTags'
-- for a payment, and for a replacement whether the coins it adds bring a
-- group ('Haskoin.Wallet.Tx.Mixing.addsGroup').
--
-- --yes says nothing of the groups. The answer of the command shows the
-- tags under each address the transaction spends from, as it always does.
confirmMixing :: Bool -> AmountUnit -> Pool -> ([SpentGroup] -> Bool) -> String -> String -> TxSignData -> ExceptT String (DB IO) ()
confirmMixing yes unit pool asksAbout asked declined tsd =
  unless yes $ do
    groups <- spentGroups <$> txInfoOf pool tsd
    when (asksAbout groups) $ do
      liftIO $ renderOn stderr (mixingDoc unit groups)
      agreed <- askYes terminal (ScriptOption "--yes") asked
      unless agreed $ throwError declined

-- | 'confirmMixing' for a payment just built, which preparetx, send,
-- preparesendall and sendall ask the same way. The tags are those the
-- payment named with --spend-from-tag: coins that all have them are the
-- group the user asked for ('joinsOutsideTags'). A no says how to choose
-- the coins of one group.
confirmPaymentMixing :: Bool -> AmountUnit -> Pool -> [Tag] -> TxSignData -> ExceptT String (DB IO) ()
confirmPaymentMixing yes unit pool named =
  confirmMixing yes unit pool (joinsOutsideTags named) "Prepare the transaction anyway?" $
    "The transaction was not prepared: --spend-from-tag, --spend-from and --coin choose the coins it spends"

-- | The height the lock time of a new transaction is drawn from, which
-- 'behindTheChain' gives: none when the wallet is behind the chain and the
-- user agrees to go on ('confirmBehind'), for a lock time of 0
lockTimeHeight :: Bool -> Network -> ExceptT String (DB IO) (Maybe BlockHeight)
lockTimeHeight yes net = do
  behind <- behindTheChain net
  case behind of
    Left why -> confirmBehind yes why >> return Nothing
    Right heightM -> return heightM

-- | The fee rate of a transaction, in satoshis per virtual byte: the
-- number given, or one of the rates recent blocks paid, read from the
-- store, at the level given. Without either, hw shows those rates and
-- asks, taking a level or a number as --fee-rate does, the median by
-- default. A rate below the lowest the transaction can pay, which a
-- replacement has, is not offered, and a number below it is asked again.
-- The rates and the question go to stderr, to keep the --json answer
-- clean. hw never guesses a rate: without a --fee-rate, a store that
-- cannot be reached stops the command.
chooseFeeRate :: Config -> Network -> Store -> Natural -> Maybe FeeRateChoice -> ExceptT String (DB IO) Natural
chooseFeeRate _ _ _ _ (Just (FeeRateOf rate)) = return rate
chooseFeeRate cfg net store lowest choice = do
  rates <- fromStore $ withExceptT (<> ". Give a --fee-rate to go on without the rates recent blocks paid") $ recentFeeRates (configParallelRequests cfg) store
  case choice of
    Just (FeeLevelOf level) -> do
      let rate = rateAt level rates
      when (rate < lowest) $ throwError (levelTooLow level rate)
      liftIO . tell terminal $ "The " <> cs (levelName level) <> " rate recent blocks paid: " <> show rate <> " satoshis per virtual byte"
      return rate
    _ -> do
      now <- liftIO getCurrentTime
      liftIO $ renderOn stderr (feeRatesDoc net now rates)
      askAnswer terminal (ScriptOption "--fee-rate") (feeRateQuestion lowest rates) (feeRateAnswer lowest rates)
  where
    levelTooLow level rate =
      "The " <> cs (levelName level) <> " rate recent blocks paid, " <> show rate
        <> " satoshis per virtual byte, is below " <> show lowest <> ": give a --fee-rate of at least " <> show lowest

-- | Show the fee rates recent blocks paid on the network, read from the
-- store. No wallet is needed.
cmdFees :: Config -> Maybe Network -> IO Response
cmdFees cfg netM =
  runResponse $ do
    let net = commandNetwork cfg netM
    storeOn <- liftIO $ webStores cfg
    rates <- recentFeeRates (configParallelRequests cfg) (storeOn net)
    return $ ResponseFees net rates

-- | A transaction just prepared, as the wallet keeps it: what preparetx,
-- preparesendall and bumpfee answer with, and what send and sendall go on
-- from
data Prepared = Prepared
  { -- | The pool that pays it
    preparedPool :: !Pool,
    -- | Its nosigHash, which names it among the pending transactions
    -- until it is signed
    preparedHash :: !TxHash,
    -- | The transaction as the wallet holds it
    preparedInfo :: !TxInfo,
    -- | The coins it leaves in the portfolio, each as coins shows it, and
    -- why it stays
    preparedLeft :: ![LeftCoin]
  }

-- | Keep a transaction of the pool just prepared, as 'storePrepared' does,
-- with the coins it leaves in the portfolio. The tags are those the
-- payment named with --spend-from-tag ('storePrepared').
keepPreparedLeaving :: Pool -> Maybe Address -> [Tag] -> Maybe FilePath -> TxSignData -> [(OutPoint, LeftReason)] -> ExceptT String (DB IO) Prepared
keepPreparedLeaving pool changeBackM opened output tsd leftOut = do
  left <- leftCoinsOf pool opened leftOut
  info <- storePrepared pool changeBackM opened output tsd
  return (Prepared pool (nosigHash (txSignDataTx tsd)) info left)

-- | A transaction just prepared, with the tags of --tag put on it. This is
-- what the command tag does with the hash of the transaction afterwards,
-- done at the moment the user knows what the payment is for. A tag put
-- later is the step that is skipped.
--
-- The tags are kept under the nosigHash, as those of any pending
-- transaction, and go to its txid when a sync finds it on the network
-- ('retirePendingTx'). A tag that is not in the database yet is made, as
-- the command tag makes one. The command is one transaction of the
-- database, so a payment that fails after this leaves neither the tags nor
-- a new tag behind.
--
-- The summary is read again with its tags, since the answer shows them,
-- and send and sendall show the transaction from it before the mnemonic.
-- The --output file is written before, and holds no tag: a file carries
-- the label of a transaction alone.
taggedPrepared :: [Tag] -> Prepared -> ExceptT String (DB IO) Prepared
taggedPrepared [] prepared = return prepared
taggedPrepared tags prepared = lift $ do
  tagTx prepared.preparedPool prepared.preparedHash tags
  info <- fillTxInfoTags prepared.preparedPool prepared.preparedInfo
  return prepared {preparedInfo = info}

-- | A transaction just prepared, with the tags of the group of its coins
-- on its new change address, when --tag-change asks for them
-- ("Haskoin.Wallet.Database.PendingTx".'changeTakesTags'). The summary is
-- read again with them, since the answer shows the tags of the change
-- under its address. A payment without change tags nothing. A change
-- that takes no tag, because the coins spent share none, is told on
-- stderr: the option was typed to keep the change in a group, and the
-- answer would otherwise look like that of any payment.
changeTaggedPrepared :: Bool -> Prepared -> ExceptT String (DB IO) Prepared
changeTaggedPrepared False prepared = return prepared
changeTaggedPrepared True prepared = do
  taken <- lift $ changeTakesTags prepared.preparedPool prepared.preparedInfo.txInfoSummary
  when (any (null . snd) taken) . liftIO $
    tell terminal "--tag-change put no tag on the change address: the coins the payment spends share no tag"
  info <- lift $ fillTxInfoTags prepared.preparedPool prepared.preparedInfo
  return prepared {preparedInfo = info}

-- | The answer that shows a transaction just prepared, with the coins it
-- leaves in the portfolio
preparedResponse :: Prepared -> ExceptT String (DB IO) Response
preparedResponse prepared = do
  place <- poolPlace prepared.preparedPool
  ResponsePrepared place prepared.preparedInfo prepared.preparedLeft <$> activePortfolioRef

-- | Keep a transaction of the pool just prepared, as 'storePrepared' does,
-- and answer with the coins it leaves in the portfolio, each as coins
-- shows it, and why it stays
preparedAnswer :: Pool -> Maybe Address -> Maybe FilePath -> TxSignData -> [(OutPoint, LeftReason)] -> ExceptT String (DB IO) Response
preparedAnswer pool changeBackM output tsd leftOut =
  preparedResponse =<< keepPreparedLeaving pool changeBackM [] output tsd leftOut

-- | The pool a transaction is prepared from, with the addresses that coin
-- control chose, read on its network. The pool is that of the active
-- portfolio on a network: the --network, or else the one the coins of
-- --coin and the addresses of --spend-from are on, or else the default
-- network. Those coins and addresses must be in the active portfolio: one
-- in another portfolio is refused, with the portfolio that holds it and
-- the switch to it. A payment from another portfolio than the one the
-- user works in would surprise. The test networks share their keys, so
-- the coins and addresses may be on several networks: --network or the
-- default network then names one ('paymentNetwork'). Each --spend-from
-- address must be one of the pool: the address counts, not its text, so a
-- Bitcoin Cash address is found without its prefix, or in its legacy
-- form.
--
-- A --spend-from path, such as bip44\/1\/0, is one of the active portfolio.
-- It tells the networks where the portfolio has an address at that path,
-- as an address tells the networks that hold it ('rowsAtPath'). The
-- address spent from is then the one at the path on the network of the
-- payment.
--
-- A --spend-from-tag tag chooses every address of the pool that has it,
-- as if each had its own --spend-from. It tells the networks where the
-- active portfolio has an address with the tag. A word that is no tag of
-- the database is refused ('requireTags'), and so is a tag that no
-- address of the active portfolio has, on the network of the payment:
-- the user means to spend something, and a payment built from other coins
-- would not be the one asked for.
--
-- Only the networks hw shows count. A coin or an address the wallet has
-- on hidden networks alone is refused, with the command that shows its
-- network ('shownAmong').
--
-- The addresses named and those of the tags come apart. --coin-type
-- refuses an address named that is of the other type, since the two
-- options disagree about it. A tag is a group, which may hold addresses
-- of both types, so --coin-type narrows it instead ('chosenAddresses').
controlledPool :: Config -> Maybe Network -> [OutPoint] -> [AddressNamed] -> [Tag] -> ExceptT String (DB IO) (Pool, [Address], [Address])
controlledPool cfg netM outpoints spendFrom spendFromTags = do
  portfolio <- requireActivePortfolio
  _ <- requireTags spendFromTags
  accounts <- onShownNetworks cfg (accountNetwork . snd) <$> lift (portfolioAccountRows portfolio.portfolioId)
  byTag <- forM spendFromTags $ \tag -> do
    tagged <- lift $ filterM (\(accId, _) -> not . null <$> taggedAddressRows accId [tag]) accounts
    when (null tagged) $
      throwError ("No address of " <> portfolioText portfolio <> " has the tag " <> cs (tagText tag) <> ": hw addresses --tag " <> cs (tagText tag) <> " lists those of the active portfolio")
    return (nub (accountNetwork . snd <$> tagged))
  byCoin <- forM outpoints $ \outpoint -> do
    let what = "The coin " <> shortCoinText outpoint
    holders <- shownAmong cfg what (.poolNetwork) . map (accountPool . snd) =<< coinHolders [outpoint]
    networksInActive what portfolio holders
  byAddress <- forM spendFrom $ \case
    NamedByAddress typed -> do
      let what = "The address " <> cs typed
      holders <- shownAmong cfg what (.poolNetwork) . map (\(_, _, acc) -> accountPool acc) =<< accountsHolding (\net -> maybeToList (textToAddr net typed))
      networksInActive what portfolio holders
    NamedByPath kind addrType index -> onShownNetworks cfg id . map snd <$> lift (rowsAtPath cfg portfolio (Just kind) addrType index)
  net <- liftEither $ paymentNetwork cfg netM (nub (concat (byCoin <> byAddress <> byTag)))
  let pool = portfolioPool portfolio net
      place = portfolioPlace portfolio net
  addrs <- forM spendFrom $ \named -> do
    addrM <- poolAddressNamed pool named
    case (addrM, named) of
      (Just addr, _) -> return addr
      (Nothing, NamedByAddress typed) -> throwError ("The address " <> cs typed <> " is not in " <> placeText place)
      (Nothing, NamedByPath {}) -> throwError (placeText place <> " has no address at " <> addressNamedText named)
  tagged <- forM spendFromTags $ \tag -> do
    held <- map fst <$> taggedPoolAddresses pool tag
    when (null held) $
      throwError ("No address of " <> placeText place <> " has the tag " <> cs (tagText tag))
    return held
  return (pool, addrs, nub (concat tagged))

-- | The addresses coin control spends from: those named with
-- --spend-from, and those of the tags of --spend-from-tag that --coin-type
-- keeps. An address of a tag that is of the other type is left out
-- without a word, as a filter leaves out a coin of every coin of the
-- portfolio: --spend-from-tag kyc --coin-type bip44 spends the bip44
-- coins of the tag. An address named is kept whatever its type, and its
-- coins are refused later when the filter leaves them out
-- ('refuseOtherType').
chosenAddresses :: Maybe AccountKind -> [Address] -> [Address] -> [Address]
chosenAddresses coinTypeM named tagged =
  nub (named <> [addr | addr <- tagged, maybe True ((== addressKind addr) . Just) coinTypeM])

-- | The addresses of a pool that have a tag, each with its path in the
-- pool, in the order of the accounts and of their paths: what
-- --spend-from-tag spends from
taggedPoolAddresses :: Pool -> Tag -> ExceptT String (DB IO) [(Address, PoolPath)]
taggedPoolAddresses pool tag = do
  accounts <- lift $ poolAccounts pool
  fmap concat . forM accounts $ \(accId, acc) -> do
    rows <- lift $ taggedAddressRows accId [tag]
    forM rows $ \(internal, index, text) -> do
      addr <- liftEither $ textToAddrE pool.poolNetwork text
      return (addr, PoolPath (accountKind acc) (chainPath internal index))

-- | The networks of the active portfolio that hold a coin or an address
-- the user gave, among the pools that hold it, or the refusal of one that
-- only another portfolio holds, with the switch to that portfolio. The
-- text names what was given, as in "The coin a1075db5:1". Nothing at all
-- holds a coin the wallet does not have, which coin control refuses
-- later, with the reason.
networksInActive :: String -> Portfolio -> [Pool] -> ExceptT String (DB IO) [Network]
networksInActive what active holders =
  case ([pool.poolNetwork | pool <- holders, pool.poolPortfolio == active.portfolioId], holders) of
    ([], holder : _) -> do
      other <- getPortfolio holder.poolPortfolio
      throwError (what <> " is in " <> portfolioText other <> ", not in the active portfolio: run " <> switchCommandText (portfolioRef other) <> " first")
    (networks, _) -> return networks

-- | The network of a payment: the --network, or else the one the coins
-- and addresses of coin control are on, given as the networks that hold
-- them, or else the default network. When they are on several, as the
-- test networks share their keys, the default network is taken if it is
-- one of them. A --network that holds none of them is refused: the coins
-- would not be found there.
paymentNetwork :: Config -> Maybe Network -> [Network] -> Either String Network
paymentNetwork cfg netM held = case (netM, held) of
  (Just net, _)
    | null held || net `elem` held -> Right net
    | otherwise -> Left ("The coins, addresses or tags given are on " <> networksText held <> ", not on the --network " <> net.name)
  (Nothing, []) -> Right (configDefaultNetwork cfg)
  (Nothing, [only]) -> Right only
  (Nothing, several)
    | configDefaultNetwork cfg `elem` several -> Right (configDefaultNetwork cfg)
    | otherwise -> Left ("The coins, addresses or tags given are on " <> networksText several <> ": give the --network")

-- | Refuse the coins chosen with --coin or --spend-from that --coin-type
-- leaves out. The filter never adds a coin to those the choosers chose.
-- It only narrows them, and a chosen coin it would drop means the two
-- options disagree. Dropping the coin without a word would hide that.
refuseOtherType :: Maybe AccountKind -> [Store.Unspent] -> ExceptT String (DB IO) ()
refuseOtherType kindM chosen =
  for_ kindM $ \kind ->
    for_ (filter ((/= kind) . coinKind) chosen) $ \coin ->
      throwError $
        "The coin " <> shortCoinText coin.outpoint <> " is a " <> cs (kindName (coinKind coin)) <> " coin, and --coin-type " <> cs (kindName kind) <> " spends " <> cs (kindName kind) <> " coins only"

-- | Prepare a payment from the active portfolio on a network, unsigned,
-- and answer with it: 'preparePayment', which send starts with too, says
-- how the options are read.
cmdPrepareTx :: Config -> UnitChoice -> PrepareTx -> IO Response
cmdPrepareTx cfg choice opts =
  runDBResponse cfg $ do
    storeOn <- liftIO $ webStores cfg
    preparedResponse =<< preparePayment cfg storeOn choice opts

-- | Build the payment the options ask for, and keep it as a pending
-- transaction of the active portfolio, unsigned, which locks its coins.
-- It may spend the coins of both types of address together. This is all
-- that preparetx does but its answer, and what send starts with. The two
-- commands share it, so they make the same transaction of the same
-- options, with the same questions and the same refusals. The stores are
-- those of the command. The fee rates of recent blocks are read from one
-- when the options give no rate.
--
-- Coin control has choosers and filters. The choosers, --coin,
-- --spend-from and --spend-from-tag, add up: the coins chosen are those
-- of every --coin, those at every --spend-from address, and those at
-- every address that has a --spend-from-tag tag. The payment spends all of those
-- that can be spent, and only them. With --select, the strategy selects
-- among them instead, as it does among every coin of the pool without a
-- chooser. The filters, --min-conf and --coin-type, narrow the coins down:
-- every coin of the pool, or the coins chosen. They never add a coin, so
-- --coin-type with a --spend-from of the same type spends the coins at
-- that address alone. The answer lists the coins chosen that cannot be
-- spent, and why. The chosen coins --select does not need are not listed,
-- like the coins a selection leaves without a chooser.
--
-- The tags of --spend-from-tag are also the keys of the payment: they
-- open the lock of each of them that locks its coins
-- ("Haskoin.Wallet.Database.Coin".'spendableCoinsOpening'). The coins of
-- every other tag that locks stay unspendable, for the strategy and for
-- --coin and --spend-from alike, and the pending transaction is kept
-- with the same tags opened (@keepPreparedLeaving@).
--
-- The change goes to a new change address of the portfolio, of the type
-- the builder works out ('Haskoin.Wallet.Tx.Building.changeKindRule').
-- --change-back sends it back to an address of the portfolio on the
-- network of the payment instead: the one it names, by itself or by its
-- path, or with largest that of the largest coin the payment spends, of
-- the type of the change, or with smallest that of the smallest one.
--
-- --tag-change gives the new change address the tags every coin the
-- payment spends has, once the transaction is kept (@changeTaggedPrepared@).
-- So --spend-from-tag with it keeps the change in the group it spends
-- from. Without it the change address takes no tag. It is refused with
-- --change-back: the address the change goes back to keeps its own tags.
--
-- The coins of the payment are known once it is built. When they do not
-- all have the same tags, the user is asked before anything is kept
-- (@confirmPaymentMixing@): the selection knows nothing of tags, and one
-- transaction joins for good the groups the tags keep apart.
--
-- --label and --tag say what the payment is for. The label goes into the
-- signing data, and with it to the --output file. The tags are put on the
-- pending transaction (@taggedPrepared@).
--
-- The amounts are read in the unit of the command, which must be a unit
-- of the network of the payment: --unit bch is refused on a network of
-- Bitcoin, and --unit btc on one of Bitcoin Cash ('paymentUnit').
preparePayment :: Config -> (Network -> Store) -> UnitChoice -> PrepareTx -> ExceptT String (DB IO) Prepared
preparePayment cfg storeOn choice opts = do
  coins <- mapM walletCoinOf opts.coins
  (pool, namedAddrs, taggedAddrs) <- controlledPool cfg opts.network coins opts.spendFrom opts.spendFromTags
  let net = pool.poolNetwork
      spendFrom = chosenAddresses opts.coinType namedAddrs taggedAddrs
      controlled = not (null coins && null namedAddrs && null opts.spendFromTags)
  unit <- liftEither (paymentUnit net choice)
  when (opts.select && not controlled) $
    throwError "--select needs coins to select among: give --coin, --spend-from or --spend-from-tag"
  when (opts.tagChange && isJust opts.changeBack) $
    throwError "--tag-change tags a new change address, and --change-back sends the change to an address that keeps its tags: give one of them"
  -- An address named has a type of its own. largest and smallest pick a
  -- coin of the type of the change, which --change-type may set.
  case (opts.changeBack, opts.changeType) of
    (Just (BackToNamed _), Just _) ->
      throwError "--change-back ADDRESS names the address of the change, which has a type: it takes no --change-type. --change-back largest and smallest do."
    _ -> return ()
  -- A type the network does not have is refused before anything is asked
  for_ opts.coinType $ liftEither . checkKindOnNetwork net
  for_ opts.changeType $ \kind -> do
    liftEither $ checkKindOnNetwork net kind
    void $ poolAccountOf pool kind
  changeTo <- case opts.changeBack of
    Nothing -> return (NewChangeAddress opts.changeType)
    Just BackToLargestCoin -> return (ChangeToLargestCoin opts.changeType)
    Just BackToSmallestCoin -> return (ChangeToSmallestCoin opts.changeType)
    Just (BackToNamed named) -> do
      addrM <- poolAddressNamed pool named
      place <- poolPlace pool
      let refused = "The change can only go back to an address of " <> placeText place <> ", and "
      case (addrM, named) of
        (Just addr, _) -> return (ChangeBackTo addr)
        (Nothing, NamedByAddress typed) -> throwError (refused <> cs typed <> " is not one")
        (Nothing, NamedByPath {}) -> throwError (refused <> "it has none at " <> addressNamedText named)
  -- Before the transaction is built, not after
  for_ opts.output checkFileToWrite
  recipients <- liftEither $ mapM (readPayment net unit) opts.recipients
  liftEither $ checkPaidOnce net opts.repeatAddress recipients
  heightM <- lockTimeHeight opts.yes net
  feeByte <- chooseFeeRate cfg net (storeOn net) 1 opts.feeRate
  confirmFeeRate opts.yes feeByte
  (control, left) <-
    if not controlled
      then return (NoCoinControl, [])
      else do
        spendable <- spendableCoinsOpening opts.spendFromTags pool opts.minConf
        unspendable <- unspendableCoinsOpening opts.spendFromTags pool opts.minConf
        named <- lockNames opts.spendFromTags net
        (chosen, held) <- liftEither $ chosenCoins coins spendFrom named spendable unspendable
        refuseOtherType opts.coinType (chosen <> (fst <$> held))
        let outpoints = (.outpoint) <$> chosen
        return (if opts.select then SelectAmongChosen outpoints else SpendChosen outpoints, leftOutpoints [] held [])
  gen <- liftIO initStdGen
  let (lockTime, buildGen) = lockTimeFor heightM gen
  prepared <-
    buildTxSignData cfg buildGen opts.strategy pool opts.coinType recipients feeByte lockTime opts.feePayer opts.minConf opts.spendFromTags control changeTo
      `catchError` tellingHeldBack unit pool opts.minConf opts.spendFromTags coinsDoNotCover
  -- The change has its address only now, and nothing is kept yet
  liftEither $ checkChangeApart net opts.repeatAddress recipients prepared.changePaid
  confirmPaymentMixing opts.yes unit pool opts.spendFromTags prepared.signData
  taggedPrepared opts.tags
    =<< changeTaggedPrepared opts.tagChange
    =<< keepPreparedLeaving pool prepared.changeChosen opts.spendFromTags opts.output (withLabel opts.label prepared.signData) left

-- | Fail as a payment failed, and, when it failed for the reason given,
-- say what the portfolio holds on the network that it cannot spend now.
--
-- "The coins do not cover the amount and the fee" was all the user read
-- when the portfolio held enough: a payment of somebody else that waits
-- for its block, a coinbase under a hundred blocks, coins locked by a
-- pending transaction, by the user or by a tag the payment did not name
-- (the tags given are those it named). The balance said one thing and the refusal
-- another. The sentence added gives the amount and points to coins, which
-- says why each coin waits.
--
-- The coins that a transaction already sent took have a sentence of
-- their own, which points to sync ('takenSentence').
tellingHeldBack :: AmountUnit -> Pool -> Natural -> [Tag] -> String -> String -> ExceptT String (DB IO) a
tellingHeldBack unit pool minConf opened reason failure
  | failure /= reason = throwError failure
  | otherwise = do
      (waiting, taken) <- notSpendableNow opened pool minConf
      let net = pool.poolNetwork
      throwError . intercalate ". " $
        failure : maybeToList (heldBackSentence unit net <$> waiting) <> maybeToList (takenSentence unit net <$> taken)

-- | What a portfolio holds on a network and cannot spend now, with the
-- command that says why
heldBackSentence :: AmountUnit -> Network -> HeldBack -> String
heldBackSentence unit net back =
  "The portfolio holds " <> coinsWorth unit back.heldBackValue <> " more on " <> net.name <> ", in " <> coinCount back <> " it cannot spend now: hw coins says why"

-- | What the transactions already sent took of a portfolio on a network,
-- with the command that brings what they left.
--
-- hw learns the change of a payment from the next sync, so a second
-- payment right after the first found no coin, and read only that the
-- portfolio held a coin it could not spend. The coin was spent, and a
-- sync was all it took. The change is promised only when one of the
-- transactions pays the portfolio.
takenSentence :: AmountUnit -> Network -> TakenBySent -> String
takenSentence unit net taken =
  (if one then "A transaction" else show taken.takenBy <> " transactions")
    <> (" sent from this wallet took " <> coinsWorth unit taken.takenCoins.heldBackValue <> " of the portfolio on " <> net.name)
    <> (", in " <> coinCount taken.takenCoins <> ": run hw sync to " <> next)
  where
    one = taken.takenBy == 1
    next
      | not taken.takenPaysBack = "bring the coins up to date"
      | one = "get its change"
      | otherwise = "get their change"

-- | An amount of satoshis with its unit, as a refusal writes it
coinsWorth :: AmountUnit -> Word64 -> String
coinsWorth unit value = cs (showAmount unit (toInteger value)) <> " " <> cs (showUnit unit (toInteger value))

-- | How many coins were counted: \"1 coin\", \"3 coins\"
coinCount :: HeldBack -> String
coinCount back = if back.heldBackCoins == 1 then "1 coin" else show back.heldBackCoins <> " coins"

-- | Prepare a payment of all the coins of the active portfolio on a
-- network, unsigned, and answer with it: 'prepareSendAll', which sendall
-- starts with too, says which coins are sent and how the options are
-- read.
cmdPrepareSendAll :: Config -> UnitChoice -> PrepareSendAll -> IO Response
cmdPrepareSendAll cfg choice opts =
  runDBResponse cfg $ do
    storeOn <- liftIO $ webStores cfg
    preparedResponse =<< prepareSendAll cfg storeOn choice opts

-- | Build a transaction sending the coins of the portfolio on a network
-- to the recipients, with no change, and keep it as a pending transaction
-- of the active portfolio, unsigned, which locks its coins. This is all
-- that preparesendall does but its answer, and what sendall starts with.
-- The two commands share it, as preparetx and send share
-- 'preparePayment', so they make the same transaction of the same
-- options, with the same questions and the same refusals. The stores are
-- those of the command. The fee rates of recent blocks are read from one
-- when the options give no rate.
--
-- The coins sent are all those the portfolio can spend now, of both
-- types, or those that the choosers of coin control chose, as for
-- preparetx. The filters narrow them down as for preparetx: --coin-type
-- sends the coins of one type. Without a chooser, the coins
-- worth less than their fee are spent or left as the user answers, or as
-- --spend-dust or --leave-dust says. The transaction prepared lists every
-- coin left in the portfolio on the network, and why. See
-- "Haskoin.Wallet.Tx.SendAll".
--
-- The user is asked before coins that do not all have the same tags are
-- sent together, as for preparetx (@confirmPaymentMixing@).
--
-- --label and --tag say what the transaction is for, as for preparetx
-- (@taggedPrepared@).
--
-- The fixed amounts are read in the unit of the command, which must be a
-- unit of the network, as for preparetx ('paymentUnit').
prepareSendAll :: Config -> (Network -> Store) -> UnitChoice -> PrepareSendAll -> ExceptT String (DB IO) Prepared
prepareSendAll cfg storeOn choice opts = do
  coins <- mapM walletCoinOf opts.coins
  (pool, namedAddrs, taggedAddrs) <- controlledPool cfg opts.network coins opts.spendFrom opts.spendFromTags
  let net = pool.poolNetwork
      spendFrom = chosenAddresses opts.coinType namedAddrs taggedAddrs
      controlled = not (null coins && null namedAddrs && null opts.spendFromTags)
  unit <- liftEither (paymentUnit net choice)
  for_ opts.coinType $ liftEither . checkKindOnNetwork net
  -- Before the transaction is built, not after
  for_ opts.output checkFileToWrite
  recipients <- liftEither $ mapM (readRecipient net unit) opts.recipients
  liftEither $ checkSharing recipients
  liftEither $ checkPaidOnce net opts.repeatAddress recipients
  when (controlled && isJust opts.dustCoins) $
    throwError "The coins chosen with --coin, --spend-from or --spend-from-tag are all sent: give --spend-dust or --leave-dust without them"
  heightM <- lockTimeHeight opts.yes net
  feeByte <- chooseFeeRate cfg net (storeOn net) 1 opts.feeRate
  confirmFeeRate opts.yes feeByte
  spendable <- spendableCoinsOpening opts.spendFromTags pool opts.minConf
  unspendable <- unspendableCoinsOpening opts.spendFromTags pool opts.minConf
  (sent, left) <-
    if controlled
      then do
        named <- lockNames opts.spendFromTags net
        (sent, held) <- liftEither $ chosenCoins coins spendFrom named spendable unspendable
        refuseOtherType opts.coinType (sent <> (fst <$> held))
        return (sent, leftOutpoints [] unspendable (notSent sent spendable))
      else do
        -- The coins of the other type stay, whatever they are worth
        let (ofType, others) = partition (\coin -> maybe True (== coinKind coin) opts.coinType) spendable
        sent <- coinsWithDust opts.dustCoins feeByte recipients walletSized (askAboutDust pool feeByte recipients (not (null unspendable && null others))) ofType
        return (sent, leftOutpoints (notSent sent ofType) unspendable [] <> leftOfOtherType others)
  gen <- liftIO initStdGen
  let (lockTime, buildGen) = lockTimeFor heightM gen
  tsd <-
    buildSendAllSignData buildGen pool sent recipients feeByte lockTime
      `catchError` tellingHeldBack unit pool opts.minConf opts.spendFromTags noCoinsToSend
  confirmPaymentMixing opts.yes unit pool opts.spendFromTags tsd
  taggedPrepared opts.tags =<< keepPreparedLeaving pool Nothing opts.spendFromTags opts.output (withLabel opts.label tsd) left
  where
    -- The spendable coins the transaction does not send
    notSent sent = filter (`notElem` sent)
    -- The report on the dust coins of the portfolio and the question, on
    -- stderr, to keep the --json answer clean. Each coin is shown as coins
    -- shows it.
    askAboutDust :: Pool -> Natural -> [(Address, Share)] -> Bool -> [Store.Unspent] -> [Store.Unspent] -> ExceptT String (DB IO) Bool
    askAboutDust pool feeByte recipients othersStay worth dust = do
      let report = sendAllDustReport feeByte recipients worth dust othersStay
      views <- poolCoinViews pool ((.coin.outpoint) <$> report.dustCoins)
      liftIO $ renderOn stderr =<< dustReportDoc pool.poolNetwork (unitOn pool.poolNetwork choice) views report
      askYes terminal dustOptions (dustQuestion report)

-- | The options that answer the question about the dust coins in a
-- script
dustOptions :: ScriptAnswer
dustOptions = ScriptOption "--spend-dust or --leave-dust"

-- | The coins that a command spends among those it would spend in full,
-- preparesendall or preparesweep: all of them, or all but the dust coins
-- ("Haskoin.Wallet.Tx.Dust"). --spend-dust and --leave-dust say which, and
-- the user is asked otherwise, once and for every dust coin: the command
-- gives what shows the report and asks, from the coins worth spending and
-- the dust coins, and any answer but yes leaves them. Nothing is asked
-- when no coin is dust, and when every coin is: the builder then tells that
-- the coins do not pay their fee. The dust coins are left without a
-- question when spending them too would leave too little for the
-- recipients, and when the other coins cannot pay the recipients either,
-- which the builder then tells. Each command sizes its coins its own way.
coinsWithDust ::
  (Monad m) =>
  -- | The answer of --spend-dust or --leave-dust
  Maybe DustCoins ->
  -- | The fee rate
  Natural ->
  [(Address, Share)] ->
  -- | A coin with the input that spends it
  (coin -> SizedCoin) ->
  -- | Show the report on the coins worth spending and the dust coins, and
  -- ask whether to spend the dust coins too
  ([coin] -> [coin] -> m Bool) ->
  [coin] ->
  m [coin]
coinsWithDust choice feeByte recipients sized ask coins
  | null dust || null worth = return coins
  | otherwise =
      case choice of
        Just SpendDust -> return coins
        Just LeaveDust -> return worth
        Nothing
          | isLeft (paying coins) || isLeft (paying worth) -> return worth
          | otherwise -> do
              spending <- ask worth dust
              return $ if spending then coins else worth
  where
    (dust, worth) = partition (isDustCoin feeByte . sized) coins
    paying sending = spendingAmounts feeByte (sized <$> sending) recipients
