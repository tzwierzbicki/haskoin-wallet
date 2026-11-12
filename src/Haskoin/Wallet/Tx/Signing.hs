{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Analysis and signing of wallet transactions.
--
-- A transaction of the wallet is one of a pool: the accounts of a
-- portfolio on one network, a BIP44 one with P2PKH addresses and, where
-- the network has SegWit, a BIP84 one with P2WPKH addresses
-- ("Haskoin.Wallet.Keys".'keyAddress'). One transaction may spend coins of
-- both. A 'TxSignData' carries a transaction, the transactions it spends
-- from and the paths of the portfolio's own inputs and outputs, each with
-- the kind of its account. 'parseTxSignData' summarizes it against the
-- public keys of the accounts, for the commands that display or store it,
-- and 'signWalletTx' signs it with their private keys:
--
-- 1. finds the coin spent by every input in the dependent transactions;
-- 2. checks that the transaction has inputs and outputs and spends every
--    coin once, that the input and output paths are of the two chains of
--    an account, /0/n and /1/n, and derive exactly the portfolio's
--    addresses in the transaction, and computes the fee and the amount;
-- 3. checks that no output path lies more than 'scanLimit' past what the
--    wallet holds of its chain or an input spends from
--    ('checkOutputReach'), as the reader of a file does once it knows the
--    portfolio;
-- 4. signs every input of the portfolio, which must be a coin of the kind
--    of the account its path names, with the key derived from that path
--    below the key of that account: a P2PKH coin with the legacy digest, a
--    P2WPKH coin with the digest of BIP 143, its signature in the witness.
--    An input that is not the portfolio's must already be signed;
-- 5. verifies every input of the signed transaction against the output
--    script of its coin, and that every signature has the sighash the
--    wallet signs with;
-- 6. reports the signed transaction with the 'TxInfo' of the signed bytes,
--    its size in virtual bytes.
--
-- A transaction file carries the labels of the portfolio's addresses in
-- the transaction, by their paths, and those of the transactions its
-- coins come from.
-- 'parseTxFile' reads a file with them, 'labelsFromFile' puts them on a
-- 'TxInfo', and 'labelledTxFile' writes them from one.
module Haskoin.Wallet.Tx.Signing
  ( HeldChains (..),
    HeldPool,
    noneHeld,
    heldChainsOf,
    heldPoolOf,
    checkOutputReach,
    parseTxSignData,
    parseOutsideTxSignData,
    parseTxFile,
    labelsFromFile,
    labelledTxFile,
    signWalletTx,
    signHeldWalletTx,
    signInputs,
    verifySignedTx,
    SpentCoin (..),
    spentCoins,
    coinSigInput,
    walletSigHash,
    sigHashName,
    offChainPath,
  )
where

import Control.Monad (foldM, forM_, guard, unless, when)
import Data.Bits ((.&.))
import Data.Containers.ListUtils (nubOrd)
import Data.List (zip4)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe, isJust, listToMaybe)
import qualified Data.Set as Set
import Data.String.Conversions (cs)
import qualified Data.Text as Text
import Data.Word (Word32, Word64)
import Haskoin
import Haskoin.Wallet.Database.TxInfo
  ( MyInputs (..),
    MyOutputs (..),
    PendingStatus (..),
    TxInfo (..),
    TxStatus (..),
    TxSummary (..),
    relabelPaths,
    spentTxids,
    txType,
  )
import Haskoin.Wallet.Database (DBAccount (..))
import Haskoin.Wallet.Database.Address (scanLimit)
import Haskoin.Wallet.Database.Account (accountKind)
import Haskoin.Wallet.Keys (AccountKind, PoolPath (..), isChainPath, keyAddress, kindCoinName, kindName, poolPathText, spendsScript)
import Haskoin.Wallet.Tx.SignData
import Haskoin.Wallet.Tx.Size (accountOutputKind, accountOutputSize, inputKind, outputDustLimit, scriptOutputKind, signedTxSize, txSize)
import Haskoin.Wallet.Util (addrToString, safeSubtract)
import Numeric.Natural (Natural)

{- Entry points -}

-- | How many addresses of each chain of the account the wallet has
-- generated: a path of an output of the account must lie within
-- 'scanLimit' of them, or of the furthest input of its chain. An address
-- further away is past any gap a wallet scans, so no sync, recovery or
-- other wallet of the seed would find the coins paid there: a file could
-- send the change to /1/2000000000 and strand it.
data HeldChains = HeldChains
  { -- | The receiving addresses generated, /0/0 to /0/(n - 1)
    heldReceiving :: !Natural,
    -- | The change addresses generated, /1/0 to /1/(n - 1)
    heldChange :: !Natural
  }
  deriving (Eq, Show)

-- | The addresses the wallet has generated of each account of a pool, by
-- the kind of the account. An account that is not listed holds none.
type HeldPool = Map AccountKind HeldChains

-- | No address of any account, as on an offline signer that never
-- generates one
noneHeld :: HeldPool
noneHeld = Map.empty

-- | The addresses the wallet holds of the account of the row
heldChainsOf :: DBAccount -> HeldChains
heldChainsOf acc = HeldChains (fromIntegral (dBAccountExternal acc)) (fromIntegral (dBAccountInternal acc))

-- | The addresses the wallet holds of the accounts of a pool
heldPoolOf :: [DBAccount] -> HeldPool
heldPoolOf accounts = Map.fromList [(accountKind acc, heldChainsOf acc) | acc <- accounts]

-- | The transaction of the signing data as a pool sees it, given the kind
-- and the public key of each of its accounts: its summary, and its status
-- as a transaction in preparation, signed or not. Fails when the signing
-- data is not consistent, or when an input or output is at an address the
-- network cannot write. How far its output paths lie is not looked at
-- here: a reader finds the portfolio of a file by whether its summary
-- reads, and then checks them ('checkOutputReach').
parseTxSignData :: Network -> Ctx -> [(AccountKind, XPubKey)] -> TxSignData -> Either String TxInfo
parseTxSignData net ctx accounts tsd = do
  coins <- spentCoins net ctx tsd
  summary <- summarize net ctx accounts tsd coins
  let tx = tsd.txSignDataTx
      txid = if isSigned tx then Just (txHash tx) else Nothing
  -- What only the database knows, the wallet's own state, is filled in
  -- for a transaction it holds
  return $ TxInfo summary (Pending (PendingStatus (nosigHash tx) txid False False Nothing tsd.txSignDataReplaces)) tsd.txSignDataLabel Map.empty Map.empty

-- | The transaction of signing data that involves no portfolio of the
-- wallet, as a sweep between outside addresses: its summary, as
-- 'parseTxSignData' gives it, with every input and output another
-- wallet's. The paths it may have are those of an account the wallet does
-- not have.
parseOutsideTxSignData :: Network -> Ctx -> TxSignData -> Either String TxInfo
parseOutsideTxSignData net ctx = parseTxSignData net ctx []

{- Files -}

-- | The transaction of a file as the pool sees it, with its labels as
-- the file carries them, not as this wallet has them: an offline computer
-- has none of its own
parseTxFile :: Network -> Ctx -> [(AccountKind, XPubKey)] -> TxFile -> Either String TxInfo
parseTxFile net ctx accounts file =
  labelsFromFile file <$> parseTxSignData net ctx accounts (txFileData file)

-- | The transaction with its labels as a file carries them: those of the
-- portfolio's addresses, by their paths, and those the coins it spends show.
-- An address or a coin the file does not label has none. A label at a
-- path, or of a transaction, that the transaction does not touch is left
-- aside.
labelsFromFile :: TxFile -> TxInfo -> TxInfo
labelsFromFile file info =
  (relabelPaths labelOf info)
    { txInfoInputLabels = Map.restrictKeys (txFileInputLabels file) (Set.fromList (spentTxids (txInfoSummary info)))
    }
  where
    labelOf path = Map.findWithDefault "" path (txFilePathLabels file)

-- | The file of a transaction of the portfolio, with its labels as the
-- summary has them: those of its addresses that have one, by their paths,
-- and those the coins it spends show
labelledTxFile :: Network -> TxInfo -> TxSignData -> TxFile
labelledTxFile net info tsd = TxFile (Just net) tsd (Map.fromList labelled) (txInfoInputLabels info)
  where
    summary = txInfoSummary info
    mine =
      [(i.myInputsPath, i.myInputsLabel) | i <- Map.elems summary.summaryMyInputs]
        <> [(o.myOutputsPath, o.myOutputsLabel) | o <- Map.elems summary.summaryMyOutputs]
    labelled = [(path, label) | (path, label) <- mine, not (Text.null label)]

-- | Check that every input of a signed transaction carries a valid signature
-- for the coin it spends
verifySignedTx :: Network -> Ctx -> TxSignData -> Either String ()
verifySignedTx net ctx tsd = do
  coins <- spentCoins net ctx tsd
  verifyInputs net ctx tsd.txSignDataTx coins

-- | Sign a transaction of a pool with the private keys of its accounts,
-- each with its kind, the wallet holding no address of them
-- ('signHeldWalletTx')
signWalletTx ::
  Network ->
  Ctx ->
  TxSignData ->
  [(AccountKind, XPrvKey)] ->
  Either String (TxSignData, TxInfo)
signWalletTx net ctx = signHeldWalletTx net ctx noneHeld

-- | Sign a transaction of a pool with the private keys of its accounts,
-- each with its kind, the wallet holding the given addresses of them: an
-- output path too far past them is refused ('checkOutputReach'). One
-- call signs the inputs of every account, so a payment that spends BIP44
-- and BIP84 coins together is signed at once.
signHeldWalletTx ::
  Network ->
  Ctx ->
  HeldPool ->
  TxSignData ->
  [(AccountKind, XPrvKey)] ->
  Either String (TxSignData, TxInfo)
signHeldWalletTx net ctx held tsd accountKeys = do
  when (isSigned tsd.txSignDataTx) $ Left "The transaction is already signed"
  let accountPubs = [(kind, deriveXPubKey ctx key) | (kind, key) <- accountKeys]
  coins <- spentCoins net ctx tsd
  summary <- summarize net ctx accountPubs tsd coins
  checkOutputReach held tsd
  -- The builder refuses a fee rate of zero; the signer checks the fee itself
  when (summary.summaryFee == 0) $ Left "The transaction pays no fee"
  signedTx <- signInputs net ctx accountKeys summary.summaryMyInputs tsd.txSignDataTx coins
  verifyInputs net ctx signedTx coins
  let signedData = tsd {txSignDataTx = signedTx}
  -- Summarized again: the signed transaction has its real size and its id
  info <- parseTxSignData net ctx accountPubs signedData
  return (signedData, info)

{- Coins -}

-- | A coin spent by the transaction
data SpentCoin = SpentCoin
  { outpoint :: !OutPoint,
    value :: !Word64,
    -- | The output script of the coin, as found in the dependent transaction
    script :: !ScriptOutput,
    address :: !Address
  }

-- | The coin spent by every input, in the order of the inputs. hw names a
-- coin by its address, the one haskoin-core gives its script on the
-- network. A coin without one is refused as non-standard: one paid to a
-- public key itself (P2PK) has no address, and the P2PKH address of its
-- key would mislead, since it is not spent as a P2PKH coin.
spentCoins :: Network -> Ctx -> TxSignData -> Either String [SpentCoin]
spentCoins net ctx tsd = mapM spentCoin tsd.txSignDataTx.inputs
  where
    outputs =
      Map.fromList [(txHash tx, tx.outputs) | tx <- tsd.txSignDataInputs]
    spentCoin :: TxIn -> Either String SpentCoin
    spentCoin input = do
      let op = input.outpoint
      txOut <-
        maybe (Left "Referenced input transactions are missing") Right $
          nth op.index =<< Map.lookup op.hash outputs
      script <- either (const nonStandard) Right $ unmarshal ctx txOut.script
      address <- maybe nonStandard Right $ outputAddress net ctx script
      return SpentCoin {outpoint = op, value = txOut.value, script, address}
    nonStandard = Left "There are non-standard inputs in the transaction"
    nth i xs = listToMaybe $ drop (fromIntegral i) xs

-- | The signing parameters of a coin: its output script and value, signed
-- with SIGHASH_ALL (with the FORKID flag on Bitcoin Cash networks)
coinSigInput :: Network -> SpentCoin -> SigInput
coinSigInput net coin =
  SigInput coin.script coin.value coin.outpoint (walletSigHash net) Nothing

-- | SIGHASH_ALL, with the FORKID flag on Bitcoin Cash networks
walletSigHash :: Network -> SigHash
walletSigHash net
  | isJust net.sigHashForkId = setForkIdFlag sigHashAll
  | otherwise = sigHashAll

-- | The name of a sighash, as Bitcoin Core shows it: ALL, NONE or SINGLE,
-- with ANYONECANPAY and FORKID after a bar when their flags are set, and
-- the number of one that is none of these
sigHashName :: SigHash -> String
sigHashName sh = base <> concat ["|ANYONECANPAY" | anyoneCanPay sh] <> concat ["|FORKID" | hasForkIdFlag sh]
  where
    base = case sh .&. 0x1f of
      1 -> "ALL"
      2 -> "NONE"
      3 -> "SINGLE"
      _ -> show (fromIntegral sh :: Word32)

{- Summary -}

-- | The summary of the transaction of the signing data: the inputs and
-- outputs that are the portfolio's, told by the paths and by the kind and
-- public key of each account of the pool, the fee, the amount and the
-- size in virtual bytes, that of the bytes once signed and an estimate
-- before. Without an account, no input or output is the portfolio's.
summarize :: Network -> Ctx -> [(AccountKind, XPubKey)] -> TxSignData -> [SpentCoin] -> Either String TxSummary
summarize net ctx accounts tsd coins = do
  when (null coins) $ Left "The transaction has no inputs"
  when (null tx.outputs) $ Left "The transaction has no outputs"
  when (length (nubOrd outpoints) /= length coins) $
    Left "The transaction spends the same coin twice"
  forM_ (tsd.txSignDataInputPaths <> tsd.txSignDataOutputPaths) $ \path ->
    unless (isChainPath (pathInAccount path)) $ Left (offChainPath path)
  outputs <- mapM outputAddress' tx.outputs
  fee <-
    maybe (Left "Fee is negative") Right $
      sum (fromIntegral . (.value) <$> coins) `safeSubtract` sum (snd <$> outputs)
  inputPaths <-
    pathAddresses
      "Input derivations don't match the transaction inputs"
      tsd.txSignDataInputPaths
      ((.address) <$> coins)
  outputPaths <-
    pathAddresses
      "Output derivations don't match the transaction outputs"
      tsd.txSignDataOutputPaths
      (fst <$> outputs)
  -- The coins of each address in the order of the inputs: each new one
  -- goes in front, and the lists are turned around once at the end
  let inputsByAddress = Map.map reverse $ Map.fromListWith (<>) [(c.address, [c]) | c <- coins]
      outputsByAddress = Map.fromListWith (+) outputs
      myInputs =
        Map.intersectionWith
          (\spent path -> MyInputs (coinsValue spent) path "" ((.outpoint) <$> spent))
          inputsByAddress
          inputPaths
      otherInputs = Map.map coinsValue $ inputsByAddress `Map.difference` inputPaths
      myOutputs =
        Map.intersectionWith (\v path -> MyOutputs v path "") outputsByAddress outputPaths
      otherOutputs = outputsByAddress `Map.difference` outputPaths
      amount =
        toInteger (sum $ (.myOutputsValue) <$> myOutputs)
          - toInteger (sum $ (.myInputsValue) <$> myInputs)
      estimated = estimatedSize ctx coins tx
      size
        | isSigned tx = signedTxSize tx
        | otherwise = estimated
      -- What the fee pays beyond the requested rate on the size the builder
      -- estimated, which signing does not change: the change that was not
      -- worth an output. Only a transaction of the wallet's own coins has
      -- change folded into its fee; one that spends other coins, a sweep,
      -- pays what its builder decided and has no change. The rate comes
      -- from the file, so the excess is change only while a change output
      -- could not have held it: what is left once that output is paid for
      -- at the rate is below its dust limit, as the builder drops it.
      -- Beyond that, the fee is shown as it is, with no word to explain it.
      -- The signing data does not say the type the change would have had,
      -- so the limit is that of the types of the coins spent, the higher
      -- one when they are of both.
      changeInFee = do
        guard (Map.null otherInputs)
        rate <- tsd.txSignDataFeeRate
        let excess = fee - min fee (rate * estimated)
            droppedBelow kind = outputDustLimit (accountOutputKind kind) + rate * fromIntegral (accountOutputSize kind)
            spentKinds = nubOrd [i.myInputsPath.pathKind | i <- Map.elems myInputs]
        guard (not (null spentKinds))
        guard (excess < maximum (droppedBelow <$> spentKinds))
        return excess
  return
    TxSummary
      { summaryType = txType amount fee,
        summaryAmount = amount,
        summaryFee = fee,
        summaryFeeByte = fee `div` size,
        summaryChangeInFee = changeInFee,
        summarySize = size,
        summaryLockTime = tx.locktime,
        summaryMyOutputs = myOutputs,
        summaryOtherOutputs = otherOutputs,
        summaryNonStdOutputs = [],
        summaryMyInputs = myInputs,
        summaryOtherInputs = otherInputs,
        summaryNonStdInputs = []
      }
  where
    tx = tsd.txSignDataTx
    outpoints = (.outpoint) <$> coins
    outputAddress' :: TxOut -> Either String (Address, Natural)
    outputAddress' txOut = do
      script <- either (const nonStandard) Right $ unmarshal ctx txOut.script
      address <- maybe nonStandard Right $ outputAddress net ctx script
      return (address, fromIntegral txOut.value)
    nonStandard = Left "There are non-standard outputs in the transaction"
    coinsValue :: [SpentCoin] -> Natural
    coinsValue spent = sum $ fromIntegral . (.value) <$> spent
    -- The addresses of the portfolio at the paths, each of the kind of
    -- its account and derived from the key of that account, each path
    -- once, and all of them present in the transaction; none without an
    -- account. A path of a kind the pool has no account of is refused.
    pathAddresses :: String -> [PoolPath] -> [Address] -> Either String (Map Address PoolPath)
    pathAddresses err paths present
      | null accounts = return Map.empty
      | otherwise = do
          derived <- mapM addressAt paths
          let addresses = Map.fromList derived
          unless (Map.size addresses == length paths && all (`Set.member` Set.fromList present) (Map.keys addresses)) $
            Left err
          return addresses
    addressAt :: PoolPath -> Either String (Address, PoolPath)
    addressAt path = case lookup path.pathKind accounts of
      Just accountPub -> Right (keyAddress path.pathKind ctx (derivePubPath ctx path.pathInAccount accountPub), path)
      Nothing -> Left ("The path " <> cs (poolPathText path) <> " is of a " <> cs (kindName path.pathKind) <> " account, which the portfolio does not have on " <> net.name)

-- | Refuse an output path of an account more than 'scanLimit' addresses
-- past the last address of its chain that the wallet holds or that an
-- input spends from: at /1/n, n must be below the change addresses held,
-- or one past the highest input at /1/m of the same account, plus
-- 'scanLimit', whichever is further. Each account of the pool has chains
-- of its own, so a path is measured against the chain of its kind. A
-- path not of the chains is refused apart ('isChainPath'). An input
-- spends a coin, so its address is in use, however far it lies: it is
-- not refused.
checkOutputReach :: HeldPool -> TxSignData -> Either String ()
checkOutputReach held tsd =
  forM_ outputPaths $ \path -> forM_ (chainIndex path) $ \(chain, index) ->
    unless (fromIntegral index < reach path.pathKind chain + scanLimit) $ Left (farPath path)
  where
    inputPaths = tsd.txSignDataInputPaths
    outputPaths = tsd.txSignDataOutputPaths
    -- One past the last address of the chain known to be the account's
    reach kind chain = maximum $ held' kind chain : [fromIntegral i + 1 | input <- inputPaths, input.pathKind == kind, Just (c, i) <- [chainIndex input], c == chain]
    held' kind chain = case Map.lookup kind held of
      Nothing -> 0
      Just chains -> if chain == 0 then chains.heldReceiving else chains.heldChange
    chainIndex :: PoolPath -> Maybe (KeyIndex, KeyIndex)
    chainIndex path = case pathToList path.pathInAccount of
      [chain, index] -> Just (chain, index)
      _ -> Nothing

-- | The refusal of an output path too far past the addresses of its chain
-- ('checkOutputReach')
farPath :: PoolPath -> String
farPath path =
  "The path "
    <> cs (poolPathText path)
    <> " of an output is over "
    <> show scanLimit
    <> " addresses past the last one of its chain that the wallet holds or the transaction spends from: no scan of the account would find what it pays there"

-- | The refusal of a path at which an account has no address: a path
-- of a chain other than its two ('Haskoin.Wallet.Keys.isChainPath'). hw
-- neither shows nor signs a transaction with one, and writes none to a
-- file.
offChainPath :: PoolPath -> String
offChainPath path =
  "The path " <> cs (poolPathText path) <> " is of no address of the account: its addresses are at /0/n and /1/n"

-- | The size the builder estimated, from the kinds of the coins and the
-- scripts of the outputs. The signing data does not say whether a P2PKH
-- coin belongs to an uncompressed key, so such a coin is counted for a
-- compressed one, as is a coin of a kind hw cannot sign.
estimatedSize :: Ctx -> [SpentCoin] -> Tx -> Natural
estimatedSize ctx coins tx =
  fromIntegral $
    txSize (fromMaybe InputPKHash . inputKind . (.script) <$> coins) (scriptOutputKind ctx . (.script) <$> tx.outputs)

{- Signing -}

-- | Sign every input of the portfolio with the key derived from the path
-- of its address, below the private key of the account of its kind. An
-- account signs the coins of its kind alone: P2PKH coins for a BIP44
-- account, P2WPKH coins for a BIP84 one, with the digest of BIP 143 and
-- the signature in the witness, which haskoin-core's signer does by the
-- script of the coin. An input that is not the portfolio's is left as it
-- is: it must already be signed.
signInputs ::
  Network -> Ctx -> [(AccountKind, XPrvKey)] -> Map Address MyInputs -> Tx -> [SpentCoin] -> Either String Tx
signInputs net ctx accountKeys myInputs tx coins =
  foldM signInput' tx (zip3 [0 ..] coins (signedInputs tx))
  where
    -- An input that is not the wallet's must already carry a signature, in
    -- its script or in its witness, as another signer finalized it
    signInput' :: Tx -> (Int, SpentCoin, Bool) -> Either String Tx
    signInput' partial (i, coin, signedAlready) =
      case Map.lookup coin.address myInputs of
        Just mine
          -- The summary refuses such a path before anything is signed; the
          -- one place a private key is derived from a path does not rely
          -- on that
          | not (isChainPath mine.myInputsPath.pathInAccount) ->
              Left (offChainPath mine.myInputsPath)
          | spendsScript kind coin.script -> do
              accountKey <-
                maybe (Left (describeInput net i coin <> " is of a " <> cs (kindName kind) <> " account, and no key of one was given")) Right $
                  lookup kind accountKeys
              signInput net ctx partial i (coinSigInput net coin) (inputKey accountKey mine.myInputsPath.pathInAccount)
          | otherwise ->
              Left $ describeInput net i coin <> " is not a " <> kindCoinName kind <> " coin"
          where
            kind = mine.myInputsPath.pathKind
        Nothing
          | not signedAlready ->
              Left $ describeInput net i coin <> " does not belong to the wallet"
          | otherwise -> return partial
    inputKey accountKey path = wrapSecKey True (derivePath ctx path accountKey).key

-- | Check every input of the signed transaction against the output script
-- of its coin, and that every signature in it has the sighash the wallet
-- signs with. Another signer can give a valid signature with another
-- sighash, which commits to less than the whole transaction: with NONE,
-- anyone could change where the coins go once the transaction is out.
--
-- The transaction is then verified as a whole, with haskoin-core's
-- verifyStdTx, which also refuses a coin spent twice and outputs that pay
-- more than the coins hold. The inputs are checked one by one first, to
-- name the input that fails.
verifyInputs :: Network -> Ctx -> Tx -> [SpentCoin] -> Either String ()
verifyInputs net ctx tx coins = do
  forM_ (zip4 [0 ..] coins stacks tx.inputs) $ \(i, coin, stack, input) -> do
    unless (verifyStdInput net ctx tx spent i) $
      Left $ "Invalid signature for " <> describeInput net i coin
    forM_ (inputSigHashes net ctx input stack) $ \sh ->
      unless (sh == walletSigHash net) $
        Left $ "The signature of " <> describeInput net i coin <> " has the sighash " <> sigHashName sh <> ", not " <> sigHashName (walletSigHash net)
  unless (verifyStdTx net ctx tx spent) $
    Left "The transaction does not verify: it spends a coin twice, or pays more than its coins hold"
  where
    stacks = tx.witness <> repeat []
    -- The coins as the verifier takes them: it finds the coin of an input
    -- by its outpoint
    spent = [(coin.script, coin.value, coin.outpoint) | coin <- coins]

-- | The sighash of every signature of an input: those of its script when
-- it is a standard one, and those of the items of its witness that are
-- signatures. A script or an item that is no signature has none; the
-- verification against the coin is what says whether the input is signed.
inputSigHashes :: Network -> Ctx -> TxIn -> WitnessStack -> [SigHash]
inputSigHashes net ctx input stack = ofScript <> ofWitness
  where
    ofScript = maybe [] signaturesOf (eitherToMaybe (unmarshal (net, ctx) input.script))
    ofWitness = [sh | item <- stack, Just (TxSignature _ sh) <- [eitherToMaybe (decodeTxSig net ctx item)]]
    signaturesOf :: ScriptInput -> [SigHash]
    signaturesOf scriptInput = case scriptInput of
      RegularInput simple -> ofSimple simple
      ScriptHashInput simple _ -> ofSimple simple
      -- The input of a P2SH-P2WPKH coin pushes its witness script alone:
      -- its signature is in the witness
      NestedWitnessInput _ -> []
    ofSimple simple = [sh | TxSignature _ sh <- case simple of SpendPK s -> [s]; SpendPKHash s _ -> [s]; SpendMulSig ss -> ss]

describeInput :: Network -> Int -> SpentCoin -> String
describeInput net i coin =
  "input " <> show i <> " (" <> addrToString net coin.address <> ")"
