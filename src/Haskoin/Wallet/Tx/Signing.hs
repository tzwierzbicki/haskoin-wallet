{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Analysis and signing of wallet transactions.
--
-- An account has P2PKH addresses, when it is a BIP44 one, or P2WPKH
-- addresses, when it is a BIP84 one: its kind says which, and derives its
-- addresses from its public key ("Haskoin.Wallet.Keys".'keyAddress'). A
-- 'TxSignData' carries a transaction, the transactions it spends from and
-- the derivation paths of the wallet's own inputs and outputs.
-- 'parseTxSignData' summarizes it, for the commands that display or store
-- it, and 'signWalletTx' signs it:
--
-- 1. finds the coin spent by every input in the dependent transactions;
-- 2. checks that the transaction has inputs and outputs and spends every
--    coin once, that the input and output paths derive exactly the wallet's
--    addresses in the transaction, and computes the fee and the amount;
-- 3. signs every input of the wallet, which must be a coin of the kind of
--    the account, with the key derived from the path of its address: a
--    P2PKH coin with the legacy digest, a P2WPKH coin with the digest of
--    BIP 143, its signature in the witness. An input that is not the
--    wallet's must already be signed;
-- 4. verifies every input of the signed transaction against the output
--    script of its coin, and that every signature has the sighash the
--    wallet signs with;
-- 5. reports the signed transaction with the 'TxInfo' of the signed bytes,
--    its size in virtual bytes.
--
-- A transaction file carries the labels of the account's addresses in the
-- transaction, by their paths, and those of the transactions its coins
-- come from.
-- 'parseTxFile' reads a file with them, 'labelsFromFile' puts them on a
-- 'TxInfo', and 'labelledTxFile' writes them from one.
module Haskoin.Wallet.Tx.Signing
  ( parseTxSignData,
    parseOutsideTxSignData,
    parseTxFile,
    labelsFromFile,
    labelledTxFile,
    signWalletTx,
    verifySignedTx,
    SpentCoin (..),
    spentCoins,
    spentAddress,
    coinSigInput,
    walletSigHash,
    sigHashName,
  )
where

import Control.Monad (foldM, forM_, mfilter, unless, when)
import Data.Bits ((.&.))
import qualified Data.ByteString as BS
import Data.List (nub)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe, isJust, listToMaybe)
import qualified Data.Set as Set
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
import Haskoin.Wallet.Keys (AccountKind, keyAddress, kindCoinName, spendsScript)
import Haskoin.Wallet.Tx.SignData
import Haskoin.Wallet.Tx.Size (inputKind, scriptOutputKind, signedTxSize, txSize)
import Haskoin.Wallet.Util (addrToString, onNetwork, safeSubtract)
import Numeric.Natural (Natural)

{- Entry points -}

-- | The transaction of the signing data as the account of the kind and
-- the public key sees it: its summary, and its status as a transaction in
-- preparation, signed or not. Fails when the signing data is not
-- consistent, or when an input or output is at an address the network
-- cannot write.
parseTxSignData :: Network -> Ctx -> AccountKind -> XPubKey -> TxSignData -> Either String TxInfo
parseTxSignData net ctx kind accountPub = parseAs net ctx (Just (kind, accountPub))

-- | The transaction of signing data that involves no account of the
-- wallet, as a sweep between outside addresses: its summary, as
-- 'parseTxSignData' gives it, with every input and output another
-- wallet's. The paths it may have are those of an account the wallet does
-- not have.
parseOutsideTxSignData :: Network -> Ctx -> TxSignData -> Either String TxInfo
parseOutsideTxSignData net ctx = parseAs net ctx Nothing

-- | The transaction of signing data, as the account of the kind and the
-- public key sees it, or as an outsider does without one
parseAs :: Network -> Ctx -> Maybe (AccountKind, XPubKey) -> TxSignData -> Either String TxInfo
parseAs net ctx accountPubM tsd = do
  coins <- spentCoins net ctx tsd
  summary <- summarize net ctx accountPubM tsd coins
  let tx = tsd.txSignDataTx
      txid = if isSigned tx then Just (txHash tx) else Nothing
  -- What only the database knows, the wallet's own state, is filled in
  -- for a transaction it holds
  return $ TxInfo summary (Pending (PendingStatus (nosigHash tx) txid False False Nothing tsd.txSignDataReplaces)) tsd.txSignDataLabel Map.empty

{- Files -}

-- | The transaction of a file as the account sees it, with its labels as
-- the file carries them, not as this wallet has them: an offline computer
-- has none of its own
parseTxFile :: Network -> Ctx -> AccountKind -> XPubKey -> TxFile -> Either String TxInfo
parseTxFile net ctx kind accountPub file =
  labelsFromFile file <$> parseTxSignData net ctx kind accountPub (txFileData file)

-- | The transaction with its labels as a file carries them: those of the
-- account's addresses, by their paths, and those the coins it spends show.
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

-- | The file of a transaction of the account, with its labels as the
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

-- | Sign a transaction of an account of the kind with its account private
-- key
signWalletTx ::
  Network ->
  Ctx ->
  AccountKind ->
  TxSignData ->
  XPrvKey ->
  Either String (TxSignData, TxInfo)
signWalletTx net ctx kind tsd accountKey = do
  when (isSigned tsd.txSignDataTx) $ Left "The transaction is already signed"
  let accountPub = deriveXPubKey ctx accountKey
  coins <- spentCoins net ctx tsd
  summary <- summarize net ctx (Just (kind, accountPub)) tsd coins
  -- The builder refuses a fee rate of zero; the signer checks the fee itself
  when (summary.summaryFee == 0) $ Left "The transaction pays no fee"
  signedTx <- signInputs net ctx kind accountKey summary.summaryMyInputs tsd.txSignDataTx coins
  verifyInputs net ctx signedTx coins
  let signedData = tsd {txSignDataTx = signedTx}
  -- Summarized again: the signed transaction has its real size and its id
  info <- parseTxSignData net ctx kind accountPub signedData
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

-- | The coin spent by every input, in the order of the inputs
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
      address <- maybe nonStandard Right $ spentAddress net ctx script
      return SpentCoin {outpoint = op, value = txOut.value, script, address}
    nonStandard = Left "There are non-standard inputs in the transaction"
    nth i xs = listToMaybe $ drop (fromIntegral i) xs

-- | The address an output script pays, when the network can write it
paidAddress :: Network -> Ctx -> ScriptOutput -> Maybe Address
paidAddress net ctx script = mfilter (onNetwork net) (outputAddress ctx script)

-- | The address of a coin an input spends, when the network can write it.
-- A coin paid to a public key itself (P2PK), which only a sweep spends, is
-- listed under the P2PKH address of that key. haskoin-core gives the
-- script no address, since the coin is not spent as a P2PKH one, but the
-- same key signs for both, and hw has no other name for the coin.
spentAddress :: Network -> Ctx -> ScriptOutput -> Maybe Address
spentAddress net ctx = \case
  PayPK key -> mfilter (onNetwork net) (Just (pubKeyAddr ctx key))
  script -> paidAddress net ctx script

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
-- outputs that are the wallet's, told by the paths and the kind and
-- public key of the account, the fee, the amount and the size in virtual
-- bytes, that of the bytes once signed and an estimate before. Without
-- the account, no input or output is the wallet's.
summarize :: Network -> Ctx -> Maybe (AccountKind, XPubKey) -> TxSignData -> [SpentCoin] -> Either String TxSummary
summarize net ctx accountPubM tsd coins = do
  when (null coins) $ Left "The transaction has no inputs"
  when (null tx.outputs) $ Left "The transaction has no outputs"
  when (length (nub outpoints) /= length coins) $
    Left "The transaction spends the same coin twice"
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
  let inputsByAddress = Map.fromListWith (flip (<>)) [(c.address, [c]) | c <- coins]
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
      estimated = estimatedSize coins tx
      size
        | isSigned tx = signedTxSize tx
        | otherwise = estimated
      -- What the fee pays beyond the requested rate on the size the builder
      -- estimated, which signing does not change: the change that was not
      -- worth an output. Only a transaction of the wallet's own coins has
      -- change folded into its fee; one that spends other coins, a sweep,
      -- pays what its builder decided and has no change.
      changeInFee
        | Map.null otherInputs = (\rate -> fee - min fee (rate * estimated)) <$> tsd.txSignDataFeeRate
        | otherwise = Nothing
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
      address <- maybe nonStandard Right $ paidAddress net ctx script
      return (address, fromIntegral txOut.value)
    nonStandard = Left "There are non-standard outputs in the transaction"
    coinsValue :: [SpentCoin] -> Natural
    coinsValue spent = sum $ fromIntegral . (.value) <$> spent
    -- The wallet addresses of the paths, of the kind of the account, each
    -- path once, and all of them present in the transaction; none without
    -- the account
    pathAddresses :: String -> [SoftPath] -> [Address] -> Either String (Map Address SoftPath)
    pathAddresses err paths present = case accountPubM of
      Nothing -> return Map.empty
      Just (kind, accountPub) -> do
        let addresses =
              Map.fromList
                [(keyAddress kind ctx (derivePubPath ctx path accountPub), path) | path <- paths]
        unless (Map.size addresses == length paths && all (`elem` present) (Map.keys addresses)) $
          Left err
        return addresses

-- | The size the builder estimated, from the kinds of the coins and the
-- scripts of the outputs. The signing data does not say whether a P2PKH
-- coin belongs to an uncompressed key, so such a coin is counted for a
-- compressed one, as is a coin of a kind hw cannot sign.
estimatedSize :: [SpentCoin] -> Tx -> Natural
estimatedSize coins tx =
  fromIntegral $
    txSize (fromMaybe InputPKHash . inputKind . (.script) <$> coins) (scriptOutputKind . (.script) <$> tx.outputs)

{- Signing -}

-- | Sign every input of the wallet with the key derived from the path of
-- its address. An account signs the coins of its kind alone: P2PKH coins
-- for a BIP44 account, P2WPKH coins for a BIP84 one, with the digest of
-- BIP 143 and the signature in the witness, which haskoin-core's signer
-- does by the script of the coin. An input that is not the wallet's is
-- left as it is: it must already be signed.
signInputs ::
  Network -> Ctx -> AccountKind -> XPrvKey -> Map Address MyInputs -> Tx -> [SpentCoin] -> Either String Tx
signInputs net ctx kind accountKey myInputs tx coins =
  foldM signInput' tx (zip [0 ..] coins)
  where
    signInput' :: Tx -> (Int, SpentCoin) -> Either String Tx
    signInput' partial (i, coin) =
      case Map.lookup coin.address myInputs of
        Just mine
          | spendsScript kind coin.script ->
              signInput net ctx partial i (coinSigInput net coin) (inputKey mine.myInputsPath)
          | otherwise ->
              Left $ describeInput net i coin <> " is not a " <> kindCoinName kind <> " coin"
        Nothing
          | BS.null (tx.inputs !! i).script ->
              Left $ describeInput net i coin <> " does not belong to the wallet"
          | otherwise -> return partial
    inputKey path = wrapSecKey True (derivePath ctx path accountKey).key

-- | Check every input of the signed transaction against the output script
-- of its coin, and that every signature in it has the sighash the wallet
-- signs with. Another signer can give a valid signature with another
-- sighash, which commits to less than the whole transaction: with NONE,
-- anyone could change where the coins go once the transaction is out.
verifyInputs :: Network -> Ctx -> Tx -> [SpentCoin] -> Either String ()
verifyInputs net ctx tx coins =
  forM_ (zip3 [0 ..] coins stacks) $ \(i, coin, stack) -> do
    unless (verifyStdInput net ctx tx spent i) $
      Left $ "Invalid signature for " <> describeInput net i coin
    forM_ (inputSigHashes net ctx (tx.inputs !! i) stack) $ \sh ->
      unless (sh == walletSigHash net) $
        Left $ "The signature of " <> describeInput net i coin <> " has the sighash " <> sigHashName sh <> ", not " <> sigHashName (walletSigHash net)
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
