{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Analysis and signing of wallet transactions.
--
-- The wallet only has P2PKH addresses. A 'TxSignData' carries a transaction,
-- the transactions it spends from and the derivation paths of the wallet's
-- own inputs and outputs. 'parseTxSignData' summarizes it, for the commands
-- that display or store it, and 'signWalletTx' signs it:
--
-- 1. finds the coin spent by every input in the dependent transactions;
-- 2. checks that the transaction has inputs and outputs and spends every
--    coin once, that the input and output paths derive exactly the wallet's
--    addresses in the transaction, and computes the fee and the amount;
-- 3. signs every input of the wallet, which must be a P2PKH coin, with the
--    key derived from the path of its address. An input that is not the
--    wallet's must already be signed;
-- 4. verifies every input of the signed transaction against the output
--    script of its coin;
-- 5. reports the signed transaction with the 'TxInfo' of the signed bytes.
module Haskoin.Wallet.TxSigning
  ( parseTxSignData,
    signWalletTx,
    verifySignedTx,
    SpentCoin (..),
    spentCoins,
    coinSigInput,
  )
where

import Control.Monad (foldM, forM_, mfilter, unless, when)
import qualified Data.ByteString as BS
import Data.List (nub)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe, isJust, listToMaybe)
import qualified Data.Serialize as S
import Data.Word (Word64)
import Haskoin
import Haskoin.Wallet.TxInfo
  ( MyInputs (..),
    MyOutputs (..),
    PendingStatus (..),
    TxInfo (..),
    TxStatus (..),
    TxSummary (..),
    txType,
  )
import Haskoin.Wallet.TxSignData
import Haskoin.Wallet.TxSize (inputSize, outputSize, p2pkhInput, txSize)
import Haskoin.Wallet.Util (addrToString, onNetwork, safeSubtract)
import Numeric.Natural (Natural)

{- Entry points -}

-- | The transaction of the signing data as the wallet sees it: its summary,
-- and its status as a transaction in preparation, signed or not. Fails when
-- the signing data is not consistent, or when an input or output is at an
-- address the network cannot write.
parseTxSignData :: Network -> Ctx -> XPubKey -> TxSignData -> Either String TxInfo
parseTxSignData net ctx accountPub tsd = do
  coins <- spentCoins net ctx tsd
  summary <- summarize net ctx accountPub tsd coins
  let tx = tsd.txSignDataTx
      txid = if isSigned tx then Just (txHash tx) else Nothing
  return $ TxInfo summary $ Pending $ PendingStatus (nosigHash tx) txid False False

-- | Check that every input of a signed transaction carries a valid signature
-- for the coin it spends
verifySignedTx :: Network -> Ctx -> TxSignData -> Either String ()
verifySignedTx net ctx tsd = do
  coins <- spentCoins net ctx tsd
  verifyInputs net ctx tsd.txSignDataTx coins

-- | Sign a transaction of the wallet with its account private key
signWalletTx ::
  Network ->
  Ctx ->
  TxSignData ->
  XPrvKey ->
  Either String (TxSignData, TxInfo)
signWalletTx net ctx tsd accountKey = do
  when (isSigned tsd.txSignDataTx) $ Left "The transaction is already signed"
  let accountPub = deriveXPubKey ctx accountKey
  coins <- spentCoins net ctx tsd
  summary <- summarize net ctx accountPub tsd coins
  -- The builder refuses a fee rate of zero; the signer checks the fee itself
  when (summary.summaryFee == 0) $ Left "The transaction pays no fee"
  signedTx <- signInputs net ctx accountKey summary.summaryMyInputs tsd.txSignDataTx coins
  verifyInputs net ctx signedTx coins
  let signedData = tsd {txSignDataTx = signedTx}
  -- Summarized again: the signed transaction has its real size and its id
  info <- parseTxSignData net ctx accountPub signedData
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
      address <- maybe nonStandard Right $ paidAddress net ctx script
      return SpentCoin {outpoint = op, value = txOut.value, script, address}
    nonStandard = Left "There are non-standard inputs in the transaction"
    nth i xs = listToMaybe $ drop (fromIntegral i) xs

-- | The address an output script pays, when the network can write it
paidAddress :: Network -> Ctx -> ScriptOutput -> Maybe Address
paidAddress net ctx script = mfilter (onNetwork net) (outputAddress ctx script)

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

{- Summary -}

-- | The summary of the transaction of the signing data: the inputs and
-- outputs that are the wallet's, told by the paths, the fee, the amount and
-- the size, that of the bytes once signed and an estimate before
summarize :: Network -> Ctx -> XPubKey -> TxSignData -> [SpentCoin] -> Either String TxSummary
summarize net ctx accountPub tsd coins = do
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
        | isSigned tx = fromIntegral $ BS.length $ S.encode tx
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
    -- The wallet addresses of the paths, each path once, and all of them
    -- present in the transaction
    pathAddresses :: String -> [SoftPath] -> [Address] -> Either String (Map Address SoftPath)
    pathAddresses err paths present = do
      let addresses =
            Map.fromList
              [(xPubAddr ctx (derivePubPath ctx path accountPub), path) | path <- paths]
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
    txSize (fromMaybe p2pkhInput . inputSize . (.script) <$> coins) (outputSize . (.script) <$> tx.outputs)

{- Signing -}

-- | Sign every input of the wallet with the key derived from the path of its
-- address. The wallet only signs P2PKH coins. An input that is not the
-- wallet's is left as it is: it must already be signed.
signInputs ::
  Network -> Ctx -> XPrvKey -> Map Address MyInputs -> Tx -> [SpentCoin] -> Either String Tx
signInputs net ctx accountKey myInputs tx coins =
  foldM signInput' tx (zip [0 ..] coins)
  where
    signInput' :: Tx -> (Int, SpentCoin) -> Either String Tx
    signInput' partial (i, coin) =
      case Map.lookup coin.address myInputs of
        Just mine
          | isPayPKHash coin.script ->
              signInput net ctx partial i (coinSigInput net coin) (inputKey mine.myInputsPath)
          | otherwise ->
              Left $ describeInput net i coin <> " is not a P2PKH coin"
        Nothing
          | BS.null (tx.inputs !! i).script ->
              Left $ describeInput net i coin <> " does not belong to the wallet"
          | otherwise -> return partial
    inputKey path = wrapSecKey True (derivePath ctx path accountKey).key

-- | Check every input of the signed transaction against the output script
-- of its coin
verifyInputs :: Network -> Ctx -> Tx -> [SpentCoin] -> Either String ()
verifyInputs net ctx tx coins =
  forM_ (zip [0 ..] coins) $ \(i, coin) ->
    unless (verifyStdInput net ctx tx i coin.script coin.value) $
      Left $ "Invalid signature for " <> describeInput net i coin

describeInput :: Network -> Int -> SpentCoin -> String
describeInput net i coin =
  "input " <> show i <> " (" <> addrToString net coin.address <> ")"
