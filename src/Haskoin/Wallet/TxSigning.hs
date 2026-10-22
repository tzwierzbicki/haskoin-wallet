{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Analysis and signing of wallet transactions.
--
-- The wallet only has P2PKH addresses. A 'TxSignData' carries a transaction,
-- the transactions it spends from and the derivation paths of the wallet's
-- own inputs and outputs. 'parseTxSignData' analyzes it, for the commands
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
  )
where

import Control.Monad (foldM, forM_, unless, when)
import qualified Data.ByteString as BS
import Data.List (nub)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe, isJust, listToMaybe)
import qualified Data.Serialize as S
import Data.Word (Word64)
import Haskoin
import qualified Haskoin.Store.Data as Store
import Haskoin.Wallet.TxInfo
  ( MyInputs (..),
    MyOutputs (..),
    OtherInputs (..),
    TxInfo (..),
    TxInfoPending (..),
    txType,
  )
import Haskoin.Wallet.TxSignData
import Haskoin.Wallet.TxSize (inputSize, outputSize, p2pkhInput, txSize)
import Haskoin.Wallet.Util (addrToString, safeSubtract)
import Numeric.Natural (Natural)

{- Entry points -}

-- | The 'TxInfo' of a transaction to sign, or already signed, as the wallet
-- sees it. Fails when the 'TxSignData' is not consistent.
parseTxSignData ::
  Network -> Ctx -> XPubKey -> TxSignData -> Either String TxInfo
parseTxSignData net ctx accountPub tsd = do
  coins <- spentCoins ctx tsd
  analysis <- analyze net ctx tsd accountPub coins
  return $ txInfo tsd.txSignDataFeeRate analysis coins tsd.txSignDataTx tsd.txSignDataSigned

-- | Check that every input of a signed transaction carries a valid signature
-- for the coin it spends
verifySignedTx :: Network -> Ctx -> TxSignData -> Either String ()
verifySignedTx net ctx tsd = do
  coins <- spentCoins ctx tsd
  verifyInputs net ctx tsd.txSignDataTx coins

-- | Sign a transaction of the wallet with its account private key
signWalletTx ::
  Network ->
  Ctx ->
  TxSignData ->
  XPrvKey ->
  Either String (TxSignData, TxInfo)
signWalletTx net ctx tsd accountKey = do
  when tsd.txSignDataSigned $ Left "The transaction is already signed"
  coins <- spentCoins ctx tsd
  analysis <- analyze net ctx tsd (deriveXPubKey ctx accountKey) coins
  -- The builder refuses a fee rate of zero; the signer checks the fee itself
  when (analysis.fee == 0) $ Left "The transaction pays no fee"
  signedTx <- signInputs net ctx accountKey analysis tsd.txSignDataTx coins
  verifyInputs net ctx signedTx coins
  let signedData = tsd {txSignDataTx = signedTx, txSignDataSigned = True}
  return (signedData, txInfo tsd.txSignDataFeeRate analysis coins signedTx True)

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
spentCoins :: Ctx -> TxSignData -> Either String [SpentCoin]
spentCoins ctx tsd = mapM spentCoin tsd.txSignDataTx.inputs
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
      address <- maybe nonStandard Right $ outputAddress ctx script
      return SpentCoin {outpoint = op, value = txOut.value, script, address}
    nonStandard = Left "There are non-standard inputs in the transaction"
    nth i xs = listToMaybe $ drop (fromIntegral i) xs

{- Analysis -}

-- | What the wallet knows about the transaction, before signing it
data Analysis = Analysis
  { myInputs :: !(Map Address MyInputs),
    otherInputs :: !(Map Address OtherInputs),
    myOutputs :: !(Map Address MyOutputs),
    otherOutputs :: !(Map Address Natural),
    fee :: !Natural,
    -- | What the wallet gains (positive) or spends (negative), fee included
    amount :: !Integer
  }

analyze ::
  Network -> Ctx -> TxSignData -> XPubKey -> [SpentCoin] -> Either String Analysis
analyze net ctx tsd accountPub coins = do
  when (null coins) $ Left "The transaction has no inputs"
  when (null tsd.txSignDataTx.outputs) $ Left "The transaction has no outputs"
  when (length (nub outpoints) /= length coins) $
    Left "The transaction spends the same coin twice"
  outputs <- mapM outputAddress' tsd.txSignDataTx.outputs
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
          (\cs path -> MyInputs (coinsValue cs) path "" (coinSigInput net <$> cs))
          inputsByAddress
          inputPaths
      otherInputs =
        Map.map (\cs -> OtherInputs (coinsValue cs) (coinSigInput net <$> cs)) $
          inputsByAddress `Map.difference` inputPaths
      myOutputs =
        Map.intersectionWith (\v path -> MyOutputs v path "") outputsByAddress outputPaths
      otherOutputs = outputsByAddress `Map.difference` outputPaths
      amount =
        toInteger (sum $ (.myOutputsValue) <$> myOutputs)
          - toInteger (sum $ (.myInputsValue) <$> myInputs)
  return Analysis {myInputs, otherInputs, myOutputs, otherOutputs, fee, amount}
  where
    outpoints = (.outpoint) <$> coins
    outputAddress' :: TxOut -> Either String (Address, Natural)
    outputAddress' txOut =
      either
        (const $ Left "There are non-standard outputs in the transaction")
        (\a -> Right (a, fromIntegral txOut.value))
        (scriptToAddressBS ctx txOut.script)
    coinsValue :: [SpentCoin] -> Natural
    coinsValue cs = sum $ fromIntegral . (.value) <$> cs
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

{- Signing -}

-- | Sign every input of the wallet with the key derived from the path of its
-- address. The wallet only signs P2PKH coins. An input that is not the
-- wallet's is left as it is: it must already be signed.
signInputs ::
  Network -> Ctx -> XPrvKey -> Analysis -> Tx -> [SpentCoin] -> Either String Tx
signInputs net ctx accountKey analysis tx coins =
  foldM signInput' tx (zip [0 ..] coins)
  where
    signInput' :: Tx -> (Int, SpentCoin) -> Either String Tx
    signInput' partial (i, coin) =
      case Map.lookup coin.address analysis.myInputs of
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

{- Result -}

-- | The information reported about the transaction. The size of a signed
-- transaction is that of its bytes; the size of an unsigned one is
-- estimated from the coins it spends and the outputs it pays.
txInfo :: Maybe Natural -> Analysis -> [SpentCoin] -> Tx -> Bool -> TxInfo
txInfo feeRate analysis coins tx signed =
  TxInfo
    { txInfoHash = if signed then Just (txHash tx) else Nothing,
      txInfoType = txType analysis.amount analysis.fee,
      txInfoAmount = analysis.amount,
      txInfoMyOutputs = analysis.myOutputs,
      txInfoOtherOutputs = analysis.otherOutputs,
      txInfoNonStdOutputs = [],
      txInfoMyInputs = analysis.myInputs,
      txInfoOtherInputs = analysis.otherInputs,
      txInfoNonStdInputs = [],
      txInfoSize = size,
      txInfoFee = analysis.fee,
      txInfoFeeByte = analysis.fee `div` size,
      txInfoChangeInFee = changeInFee,
      txInfoBlockRef = Store.MemRef 0,
      txInfoConfirmations = 0,
      txInfoPending = Just $ TxInfoPending (nosigHash tx) signed False False,
      txInfoDead = False
    }
  where
    size
      | signed = fromIntegral $ BS.length $ S.encode tx
      | otherwise = estimatedSize
    -- The size the builder estimated, from the kinds of the coins and the
    -- scripts of the outputs. The signing data does not say whether a P2PKH
    -- coin belongs to an uncompressed key, so such a coin is counted for a
    -- compressed one, as is a coin of a kind hw cannot sign.
    estimatedSize =
      fromIntegral $
        txSize (fromMaybe p2pkhInput . inputSize . (.script) <$> coins) (outputSize . (.script) <$> tx.outputs)
    -- What the fee pays beyond the requested rate on the size the builder
    -- estimated, which signing does not change: the change that was not
    -- worth an output. Only a transaction of the wallet's own coins has
    -- change folded into its fee; one that spends other coins, a sweep,
    -- pays what its builder decided and has no change.
    changeInFee
      | Map.null analysis.otherInputs = (\rate -> analysis.fee - min analysis.fee (rate * estimatedSize)) <$> feeRate
      | otherwise = Nothing
