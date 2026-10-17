{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module Haskoin.Wallet.Signing where

import Conduit (MonadUnliftIO)
import Control.Arrow (second)
import Control.Monad (unless, when, (<=<))
import Control.Monad.Except
import Control.Monad.State
import qualified Data.ByteString as BS
import Data.Char (isSpace)
import Data.Default (def)
import Data.Either (rights)
import Data.List (nub, partition)
import qualified Data.Map.Strict as Map
import qualified Data.Serialize as S
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Text.Encoding (encodeUtf8)
import Data.Word (Word64, Word8)
import Haskoin
import qualified Haskoin.Store.Data as Store
import Haskoin.Store.WebClient
import Haskoin.Wallet.Config
import Haskoin.Wallet.Database
import Haskoin.Wallet.FileIO
import Haskoin.Wallet.TxInfo
import Haskoin.Wallet.Util
import Numeric.Natural (Natural)
import System.Random (Random (randomR), StdGen, initStdGen)

{- Building Transactions -}

buildTxSignData ::
  (MonadUnliftIO m) =>
  Network ->
  Ctx ->
  Config ->
  StdGen ->
  DBAccountId ->
  [(Address, Natural)] ->
  Natural ->
  Natural ->
  Bool ->
  Natural ->
  [OutPoint] ->
  ExceptT String (DB m) TxSignData
buildTxSignData net ctx cfg gen accId rcpts feeByte dust rcptPay minConf coinControl
  | null rcpts = throwError "No recipients provided"
  | otherwise = do
      -- Get all spendable coins in the account
      allCoins <- getSpendableCoins net accId minConf
      -- Get a change address
      dbAddr <- nextFreeIntAddr ctx cfg accId
      (change, changeDeriv) <- liftEither $ fromDBAddr net dbAddr
      -- Build a transaction and pick the coins
      (tx, pickedCoins) <-
        liftEither $
          buildWalletTx net ctx gen rcpts change allCoins coinControl feeByte dust rcptPay
      -- Get the derivations of our recipients and picked coins
      rcptsDerivsE <- mapM (lift . getAddrDeriv net accId) $ fst <$> rcpts
      let rcptsDerivs = rights rcptsDerivsE -- It's OK to fail here
      inDerivs <- mapM (liftEither <=< lift . getCoinDeriv net accId) pickedCoins
      -- Check if we have a change output
      let noChange = length tx.outputs == length rcpts
          outDerivs =
            if noChange
              then rcptsDerivs
              else changeDeriv : rcptsDerivs
      -- Get the dependent transactions
      let depTxHash = (.hash) . (.outpoint) <$> tx.inputs
      depTxs <- mapM getRawTx depTxHash
      -- Return the result
      return $ TxSignData tx depTxs (nub inDerivs) (nub outDerivs) False

updateControlCoins ::
     [Store.Unspent]
  -> [OutPoint]
  -> Either String ([Store.Unspent], [Store.Unspent])
updateControlCoins allCoins [] = return ([], allCoins)
updateControlCoins allCoins control
  | length l < length control =
    Left "Selected coin does not exist or is not spendable"
  | otherwise = return (l, r)
  where
    (l, r) =
      partition (\(Store.Unspent _ op _ _ _) -> op `elem` control) allCoins

buildWalletTx ::
  Network ->
  Ctx ->
  StdGen -> -- Randomness for coin selection and change address order
  [(Address, Natural)] -> -- recipients
  Address -> -- change
  [Store.Unspent] -> -- Coins to choose from
  [OutPoint] ->
  Natural -> -- Fee per byte
  Natural -> -- Dust
  Bool -> -- Recipients Pay for Fee
  Either String (Tx, [Store.Unspent])
buildWalletTx net ctx gen rcptsN change coins coinControl feeByteN dustN rcptPay =
  flip evalStateT gen $ do
    -- If coin control was used, move those coins to the front
    (l,r) <- lift $ updateControlCoins coins $ nub coinControl
    rdmCoins <- randomShuffle r
    (pickedCoins, changeAmnt) <-
      lift $ chooseCoins tot feeCoinSel (length rcptsN + 1) False (l ++ rdmCoins)
    let nOuts =
          if changeAmnt <= dust
            then length rcptsN
            else length rcptsN + 1
        totFee =
          guessTxFee (fromIntegral feeByteN) nOuts (length pickedCoins)
    rcptsPayN <-
      if rcptPay
        then lift $ makeRcptsPay (fromIntegral totFee) rcptsN
        else return rcptsN
    let rcpts = second fromIntegral <$> rcptsPayN
        allRcpts
          | changeAmnt <= dust = rcpts
          | otherwise = (change, changeAmnt) : rcpts
        ops = (.outpoint) <$> pickedCoins
    when (any ((<= dust) . snd) allRcpts) $
      lift $
        Left "Recipient output is smaller than the dust value"
    rdmRcpts <- randomShuffle allRcpts
    tx <- lift $ buildAddrTx net ctx ops =<< mapM (addrToText2 net) rdmRcpts
    return (tx, pickedCoins)
  where
    feeCoinSel =
      if rcptPay
        then 0
        else fromIntegral feeByteN :: Word64
    dust = fromIntegral dustN :: Word64
    tot = fromIntegral $ sum $ snd <$> rcptsN :: Word64

makeRcptsPay ::
  Natural -> [(Address, Natural)] -> Either String [(Address, Natural)]
makeRcptsPay fee rcpts =
  mapM f rcpts
  where
    f (a, v) = (a,) <$> maybeToEither err (v `safeSubtract` toPay)
    err = "Recipients can't pay for the fee"
    (q, r) = fee `quotRem` fromIntegral (length rcpts)
    toPay = if r == 0 then q else q + 1

{- Signing Transactions -}

data MnemonicPass = MnemonicPass
  { mnemonicWords :: !Text,
    mnemonicPass :: !Text
  }
  deriving (Eq, Show)

-- Compute an account signing key
signingKey :: Network -> Ctx -> MnemonicPass -> Natural -> Either String XPrvKey
signingKey net ctx mnem acc = do
  seed <- mnemonicToSeed (mnemonicPass mnem) (mnemonicWords mnem)
  return $ derivePath ctx (bip44Deriv net acc) (makeXPrvKey seed)

-- Compute the unique wallet fingerprint given a mnemonic
walletFingerprint :: Network -> Ctx -> MnemonicPass -> Either String Fingerprint
walletFingerprint net ctx mnem = do
  xPrvKey <- signingKey net ctx mnem 0
  return $ xPubFP ctx $ deriveXPubKey ctx xPrvKey

signWalletTx ::
  Network ->
  Ctx ->
  TxSignData ->
  XPrvKey ->
  Either String (TxSignData, TxInfo)
signWalletTx net ctx tsd@TxSignData {txSignDataInputPaths = inPaths} signKey =
  signTxWithKeys net ctx tsd publicKey prvKeys
  where
    publicKey = deriveXPubKey ctx signKey
    prvKeys = (.key) . (\p -> derivePath ctx p signKey) <$> inPaths

signTxWithKeys ::
  Network ->
  Ctx ->
  TxSignData ->
  XPubKey ->
  [SecKey] ->
  Either String (TxSignData, TxInfo)
signTxWithKeys net ctx tsd@(TxSignData tx _ _ _ signed) publicKey secKeys = do
  when signed $ Left "The transaction is already signed"
  when (null secKeys) $ Left "There are no private keys to sign"
  txInfo <- parseTxSignData net ctx publicKey tsd
  -- signing
  let myInputs = txInfoMyInputs txInfo
      othInputs = txInfoOtherInputs txInfo
      mySigInputs = mconcat $ myInputsSigInput <$> Map.elems myInputs
      othSigInputs = mconcat $ otherInputsSigInput <$> Map.elems othInputs
      sigInputs = mySigInputs <> othSigInputs
  signedTx <- signTx net ctx tx sigInputs secKeys
  let txInfoS = signTxInfo signedTx txInfo
      isSigned = verifyTxInfo net ctx signedTx txInfoS
  unless isSigned $ Left "The transaction could not be signed"
  return
    ( tsd {txSignDataTx = signedTx, txSignDataSigned = True},
      txInfoS
    )

signTxInfo :: Tx -> TxInfo -> TxInfo
signTxInfo tx txInfo =
  txInfo
    { txInfoHash = Just $ txHash tx,
      txInfoPending = Just $ TxInfoPending (nosigTxHash tx) True False
    }

verifyTxInfo :: Network -> Ctx -> Tx -> TxInfo -> Bool
verifyTxInfo net ctx tx txInfo =
  let myInputs = txInfoMyInputs txInfo
      othInputs = txInfoOtherInputs txInfo
      mySigInputs = mconcat $ myInputsSigInput <$> Map.elems myInputs
      othSigInputs = mconcat $ otherInputsSigInput <$> Map.elems othInputs
      sigInputs = mySigInputs <> othSigInputs
      f i = (i.script, i.value, i.outpoint)
      vDat = f <$> sigInputs
   in Just (txHash tx) == txInfoHash txInfo
        && noEmptyInputs tx
        && verifyStdTx net ctx tx vDat

noEmptyInputs :: Tx -> Bool
noEmptyInputs = (not . any BS.null) . fmap (.script) . (.inputs)

{- Message Signing -}

-- Message signatures follow the "legacy" format of BIP-322 (Generic Signed
-- Message Format), which is the Bitcoin Core signmessage/verifymessage scheme
-- (src/common/signmessage.cpp in Bitcoin Core):
--
--   * BIP-322 states that the legacy format "MUST be restricted to the legacy
--     P2PKH invoice address format". hw only generates compressed P2PKH
--     addresses and signMessage rejects any address that is not the compressed
--     P2PKH address of the signing key.
--   * BIP-322 recommends (SHOULD) its new formats even for P2PKH. We use the
--     legacy format because it is accepted by Bitcoin Core and other legacy
--     verifiers. Supporting SegWit or Taproot addresses would require the
--     BIP-322 simple or full formats instead.
--   * The message is signed byte for byte as UTF-8, with no normalization.
--     Leading or trailing whitespace and line endings are part of the signed
--     message (see messageWarnings).
--   * There is no standard for the armored "BEGIN BITCOIN SIGNED MESSAGE"
--     block. Bitcoin Core never parses it: verifiers take the address, the
--     signature and the message as three separate values.

-- | Hash a message using the Bitcoin Signed Message format:
-- SHA256d(VarString "Bitcoin Signed Message:\n" <> VarString message)
-- The prefix provides domain separation so that a message signature can
-- never be valid as a transaction signature.
signedMessageHash :: Text -> Hash256
signedMessageHash message =
  doubleSHA256 $
    S.encode (VarString "Bitcoin Signed Message:\n")
      <> S.encode (VarString $ encodeUtf8 message)

-- | Sign a message using the Bitcoin Signed Message format. The address must
-- be the compressed P2PKH address of the signing key. Returns the 65-byte
-- recoverable signature: a header byte followed by r and s (32 bytes each).
-- This matches CKey::SignCompact in Bitcoin Core: the header byte is
-- 27 + recovery id + 4 for a compressed public key (31 or 32 in practice).
signMessage :: Ctx -> Address -> SecKey -> Text -> Either String BS.ByteString
signMessage ctx addr key message = do
  unless (pubKeyAddr ctx (wrapPubKey True pub) == addr) $
    Left "The address does not match the signing key"
  recId <- recoveryId ctx pub h r s
  -- Header byte: 27 + recovery id + 4 (compressed public key)
  return $ BS.cons (27 + 4 + recId) rs
  where
    pub = derivePubKey ctx key
    h = signedMessageHash message
    -- libsecp256k1 signs with RFC6979 nonces and returns low-S signatures
    rs = (exportCompactSig ctx $ signHash ctx key h).get
    (r, s) = BS.splitAt 32 rs

-- | Find the recovery id of the signature (r, s) of hash e by the public key Q.
-- It is the y-parity of the nonce point R, whose x coordinate is r. We test
-- both candidate points for the ECDSA relation s*R == e*G + r*Q. The case
-- where the x coordinate of R is r + n (recovery ids 2 and 3) happens with
-- negligible probability and is reported as an error.
recoveryId ::
  Ctx ->
  PubKey ->
  Hash256 ->
  BS.ByteString ->
  BS.ByteString ->
  Either String Word8
recoveryId ctx pub h r s = do
  rT <- toTweak r
  sT <- toTweak s
  eT <- toTweak $ toBytes32 $ bsToInteger (S.encode h) `mod` secp256k1Order
  rhs <- maybeToEither err $ (`tweakAddPubKey'` eT) =<< tweakMulPubKey ctx pub rT
  let candidate i = do
        rPoint <- importPubKey ctx $ BS.cons (0x02 + i) r
        sR <- tweakMulPubKey ctx rPoint sT
        return $ exportPubKey ctx True sR == rhs
  case filter ((== Just True) . candidate) [0, 1] of
    [i] -> return i
    _ -> Left err
  where
    err = "Could not compute the recovery id of the message signature"
    toTweak = maybeToEither err . tweak
    tweakAddPubKey' p t = exportPubKey ctx True <$> tweakAddPubKey ctx p t
    toBytes32 i =
      let bs = integerToBS i
       in BS.replicate (32 - BS.length bs) 0 <> bs

-- | Order n of the secp256k1 curve
secp256k1Order :: Integer
secp256k1Order =
  0xFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFEBAAEDCE6AF48A03BBFD25E8CD0364141

-- | Warnings for messages that are hard to reproduce exactly. A verifier must
-- use the exact signed bytes, but whitespace at the edges of a message and
-- line endings are easily altered when copying it (for example from the
-- armored block, or through email or Windows tools converting LF to CRLF).
-- A single altered byte makes a valid signature fail verification.
messageWarnings :: Text -> [String]
messageWarnings message =
  [ "Warning: The message has leading or trailing whitespace. It is part of\
    \ the signed message and is easily lost when copying it."
    | edgeSpace
  ]
    <> [ "Warning: The message contains line breaks. Line endings are easily\
         \ altered when copying the message (for example LF to CRLF)."
         | lineBreaks
       ]
    <> [ "A verifier must use the exact same message or the signature will\
         \ be invalid. Consider using the --json output to copy the message."
         | edgeSpace || lineBreaks
       ]
  where
    edgeSpace =
      not (Text.null message)
        && (isSpace (Text.head message) || isSpace (Text.last message))
    lineBreaks = Text.any (`elem` ['\n', '\r']) message

{- Transaction Sweeping -}

buildSweepSignData ::
  (MonadUnliftIO m) =>
  Network ->
  Ctx ->
  Config ->
  DBAccountId ->
  [SecKey] ->
  [Address] ->
  Natural ->
  Natural ->
  ExceptT String (DB m) TxSignData
buildSweepSignData net ctx cfg accId prvKeys sweepTo feeByte dust
  | null prvKeys = throwError "No private keys to sweep from"
  | null sweepTo = throwError "No addresses to sweep to"
  | otherwise = do
      let host = apiHost net cfg
      -- Generate the addresses to sweep from
      let sweepFrom = nub $ concatMap (genPossibleAddrs net ctx) prvKeys
      -- Get the unspent coins of the sweepFrom addresses
      Store.SerialList coins <-
        liftExcept . apiBatch ctx (configCoinBatch cfg) host $
          GetAddrsUnspent sweepFrom def {limit = Just 0}
      when (null coins) $
        throwError "There are no coins to sweep in those addresses"
      -- Build a set of sweep transactions
      gen <- liftIO initStdGen
      tx <- liftEither $ buildSweepTx net ctx gen coins sweepTo feeByte dust
      -- Get the dependent transactions
      let depTxHash = (.hash) . (.outpoint) <$> (.inputs) tx
      Store.RawResultList depTxs <-
        liftExcept . apiBatch ctx (configTxFullBatch cfg) host $
          GetTxsRaw depTxHash
      -- Check if any of the coins belong to us
      inDerivs <- rights <$> lift (mapM (getAddrDeriv net accId) sweepFrom)
      -- Check if any of the sweepTo addrs belong to us
      outDerivs <- rights <$> lift (mapM (getAddrDeriv net accId) sweepTo)
      return $ TxSignData tx depTxs (nub inDerivs) (nub outDerivs) False

genPossibleAddrs :: Network -> Ctx -> SecKey -> [Address]
genPossibleAddrs net ctx k
  | net `elem` [btc, btcTest, btcRegTest] =
      [ pubKeyAddr ctx pc,
        pubKeyAddr ctx pu,
        pubKeyWitnessAddr ctx pc,
        pubKeyWitnessAddr ctx pu,
        pubKeyCompatWitnessAddr ctx pc,
        pubKeyCompatWitnessAddr ctx pu
      ]
  | otherwise =
      [ pubKeyAddr ctx pc,
        pubKeyAddr ctx pu
      ]
  where
    c = wrapSecKey False k :: PrivateKey -- Compressed
    u = wrapSecKey True k :: PrivateKey -- Uncompressed
    pc = derivePublicKey ctx c
    pu = derivePublicKey ctx u

buildSweepTx ::
  Network ->
  Ctx ->
  StdGen ->
  [Store.Unspent] ->
  [Address] ->
  Natural ->
  Natural ->
  Either String Tx
buildSweepTx net ctx gen coins sweepTo feeByte dust =
  flip evalStateT gen $ do
    rdmOutpoints <- ((.outpoint) <$>) <$> randomShuffle coins
    rdmSweepTo <- randomShuffle sweepTo
    let coinsTot = sum $ (.value) <$> coins
        fee = guessTxFee (fromIntegral feeByte) (length sweepTo) (length coins)
    when (coinsTot < fee) $
      throwError "Could not find a sweep solution: fee is too large"
    let (q, r) = (coinsTot - fee) `quotRem` fromIntegral (length sweepTo)
        amnts = (q + r) : repeat q
    when (q <= fromIntegral dust) $
      throwError "Outputs are smaller than the dust value"
    addrsT <- lift $ mapM (maybeToEither "Addr" . addrToText net) rdmSweepTo
    lift $ buildAddrTx net ctx rdmOutpoints (zip addrsT amnts)

-- Utilities --

randomRange :: (Random a, MonadState StdGen m) => a -> a -> m a
randomRange low hi = do
  gen <- get
  let (res, newGen) = randomR (low, hi) gen
  put newGen
  return res

randomShuffle :: (MonadState StdGen m) => [a] -> m [a]
randomShuffle [] = return []
randomShuffle [x] = return [x]
randomShuffle xs = do
  i <- randomRange 0 (length xs - 1)
  case splitAt i xs of
    (as, e : bs) -> (e :) <$> randomShuffle (as <> bs)
    _ -> error "randomShuffle"
