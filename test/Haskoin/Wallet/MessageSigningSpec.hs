{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

module Haskoin.Wallet.MessageSigningSpec where

import Control.Monad (forM_, unless)
import Data.Aeson (FromJSON (..), withObject, (.!=), (.:), (.:?))
import Data.Bits (xor)
import qualified Data.ByteString as BS
import Data.Char (isAscii)
import Data.Either (isLeft)
import Data.List (sort)
import qualified Data.Serialize as S
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Text.Encoding (encodeUtf8)
import Data.Word (Word8)
import Haskoin
import Haskoin.Util.Arbitrary (arbitraryPrivateKey)
import Haskoin.Wallet.Arbitrary (arbitraryText)
import Haskoin.Wallet.MessageSigning
import Haskoin.Wallet.TestUtils
import Test.Hspec
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck (choose, forAll, (==>))

-- The test vectors live in JSON files in this directory (see the README.md
-- file there for the format and the origin of the vectors). Every .json file
-- is loaded, so new vectors can be added without changing this module.
vectorDir :: FilePath
vectorDir = "test/data/message-signing"

spec :: Spec
spec =
  prepareContext $ \ctx -> do
    files <- runIO $ loadVectorFiles vectorDir
    describe "Message signing test vectors" $ do
      forM_ files $ vectorFileSpec vectorCount (fileProblems ctx)
      coverageSpec files
    verifySpec ctx
    blockSpec
    propertiesSpec ctx

{- Vector files -}

data VectorFile = VectorFile
  { fileHashVectors :: ![HashVector],
    fileVectors :: ![Vector],
    fileRejected :: ![Rejected],
    fileVerification :: ![Verification]
  }

instance FromJSON VectorFile where
  parseJSON =
    withObject "VectorFile" $ \o ->
      VectorFile
        <$> o .:? "hashVectors" .!= []
        <*> o .:? "vectors" .!= []
        <*> o .:? "rejected" .!= []
        <*> o .:? "verification" .!= []

data HashVector = HashVector
  { hashName :: !Text,
    hashMessage :: !Text,
    hashUtf8Hex :: !(Maybe Text),
    hashExpected :: !Text
  }

instance FromJSON HashVector where
  parseJSON =
    withObject "HashVector" $ \o ->
      HashVector
        <$> o .: "name"
        <*> o .: "message"
        <*> o .:? "messageUtf8Hex"
        <*> o .: "messageHash"

-- | What a vector expects from hw's signature
data Expect
  = -- | hw must produce exactly the signature of the vector
    ExpectMatch
  | -- | The signature was made with a different nonce. It must be a valid
    -- signature of hw's message hash by the key.
    ExpectVerify
  | -- | The source signs different bytes, so hw must produce a different
    -- signature
    ExpectDiffer
  deriving (Eq)

data Vector = Vector
  { vecName :: !Text,
    vecNetwork :: !Network,
    vecKey :: !KeySpec,
    vecAddress :: !Text,
    vecMessage :: !Text,
    vecUtf8Hex :: !(Maybe Text),
    vecHash :: !(Maybe Text),
    vecSignature :: !Text,
    vecExpect :: !Expect,
    -- | Expected signmsg warnings. An absent field means no warnings.
    vecWarnings :: ![Text]
  }

instance FromJSON Vector where
  parseJSON =
    withObject "Vector" $ \o -> do
      net <- parseNetwork =<< o .: "network"
      message <- o .: "message"
      rep <- o .:? "messageRepeat" .!= 1
      expect <-
        (o .:? "expect" .!= "match") >>= \case
          "match" -> return ExpectMatch
          "verify" -> return ExpectVerify
          "differ" -> return ExpectDiffer
          e -> fail $ "Invalid expect value: " <> Text.unpack e
      Vector
        <$> o .: "name"
        <*> pure net
        <*> o .: "key"
        <*> o .: "address"
        <*> pure (Text.replicate rep message)
        <*> o .:? "messageUtf8Hex"
        <*> o .:? "messageHash"
        <*> o .: "signature"
        <*> pure expect
        <*> o .:? "warnings" .!= []

-- | A key and an address that signMessage must refuse to sign for
data Rejected = Rejected
  { rejName :: !Text,
    rejNetwork :: !Network,
    rejKey :: !KeySpec,
    rejAddress :: !Text
  }

instance FromJSON Rejected where
  parseJSON =
    withObject "Rejected" $ \o ->
      Rejected
        <$> o .: "name"
        <*> (parseNetwork =<< o .: "network")
        <*> o .: "key"
        <*> o .: "address"

-- | What verifyMessage must answer: the signature is valid, it is not, or
-- the check is refused (the address is not P2PKH, or the signature is not
-- 65 bytes in base64)
data Verdict = Valid | Invalid | Refused
  deriving (Eq, Show)

-- | An address, a signature and a message, with the verdict expected. No key:
-- a verifier recovers it from the signature.
data Verification = Verification
  { verName :: !Text,
    verNetwork :: !Network,
    verAddress :: !Text,
    verMessage :: !Text,
    verUtf8Hex :: !(Maybe Text),
    verSignature :: !Text,
    verResult :: !Verdict
  }

instance FromJSON Verification where
  parseJSON =
    withObject "Verification" $ \o -> do
      net <- parseNetwork =<< o .: "network"
      message <- o .: "message"
      rep <- o .:? "messageRepeat" .!= 1
      result <-
        (o .: "result") >>= \case
          "valid" -> return Valid
          "invalid" -> return Invalid
          "refused" -> return Refused
          r -> fail $ "Invalid result value: " <> Text.unpack r
      Verification
        <$> o .: "name"
        <*> pure net
        <*> o .: "address"
        <*> pure (Text.replicate rep message)
        <*> o .:? "messageUtf8Hex"
        <*> o .: "signature"
        <*> pure result

{- Tests -}

vectorCount :: VectorFile -> Int
vectorCount doc =
  length doc.fileHashVectors + length doc.fileVectors + length doc.fileRejected + length doc.fileVerification

fileProblems :: Ctx -> VectorFile -> Problems
fileProblems ctx doc =
  concatMap hashVectorProblems doc.fileHashVectors
    <> concatMap (vectorProblems ctx) doc.fileVectors
    <> concatMap (rejectedProblems ctx) doc.fileRejected
    <> concatMap (verificationProblems ctx) doc.fileVerification

hashVectorProblems :: HashVector -> Problems
hashVectorProblems v =
  named v.hashName $
    utf8Problems v.hashMessage v.hashUtf8Hex
      <> check
        "message hash"
        v.hashExpected
        (encodeHex $ S.encode $ signedMessageHash v.hashMessage)

-- | A signing vector: hw must sign as the vector expects, and the signature
-- of the vector must verify for hw's message bytes, unless it signs others
vectorProblems :: Ctx -> Vector -> Problems
vectorProblems ctx v =
  named v.vecName $ reportSetupError $ do
    key <- prefixError "key" $ vectorKey ctx v.vecNetwork v.vecKey
    let pub = derivePubKey ctx key
        addr = pubKeyAddr ctx (wrapPubKey True pub)
        h = signedMessageHash v.vecMessage
        own = encodeSignature <$> signMessage ctx addr key v.vecMessage
        verified signature = verifyMessage ctx addr signature v.vecMessage
        ownVerified = (verified =<< own) == Right True
    return $
      -- Compare parsed addresses, but report them as text
      ( if textToAddr v.vecNetwork v.vecAddress == Just addr
          then []
          else check "address of the key" (Just v.vecAddress) (addrToText v.vecNetwork addr)
      )
        <> utf8Problems v.vecMessage v.vecUtf8Hex
        <> maybe [] (\hash -> check "message hash" hash (encodeHex $ S.encode h)) v.vecHash
        <> case v.vecExpect of
          ExpectMatch ->
            check "signature (base64)" (Right v.vecSignature) own
              <> check "the vector signature verifies" (Right True) (verified v.vecSignature)
          ExpectVerify ->
            check "the vector signature verifies" (Right True) (verified v.vecSignature)
              <> require "hw produces a signature that verifies" ownVerified
          ExpectDiffer ->
            require "hw produces a different signature" (own /= Right v.vecSignature)
              <> check "the vector signature does not verify for hw's message bytes" (Right False) (verified v.vecSignature)
              <> require "hw produces a signature that verifies" ownVerified
        <> check "warnings" (sort v.vecWarnings) (warningKinds v.vecMessage)

-- | Problems with the UTF-8 bytes of a message. A message that is not plain
-- ASCII must give its expected bytes in a messageUtf8Hex field: Unicode text
-- can be changed invisibly, for example by an editor normalizing it, and the
-- signature would then be computed over different bytes.
utf8Problems :: Text -> Maybe Text -> Problems
utf8Problems message Nothing =
  require "a non-ASCII message has a messageUtf8Hex field" (Text.all isAscii message)
utf8Problems message (Just bytes) =
  check "UTF-8 bytes of the message" bytes (encodeHex $ encodeUtf8 message)

rejectedProblems :: Ctx -> Rejected -> Problems
rejectedProblems ctx v =
  named v.rejName $ reportSetupError $ do
    key <- prefixError "key" $ vectorKey ctx v.rejNetwork v.rejKey
    addr <- maybeToEither "invalid address" $ textToAddr v.rejNetwork v.rejAddress
    return $
      require "signMessage refuses to sign for this address" $
        isLeft (signMessage ctx addr key "Rejected address")

-- | A verification vector: verifyMessage must give the verdict expected
verificationProblems :: Ctx -> Verification -> Problems
verificationProblems ctx v =
  named v.verName $ reportSetupError $ do
    addr <- maybeToEither "invalid address" $ textToAddr v.verNetwork v.verAddress
    let verdict = case verifyMessage ctx addr v.verSignature v.verMessage of
          Right True -> Valid
          Right False -> Invalid
          Left _ -> Refused
    return $ utf8Problems v.verMessage v.verUtf8Hex <> check "verdict" v.verResult verdict

-- | Checks across all the vectors
coverageSpec :: [(FilePath, Either String VectorFile)] -> Spec
coverageSpec files = do
  it "contains exact signatures with recovery ids 0 and 1" $ do
    let sigs =
          [ v.vecSignature
            | (_, Right doc) <- files,
              v <- doc.fileVectors,
              v.vecExpect == ExpectMatch
          ]
    unless (all (`elem` headersOf sigs) [31, 32]) $
      expectationFailure "Missing signatures with header bytes 31 and 32"
  it "contains valid signatures of uncompressed and compressed keys, with recovery ids 0 and 1" $ do
    let sigs =
          [ v.verSignature
            | (_, Right doc) <- files,
              v <- doc.fileVerification,
              v.verResult == Valid
          ]
    forM_ [27, 28, 31, 32] $ \header ->
      unless (header `elem` headersOf sigs) $
        expectationFailure $ "Missing a valid signature with the header byte " <> show header
  where
    headersOf sigs = [header | Right (header, _, _) <- decodeSignature <$> sigs]

-- | What verifyMessage says of signatures written by hand
verifySpec :: Ctx -> Spec
verifySpec ctx =
  describe "Verifying a message signature" $ do
    -- The signature of "hello" by the address /0/0 of the test wallet, from
    -- signmsg, checked apart from hw with the pure-Python verifier of the
    -- message-signing vectors
    it "finds the signature of signmsg valid for the message signed" $
      verifyMessage ctx (externalAddr 0) helloSignature "hello" `shouldBe` Right True

    it "finds it invalid for a message that differs in one byte" $
      verifyMessage ctx (externalAddr 0) helloSignature "hello " `shouldBe` Right False

    it "finds it invalid for another address of the wallet" $
      verifyMessage ctx (externalAddr 1) helloSignature "hello" `shouldBe` Right False

    it "finds a signature whose r or s is outside the curve order invalid" $
      forM_ [(0, 1), (secp256k1Order, 1), (1, secp256k1Order)] $ \(r, s) ->
        verifyMessage ctx (externalAddr 0) (signatureOf 31 r s) "hello" `shouldBe` Right False

    -- 26 is below the format; 36 and 40 are the BIP-137 header bytes of
    -- SegWit signatures with the same recovery id as the signature
    it "finds a signature invalid when its header byte is outside 27 to 34, whatever its low bits say" $
      forM_ [26, 36, 40] $ \header ->
        verifyMessage ctx (externalAddr 0) (onBytes (\_ r s -> BS.cons header (r <> s)) helloSignature) "hello"
          `shouldBe` Right False

    it "refuses an address that is not P2PKH" $
      -- The P2SH address of Bitcoin Core's message_verify test
      verifyMessage ctx (testAddress "3B5fQsEXEaV8v6U3ejYc8XaKXAkyQj2MjV") helloSignature "hello"
        `shouldBe` Left "The address is not a P2PKH address: a message can only be signed from one"

    it "refuses a signature that is not in base64" $
      verifyMessage ctx (externalAddr 0) "invalid signature, not in base64 encoding" "hello"
        `shouldBe` Left "The signature is not in base64"

    it "refuses a signature that is not 65 bytes" $
      verifyMessage ctx (externalAddr 0) (encodeSignature (BS.replicate 64 1)) "hello"
        `shouldBe` Left "The signature must be 65 bytes, not 64"
  where
    signatureOf header r s = encodeSignature $ BS.cons header (toBytes32 r <> toBytes32 s)

-- | The armored block signmsg prints, written and read back
blockSpec :: Spec
blockSpec =
  describe "The armored block of a signed message" $ do
    it "is printed with the markers Sparrow and other wallets read" $
      signedMessageBlock addr helloSignature "hello"
        `shouldBe` [ "-----BEGIN BITCOIN SIGNED MESSAGE-----",
                     "hello",
                     "-----BEGIN BITCOIN SIGNATURE-----",
                     addr,
                     helloSignature,
                     "-----END BITCOIN SIGNATURE-----"
                   ]

    prop "is read back as it was written, whatever the message" $
      forAll arbitraryText $ \message ->
        readSignedMessageBlock (Text.unlines (signedMessageBlock addr helloSignature message))
          == Right (addr, helloSignature, message)

    it "reads the block hw 0.10 printed, with its own markers" $
      readSignedMessageBlock
        (Text.unlines ["-----BEGIN BITCOIN SIGNED MESSAGE-----", "hello", "-----BEGIN SIGNATURE-----", addr, helloSignature, "-----END BITCOIN SIGNED MESSAGE-----"])
        `shouldBe` Right (addr, helloSignature, "hello")

    -- The warning about line endings then tells the user what happened
    it "keeps the lines of the message as they are: a block saved with CRLF line endings holds another message" $
      readSignedMessageBlock
        "-----BEGIN BITCOIN SIGNED MESSAGE-----\r\n  two \r\nlines \r\n\r\n-----BEGIN BITCOIN SIGNATURE-----\r\nADDRESS\r\nSIGNATURE\r\n-----END BITCOIN SIGNATURE-----\r\n"
        `shouldBe` Right ("ADDRESS", "SIGNATURE", "  two \r\nlines \r\n\r")

    it "passes over the blank lines, the headers and the wrapping of the signature part" $ do
      let (firstHalf, secondHalf) = Text.splitAt 44 helloSignature
      readSignedMessageBlock
        (Text.unlines ["-----BEGIN BITCOIN SIGNED MESSAGE-----", "hello", "-----BEGIN BITCOIN SIGNATURE-----", "Version: Bitcoin-qt (1.0)", "", addr, "", firstHalf, secondHalf, "-----END BITCOIN SIGNATURE-----"])
        `shouldBe` Right (addr, helloSignature, "hello")

    it "leaves aside the text around the block" $
      readSignedMessageBlock
        (Text.unlines ["Here is my proof:", "", "-----BEGIN BITCOIN SIGNED MESSAGE-----", "hello", "-----BEGIN BITCOIN SIGNATURE-----", addr, helloSignature, "-----END BITCOIN SIGNATURE-----", "", "Regards"])
        `shouldBe` Right (addr, helloSignature, "hello")

    it "refuses text without the three parts of a block" $ do
      readSignedMessageBlock "hello"
        `shouldBe` Left "no \"-----BEGIN BITCOIN SIGNED MESSAGE-----\" line"
      readSignedMessageBlock "-----BEGIN BITCOIN SIGNED MESSAGE-----\nhello\n"
        `shouldBe` Left "no \"-----BEGIN BITCOIN SIGNATURE-----\" line after the message"
      readSignedMessageBlock (Text.unlines ["-----BEGIN BITCOIN SIGNED MESSAGE-----", "hello", "-----BEGIN BITCOIN SIGNATURE-----", addr, "-----END BITCOIN SIGNATURE-----"])
        `shouldBe` Left "the signature part of the block must hold the address, then the signature"
  where
    addr = externalAddrsText !! 0

propertiesSpec :: Ctx -> Spec
propertiesSpec ctx =
  -- A second, separately created and randomized context. Signing with both
  -- contexts gives two calls with different arguments, which GHC can not merge
  -- into one (common subexpression elimination). The signatures must still be
  -- identical: RFC6979 nonces do not depend on the context randomization.
  prepareContext $ \ctx2 ->
    describe "Message signing properties" $ do
      prop "hashes messages with the Bitcoin Signed Message prefix" $
        forAll arbitraryText $ \message ->
          signedMessageHash message /= doubleSHA256 (encodeUtf8 message)
      prop "produces deterministic signatures, which verify" $
        forAll arbitraryPrivateKey $ \prv ->
          forAll arbitraryText $ \message ->
            let (addr, signature) = signedBy ctx prv.key message
             in signature == snd (signedBy ctx2 prv.key message)
                  && verifyMessage ctx addr signature message == Right True
      prop "refuses to sign for the address of another key" $
        forAll arbitraryPrivateKey $ \prv1 ->
          forAll arbitraryPrivateKey $ \prv2 ->
            let addr2 = pubKeyAddr ctx (wrapPubKey True (derivePubKey ctx prv2.key))
             in prv1.key == prv2.key || isLeft (signMessage ctx addr2 prv1.key "msg")
      prop "does not verify a signature for another message" $
        forAll arbitraryPrivateKey $ \prv ->
          forAll arbitraryText $ \message ->
            forAll arbitraryText $ \other ->
              message /= other ==>
                let (addr, signature) = signedBy ctx prv.key message
                 in verifyMessage ctx addr signature other == Right False
      prop "does not verify a signature for the address of another key" $
        forAll arbitraryPrivateKey $ \prv1 ->
          forAll arbitraryPrivateKey $ \prv2 ->
            prv1.key /= prv2.key ==>
              let (_, signature) = signedBy ctx prv1.key "msg"
                  (addr2, _) = signedBy ctx prv2.key "msg"
               in verifyMessage ctx addr2 signature "msg" == Right False
      prop "accepts a signature with a high S value, as Bitcoin Core does" $
        forAll arbitraryPrivateKey $ \prv ->
          forAll arbitraryText $ \message ->
            let (addr, signature) = signedBy ctx prv.key message
             in verifyMessage ctx addr (highS signature) message == Right True
      prop "verifies the signature of an uncompressed key for its own address only" $
        forAll arbitraryPrivateKey $ \prv ->
          forAll arbitraryText $ \message ->
            let (compressedAddr, signature) = signedBy ctx prv.key message
                uncompressedAddr = pubKeyAddr ctx (wrapPubKey False (derivePubKey ctx prv.key))
                uncompressedSig = uncompressed signature
             in verifyMessage ctx uncompressedAddr uncompressedSig message == Right True
                  && verifyMessage ctx compressedAddr uncompressedSig message == Right False
      prop "finds a signature with one byte altered invalid" $
        forAll arbitraryPrivateKey $ \prv ->
          forAll arbitraryText $ \message ->
            forAll (choose (0, 64)) $ \position ->
              forAll (choose (1, 255)) $ \change ->
                let (addr, signature) = signedBy ctx prv.key message
                 in verifyMessage ctx addr (alteredAt position change signature) message == Right False

{- Signatures -}

-- | The compressed address of the key and its signature of the message in
-- base64, as signmsg gives them
signedBy :: Ctx -> SecKey -> Text -> (Address, Text)
signedBy ctx key message = (addr, either error encodeSignature (signMessage ctx addr key message))
  where
    addr = pubKeyAddr ctx (wrapPubKey True (derivePubKey ctx key))

-- | The same signature with s replaced by n - s, as valid in ECDSA: the
-- nonce point is negated, so the recovery id of the header byte flips
highS :: Text -> Text
highS = onBytes $ \header r s ->
  BS.cons (27 + ((header - 27) `xor` 1)) (r <> toBytes32 (secp256k1Order - bsToInteger s))

-- | The same signature with its header byte saying the key is uncompressed
uncompressed :: Text -> Text
uncompressed = onBytes $ \header r s -> BS.cons (header - 4) (r <> s)

-- | The signature with the byte at the position changed by xor
alteredAt :: Int -> Word8 -> Text -> Text
alteredAt position change = onBytes $ \header r s ->
  let bytes = BS.cons header (r <> s)
   in BS.take position bytes <> BS.singleton (BS.index bytes position `xor` change) <> BS.drop (position + 1) bytes

-- | A change to the bytes of a signature in base64: its header byte, r and s
onBytes :: (Word8 -> BS.ByteString -> BS.ByteString -> BS.ByteString) -> Text -> Text
onBytes change signature =
  case decodeSignature signature of
    Right (header, r, s) -> encodeSignature (change header r s)
    Left err -> error err

-- | The kinds of warnings that signmsg prints for a message, as written in the
-- vectors: "whitespace" and "linebreak"
warningKinds :: Text -> [Text]
warningKinds message = sort (map (warningKind . Text.pack) (messageWarnings message))
  where
    warningKind warning
      | "whitespace" `Text.isInfixOf` warning = "whitespace"
      | "line breaks" `Text.isInfixOf` warning = "linebreak"
      | otherwise = "unknown warning: " <> warning
