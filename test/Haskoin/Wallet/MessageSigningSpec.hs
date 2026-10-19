{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

module Haskoin.Wallet.MessageSigningSpec where

import Control.Monad (forM_, unless)
import Data.Aeson (FromJSON (..), withObject, (.!=), (.:), (.:?))
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base64 as B64
import Data.Char (isAscii)
import Data.Either (isLeft)
import Data.List (sort)
import qualified Data.Serialize as S
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import Haskoin
import Haskoin.Util.Arbitrary (arbitraryPrivateKey)
import Haskoin.Wallet.Arbitrary (arbitraryText)
import Haskoin.Wallet.MessageSigning
import Haskoin.Wallet.TestUtils
import Test.Hspec
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck (forAll)

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
    propertiesSpec ctx

{- Vector files -}

data VectorFile = VectorFile
  { fileHashVectors :: ![HashVector],
    fileVectors :: ![Vector],
    fileRejected :: ![Rejected]
  }

instance FromJSON VectorFile where
  parseJSON =
    withObject "VectorFile" $ \o ->
      VectorFile
        <$> o .:? "hashVectors" .!= []
        <*> o .:? "vectors" .!= []
        <*> o .:? "rejected" .!= []

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

{- Tests -}

vectorCount :: VectorFile -> Int
vectorCount doc =
  length doc.fileHashVectors + length doc.fileVectors + length doc.fileRejected

fileProblems :: Ctx -> VectorFile -> Problems
fileProblems ctx doc =
  concatMap hashVectorProblems doc.fileHashVectors
    <> concatMap (vectorProblems ctx) doc.fileVectors
    <> concatMap (rejectedProblems ctx) doc.fileRejected

hashVectorProblems :: HashVector -> Problems
hashVectorProblems v =
  named v.hashName $
    utf8Problems v.hashMessage v.hashUtf8Hex
      <> check
        "message hash"
        v.hashExpected
        (encodeHex $ S.encode $ signedMessageHash v.hashMessage)

vectorProblems :: Ctx -> Vector -> Problems
vectorProblems ctx v =
  named v.vecName $ reportSetupError $ do
    key <- prefixError "key" $ vectorKey ctx v.vecNetwork v.vecKey
    expected <- prefixError "signature" $ decodeSignature v.vecSignature
    let pub = derivePubKey ctx key
        addr = pubKeyAddr ctx (wrapPubKey True pub)
        h = signedMessageHash v.vecMessage
        own = signMessage ctx addr key v.vecMessage
        ownValid = either (const False) (validSignature ctx pub h) own
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
            check "signature (base64)" (Right v.vecSignature) (encodeSignature <$> own)
          ExpectVerify ->
            require "the vector signature is valid" (validSignature ctx pub h expected)
              <> require "hw produces a valid signature" ownValid
          ExpectDiffer ->
            require "hw produces a different signature" (own /= Right expected)
              <> require
                "the vector signature is not valid for hw's message hash"
                (not $ validSignature ctx pub h expected)
              <> require "hw produces a valid signature" ownValid
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

-- | Checks across all the vectors
coverageSpec :: [(FilePath, Either String VectorFile)] -> Spec
coverageSpec files =
  it "contains exact signatures with recovery ids 0 and 1" $ do
    let sigs =
          [ v.vecSignature
            | (_, Right doc) <- files,
              v <- doc.fileVectors,
              v.vecExpect == ExpectMatch
          ]
        headers = [BS.head s | Right s <- decodeSignature <$> sigs]
    unless (31 `elem` headers && 32 `elem` headers) $
      expectationFailure "Missing signatures with header bytes 31 and 32"

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
      prop "produces deterministic and valid signatures" $
        forAll arbitraryPrivateKey $ \prv ->
          forAll arbitraryText $ \message ->
            let pub = derivePubKey ctx prv.key
                addr = pubKeyAddr ctx (wrapPubKey True pub)
                sigE = signMessage ctx addr prv.key message
                h = signedMessageHash message
             in sigE == signMessage ctx2 addr prv.key message
                  && either (const False) (validSignature ctx pub h) sigE
      prop "refuses to sign for the address of another key" $
        forAll arbitraryPrivateKey $ \prv1 ->
          forAll arbitraryPrivateKey $ \prv2 ->
            let addr2 = pubKeyAddr ctx (wrapPubKey True (derivePubKey ctx prv2.key))
             in prv1.key == prv2.key || isLeft (signMessage ctx addr2 prv1.key "msg")

{- Signatures -}

encodeSignature :: BS.ByteString -> Text
encodeSignature = decodeUtf8 . B64.encode

decodeSignature :: Text -> Either String BS.ByteString
decodeSignature txt = do
  bs <- B64.decode $ encodeUtf8 txt
  unless (BS.length bs == 65) $ Left "The signature must be 65 bytes"
  return bs

-- | Verify a 65-byte legacy signature against a known compressed public key.
-- The header must flag a compressed key with the correct recovery id and
-- (r, s) must be a valid ECDSA signature of the hash. High S values are
-- accepted, like Bitcoin Core's verifymessage does.
validSignature :: Ctx -> PubKey -> Hash256 -> BS.ByteString -> Bool
validSignature ctx pub h sigBS =
  case BS.uncons sigBS of
    Just (header, rs) | BS.length rs == 64 ->
      let (r, s) = BS.splitAt 32 rs
       in header >= 31
            && recoveryId ctx pub h r s == Right (header - 31)
            && maybe False (\ecdsaSig -> verifyHashSig ctx h ecdsaSig pub) (importCompactSig ctx (CompactSig rs))
    _ -> False

-- | The kinds of warnings that signmsg prints for a message, as written in the
-- vectors: "whitespace" and "linebreak". Only the lines starting with
-- "Warning:" are warnings. The note for the verifier is not checked.
warningKinds :: Text -> [Text]
warningKinds message = sort (map warningKind warnings)
  where
    printed = map Text.pack (messageWarnings message)
    warnings = filter ("Warning:" `Text.isPrefixOf`) printed
    warningKind warning
      | "whitespace" `Text.isInfixOf` warning = "whitespace"
      | "line breaks" `Text.isInfixOf` warning = "linebreak"
      | otherwise = "unknown warning: " <> warning
