{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Signing of messages with the key of a wallet address, in the Bitcoin
-- Signed Message format, and the verification of such signatures from any
-- address.
module Haskoin.Wallet.MessageSigning where

import Control.Monad (guard, unless)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base64 as B64
import Data.Char (isSpace)
import Data.List (find)
import qualified Data.Serialize as S
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import Data.Word (Word8)
import Haskoin

{- Signing -}

-- Message signatures follow the "legacy" format of BIP-322 (Generic Signed
-- Message Format), which is the Bitcoin Core signmessage/verifymessage scheme
-- (src/common/signmessage.cpp in Bitcoin Core):
--
--   * BIP-322 states that the legacy format "MUST be restricted to the legacy
--     P2PKH invoice address format". hw only generates compressed P2PKH
--     addresses and signMessage rejects any address that is not the compressed
--     P2PKH address of the signing key. verifyMessage takes the signatures of
--     uncompressed keys too, for their P2PKH address, as Bitcoin Core does.
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

-- | The recovery id of the signature (r, s) of the hash by the public key:
-- the id that recovers the key, found as Bitcoin Core finds it
-- (CKey::SignCompact)
recoveryId ::
  Ctx ->
  PubKey ->
  Hash256 ->
  BS.ByteString ->
  BS.ByteString ->
  Either String Word8
recoveryId ctx pub h r s =
  maybeToEither "Could not compute the recovery id of the message signature" $
    find recovers [0 .. 3]
  where
    recovers recId =
      (exportPubKey ctx True <$> recoverPubKey ctx h recId r s) == Just (exportPubKey ctx True pub)

-- | A signature in base64, as signmsg writes it
encodeSignature :: BS.ByteString -> Text
encodeSignature = decodeUtf8 . B64.encode

{- Verifying -}

-- | Whether the key of the address signed the message, from the signature in
-- base64 as signmsg writes it. The check is refused (Left) when the address
-- is not P2PKH, the only kind the format signs from, or when the signature
-- does not decode to 65 bytes. The verdict is the one of Bitcoin Core's
-- verifymessage: the signature recovers a public key, compressed or not as
-- its header byte says, and the address must be the address of that key. A
-- signature that recovers no key is not valid: a header byte outside 27 to
-- 34, or r or s outside the curve order.
verifyMessage :: Ctx -> Address -> Text -> Text -> Either String Bool
verifyMessage ctx addr signature message = do
  unless (isPubKeyAddress addr) $
    Left "The address is not a P2PKH address: a message can only be signed from one"
  (header, r, s) <- decodeSignature signature
  return $ case keyHeader header of
    Nothing -> False
    Just (compressed, recId) ->
      let signer = recoverPubKey ctx (signedMessageHash message) recId r s
       in (pubKeyAddr ctx . wrapPubKey compressed <$> signer) == Just addr

-- | The header byte, r and s of a signature written in base64: 65 bytes
decodeSignature :: Text -> Either String (Word8, BS.ByteString, BS.ByteString)
decodeSignature signature = do
  bytes <- either (const $ Left "The signature is not in base64") Right $ B64.decode (encodeUtf8 signature)
  case BS.uncons bytes of
    Just (header, rs) | BS.length rs == 64 -> Right (header, BS.take 32 rs, BS.drop 32 rs)
    _ -> Left $ "The signature must be 65 bytes, not " <> show (BS.length bytes)

-- | What the header byte of a signature says of the key: whether it is
-- compressed, and the recovery id. The bytes 27 to 30 are the ids 0 to 3 of
-- an uncompressed key, 31 to 34 those of a compressed one. Nothing for any
-- other byte: BIP-137 gives 35 to 42 to SegWit addresses, which the format
-- does not sign from.
keyHeader :: Word8 -> Maybe (Bool, Word8)
keyHeader header
  | header >= 27 && header <= 34 = Just (header >= 31, (header - 27) `mod` 4)
  | otherwise = Nothing

{- Recovering the key -}

-- | The public key that made the signature (r, s) of the hash, given the
-- recovery id: Q = r⁻¹(s·R − e·G), where e is the hash as a number and R the
-- point whose x coordinate is r, or r + n for the ids 2 and 3, and whose y
-- has the parity of the id. Nothing when no key comes out: r or s outside
-- the curve order, no point at that x, or a point at infinity on the way.
-- (A hash that is a multiple of the order recovers nothing either: no
-- message has one in practice.)
recoverPubKey :: Ctx -> Hash256 -> Word8 -> BS.ByteString -> BS.ByteString -> Maybe PubKey
recoverPubKey ctx h recId r s = do
  guard (inOrder rI && inOrder sI)
  -- importPubKey gives no point for an x at or past the field size
  rPoint <- importPubKey ctx $ BS.cons (0x02 + recId `mod` 2) (toBytes32 x)
  sT <- tweak s
  eNegated <- tweakNegate ctx =<< tweak (toBytes32 e)
  rInverse <- tweak $ toBytes32 $ inverseModOrder rI
  sR <- tweakMulPubKey ctx rPoint sT
  sRMinusEG <- tweakAddPubKey ctx sR eNegated
  tweakMulPubKey ctx sRMinusEG rInverse
  where
    rI = bsToInteger r
    sI = bsToInteger s
    inOrder i = 0 < i && i < secp256k1Order
    x = rI + if recId >= 2 then secp256k1Order else 0
    e = bsToInteger (S.encode h) `mod` secp256k1Order

-- | The inverse of a scalar modulo the curve order, which is prime: a^(n-2)
-- by Fermat's little theorem
inverseModOrder :: Integer -> Integer
inverseModOrder a = powMod a (secp256k1Order - 2)
  where
    powMod _ 0 = 1
    powMod b k
      | even k = square (powMod b (k `div` 2))
      | otherwise = (b * powMod b (k - 1)) `mod` secp256k1Order
    square c = (c * c) `mod` secp256k1Order

-- | A scalar as libsecp256k1 reads it: 32 bytes, big-endian
toBytes32 :: Integer -> BS.ByteString
toBytes32 i = BS.replicate (32 - BS.length bs) 0 <> bs
  where
    bs = integerToBS i

-- | Order n of the secp256k1 curve
secp256k1Order :: Integer
secp256k1Order =
  0xFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFEBAAEDCE6AF48A03BBFD25E8CD0364141

{- Warnings -}

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
  where
    edgeSpace =
      not (Text.null message)
        && (isSpace (Text.head message) || isSpace (Text.last message))
    lineBreaks = Text.any (`elem` ['\n', '\r']) message

-- | The warnings of a message that is being signed, with what the signer can
-- do about them
signingWarnings :: Text -> [String]
signingWarnings message =
  warnings
    <> [ "A verifier must use the exact same message or the signature will\
         \ be invalid. Consider using the --json output to copy the message."
         | not (null warnings)
       ]
  where
    warnings = messageWarnings message
