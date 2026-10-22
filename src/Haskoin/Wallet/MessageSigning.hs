{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Signing of messages with the key of a wallet address, in the Bitcoin
-- Signed Message format.
module Haskoin.Wallet.MessageSigning where

import Control.Monad (unless)
import qualified Data.ByteString as BS
import Data.Char (isSpace)
import qualified Data.Serialize as S
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Text.Encoding (encodeUtf8)
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

{- The recovery id -}

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
    <> [ "A verifier must use the exact same message or the signature will\
         \ be invalid. Consider using the --json output to copy the message."
         | edgeSpace || lineBreaks
       ]
  where
    edgeSpace =
      not (Text.null message)
        && (isSpace (Text.head message) || isSpace (Text.last message))
    lineBreaks = Text.any (`elem` ['\n', '\r']) message
