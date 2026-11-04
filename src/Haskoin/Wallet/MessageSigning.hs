{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Signing of messages with the key of a wallet address, in the Bitcoin
-- Signed Message format, with the header byte of BIP 137 for a SegWit
-- address, and the verification of such signatures from any address of
-- one key.
module Haskoin.Wallet.MessageSigning where

import Control.Monad (unless)
import Crypto.Secp256k1.Recovery (exportCompactRecSig, importCompactRecSig, recover, signRecMsg)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base64 as B64
import Data.Char (isSpace)
import Data.Maybe (catMaybes, fromMaybe)
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
--     P2PKH invoice address format". From a P2PKH address, hw signs as
--     Bitcoin Core does, and signMessage rejects any address that is not
--     an address of the compressed key. verifyMessage takes the signatures
--     of uncompressed keys too, for their P2PKH address, as Bitcoin Core
--     does.
--   * From a native SegWit address, the P2WPKH address of a BIP84 account,
--     hw signs the same way with the header byte of BIP 137, 39 to 42,
--     which marks the kind of address: what Trezor, Ledger, Coldcard and
--     Sparrow give for such an address. Bitcoin Core verifies no signature
--     from a SegWit address, and Electrum signs from one with the header
--     byte of a P2PKH address. verifyMessage therefore takes any header
--     byte of a compressed key for a SegWit address, as Sparrow does: the
--     verdict is whether the key the signature recovers gives the address.
--   * BIP-322 recommends (SHOULD) its new formats even for P2PKH. We use the
--     legacy format because it is accepted by Bitcoin Core and other legacy
--     verifiers. Taproot addresses would require the BIP-322 simple or
--     full formats instead.
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

-- | Sign a message using the Bitcoin Signed Message format. The address
-- must be the P2PKH or the P2WPKH address of the compressed signing key.
-- Returns the 65-byte recoverable signature: a header byte followed by r
-- and s (32 bytes each). The header byte is 27 + recovery id + 4 for the
-- P2PKH address of a compressed public key (31 or 32 in practice), as
-- CKey::SignCompact in Bitcoin Core makes it, and 27 + recovery id + 12
-- for a P2WPKH address, as BIP 137 marks it (39 or 40 in practice).
signMessage :: Ctx -> Address -> SecKey -> Text -> Either String BS.ByteString
signMessage ctx addr key message = do
  headerBase <-
    if pubKeyAddr ctx public == addr
      then Right compressedHeader
      else
        if pubKeyWitnessAddr ctx public == Just addr
          then Right witnessHeader
          else Left "The address does not match the signing key"
  return $ BS.cons (headerBase + recId) compact.get
  where
    public = wrapPubKey True (derivePubKey ctx key)
    -- libsecp256k1 signs with an RFC 6979 nonce and gives a low-S
    -- signature, with the id that recovers the key
    (compact, recId) = exportCompactRecSig ctx (signRecMsg ctx key (hashMsg (signedMessageHash message)))

-- | A signature in base64, as signmsg writes it
encodeSignature :: BS.ByteString -> Text
encodeSignature = decodeUtf8 . B64.encode

{- Verifying -}

-- | Whether the key of the address signed the message, from the signature in
-- base64 as signmsg writes it. The check is refused (Left) when the address
-- is not one of a single key, P2PKH, P2WPKH or P2SH-P2WPKH, the kinds the
-- format signs from, or when the signature does not decode to 65 bytes.
-- The verdict for a P2PKH address is the one of Bitcoin Core's
-- verifymessage: the signature recovers a public key, compressed or not
-- as its header byte says, and the address must be the address of that
-- key. For a SegWit address, the address must be the one of that kind of
-- the recovered key, whatever header byte of a compressed key the signer
-- chose: the one of BIP 137 or the one of a P2PKH address. A signature
-- that recovers no key is not valid: a header byte outside 27 to 42, or r
-- or s outside the curve order.
verifyMessage :: Ctx -> Address -> Text -> Text -> Either String Bool
verifyMessage ctx addr signature message = do
  unless (isPubKeyAddress addr || isWitnessPubKeyAddress addr || isScriptAddress addr) $
    Left "The address is not a P2PKH or a SegWit address of one key: a message can only be signed from one"
  (header, r, s) <- decodeSignature signature
  return $ case keyHeader header of
    Nothing -> False
    Just (compressed, recId) ->
      let signer = wrapPubKey compressed <$> recoverSigner ctx (signedMessageHash message) recId (r <> s)
       in maybe False (\key -> addr `elem` keyAddresses ctx key) signer

-- | The addresses of a key a message can be signed from: its P2PKH
-- address, and, for a compressed key, its P2WPKH and P2SH-P2WPKH ones
keyAddresses :: Ctx -> PublicKey -> [Address]
keyAddresses ctx key = pubKeyAddr ctx key : catMaybes [pubKeyWitnessAddr ctx key, pubKeyCompatWitnessAddr ctx key]

-- | The header byte, r and s of a signature written in base64: 65 bytes
decodeSignature :: Text -> Either String (Word8, BS.ByteString, BS.ByteString)
decodeSignature signature = do
  bytes <- either (const $ Left "The signature is not in base64") Right $ B64.decode (encodeUtf8 signature)
  case BS.uncons bytes of
    Just (header, rs) | BS.length rs == 64 -> Right (header, BS.take 32 rs, BS.drop 32 rs)
    _ -> Left $ "The signature must be 65 bytes, not " <> show (BS.length bytes)

-- | What the header byte of a signature says of the key: whether it is
-- compressed, and the recovery id. The bytes 27 to 30 are the ids 0 to 3 of
-- an uncompressed key, 31 to 34 those of a compressed one, and 35 to 42
-- those of a compressed one again, where BIP 137 marks a P2SH-P2WPKH
-- address with 35 to 38 and a P2WPKH one with 39 to 42. Nothing for any
-- other byte.
keyHeader :: Word8 -> Maybe (Bool, Word8)
keyHeader header
  | header >= uncompressedHeader && header <= witnessHeader + 3 = Just (header >= compressedHeader, (header - uncompressedHeader) `mod` 4)
  | otherwise = Nothing

-- | The header byte of the recovery id 0 for each kind of key and
-- address: 27 for an uncompressed key, 31 for a compressed one and its
-- P2PKH address, 39 for a P2WPKH address as BIP 137 marks it
uncompressedHeader, compressedHeader, witnessHeader :: Word8
uncompressedHeader = 27
compressedHeader = 31
witnessHeader = 39

{- Recovering the key -}

-- | The public key that made the signature r‖s of the hash, given the
-- recovery id, as libsecp256k1 recovers it. Nothing when no key comes
-- out: r or s zero or not below the curve order, or no point at that x.
recoverSigner :: Ctx -> Hash256 -> Word8 -> BS.ByteString -> Maybe PubKey
recoverSigner ctx h recId rs = do
  compact <- compactSig rs
  recSig <- importCompactRecSig ctx compact recId
  recover ctx recSig (hashMsg h)

-- | A hash as libsecp256k1 signs and recovers it. A hash is 32 bytes, the
-- one length it takes.
hashMsg :: Hash256 -> Msg
hashMsg h = fromMaybe (error "A hash is 32 bytes") (msg (S.encode h))

{- The armored block -}

-- | The lines of the block signmsg prints: the message between a marker
-- and the signature marker, then the address and the signature. The
-- markers are those of Sparrow and of the block Bitcoin users have passed
-- around since Bitcoin-Qt, so that other wallets read it.
signedMessageBlock :: Text -> Text -> Text -> [Text]
signedMessageBlock address signature message =
  [ "-----BEGIN BITCOIN SIGNED MESSAGE-----",
    message,
    "-----BEGIN BITCOIN SIGNATURE-----",
    address,
    signature,
    "-----END BITCOIN SIGNATURE-----"
  ]

-- | The block as signmsg prints it, or writes it to its --output file:
-- its lines, each ending with a line break
signedMessageText :: Text -> Text -> Text -> Text
signedMessageText address signature message = Text.unlines (signedMessageBlock address signature message)

-- | The address, the signature and the message of an armored block, such
-- as signmsg prints, or why the text is not one. A marker line is five dashes, words and five dashes:
-- the message is every line between the BEGIN ... SIGNED MESSAGE marker and
-- the BEGIN ... SIGNATURE marker, taken as it is, line endings included.
-- The signature part, up to the END marker, holds the address and then the
-- signature, which may be wrapped on several lines; its blank lines and
-- "Key: value" headers, which some wallets write, are passed over. Text
-- outside the markers, such as a quoting email, is left aside. hw 0.10
-- printed the markers "BEGIN SIGNATURE" and "END BITCOIN SIGNED MESSAGE",
-- which read as well. A message holding a marker line itself cannot be told
-- from the end of the message.
readSignedMessageBlock :: Text -> Either String (Text, Text, Text)
readSignedMessageBlock content =
  case break (isMarker beginsMessage) (Text.splitOn "\n" content) of
    (_, []) -> Left "no \"-----BEGIN BITCOIN SIGNED MESSAGE-----\" line"
    (_, _ : afterBegin) ->
      case break (isMarker beginsSignature) afterBegin of
        (_, []) -> Left "no \"-----BEGIN BITCOIN SIGNATURE-----\" line after the message"
        (messageLines, _ : afterSignatureLine) ->
          case filter isValue (takeWhile (not . isMarker ends) afterSignatureLine) of
            address : signatureLines@(_ : _) ->
              Right (Text.strip address, Text.concat (Text.strip <$> signatureLines), Text.intercalate "\n" messageLines)
            _ -> Left "the signature part of the block must hold the address, then the signature"
  where
    beginsMessage ws = take 1 ws == ["BEGIN"] && drop (length ws - 2) ws == ["SIGNED", "MESSAGE"]
    beginsSignature ws = take 1 ws == ["BEGIN"] && drop (length ws - 1) ws == ["SIGNATURE"]
    ends ws = take 1 ws == ["END"]
    isMarker kind line = maybe False kind (markerWords line)
    isValue line = not (Text.null (Text.strip line)) && not (isHeader line)
    -- "Version: Bitcoin-qt (1.0)": a first word ending with a colon. No
    -- address or base64 signature ends a word so.
    isHeader line = case Text.words line of
      w : _ -> ":" `Text.isSuffixOf` w
      [] -> False

-- | The words of a marker line, between its dashes: Nothing for any other line
markerWords :: Text -> Maybe [Text]
markerWords line = do
  inner <- Text.stripSuffix "-----" =<< Text.stripPrefix "-----" (Text.strip line)
  return (Text.words inner)

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
