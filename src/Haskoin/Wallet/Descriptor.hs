{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Output descriptors, the format of BIP 380 in which wallets and Bitcoin
-- Core pass an account around without its private key, as far as hw uses
-- them: the descriptor of an account of one key, such as
-- @pkh([bc560f95/44h/0h/0h]xpub6CHK.../<0;1>/*)#vq597j87@ for a BIP44
-- account, or @wpkh([bc560f95/84h/0h/0h]xpub6CQF.../<0;1>/*)#5nktextl@
-- for a BIP84 one, the script function naming the kind. Script
-- functions wrap a key expression: the origin of the key in brackets, the
-- fingerprint of the master key and the path from it, then the extended
-- key, then the path to the addresses, @/<0;1>/*@ for both chains of the
-- account as BIP 389 writes them, or @/0/*@ and @/1/*@ for one. A checksum
-- of eight characters may follow a @#@. Hardened steps end in an
-- apostrophe, an @h@ or an @H@; hw writes @h@, which a shell leaves alone.
--
-- A BIP 329 record names the account of its label by the same text
-- without the key, its origin: @pkh([bc560f95/44'/0'/0'])@. The origin is
-- defined here too, and "Haskoin.Wallet.Bip329" uses it.
--
-- The key of a descriptor is one of BIP32, an xpub or a tpub. Some wallets
-- write the key of a BIP84 account with a prefix of SLIP-132, zpub or
-- vpub, to mark its kind: 'readAccountKey' reads such a key as the xpub it
-- is, with the kind it marks, and 'slip132Key' writes it for the wallets
-- that take only that form, such as Electrum.
--
-- This module is the format alone. "Haskoin.Wallet.Labels" reads the keys
-- and the origins of the records of a file, and "Haskoin.Wallet.Database.Account"
-- writes the descriptor of an account, which @portfolios --verbose@ shows.
module Haskoin.Wallet.Descriptor
  ( -- * Key origins
    Origin (..),
    accountOrigin,
    originText,
    parseOrigin,
    kindScripts,
    kindOfScripts,

    -- * Descriptors
    Descriptor (..),
    Chains (..),
    accountDescriptor,
    descriptorOrigin,
    descriptorText,
    parseDescriptor,
    looksLikeDescriptor,

    -- * Extended keys
    isPrivateKey,
    readAccountKey,
    slip132Key,

    -- * Checksums
    checksum,

    -- * The files of other wallets
    fileDescriptors,
  )
where

import Control.Monad (guard)
import Data.Aeson ((.:), (.:?))
import qualified Data.Aeson as Json
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.Types as Json
import Data.Bits (shiftL, shiftR, testBit, xor, (.&.))
import qualified Data.ByteString as BS
import Data.Char (isAlpha, isAlphaNum, isHexDigit)
import Data.List (elemIndex, find)
import Data.Maybe (fromMaybe)
import Data.String.Conversions (cs)
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Word (Word64)
import Haskoin
import Haskoin.Wallet.Keys (AccountKind (..), kindName)

{- Key origins -}

-- | The origin of a key: the script functions around it, the fingerprint
-- of the master key it derives from, and the path of the derivation. A
-- BIP 329 record carries it alone, as @pkh([bc560f95/44'/0'/0'])@, to
-- name the account of its label.
data Origin = Origin
  { -- | The script functions from the outside in: ["pkh"] for a BIP44
    -- account, ["sh", "wpkh"] for a BIP49 one, none for a bare key origin
    originScripts :: ![Text],
    originFingerprint :: !Fingerprint,
    -- | The path from the master key to the key, with its hardened steps
    originPath :: !DerivPath
  }
  deriving (Eq, Show)

-- | The origin of an account of a kind of a seed:
-- pkh([fingerprint/44'/coin'/index']) for a BIP44 account,
-- wpkh([fingerprint/84'/coin'/index']) for a BIP84 one
accountOrigin :: AccountKind -> Fingerprint -> HardPath -> Origin
accountOrigin kind fingerprint path = Origin (kindScripts kind) fingerprint (toGeneric path)

-- | The script functions of the accounts of a kind: pkh for BIP44, wpkh
-- for BIP84
kindScripts :: AccountKind -> [Text]
kindScripts Bip44 = ["pkh"]
kindScripts Bip84 = ["wpkh"]

-- | The kind of account whose script functions these are, if hw holds it:
-- none for a bare key origin, or for the sh(wpkh()) of a BIP49 account
kindOfScripts :: [Text] -> Maybe AccountKind
kindOfScripts scripts = lookup scripts [(kindScripts kind, kind) | kind <- [minBound .. maxBound]]

-- | An origin as a BIP 329 record writes it, hardened steps marked with an
-- apostrophe, as the wallets that write such records do
originText :: Origin -> Text
originText origin =
  wrapped (originScripts origin) (keyOriginText (originFingerprint origin) (originPath origin) "'")

-- | The origin an abbreviated output descriptor describes, if it is one of
-- a single key: script functions around a key origin in brackets, and
-- nothing after the brackets. A descriptor of several keys, such as a
-- multisig one, is none.
parseOrigin :: Text -> Maybe Origin
parseOrigin text = do
  (scripts, inner) <- unwrapped (Text.strip text)
  (originM, rest) <- keyOriginIn inner
  (fingerprint, path) <- originM
  guard (Text.null rest)
  return (Origin scripts fingerprint path)

-- | The text wrapped in the script functions, from the outside in:
-- @sh(wpkh(TEXT))@
wrapped :: [Text] -> Text -> Text
wrapped scripts text = Text.concat (map (<> "(") scripts) <> text <> Text.replicate (length scripts) ")"

-- | The script functions peeled off the text from the outside in, down to
-- the key expression: a key origin in brackets, a key, or anything that
-- is not a script function around something, such as the keys of a
-- multisig descriptor
unwrapped :: Text -> Maybe ([Text], Text)
unwrapped = go []
  where
    go scripts t
      | "[" `Text.isPrefixOf` t = Just (reverse scripts, t)
      | (name, rest) <- Text.breakOn "(" t,
        not (Text.null rest),
        isScriptName name = do
          inner <- Text.stripSuffix ")" =<< Text.stripPrefix "(" rest
          go (name : scripts) inner
      | otherwise = Just (reverse scripts, t)

-- | Whether a text is the name of a script function, such as pkh or
-- sortedmulti
isScriptName :: Text -> Bool
isScriptName name = not (Text.null name) && Text.all isAlpha name

-- | A key origin in brackets at the start of the text, if it has one, and
-- the text after the brackets. Nothing for brackets that hold no origin.
keyOriginIn :: Text -> Maybe (Maybe (Fingerprint, DerivPath), Text)
keyOriginIn text
  | Just inside <- Text.stripPrefix "[" text = do
      let (origin, rest) = Text.breakOn "]" inside
      rest' <- Text.stripPrefix "]" rest
      let (fingerprintText, pathText) = Text.breakOn "/" origin
      guard (Text.length fingerprintText == 8 && Text.all isHexDigit fingerprintText)
      fingerprint <- either (const Nothing) Just (textToFingerprint fingerprintText)
      path <- parseOriginPath pathText
      return (Just (fingerprint, path), rest')
  | otherwise = Just (Nothing, text)

-- | The path after the fingerprint of a key origin: none, or steps each
-- after a slash, hardened ones ending in an apostrophe, an h or an H, all
-- of which haskoin-core reads
parseOriginPath :: Text -> Maybe DerivPath
parseOriginPath text
  | Text.null text = Just Deriv
  | otherwise = (.get) <$> parsePath (cs text)

-- | A key origin as a descriptor writes it, with the given mark on the
-- hardened steps: [bc560f95/44h/0h/0h]
keyOriginText :: Fingerprint -> DerivPath -> Text -> Text
keyOriginText fingerprint path hardenedMark =
  "[" <> fingerprintToText fingerprint <> Text.replace "'" hardenedMark (cs (pathToStr path)) <> "]"

{- Descriptors -}

-- | The descriptor of an account of one extended key, as hw reads and
-- writes it
data Descriptor = Descriptor
  { -- | The script functions around the key, from the outside in
    descriptorScripts :: ![Text],
    -- | The origin of the key, when the descriptor gives it: the
    -- fingerprint of the master key and the path from it
    descriptorKeyOrigin :: !(Maybe (Fingerprint, DerivPath)),
    -- | The extended key, as written
    descriptorKey :: !Text,
    -- | The chains of the account the descriptor derives from the key
    descriptorChains :: !Chains
  }
  deriving (Eq, Show)

-- | The addresses a descriptor derives from the key of an account: those
-- of both chains, as @/<0;1>/*@ writes them, or of one, as @/0/*@ and
-- @/1/*@ do. A bare key, without a path after it, stands for the account
-- with both chains, as the wallets that take a key alone read it.
data Chains = BothChains | ReceivingChain | ChangeChain
  deriving (Eq, Show)

-- | The descriptor of an account of a kind: its key in the script function
-- of the kind, pkh or wpkh, with its origin, the fingerprint of the seed
-- and the path of the account, and both chains. Every wallet of hw knows
-- its fingerprint, so a descriptor hw writes always has its origin, which
-- a hardware wallet needs to match the account to its seed. A descriptor
-- of another wallet may have none ('descriptorKeyOrigin').
accountDescriptor :: AccountKind -> Fingerprint -> HardPath -> Text -> Descriptor
accountDescriptor kind fingerprint path key =
  Descriptor
    { descriptorScripts = kindScripts kind,
      descriptorKeyOrigin = Just (fingerprint, toGeneric path),
      descriptorKey = key,
      descriptorChains = BothChains
    }

-- | The origin of the key of a descriptor, as a BIP 329 record names the
-- account by it, when the descriptor gives it
descriptorOrigin :: Descriptor -> Maybe Origin
descriptorOrigin d = (\(fingerprint, path) -> Origin (descriptorScripts d) fingerprint path) <$> descriptorKeyOrigin d

-- | A descriptor as hw writes it, with its checksum, hardened steps marked
-- with an h
descriptorText :: Descriptor -> Text
descriptorText d = maybe body ((body <> "#") <>) (checksum body)
  where
    body = wrapped (descriptorScripts d) (origin <> descriptorKey d <> chainsText (descriptorChains d))
    origin = maybe "" (\(fingerprint, path) -> keyOriginText fingerprint path "h") (descriptorKeyOrigin d)

-- | The path to the addresses after the key
chainsText :: Chains -> Text
chainsText chains = case chains of
  BothChains -> "/<0;1>/*"
  ReceivingChain -> "/0/*"
  ChangeChain -> "/1/*"

-- | The descriptor of an account a text holds, or why it is not one: a
-- wrong checksum, a text that is not a descriptor of one key, or a path
-- after the key that does not derive the addresses of an account. The
-- network of the key is not looked at here.
parseDescriptor :: Text -> Either String Descriptor
parseDescriptor text = do
  body <- withoutChecksum (Text.strip text)
  (scripts, inner) <- orNot (unwrapped body)
  (originM, keyAndPath) <- orNot (keyOriginIn inner)
  let (key, path) = Text.breakOn "/" keyAndPath
  orNot $ guard (not (Text.null key) && Text.all isAlphaNum key)
  chains <- case lookup path [("/<0;1>/*", BothChains), ("/0/*", ReceivingChain), ("/1/*", ChangeChain)] of
    Just chains -> Right chains
    Nothing
      | Text.null path && null scripts -> Right BothChains
      | otherwise -> Left "The descriptor must end in /<0;1>/*, /0/* or /1/* after its key, to derive the addresses of an account"
  return (Descriptor scripts originM key chains)
  where
    orNot = maybe (Left "The text is not an output descriptor of one key") Right

-- | The body of a descriptor before its checksum, which must be right when
-- there is one
withoutChecksum :: Text -> Either String Text
withoutChecksum text =
  case Text.breakOnEnd "#" text of
    ("", _) -> Right text
    (bodyAndHash, given)
      | checksum body == Just given -> Right body
      | otherwise -> Left "The checksum of the descriptor is wrong"
      where
        body = Text.dropEnd 1 bodyAndHash

-- | Whether a text that is not a descriptor hw reads looks like a
-- descriptor still: script functions around something, or a key origin in
-- brackets. Such a line of a file is refused as a descriptor, with the
-- reason, where another is only a line that is not a record.
looksLikeDescriptor :: Text -> Bool
looksLikeDescriptor text =
  "[" `Text.isPrefixOf` stripped || (isScriptName name && not (Text.null rest))
  where
    stripped = Text.strip text
    (name, rest) = Text.breakOn "(" stripped

{- Extended keys -}

-- | Whether an extended key is a private one, such as xprv or tprv, which
-- a file of accounts should not hold
isPrivateKey :: Text -> Bool
isPrivateKey key = "prv" `Text.isSuffixOf` Text.take 4 key

-- | The key of an account as BIP32 writes it, and the kind of account its
-- prefix marks, from a key as a user or another wallet writes it: an xpub
-- or a tpub as it is, which marks no kind; a zpub or a vpub, the prefix
-- SLIP-132 gives the key of a BIP84 account, with that prefix replaced by
-- the one of BIP32 for its network, since the key is the same and hw
-- stores it as BIP32 writes it. A ypub or a upub marks a BIP49 account,
-- and a capital Y, Z, U or V marks a multisig one: neither is held, so
-- both are refused. Any other text is left as it is, for the reader of
-- BIP32 keys to judge.
readAccountKey :: Text -> Either String (Maybe AccountKind, Text)
readAccountKey key =
  case lookup (Text.take 4 key) slip132Prefixes of
    Nothing -> Right (Nothing, key)
    Just (Left refused) -> Left refused
    Just (Right prefix) ->
      case extendedKeyBytes key of
        Just bytes -> Right (Just prefix.prefixKind, encodeBase58Check (prefix.bip32Bytes <> BS.drop 4 bytes))
        Nothing -> Left ("The key starts with " <> cs (Text.take 4 key) <> " but does not read as an extended key")

-- | The key of an account with the prefix of SLIP-132 that marks its kind,
-- from the key as BIP32 writes it, which hw stores: a zpub, or a vpub on a
-- test network, for a BIP84 account. Electrum, BlueWallet and Trezor Suite
-- take that form for a native SegWit account, since an xpub makes a legacy
-- wallet there. The key is the same, only its four version bytes differ.
-- SLIP-132 gives a BIP44 account the version bytes of BIP32, so the key
-- of a BIP44 account is its xpub, or its tpub, as it is. Text that is not
-- an extended key of BIP32 has none.
slip132Key :: AccountKind -> Text -> Maybe Text
slip132Key kind key = do
  bytes <- extendedKeyBytes key
  let version = BS.take 4 bytes
  case kind of
    Bip44 -> do
      guard (version `elem` (bip32Bytes <$> markedPrefixes))
      return key
    Bip84 -> do
      prefix <- find (\p -> p.prefixKind == kind && p.bip32Bytes == version) markedPrefixes
      return (encodeBase58Check (prefix.slip132Bytes <> BS.drop 4 bytes))

-- | The 78 bytes of an extended key, from its text, when its checksum and
-- its length are right. A text longer than an extended key can be is
-- refused before it is decoded, as haskoin-core does for the keys it
-- reads: the time to decode Base58 grows with the square of the length,
-- and the key comes from a file or from the command line.
extendedKeyBytes :: Text -> Maybe BS.ByteString
extendedKeyBytes key = do
  guard (Text.length key <= maxExtendedKeyLength)
  bytes <- decodeBase58Check key
  guard (BS.length bytes == 78)
  return bytes
  where
    -- The 82 bytes of a key and its checksum take at most 112 digits of
    -- Base58, since 58^112 is larger than 256^82
    maxExtendedKeyLength = 112

-- | A prefix of SLIP-132 that hw reads and writes: the kind of account it
-- marks, its first four characters, and its version bytes beside those of
-- BIP32 on the same network, which take their place in the key hw stores
data MarkedPrefix = MarkedPrefix
  { prefixKind :: !AccountKind,
    prefixText :: !Text,
    bip32Bytes :: !BS.ByteString,
    slip132Bytes :: !BS.ByteString
  }

-- | The prefixes of the key of a BIP84 account: zpub beside xpub on
-- Bitcoin, vpub beside tpub on the test networks
markedPrefixes :: [MarkedPrefix]
markedPrefixes =
  [ MarkedPrefix Bip84 "zpub" (BS.pack [0x04, 0x88, 0xb2, 0x1e]) (BS.pack [0x04, 0xb2, 0x47, 0x46]),
    MarkedPrefix Bip84 "vpub" (BS.pack [0x04, 0x35, 0x87, 0xcf]) (BS.pack [0x04, 0x5f, 0x1c, 0xf6])
  ]

-- | The prefixes of SLIP-132 by their first four characters: those of a
-- BIP84 account, which are read, and the refusal of the others
slip132Prefixes :: [(Text, Either String MarkedPrefix)]
slip132Prefixes =
  [(prefix.prefixText, Right prefix) | prefix <- markedPrefixes]
    <> [ ("ypub", Left (bip49 "ypub")),
         ("upub", Left (bip49 "upub")),
         ("Ypub", Left (multisig "Ypub")),
         ("Zpub", Left (multisig "Zpub")),
         ("Upub", Left (multisig "Upub")),
         ("Vpub", Left (multisig "Vpub"))
       ]
  where
    bip49 prefix = "The key has the prefix " <> prefix <> " of SLIP-132, which marks a BIP49 account, with P2SH-P2WPKH addresses: hw holds " <> kinds <> " accounts"
    multisig prefix = "The key has the prefix " <> prefix <> " of SLIP-132, which marks a multisig account: hw holds " <> kinds <> " accounts of one key"
    kinds = cs (kindName Bip44) <> " and " <> cs (kindName Bip84)

{- Checksums -}

-- | The characters a descriptor may hold, in the order the checksum takes
-- them: three groups of 32, by how common a mistake in each is
inputCharset :: String
inputCharset = "0123456789()[],'/*abcdefgh@:$%{}IJKLMNOPQRSTUVWXYZ&+-.;<=>?!^_|~ijklmnopqrstuvwxyzABCDEFGH`#\"\\ "

-- | The characters of a checksum: those of bech32
checksumCharset :: String
checksumCharset = "qpzry9x8gf2tvdw0s3jn54khce6mua7l"

-- | The checksum of a descriptor without one, as BIP 380 computes it: the
-- symbols of its characters through the polynomial, then eight characters
-- of five bits. Nothing for a text with a character descriptors do not
-- hold.
checksum :: Text -> Maybe Text
checksum body = do
  symbols <- symbolsOf (cs body)
  let value = polymod (symbols <> replicate 8 0) `xor` 1
  return $ Text.pack [checksumCharset !! fromIntegral ((value `shiftR` (5 * (7 - i))) .&. 31) | i <- [0 .. 7]]

-- | The symbols of the characters: the position of each in its group of
-- the charset, and after every third character a symbol for the groups of
-- the three, so that a mistake in one changes one symbol
symbolsOf :: String -> Maybe [Word64]
symbolsOf chars = do
  values <- mapM (\c -> fromIntegral <$> elemIndex c inputCharset) chars
  let positions = [v .&. 31 | v <- values]
      groups = [v `shiftR` 5 | v <- values]
  return (interleaved positions groups)
  where
    interleaved (p1 : p2 : p3 : ps) (g1 : g2 : g3 : gs) = p1 : p2 : p3 : (g1 * 9 + g2 * 3 + g3) : interleaved ps gs
    interleaved [p1, p2] [g1, g2] = [p1, p2, g1 * 3 + g2]
    interleaved [p1] [g1] = [p1, g1]
    interleaved _ _ = []

-- | The polynomial of BIP 380 over the symbols: a BCH code, as bech32
-- has one
polymod :: [Word64] -> Word64
polymod = foldl' step 1
  where
    generator = [0xf5dee51989, 0xa9fdca3312, 0x1bab10e32d, 0x3706b1677a, 0x644d626ffd]
    step chk value =
      let top = chk `shiftR` 35
          shifted = ((chk .&. 0x7ffffffff) `shiftL` 5) `xor` value
       in foldl' (\c (i, g) -> if testBit top i then c `xor` g else c) shifted (zip [0 ..] generator)

{- The files of other wallets -}

-- | The descriptors a JSON file of another wallet carries, each as it is
-- written: the file of Bitcoin Core's listdescriptors, with its list of
-- descriptors, or the generic JSON of a Coldcard, with a section per kind
-- of account, bip44, bip49 and bip84, each with its descriptor, or with
-- its path and key alone in the files of an older firmware, which the
-- fingerprint of the file completes. Nothing for an object of neither
-- shape.
fileDescriptors :: Json.Object -> Maybe [Text]
fileDescriptors o = maybe (Json.parseMaybe coldcardFile o) Just (Json.parseMaybe coreFile o)
  where
    coreFile obj = do
      entries <- obj .: "descriptors"
      mapM (Json.withObject "descriptor" (.: "desc")) entries
    coldcardFile obj = do
      fingerprint <- Text.toLower <$> obj .: "xfp"
      sections <- mapM (\(name, scripts) -> fmap ((,) scripts) <$> obj .:? Key.fromText name) coldcardSections
      let found = [(scripts, section) | Just (scripts, section) <- sections]
      guard (not (null found))
      mapM (uncurry (coldcardSection fingerprint)) found
    coldcardSection fingerprint scripts = Json.withObject "section" $ \section -> do
      descM <- section .:? "desc"
      case descM of
        Just desc -> return desc
        Nothing -> do
          deriv <- section .: "deriv"
          key <- section .: "xpub"
          let path = fromMaybe deriv (Text.stripPrefix "m" deriv)
          return (wrapped scripts ("[" <> fingerprint <> path <> "]" <> key <> chainsText BothChains))

-- | The sections of a Coldcard file hw looks at, with the script
-- functions of each kind of account
coldcardSections :: [(Text, [Text])]
coldcardSections = [("bip44", ["pkh"]), ("bip49", ["sh", "wpkh"]), ("bip84", ["wpkh"])]
