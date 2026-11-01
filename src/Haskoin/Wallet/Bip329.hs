{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

-- | The wallet labels export format of BIP 329, which other wallets read
-- and write: a UTF-8 text file with one JSON object per line, separated by
-- line feeds. Each object is a record: its @type@, the kind of thing it
-- labels, its @ref@, the thing itself, and optional fields, the @label@
-- among them. The types are @tx@, @addr@, @pubkey@, @input@, @output@,
-- @xpub@ and @spscan@, and a record of another type is ignored. The
-- @origin@ of a record, an abbreviated output descriptor such as
-- @pkh([bc560f95/44'/0'/0'])@, names the wallet the label comes from, to
-- tell the accounts of one seed apart. A file of @output@ records says with
-- @spendable@ whether the wallet may spend each coin.
--
-- This module is the format alone: the records, their lines, and the texts
-- of the origins, key paths and outpoints. "Haskoin.Wallet.Labels" turns the
-- wallet into records and records into the wallet.
--
-- hw writes and reads the fields it has a use for: @label@, @origin@,
-- @spendable@, the @keypath@ of an address or a coin, and the @height@,
-- @fee@ and @value@ of a transaction or a coin, which the BIP lists among
-- its additional fields. The other additional fields, @time@, @rate@,
-- @fmv@ and @heights@, are left aside as the BIP allows. hw adds one key of
-- its own to an @xpub@ record, @network@, since an extended public key alone
-- does not tell Bitcoin from Bitcoin Cash, nor one test network from
-- another. The BIP defines no such key; the importers checked leave a key
-- they do not know aside.
module Haskoin.Wallet.Bip329
  ( -- * Records
    Record (..),
    RecordType (..),
    record,
    recordTypeName,
    recordTypeNamed,

    -- * The lines of a file
    Line (..),
    readLines,
    parseLine,
    recordLine,
    recordsText,

    -- * Origins
    Origin (..),
    accountOrigin,
    originText,
    parseOrigin,

    -- * Key paths and outpoints
    keypathText,
    parseKeypath,
    outpointText,
    readOutpoint,
  )
where

import Control.Monad (guard)
import Data.Aeson ((.:), (.:?), (.=))
import qualified Data.Aeson as Json
import qualified Data.Aeson.Encoding as Encoding
import Data.Aeson.Types (Parser)
import qualified Data.Aeson.Types as Json
import Data.Bifunctor (first)
import Data.Char (isHexDigit)
import Data.String.Conversions (cs)
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Text.Encoding (encodeUtf8)
import Data.Word (Word32)
import Haskoin
import Haskoin.Wallet.Util (readNatural)
import Numeric.Natural (Natural)

{- Records -}

-- | The kinds of things a record labels, as the BIP names them
data RecordType
  = -- | A transaction, by its id
    TxRecord
  | -- | An address
    AddrRecord
  | -- | A public key, in hexadecimal
    PubkeyRecord
  | -- | An input of a transaction, TXID:INDEX
    InputRecord
  | -- | An output of a transaction, TXID:INDEX: a coin
    OutputRecord
  | -- | An extended public key, as BIP32 writes it
    XpubRecord
  | -- | The scan key of silent payments, which hw does not use
    SpscanRecord
  deriving (Eq, Show, Enum, Bounded)

-- | The type of a record as its line writes it
recordTypeName :: RecordType -> Text
recordTypeName kind = case kind of
  TxRecord -> "tx"
  AddrRecord -> "addr"
  PubkeyRecord -> "pubkey"
  InputRecord -> "input"
  OutputRecord -> "output"
  XpubRecord -> "xpub"
  SpscanRecord -> "spscan"

-- | The type a line names, or Nothing for a type the BIP does not define
recordTypeNamed :: Text -> Maybe RecordType
recordTypeNamed name = lookup name [(recordTypeName kind, kind) | kind <- [minBound .. maxBound]]

-- | A record of a file: what it labels, and the fields it carries. Every
-- field but the type and the reference may be missing, and a missing label
-- or spendable flag leaves the wallet as it is.
data Record = Record
  { recordType :: !RecordType,
    -- | The transaction id, address, key or outpoint, as written
    recordRef :: !Text,
    recordLabel :: !(Maybe Text),
    -- | The origin of the wallet the label comes from, as written: see
    -- 'parseOrigin'
    recordOrigin :: !(Maybe Text),
    -- | Whether the wallet may spend the coin, on an output record
    recordSpendable :: !(Maybe Bool),
    -- | The path from the origin to the key of an address or a coin, such
    -- as /0/5, or a whole descriptor when it does not start with a slash
    recordKeypath :: !(Maybe Text),
    -- | The block of a transaction or a coin, once confirmed by six blocks
    recordHeight :: !(Maybe Natural),
    -- | The fee of a transaction, in satoshis
    recordFee :: !(Maybe Natural),
    -- | What a transaction brings the wallet, negative when it pays out,
    -- or the value of a coin, in satoshis
    recordValue :: !(Maybe Integer),
    -- | The network of an extended public key, as hw names it: hw's own
    -- key, on xpub records
    recordNetwork :: !(Maybe Text)
  }
  deriving (Eq, Show)

-- | A record of the type with the reference, and no other field
record :: RecordType -> Text -> Record
record kind ref =
  Record
    { recordType = kind,
      recordRef = ref,
      recordLabel = Nothing,
      recordOrigin = Nothing,
      recordSpendable = Nothing,
      recordKeypath = Nothing,
      recordHeight = Nothing,
      recordFee = Nothing,
      recordValue = Nothing,
      recordNetwork = Nothing
    }

{- The lines of a file -}

-- | What a line of a file holds
data Line
  = -- | A record of a type the BIP defines
    LineRecord !Record
  | -- | A record of a type the BIP does not define, which an importer
    -- leaves aside, by the name of its type
    LineOfOtherType !Text
  deriving (Eq, Show)

-- | The lines of a file that hold anything, each numbered from 1 as a text
-- editor counts them, read as records. A line that is not a record says
-- why. Blank lines are passed over, and a line ending in a carriage return
-- is read without it, so that a file edited on Windows reads too.
readLines :: Text -> [(Natural, Either String Line)]
readLines content =
  [ (number, parseLine line)
    | (number, raw) <- zip [1 ..] (Text.splitOn "\n" content),
      let line = Text.strip raw,
      not (Text.null line)
  ]

-- | A line of a file as a record: a JSON object with a type and a
-- reference, whose other fields hw reads have the types the BIP gives
-- them. A spendable flag written as the text "true" or "false", which the
-- BIP warns against, is read as the flag: some files have it.
parseLine :: Text -> Either String Line
parseLine line = do
  value <- first (const "The line is not a JSON object") (Json.eitherDecodeStrict' (encodeUtf8 line))
  Json.parseEither lineParser value

lineParser :: Json.Value -> Parser Line
lineParser = Json.withObject "record" $ \o -> do
  typeName <- o .: "type"
  case recordTypeNamed typeName of
    Nothing -> return (LineOfOtherType typeName)
    Just kind -> do
      ref <- o .: "ref"
      fields <-
        Record kind ref
          <$> o .:? "label"
          <*> o .:? "origin"
          <*> (traverse spendableParser =<< o .:? "spendable")
          <*> o .:? "keypath"
          <*> o .:? "height"
          <*> o .:? "fee"
          <*> o .:? "value"
          <*> o .:? "network"
      return (LineRecord fields)

spendableParser :: Json.Value -> Parser Bool
spendableParser value = case value of
  Json.Bool flag -> return flag
  Json.String "true" -> return True
  Json.String "false" -> return False
  _ -> fail "spendable must be true or false"

-- | A record as its line writes it, without the line feed: the type and
-- the reference first, then the fields it has, in one compact JSON object.
-- Text is written as it is, in UTF-8, as the BIP asks.
recordLine :: Record -> Text
recordLine r =
  cs . Encoding.encodingToLazyByteString . Encoding.pairs . mconcat $
    [ "type" .= recordTypeName (recordType r),
      "ref" .= recordRef r,
      given "label" (recordLabel r),
      given "origin" (recordOrigin r),
      given "network" (recordNetwork r),
      given "spendable" (recordSpendable r),
      given "keypath" (recordKeypath r),
      given "height" (recordHeight r),
      given "fee" (recordFee r),
      given "value" (recordValue r)
    ]
  where
    given :: (Json.ToJSON a) => Json.Key -> Maybe a -> Json.Series
    given key = maybe mempty (key .=)

-- | The text of a file of records: each on its line, every line ended by
-- a line feed
recordsText :: [Record] -> Text
recordsText records = Text.concat [recordLine r <> "\n" | r <- records]

{- Origins -}

-- | The origin of a record, read from its abbreviated output descriptor:
-- the script functions around the key, the fingerprint of the master key
-- the key derives from, and the path of the derivation
data Origin = Origin
  { -- | The script functions from the outside in: ["pkh"] for a BIP44
    -- account, ["sh", "wpkh"] for a BIP49 one, none for a bare key origin
    originScripts :: ![Text],
    originFingerprint :: !Fingerprint,
    -- | The path from the master key to the key, with its hardened steps
    originPath :: !DerivPath
  }
  deriving (Eq, Show)

-- | The origin of a BIP44 account of a seed: pkh([fingerprint/44'/coin'/index'])
accountOrigin :: Fingerprint -> HardPath -> Origin
accountOrigin fingerprint path = Origin ["pkh"] fingerprint (toGeneric path)

-- | An origin as a record writes it, hardened steps marked with an
-- apostrophe
originText :: Origin -> Text
originText origin =
  Text.concat (map (<> "(") (originScripts origin))
    <> "["
    <> fingerprintToText (originFingerprint origin)
    <> cs (pathToStr (originPath origin))
    <> "]"
    <> Text.replicate (length (originScripts origin)) ")"

-- | The origin an abbreviated output descriptor describes, if it is one of
-- a single key: script functions around a key origin in brackets, a
-- fingerprint of eight hexadecimal digits and a path whose hardened steps
-- end in an apostrophe, an h or an H, as BIP 380 writes them. A descriptor
-- of several keys, such as a multisig one, is none.
parseOrigin :: Text -> Maybe Origin
parseOrigin text = do
  (scripts, keyOrigin) <- unwrapped [] (Text.strip text)
  inside <- Text.stripSuffix "]" =<< Text.stripPrefix "[" keyOrigin
  let (fingerprintText, pathText) = Text.breakOn "/" inside
  guard (Text.length fingerprintText == 8 && Text.all isHexDigit fingerprintText)
  fingerprint <- either (const Nothing) Just (textToFingerprint fingerprintText)
  path <- parseOriginPath pathText
  return (Origin scripts fingerprint path)
  where
    -- The script functions peeled off the text from the outside in, down
    -- to the key origin in brackets
    unwrapped :: [Text] -> Text -> Maybe ([Text], Text)
    unwrapped scripts t
      | "[" `Text.isPrefixOf` t = Just (reverse scripts, t)
      | otherwise = do
          let (name, rest) = Text.breakOn "(" t
          guard (not (Text.null name) && Text.all (`notElem` ("()[]," :: String)) name)
          inner <- Text.stripSuffix ")" =<< Text.stripPrefix "(" rest
          unwrapped (name : scripts) inner

-- | The path after the fingerprint of a key origin: none, or steps each
-- after a slash
parseOriginPath :: Text -> Maybe DerivPath
parseOriginPath text
  | Text.null text = Just Deriv
  | otherwise = (.get) <$> parsePath (cs (Text.replace "H" "'" (Text.replace "h" "'" text)))

{- Key paths and outpoints -}

-- | The key path of an address or a coin of an account, as a record writes
-- it after the origin of the account: /0/5
keypathText :: SoftPath -> Text
keypathText = cs . pathToStr

-- | The chain and index a key path gives, when it is a path from the
-- origin, starting with a slash. A key path that does not start with one
-- is a whole descriptor, which hw does not read.
parseKeypath :: Text -> Maybe SoftPath
parseKeypath text = do
  guard ("/" `Text.isPrefixOf` text)
  parseSoft (cs text)

-- | An outpoint as the records of inputs and outputs write it: the id of
-- the transaction, a colon, and the index
outpointText :: OutPoint -> Text
outpointText outpoint = txHashToHex outpoint.hash <> ":" <> cs (show outpoint.index)

-- | The outpoint of such a reference. The index has 32 bits: a larger
-- number names no output.
readOutpoint :: Text -> Maybe OutPoint
readOutpoint text = do
  let (hashText, rest) = Text.breakOn ":" text
  indexText <- Text.stripPrefix ":" rest
  hash <- hexToTxHash hashText
  index <- readNatural indexText
  guard (index <= fromIntegral (maxBound :: Word32))
  return (OutPoint hash (fromIntegral index))
