{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Short ids: the first 8 hexadecimal digits of a hash, which name a
-- transaction, a pending transaction or a coin wherever the whole hash
-- does.
--
-- A txid has 64 digits. A table of the screen has no room for it, and
-- nobody types it. Its first 8 digits tell it apart from the other
-- transactions of a wallet nearly always: two hashes share them once in
-- about 4 billion pairs, so a wallet of 10 000 transactions holds such a
-- pair about once in 86. The tables and the compact views give the short
-- id, and every command that takes a hash takes it too. The verbose views
-- give both forms.
--
-- A short id is part of the hash, not a number the wallet hands out, for
-- three reasons:
--
-- * A typo in a number counted 1, 2, 3 names another element, which the
--   command would act on. A typo in 8 hexadecimal digits nearly never
--   matches anything, and the command refuses it.
-- * The hash is the same everywhere: on the offline computer, in every
--   file, and in a block explorer, which shows the short id at the start
--   of the txid. A number would mean another element in another wallet.
-- * Nothing is stored: the database keeps its format.
--
-- A hash is given in full or by its short id, with nothing in between.
-- The rule is short to say, and a word of 8 digits is never an address,
-- so that label and show tell a transaction from an address.
--
-- When two hashes that a command can name share their short id, the
-- command names both and asks for the full hash ('pickHash'). It never
-- picks one: the other could be the one the user meant.
--
-- A coin is TXID:INDEX, and its short id is the short id of its txid,
-- then the index: a1075db5:1.
--
-- A short id suits the screen: what the user reads and types now. What
-- keeps a transaction for later keeps its whole hash, since a short id
-- that names one transaction today can match two once the wallet holds
-- more. So the answers in JSON, the transaction files and the files of
-- labels keep whole hashes: a program, or another wallet, must name its
-- transactions exactly. The sheets of --csv and exportsheet give both
-- forms, the short id to find a row seen on the screen.
module Haskoin.Wallet.ShortId
  ( -- * Short ids
    ShortId,
    shortIdOf,
    shortIdText,
    shortIdKey,
    readShortId,
    startsWith,
    shortIdRange,
    shortHashText,
    shortCoinText,

    -- * What the user types
    HashRef (..),
    readHashRef,
    hashRefText,
    CoinRef (..),
    coinRefText,

    -- * The one element a short id names
    Named (..),
    transactionsNamed,
    pendingNamed,
    walletTxsNamed,
    pickHash,
    pickCoin,
    fullHashOf,
  )
where

import Control.Applicative ((<|>))
import Control.Monad.Except (ExceptT, liftEither)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.List (intercalate, nub, sortOn)
import qualified Data.Serialize as S
import Data.String.Conversions (cs)
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Word (Word32)
import Haskoin (OutPoint (..), TxHash, decodeHex, encodeHex, hexToTxHash, txHashToHex)

{- Short ids -}

-- | The first 4 bytes of a hash, in the order hw shows them: 8
-- hexadecimal digits. Make one with 'shortIdOf' or 'readShortId'.
newtype ShortId = ShortId ByteString
  deriving (Eq, Ord, Show)

-- | How many bytes of a hash a short id keeps: 4, so 8 digits
shortIdBytes :: Int
shortIdBytes = 4

-- | The short id of a hash: its first 8 digits as hw shows them. A txid
-- is shown in the reverse order of its bytes, as the network writes it.
shortIdOf :: TxHash -> ShortId
shortIdOf = ShortId . BS.take shortIdBytes . BS.reverse . S.encode

-- | A short id as hw writes it: 8 digits, in lower case
shortIdText :: ShortId -> String
shortIdText (ShortId bytes) = cs (encodeHex bytes)

-- | The 4 bytes of a short id, in the order hw shows them, as a key of
-- the database starts with them
shortIdKey :: ShortId -> ByteString
shortIdKey (ShortId bytes) = bytes

-- | A short id as the user types it: exactly 8 hexadecimal digits, in
-- either case. Nothing for any other word.
readShortId :: Text -> Maybe ShortId
readShortId word
  | Text.length word == 2 * shortIdBytes = ShortId <$> decodeHex word
  | otherwise = Nothing

-- | Whether the hash has the short id
startsWith :: ShortId -> TxHash -> Bool
startsWith short hash = shortIdOf hash == short

-- | The keys that start with the short id, for a hash stored as hw shows
-- it (hashBytes, in "Haskoin.Wallet.Database"): those from the first bound,
-- the short id itself, to below the second, the next short id. A key
-- that starts with the short id sorts after it, as a longer key with the
-- same start does, and before the next one. The last short id, ffffffff,
-- has no next one, and no second bound. A range of keys reads the index
-- of the keys, where a test of each key would read every row.
shortIdRange :: ShortId -> (ByteString, Maybe ByteString)
shortIdRange (ShortId bytes) = (bytes, next)
  where
    number = BS.foldl' (\n b -> n * 256 + fromIntegral b) (0 :: Integer) bytes
    next
      | number + 1 >= 256 ^ shortIdBytes = Nothing
      | otherwise = Just (BS.pack [fromIntegral ((number + 1) `div` (256 ^ i) `mod` 256) | i <- [shortIdBytes - 1, shortIdBytes - 2 .. 0]])

-- | The short id of a hash, as the tables and the compact views show it:
-- "a1075db5"
shortHashText :: TxHash -> String
shortHashText = shortIdText . shortIdOf

-- | The short id of a coin: the short id of its transaction, a colon, and
-- its index among the outputs, "a1075db5:1"
shortCoinText :: OutPoint -> String
shortCoinText outpoint = shortHashText outpoint.hash <> ":" <> show outpoint.index

{- What the user types -}

-- | A hash as the user types it: in full, 64 digits, or by its short id
data HashRef
  = FullHash !TxHash
  | ShortHash !ShortId
  deriving (Eq, Show)

-- | A hash as the user types it, in full or by its short id. Nothing for
-- any other word, a part of a hash longer than a short id included.
readHashRef :: Text -> Maybe HashRef
readHashRef word = (FullHash <$> hexToTxHash word) <|> (ShortHash <$> readShortId word)

-- | A hash as the user typed it, in lower case, for the messages that
-- name it
hashRefText :: HashRef -> String
hashRefText = \case
  FullHash hash -> cs (txHashToHex hash)
  ShortHash short -> shortIdText short

-- | A coin as the user types it, TXID:INDEX: the hash of its transaction,
-- in full or by its short id, and its index among the outputs
data CoinRef = CoinRef !HashRef !Word32
  deriving (Eq, Show)

-- | A coin as the user typed it, for the messages that name it
coinRefText :: CoinRef -> String
coinRefText (CoinRef hash index) = hashRefText hash <> ":" <> show index

{- The one element a short id names -}

-- | What a command names by a hash, as a clash says it: the elements in
-- the plural, and the full form to give instead
data Named = Named
  { namedPlural :: !String,
    namedInFull :: !String
  }

-- | Transactions on the chain, named by their txids
transactionsNamed :: Named
transactionsNamed = Named "transactions" "the full txid"

-- | Pending transactions, named by their nosigHash or their txid
pendingNamed :: Named
pendingNamed = Named "pending transactions" "the full hash"

-- | Transactions on the chain and pending ones, as label and bumpfee
-- name them: by a txid, or by a nosigHash
walletTxsNamed :: Named
walletTxsNamed = Named "transactions" "the full hash"

-- | The one hash, among those found, that has the short id. None is the
-- error given, the one the command gives for a hash it does not have. Two
-- or more are a clash: the error names each in full, in the order of
-- their digits, and asks for the full hash. A hash found twice counts
-- once.
pickHash :: String -> Named -> ShortId -> [TxHash] -> Either String TxHash
pickHash none named short found =
  case sortOn txHashToHex (nub (filter (startsWith short) found)) of
    [] -> Left none
    [hash] -> Right hash
    clashing -> Left (clashText (shortIdText short) named (cs . txHashToHex <$> clashing))

-- | The one coin, among those found, that has the short id of its
-- transaction and the index: as 'pickHash', for coins
pickCoin :: String -> ShortId -> Word32 -> [OutPoint] -> Either String OutPoint
pickCoin none short index found =
  case sortOn fullCoin (nub (filter matches found)) of
    [] -> Left none
    [outpoint] -> Right outpoint
    clashing -> Left (clashText (shortIdText short <> ":" <> show index) coins (fullCoin <$> clashing))
  where
    matches :: OutPoint -> Bool
    matches outpoint = startsWith short outpoint.hash && outpoint.index == index
    coins = Named "coins" "the coin with its full txid"
    fullCoin :: OutPoint -> String
    fullCoin outpoint = cs (txHashToHex outpoint.hash) <> ":" <> show outpoint.index

-- | What a clash says: the short id, each element it matches on a line
-- of its own, and the full form to give instead
clashText :: String -> Named -> [String] -> String
clashText short named clashing =
  intercalate "\n" $
    ["The short id " <> short <> " matches " <> show (length clashing) <> " " <> named.namedPlural <> ":"]
      <> ["  " <> element | element <- clashing]
      <> ["Give " <> named.namedInFull <> "."]

-- | The hash a command names: the hash itself when the user typed it in
-- full, or the one that 'pickHash' picks among those the lookup finds for
-- the short id. The lookup gives the hashes the command can name that
-- start with the short id: those of the pending transactions, for
-- instance. A command then goes on as it would with the full hash.
fullHashOf :: (Monad m) => String -> Named -> (ShortId -> ExceptT String m [TxHash]) -> HashRef -> ExceptT String m TxHash
fullHashOf none named lookupShort = \case
  FullHash hash -> return hash
  ShortHash short -> liftEither . pickHash none named short =<< lookupShort short
