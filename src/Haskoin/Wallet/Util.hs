{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module Haskoin.Wallet.Util where

import Control.Arrow (second)
import Control.Monad ((<=<))
import Control.Monad.Except
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.State (MonadState (..))
import qualified Data.Aeson as JSON
import qualified Data.Aeson.Encode.Pretty as Pretty
import Data.Aeson.Types (Parser)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import Data.List (sortBy)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)
import qualified Data.Serialize as S
import Data.Text (Text)
import qualified Data.Text as Text
import Haskoin
import Numeric.Natural (Natural)
import System.Random (StdGen, uniformShuffleList)

{- JSON -}

-- Pretty printing of JSON documents, as hw writes them to files and to the
-- standard output.

encodeJsonPretty :: (JSON.ToJSON a) => a -> BS.ByteString
encodeJsonPretty =
  BL.toStrict
    . Pretty.encodePretty' Pretty.defConfig {Pretty.confIndent = Pretty.Spaces 2}

encodeJsonPrettyLn :: (JSON.ToJSON a) => a -> BS.ByteString
encodeJsonPrettyLn =
  BL.toStrict
    . Pretty.encodePretty'
      Pretty.defConfig
        { Pretty.confIndent = Pretty.Spaces 2,
          Pretty.confTrailingNewline = True
        }

{- Pages -}

-- Paging of lists, for the commands that list addresses, transactions or
-- coins.

data Page = Page
  { pageLimit :: !Natural,
    pageOffset :: !Natural
  }
  deriving (Eq, Show)

toPage :: Page -> [a] -> [a]
toPage (Page limit offset) xs =
  take (fromIntegral limit) $ drop (fromIntegral offset) xs

{- Errors -}

-- | The result of an IO action that may fail, as an error of the monad
liftEitherIO :: (MonadIO m) => IO (Either String a) -> ExceptT String m a
liftEitherIO = liftEither <=< liftIO

{- Addresses -}

-- Addresses as text, with an error instead of Nothing, alone or as the
-- first element of a pair or a triple.

addrToTextE :: Network -> Address -> Either String Text
addrToTextE net a =
  maybeToEither "Invalid Address in addrToTextE" (addrToText net a)

textToAddrE :: Network -> Text -> Either String Address
textToAddrE net a =
  maybeToEither "Invalid Address in textToAddrE" (textToAddr net a)

-- | An address as it is written in a message or on the screen, or a
-- mention of the mismatch when it cannot be written on the network
addrToString :: Network -> Address -> String
addrToString net a = maybe "an address of another network" Text.unpack (addrToText net a)

addrToText2 :: Network -> (Address, v) -> Either String (Text, v)
addrToText2 net (a, v) = (,v) <$> addrToTextE net a

textToAddr2 :: Network -> (Text, v) -> Either String (Address, v)
textToAddr2 net (a, v) = (,v) <$> textToAddrE net a

addrToText3 :: Network -> (Address, v, w) -> Either String (Text, v, w)
addrToText3 net (a, v, w) = (,v,w) <$> addrToTextE net a

textToAddr3 :: Network -> (Text, v, w) -> Either String (Address, v, w)
textToAddr3 net (a, v, w) = (,v,w) <$> textToAddrE net a

{- Keys -}

xPubChecksum :: Ctx -> XPubKey -> Text
xPubChecksum ctx = encodeHex . S.encode . xPubFP ctx

{- Lists -}

lastList :: Natural -> [a] -> [a]
lastList count xs = drop (max 0 $ length xs - fromIntegral count) xs

(!!?) :: [a] -> Natural -> Maybe a
xs !!? i
  | fromIntegral i < length xs = Just $ xs !! fromIntegral i
  | otherwise = Nothing

chunksOf :: Natural -> [a] -> [[a]]
chunksOf n xs
  | null xs = []
  | otherwise =
      uncurry (:) $ second (chunksOf n) $ splitAt (fromIntegral n) xs

sortDesc :: (Ord a) => [a] -> [a]
sortDesc = sortBy (flip compare)

{- Text -}

padStart :: Int -> Text -> Text -> Text
padStart n c t =
  Text.replicate (fromMaybe 0 $ n `safeSubtract` Text.length t) c <> t

padEnd :: Int -> Text -> Text -> Text
padEnd n c t =
  t <> Text.replicate (fromMaybe 0 $ n `safeSubtract` Text.length t) c

dropPatternEnd :: Text -> Text -> Text
dropPatternEnd p t = fromMaybe t $ Text.stripSuffix p t

chunksOfEnd :: Int -> Text -> [Text]
chunksOfEnd n t = reverse $ Text.reverse <$> Text.chunksOf n (Text.reverse t)

{- Numbers -}

safeSubtract :: (Integral a) => a -> a -> Maybe a
safeSubtract a b
  | b > a = Nothing
  | otherwise = Just $ a - b

{- File paths -}

(</>) :: String -> String -> String
a </> b = a <> "/" <> b

{- Random -}

-- | Shuffle a list, drawing from the 'StdGen' kept in a state monad: the
-- Fisher-Yates shuffle of the random library, for the transaction builders
randomShuffle :: (MonadState StdGen m) => [a] -> m [a]
randomShuffle xs = state (uniformShuffleList xs)

{- Address maps in JSON -}

-- Maps keyed by address are written with the address text of the network as
-- the key. Shared by the summaries of every format version, so it lives
-- here rather than with the current summary type.

marshalMap ::
  (MarshalJSON Ctx v) =>
  Network ->
  Ctx ->
  Map Address v ->
  Map Text JSON.Value
marshalMap net ctx m = mapAddrText net $ Map.map (marshalValue ctx) m

unmarshalMap ::
  (MarshalJSON Ctx v) =>
  Network ->
  Ctx ->
  Map Text JSON.Value ->
  Parser (Map Address v)
unmarshalMap net ctx m = mapTextAddr net <$> (unmarshalValue ctx `mapM` m)

mapAddrText :: Network -> Map Address v -> Map Text v
mapAddrText net m =
  either error id $ do
    let f (a, v) = (\t -> (t, v)) <$> addrToTextE net a
    Map.fromList <$> mapM f (Map.assocs m)

mapTextAddr :: Network -> Map Text v -> Map Address v
mapTextAddr net m =
  either error id $ do
    let f (a, v) = (\t -> (t, v)) <$> textToAddrE net a
    Map.fromList <$> mapM f (Map.assocs m)
