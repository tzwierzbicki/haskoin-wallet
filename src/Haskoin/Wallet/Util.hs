{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Small helpers with no meaning of their own in the wallet, shared by
-- modules that should not depend on one another.
module Haskoin.Wallet.Util where

import Control.Monad ((<=<))
import Control.Monad.Except
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.State (MonadState (..))
import qualified Data.Aeson as JSON
import qualified Data.Aeson.Encode.Pretty as Pretty
import Data.Aeson.Types (Parser)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (isJust)
import Data.Text (Text)
import qualified Data.Text as Text
import Haskoin
import Numeric.Natural (Natural)
import System.Random (StdGen, uniformShuffleList)

{- JSON -}

-- | A JSON document as hw writes it, to a file or to the screen: indented
-- by two spaces and ending with a newline
encodeJsonPretty :: (JSON.ToJSON a) => a -> BS.ByteString
encodeJsonPretty =
  BL.toStrict
    . Pretty.encodePretty'
      Pretty.defConfig
        { Pretty.confIndent = Pretty.Spaces 2,
          Pretty.confTrailingNewline = True
        }

{- Pages -}

-- | A page of a listing: how many items, after skipping how many
data Page = Page
  { pageLimit :: !Natural,
    pageOffset :: !Natural
  }
  deriving (Eq, Show)

{- Errors -}

-- | The result of an IO action that may fail, as an error of the monad
liftEitherIO :: (MonadIO m) => IO (Either String a) -> ExceptT String m a
liftEitherIO = liftEither <=< liftIO

{- Addresses -}

-- Addresses as text, with an error instead of Nothing when the address
-- cannot be written on the network.

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

-- | Whether the network can write the address: a SegWit address has no
-- text on Bitcoin Cash, and a CashAddr one none on Bitcoin
onNetwork :: Network -> Address -> Bool
onNetwork net = isJust . addrToText net

{- Numbers -}

-- | The difference of two numbers, or Nothing when it would be negative
safeSubtract :: (Integral a) => a -> a -> Maybe a
safeSubtract a b
  | b > a = Nothing
  | otherwise = Just $ a - b

{- Random -}

-- | Shuffle a list, drawing from the 'StdGen' kept in a state monad: the
-- Fisher-Yates shuffle of the random library, for the transaction builders
randomShuffle :: (MonadState StdGen m) => [a] -> m [a]
randomShuffle xs = state (uniformShuffleList xs)

{- Address maps in JSON -}

-- Maps keyed by address are written with the address text of the network as
-- the key.

-- | The map with its addresses written as text. An address the network
-- cannot write is left out: the summary builders keep the outputs at such
-- addresses apart, as raw outputs, so that none reaches here.
mapAddrText :: Network -> Map Address v -> Map Text v
mapAddrText net m = Map.fromList [(t, v) | (a, v) <- Map.assocs m, Just t <- [addrToText net a]]

-- | The map with its keys read as addresses of the network. The parser
-- fails on a text that is not one.
mapTextAddr :: Network -> Map Text v -> Parser (Map Address v)
mapTextAddr net m = Map.fromList <$> mapM key (Map.assocs m)
  where
    key (t, v) = case textToAddr net t of
      Just a -> return (a, v)
      Nothing -> fail $ "Invalid address " <> Text.unpack t
