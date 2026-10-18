{-# LANGUAGE OverloadedStrings #-}

module Haskoin.Wallet.MainSpec where

import Control.Exception (bracket)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as C8
import qualified Data.Text as Text
import Data.Text.Encoding (encodeUtf8)
import GHC.IO.Encoding
  ( char8,
    getFileSystemEncoding,
    mkTextEncoding,
    setFileSystemEncoding,
  )
import Data.Maybe (isJust, isNothing)
import Haskoin.Wallet.Main (invalidArgumentsError, setUtf8Encoding)
import System.Environment (getArgs, withArgs)
import Test.Hspec

-- The message of signmsg is a command line argument. GHC decodes the
-- arguments with the file system encoding, which follows the locale of the
-- process, so these tests set the encoding to what GHC would use in a process
-- without a UTF-8 locale and check that setUtf8Encoding takes over.
spec :: Spec
spec =
  describe "Command line arguments" $ do
    it "are decoded as UTF-8 in a process without a UTF-8 locale" $ do
      -- GHC uses the locale encoding with lone surrogates for undecodable
      -- bytes (the //ROUNDTRIP suffix). In the C locale, that is ASCII.
      cLocale <- mkTextEncoding "ASCII//ROUNDTRIP"
      withArgBytes [encodeUtf8 "café"] $ do
        setFileSystemEncoding cLocale
        corrupted <- getArgs
        map Text.pack corrupted `shouldNotBe` ["café"]
        setUtf8Encoding
        decoded <- getArgs
        map Text.pack decoded `shouldBe` ["café"]
        invalidArgumentsError >>= (`shouldSatisfy` isNothing)
    it "are rejected when they are not valid UTF-8" $
      -- "café" in Latin-1: the byte 0xE9 is not valid UTF-8
      withArgBytes ["caf\xe9"] $ do
        setUtf8Encoding
        getArgs `shouldThrow` anyIOException
        invalidArgumentsError >>= (`shouldSatisfy` isJust)

-- | Run an action with the given raw bytes as the command line arguments, and
-- restore the file system encoding afterwards. withArgs encodes its strings
-- with the file system encoding: with char8 every character becomes exactly
-- one byte, so the arguments are stored byte for byte.
withArgBytes :: [BS.ByteString] -> IO a -> IO a
withArgBytes args action =
  bracket getFileSystemEncoding setFileSystemEncoding $ \_ -> do
    setFileSystemEncoding char8
    withArgs (C8.unpack <$> args) action
