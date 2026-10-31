{-# LANGUAGE OverloadedStrings #-}

-- | The web client of hw, as it tells why it could not ask the store: an
-- error the store answered, a store out of reach, or an answer it could
-- not read. The questions themselves are played out against scripted
-- stores in the specs of the sync and of the pending transactions.
module Haskoin.Wallet.StoreSpec (spec) where

import Control.Exception (throwIO, toException)
import Control.Monad.Except (ExceptT, runExceptT, throwError)
import Control.Monad.IO.Class (liftIO)
import Data.Word (Word64)
import qualified Haskoin.Store.Data as Store
import Haskoin.Wallet.Store (webCall)
import Network.HTTP.Client (HttpException (..), HttpExceptionContent (..), defaultRequest)
import Test.Hspec

-- | A call of the web client to the store at http://store.test/
called :: ExceptT Store.Except IO [Word64] -> IO (Either String [Word64])
called = runExceptT . webCall "http://store.test/"

-- | A call that fails as the web client fails on a request it could not
-- complete
failingWith :: HttpExceptionContent -> ExceptT Store.Except IO [Word64]
failingWith content = liftIO $ throwIO (HttpExceptionRequest defaultRequest content)

spec :: Spec
spec =
  describe "A call of the web client" $ do
    it "gives the answer of the store" $
      called (return [5, 7]) `shouldReturn` Right [5, 7]

    it "fails with the error the store answered, naming the store" $
      called (throwError Store.ServerError)
        `shouldReturn` Left "The store at http://store.test/ answered with an error: ServerError"

    -- The web client fails with this error on an error page that is not
    -- the JSON of an error of haskoin-store: hw showed it as it is before
    it "fails with an error it could not read, as the error page of a proxy in front of a store that is down" $
      called (throwError (Store.StringError "could not decode error"))
        `shouldReturn` Left "The store at http://store.test/ answered with an error hw could not read"

    it "fails when the connection to the store fails" $
      called (failingWith (ConnectionFailure (toException (userError "Connection refused"))))
        `shouldReturn` Left "The store at http://store.test/ could not be reached: the connection failed"

    it "fails when the connection to the store times out" $
      called (failingWith ConnectionTimeout)
        `shouldReturn` Left "The store at http://store.test/ could not be reached: the connection timed out"

    it "fails when the store does not answer in time" $
      called (failingWith ResponseTimeout)
        `shouldReturn` Left "The store at http://store.test/ could not be reached: it did not answer in time"

    it "fails when the address of the store is not valid" $
      called (liftIO (throwIO (InvalidUrlException "store.test/btc/health" "Invalid URL")))
        `shouldReturn` Left "The store at http://store.test/ could not be reached: the address store.test/btc/health is not valid: Invalid URL"

    -- The web client decodes an answer only once it is used, and its
    -- decoder fails with an error call, as this answer does in its second
    -- number
    it "fails with an answer it could not read, wherever in the answer" $
      called (return [5, error "not enough bytes"])
        `shouldReturn` Left "The store at http://store.test/ gave an answer hw could not read: not enough bytes"
