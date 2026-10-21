module Haskoin.Wallet.Main where

import Control.Exception (IOException, try)
import Data.Foldable (forM_)
import qualified Data.ByteString.Char8 as C8
import GHC.IO.Encoding (setFileSystemEncoding, setLocaleEncoding, utf8)
import Haskoin (Ctx, MarshalJSON (..), withContext)
import Haskoin.Wallet.Commands (Response, commandResponse)
import Haskoin.Wallet.Config (initConfig)
import Haskoin.Wallet.Database (runDB)
import Haskoin.Wallet.Migration (migrateDB)
import Haskoin.Wallet.Parser (parserMain)
import Haskoin.Wallet.PrettyPrinter (prettyPrinter)
import Haskoin.Wallet.Util (encodeJsonPretty)
import System.Environment (getArgs)
import System.Exit (die)

clientMain :: IO ()
clientMain = do
  setUtf8Encoding
  argumentsProblem <- invalidArgumentsError
  forM_ argumentsProblem die
  withContext $ \ctx -> do
    cfg <- initConfig
    (cmd, unit, json) <- parserMain
    -- Only a command touches the database: help and usage errors leave it
    -- as it is, and a database newer than this hw still gets help
    either die return =<< runDB cfg (migrateDB ctx)
    res <- commandResponse ctx cfg unit cmd
    if json
      then jsonPrinter ctx res
      else prettyPrinter unit res

-- | Use UTF-8 for the command line arguments, the standard streams and file
-- paths, whatever the locale of the process is.
--
-- GHC decodes the command line arguments with the file system encoding, which
-- follows the locale. In a process without a UTF-8 locale (cron jobs,
-- containers, some ssh sessions), every non-ASCII character of an argument is
-- replaced by U+FFFD. The message given to signmsg would then be signed
-- silently altered, and printing it back would fail. This must run before the
-- arguments are parsed.
--
-- With the strict UTF-8 encoding, an argument that is not valid UTF-8 is an
-- error instead of being altered.
setUtf8Encoding :: IO ()
setUtf8Encoding = do
  setLocaleEncoding utf8
  setFileSystemEncoding utf8

-- | The error to report when the command line arguments are not valid UTF-8,
-- or Nothing when they are. GHC decodes the arguments on every call of
-- getArgs, so the command line parser would otherwise fail with a raw
-- decoding exception.
invalidArgumentsError :: IO (Maybe String)
invalidArgumentsError = do
  decoded <- try getArgs :: IO (Either IOException [String])
  return $
    case decoded of
      Left _ ->
        Just
          "The command line arguments are not valid UTF-8.\
          \ hw reads and prints text, including signed messages, as UTF-8."
      Right _ -> Nothing

jsonPrinter :: Ctx -> Response -> IO ()
jsonPrinter ctx = C8.putStrLn . encodeJsonPretty . marshalValue ctx
