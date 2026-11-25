{-# LANGUAGE OverloadedStrings #-}

-- | Running a command: its transaction of the database, and what hw does by
-- itself beside it.
--
-- A command is one transaction of the database, which no other command
-- changes meanwhile ('runDBResponse'). One that fails leaves the database
-- as it was, and answers with its error. send and sendall are the one
-- exception: each keeps the transaction it signed before it sends it
-- ('keepSoFar'). A command that needs no database only turns its failure
-- into an answer ('runResponse').
--
-- What hw does by itself after a command, a copy of the database or the
-- journal it keeps, runs in a transaction of its own ('afterCommand'). It
-- only warns when it fails: the command is done by then, and its answer
-- stands.
module Haskoin.Wallet.Commands.Run
  ( afterCommand,
    besideCommand,
    runDBResponse,
    runDBResponseOn,
    keepSoFar,
    runResponse,
  )
where

import Control.Exception (displayException)
import Control.Monad (join, when)
import Control.Monad.Except (ExceptT (..), runExceptT)
import Control.Monad.Trans.Class (lift)
import Data.Either (isLeft)
import Data.String.Conversions (cs)
import Database.Persist.Sqlite (transactionSave, transactionUndo)
import Haskoin.Wallet.Cli.Response
import Haskoin.Wallet.Cli.Wrap (warn)
import Haskoin.Wallet.Config
import Haskoin.Wallet.Database
import Haskoin.Wallet.DatabaseFailure (catchDatabaseFailure, databaseFailureExplained)
import UnliftIO.Exception (tryAny)

-- | Run what hw does by itself after a command, in a transaction of its
-- own, which holds the database: a copy (@automaticCopy@), or the journal
-- it keeps (@keepJournal@). A failure is told on stderr and stops
-- nothing, since the command is done. The warning starts with the first
-- text, which says what was not done. Then comes the reason: the second
-- text when another hw held the database for too long. It ends by saying
-- that the command is done, and with the third text, which says what to
-- run to do it now.
afterCommand :: String -> String -> String -> Config -> ExceptT String (DB IO) () -> IO ()
afterCommand notDone databaseTaken whatToRun =
  besideCommand notDone databaseTaken ("The command itself is done. " <> whatToRun)

-- | Run what hw does by itself beside a command, in a transaction of its
-- own, which holds the database. It is the work of 'afterCommand', and
-- of the journal hw writes before a command when the file is missing or
-- behind the configuration
-- ('Haskoin.Wallet.Commands.Journal.journalBeforeCommand').
--
-- A failure is told on stderr and stops nothing: the user asked for the
-- command, not for this. The warning starts with the first text, which
-- says what was not done. Then comes the reason: the second text when
-- another hw held the database for too long. It ends with the third
-- text, which says what this means for the command and what to run.
besideCommand :: String -> String -> String -> Config -> ExceptT String (DB IO) () -> IO ()
besideCommand notDone databaseTaken whatNow cfg action = do
  made <- tryAny . tryDatabase . runDB cfg $ runExceptT action
  case made of
    Left failure -> notDoneFor (displayException failure)
    Right (Left DatabaseInUse) -> notDoneFor databaseTaken
    Right (Left failure) -> notDoneFor =<< databaseFailureExplained cfg failure
    Right (Right outcome) -> either notDoneFor return outcome
  where
    notDoneFor reason = warn (notDone <> " " <> reason <> ". " <> whatNow)

-- | Run a command in one transaction of the database, which no other
-- command changes meanwhile. What a command wrote before it failed is
-- undone, back to where it last called 'keepSoFar', if it did.
runDBResponse :: Config -> ExceptT String (DB IO) Response -> IO Response
runDBResponse cfg = runDBResponseOn cfg (runDB cfg)

-- | Run a command in one transaction of the database, as 'runDBResponse'
-- does, on the connection that the function given opens. It is
-- there for restoredatabase, whose connection also reads a copy of the
-- database ('runDBWithAttached'). A failure of SQLite is an answer too,
-- which says what to do ('catchDatabaseFailure').
runDBResponseOn :: Config -> (DB IO (Either String Response) -> IO (Either String Response)) -> ExceptT String (DB IO) Response -> IO Response
runDBResponseOn cfg run action = do
  result <- catchDatabaseFailure cfg . run $ do
    answer <- runExceptT action
    when (isLeft answer) transactionUndo
    return answer
  return $ answered (join result)

-- | Keep what the command wrote so far, whatever happens next: a failure
-- after this point undoes only what was written after it.
--
-- A command is one transaction of the database, so that a failure leaves
-- no trace. send and sendall need one exception (@prepareSignSend@). Each
-- signs a transaction, then hands it to the network, and the second step
-- can fail after the transaction left the computer, or be interrupted
-- while the store waits for its nodes. The signed transaction must be in
-- the wallet by then, with its coins locked, as it is between signtx and
-- broadcast. So the command ends its transaction of the database here,
-- and goes on in a new one, which takes the lock of the database again at
-- once.
keepSoFar :: ExceptT String (DB IO) ()
keepSoFar = lift transactionSave

-- | Run a command that needs no database
runResponse :: ExceptT String IO Response -> IO Response
runResponse action = answered <$> runExceptT action

-- | The answer of a command, or its error
answered :: Either String Response -> Response
answered = either (ResponseError . cs) id
