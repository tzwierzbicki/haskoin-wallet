{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

-- | The next steps: what hw prints below the answer of a command of a
-- payment or a sweep, to say which command comes next and on which
-- computer.
module Haskoin.Wallet.Cli.PrettyPrinter.NextSteps
  ( printNextSteps,
    nextStepsPrinter,
    stepsPrinter,
  )
where

import Control.Applicative ((<|>))
import Control.Monad
import Data.List (nub)
import Data.Maybe (isJust)
import Haskoin
import Haskoin.Wallet.Cli.Command (BumpFee (..), Command (..), PrepareSendAll (..), PrepareSweep (..), PrepareTx (..), Signing (..))
import Haskoin.Wallet.Cli.PrettyPrinter.Base (andList, renderIO, styled, text)
import Haskoin.Wallet.Cli.PrettyPrinter.CommandLine (fileWord, hwLine, metavarWord, optionWord, pendingHashWord)
import Haskoin.Wallet.Cli.PrettyPrinter.Place (refText)
import Haskoin.Wallet.Cli.Response
import Haskoin.Wallet.Cli.Style
import Haskoin.Wallet.Database.Account
import Haskoin.Wallet.Database.File (DatabaseShown (..))
import Haskoin.Wallet.Database.Portfolio
import Haskoin.Wallet.Database.TxInfo
import Haskoin.Wallet.Keys (KeySource (..), portfolioNetworks)
import Haskoin.Wallet.ShortId (shortHashText)
import Haskoin.Wallet.Sync (SyncAfter (..), SyncRes (..))
import Haskoin.Wallet.WalletFile (ImportScope (..), importCreated, reportNeedsSync)
import Numeric.Natural (Natural)
import Prettyprinter (Doc, indent, pretty, vcat)

-- | What the user does next after a command of a payment or a sweep,
-- send, sendall, preparetx, preparesendall, preparesweep, signtx,
-- signsweep, broadcast, importwallet or sync, or after a command that
-- created a portfolio that is not the active one, below its answer. In the pretty output only: a
-- script reading the JSON answer knows its next step
-- ('nextStepsPrinter').
printNextSteps :: Command -> Response -> IO ()
printNextSteps cmd res =
  forM_ (nextStepsPrinter cmd res) $ \steps -> putStrLn "" >> renderIO steps

-- | The steps the user may take next, after a command of a payment or a
-- sweep, if any. Each step is a command line, styled as the help shows
-- commands, options and metavars.
--
-- A step names no portfolio: the next command works in the active one, or
-- finds its portfolio by itself. newportfolio, recover and importwallet
-- leave the active portfolio as it was when the database had one, and
-- their step is then the switch to the first portfolio they created
-- (@switchStep@). A sync covers every portfolio, so the new
-- transactions it found may be in another portfolio than the active one,
-- which txs lists alone: the steps then start with the switch to that
-- portfolio (@txsSteps@). A pending transaction in conflict needs no
-- switch, since pendingtxs lists every portfolio (@conflictSteps@).
nextStepsPrinter :: Command -> Response -> Maybe (Doc Style)
nextStepsPrinter cmd res =
  case (cmd, res) of
    (CommandNewPortfolio _, ResponseNewPortfolio _ _ view active _ _)
      | not active.activeAfterActivated -> stepsPrinter [switchStep (viewRef view)]
    (CommandRecover _, ResponseRecover _ _ walletM created _ _ _ _ activeM _ usedOnHidden) ->
      stepsPrinter $
        -- The active portfolio is none of those found paid: an older
        -- one, or the portfolio0 that a new wallet always gets
        [ switchStep (PortfolioRef wallet (minimum created))
          | Just wallet <- [walletM],
            Just active <- [activeM],
            not (null created),
            active.activeAfterRef `notElem` (PortfolioRef wallet <$> created)
        ]
          -- Each network once, in the order of the networks of a portfolio
          <> [showNetworksStep nets | nets@(_ : _) <- [filter (`elem` concatMap snd usedOnHidden) portfolioNetworks]]
    (CommandPrepareTx opts, ResponsePrepared _ info _ _)
      | Just shown <- shownOf info -> afterPreparing opts.output shown
    (CommandPrepareSendAll opts, ResponsePrepared _ info _ _)
      | Just shown <- shownOf info -> afterPreparing opts.output shown
    (CommandBumpFee opts, ResponsePrepared _ info _ _)
      | Just shown <- shownOf info -> afterPreparing opts.output shown
    -- The keys of a file at hand for the sweep are the ones to sign it with
    (CommandPrepareSweep opts, ResponseSweep (Just _) _ info _ _ _)
      | Just shown <- shownOf info ->
          stepsPrinter
            [ ("Next, sign it with the private keys of the coins", hwLine "signsweep" (pendingHashWord shown : keysArgs opts.keys)),
              case opts.output of
                Nothing -> ("Or export it, to sign it where the keys are", hwLine "exporttx" [pendingHashWord shown, metavarWord "FILE"])
                Just file -> ("Or sign its file where the keys are", hwLine "signsweep" [optionWord "--input", fileWord file])
            ]
    -- A sweep outside the wallet is in its file only
    (CommandPrepareSweep opts, ResponseSweep Nothing _ _ _ _ _)
      | Just file <- opts.output ->
          stepsPrinter [("Next, sign its file with the private keys of the coins", hwLine "signsweep" ([optionWord "--input", fileWord file] <> keysArgs opts.keys))]
    (CommandSignTx signing, ResponseTx _ info _) -> afterSigning signing info
    (CommandSignSweep signing _ _, ResponseTx _ info _) -> afterSigning signing info
    (CommandSignSweep signing _ _, ResponseOutsideTx _ info) -> afterSigning signing info
    (CommandBroadcast {}, ResponseSentPending _ _ after _) -> afterSending after
    (CommandBroadcast {}, ResponseSentTx _ _ after) -> afterSending after
    -- A copy holds the history as its last sync left it
    (CommandRestoreDatabase {}, ResponseRestored _ restored _ _)
      | not (null (databaseShownWallets restored)) ->
          stepsPrinter [("Next, sync the wallet, to bring the history of the copy up to date", hwLine "sync" [])]
    (CommandSend {}, ResponseSentPending _ _ after _) -> afterSending after
    (CommandSendAll {}, ResponseSentPending _ _ after _) -> afterSending after
    -- A sweep that pays a portfolio. One that pays none leaves nothing to
    -- follow in the wallet.
    (CommandSweep {}, ResponseSentPending _ _ after _) -> afterSending after
    -- The file may label things the wallet does not have yet, and it may
    -- bring portfolios that the active one leaves aside. The second
    -- import names the wallet and the portfolio the first one named, so
    -- that it reads no more of the file than the user asked for.
    (CommandImportWallet scope file, ResponseImport report activeM)
      | not (null steps) -> stepsPrinter steps
      where
        steps =
          concat
            [ [ ("Next, sync the wallet, for the addresses, transactions and coins the file labels", hwLine "sync" []),
                ("Then import the file again, for the labels it could not place", hwLine "importwallet" (fileWord file : fileScopeWords scope))
              ]
              | reportNeedsSync report
            ]
            <> [ switchStep first
                 | Just active <- [activeM],
                   not active.activeAfterActivated,
                   first : _ <- [importCreated report]
               ]
    -- A sync that synced nothing has nothing new to see
    (CommandSync {}, ResponseSync results _ _ activeM)
      | not (null results) ->
          case troubleSteps results of
            [] -> stepsPrinter (txsSteps activeM (nub [placePortfolio (syncResPlace result) | result <- results, syncResTxUpdates result > 0]))
            steps -> stepsPrinter steps
    _ -> Nothing
  where
    -- The short id of a transaction in preparation, as its answer shows
    -- it, to type in the next command
    shownOf info = case txInfoStatus info of
      Pending status -> Just (shortHashText (pendingShownHash status))
      OnChain _ -> Nothing
    -- A payment of the wallet is signed with its mnemonic, here or on an
    -- offline computer
    afterPreparing output shown =
      stepsPrinter
        [ ("Next, sign it", hwLine "signtx" [pendingHashWord shown]),
          case output of
            Nothing -> ("Or export it, to sign it on an offline computer", hwLine "exporttx" [pendingHashWord shown, metavarWord "FILE"])
            Just file -> ("Or sign its file on an offline computer", hwLine "signtx" [optionWord "--input", fileWord file])
        ]
    -- A transaction signed in the wallet is sent by its id, one
    -- signed from a file by the file signed: the --output file, or the
    -- --input file it replaced
    afterSigning signing info
      | Just shown <- shownOf info, isJust signing.nosigHash =
          stepsPrinter [("Next, send it to the network", hwLine "broadcast" [pendingHashWord shown])]
      | Just file <- signing.output <|> signing.input =
          stepsPrinter [("Next, send its file from the online computer", hwLine "broadcast" [optionWord "--input", fileWord file])]
      | otherwise = Nothing
    -- What a sync found wrong with pending transactions, and what to do
    -- about each: those in conflict, and those the store did not publish
    troubleSteps results =
      case (sum (syncResConflicted <$> results), concatMap (map fst . syncResNotPublished) results) of
        (0, []) -> []
        (conflicts, refused) -> conflictSteps conflicts <> notPublishedSteps (conflicts > 0) refused
    -- The command that sent a transaction synced the wallet too
    -- ('Haskoin.Wallet.Cli.PrettyPrinter.Sync.syncAfterDocs'), so there
    -- is no next step, unless that sync did not go through, or found
    -- something wrong with another pending transaction
    afterSending after =
      stepsPrinter $
        [("Next, sync the wallet to follow the transaction", hwLine "sync" []) | isJust after.notSyncedAfter]
          <> troubleSteps after.syncedAfter
    -- Without a file, signsweep asks for the keys on the terminal
    keysArgs (Just (KeysInFile file)) = [optionWord "--key-file", fileWord file]
    keysArgs _ = []

-- | The options that name one wallet and one portfolio of a file of a
-- wallet, as importwallet takes them: none for the whole file
fileScopeWords :: ImportScope -> [Doc Style]
fileScopeWords scope =
  concat [[optionWord "--wallet", pretty (fingerprintToText fingerprint)] | Just fingerprint <- [scope.scopeWallet]]
    <> concat [[optionWord "--portfolio", text (show index)] | Just index <- [scope.scopePortfolio]]

-- | The step that makes a portfolio a command created the active one, to
-- work in it: the switch to it, which names its wallet always, since the
-- active portfolio may be of another wallet
switchStep :: PortfolioRef -> (String, Doc Style)
switchStep ref = ("To work in " <> refText ref <> ", make it the active one", switchLine ref)

-- | The step that shows the hidden networks recover found portfolios
-- used on, in one command: setdefault takes --show-network several times
showNetworksStep :: [Network] -> (String, Doc Style)
showNetworksStep nets =
  ( "To show " <> andList ((.name) <$> nets),
    hwLine "setdefault" (concat [[optionWord "--show-network", text net.name] | net <- nets])
  )

-- | The command line that makes a portfolio the active one, which names
-- its wallet always
switchLine :: PortfolioRef -> Doc Style
switchLine ref = hwLine "switch" [text (show (refPortfolio ref)), optionWord "--wallet", text (show (refWallet ref))]

-- | The steps after a sync that found pending transactions in conflict,
-- given how many it found. They are pendingtxs, to find them, and
-- deletetx, to delete them, wherever they are: pendingtxs lists every
-- portfolio, and deletetx finds the portfolio of a transaction by
-- itself. So no step switches, whichever portfolio is active.
conflictSteps :: Natural -> [(String, Doc Style)]
conflictSteps conflicts
  | conflicts == 0 = []
  | conflicts == 1 =
      [ ("Next, find the pending transaction in conflict, which can no longer be sent", hwLine "pendingtxs" []),
        ("Then delete it, to free its coins", hwLine "deletetx" [metavarWord "HASH"])
      ]
  | otherwise =
      [ ("Next, find the pending transactions in conflict, which can no longer be sent", hwLine "pendingtxs" []),
        ("Then delete them, to free their coins", hwLine "deletetx" [metavarWord "HASH"])
      ]

-- | The step after a sync that sent pending transactions again which
-- the store did not publish, given whether steps about conflicts come
-- before it. The sync said why of each. Such a transaction keeps its coins
-- locked, and every sync sends it again, so the step is to delete it. Each
-- is named by its short id, as the sync named it.
notPublishedSteps :: Bool -> [TxHash] -> [(String, Doc Style)]
notPublishedSteps _ [] = []
notPublishedSteps afterOthers refused =
  [(start <> ", to free " <> its <> " coins", vcat [hwLine "deletetx" [pendingHashWord (shortHashText txid)] | txid <- refused])]
  where
    start = (if afterOthers then "Also delete " else "Next, delete ") <> what
    (what, its) = case refused of
      [_] -> ("the pending transaction that was not sent again", "its")
      _ -> ("the pending transactions that were not sent again", "their")

-- | The steps after a sync that found no conflict, given the active
-- portfolio and the portfolios whose transactions it updated, each once.
-- A sync that updated no transaction has no step: txs would show what it
-- showed before. txs lists the active portfolio alone. So the step is txs
-- when the sync updated the active portfolio. When it updated other
-- portfolios alone, the steps start with the switch to the one, or to
-- each in turn: a txs typed without a switch would show none of what the
-- sync found.
txsSteps :: Maybe PortfolioRef -> [PortfolioRef] -> [(String, Doc Style)]
txsSteps activeM updated
  | null updated = []
  | maybe False (`elem` updated) activeM = [("Next, see the transactions", hwLine "txs" [])]
  | otherwise = case updated of
      [only] ->
        [ ("Next, switch to " <> refText only <> ", where the sync found new transactions", switchLine only),
          ("Then see the transactions", hwLine "txs" [])
        ]
      several ->
        [ ("Next, switch in turn to each portfolio where the sync found new transactions", vcat (switchLine <$> several)),
          ("In each, see the transactions", hwLine "txs" [])
        ]

-- | Steps, each with what it does and the command that does it
stepsPrinter :: [(String, Doc Style)] -> Maybe (Doc Style)
stepsPrinter [] = Nothing
stepsPrinter steps = Just $ vcat [vcat [styled Field (what <> ":"), indent 2 command] | (what, command) <- steps]
