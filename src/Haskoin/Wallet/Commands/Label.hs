{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

-- | label: the label of a receiving address, of a transaction or of a
-- contact, and the description of the active portfolio or of its wallet.
-- The word before the label names what is labelled. 'namesNothing' is the
-- refusal of a word that names nothing, which tag and untag share.
module Haskoin.Wallet.Commands.Label
  ( cmdLabel,
    namesNothing,
  )
where

import Control.Applicative ((<|>))
import Control.Monad (unless, when)
import Control.Monad.Except (ExceptT (..), throwError)
import Control.Monad.Trans.Class (lift)
import Data.Foldable (for_)
import Data.Maybe (isJust)
import Data.String.Conversions (cs)
import Data.Text (Text)
import Haskoin
import Haskoin.Wallet.Cli.Parser
import Haskoin.Wallet.Cli.PrettyPrinter (shellWord)
import Haskoin.Wallet.Cli.Response
import Haskoin.Wallet.Commands.Answer (addressAnswer, viewOfPortfolio)
import Haskoin.Wallet.Commands.Contact (NamedAddress (..), contactAnswer, namedAddress, notHeldOrKept)
import Haskoin.Wallet.Commands.Run (runDBResponse)
import Haskoin.Wallet.Commands.ShortId (shownViews, walletTxHashOf)
import Haskoin.Wallet.Config
import Haskoin.Wallet.Contacts (Contact (..))
import Haskoin.Wallet.Database
import Haskoin.Wallet.Database.Address
import Haskoin.Wallet.Database.Contact
import Haskoin.Wallet.Database.PendingTx
import Haskoin.Wallet.Database.Portfolio

-- | Label what the word before the label names: a receiving address, the
-- receiving address at a path of the active portfolio, or a transaction
-- ('labelTransaction'). After the word portfolio or the word wallet, the
-- label is the description of the active portfolio or of its wallet
-- (@describeActive@). The word is never left out: a label alone is
-- refused with what to type (@labelAlone@), and so are the names
-- portfolio0 and wallet0 (@describedByName@).
--
-- An address that is none of the wallet is looked for in the address
-- book, and its contact is labelled ('namedAddress').
--
-- An address and a transaction tell where they are, so they are labelled
-- in their portfolio, whichever is active. A path means nothing without a
-- portfolio, so it is one of the active portfolio ('heldAddressAt'). An
-- address or a path the wallet has on several networks is the one on the
-- --network, the only one, or else the one on the default network
-- ('chooseOnNetwork'). --type goes only with a path without its type. An
-- address and a typed path say their type, and a transaction has none.
-- An option that would change nothing is refused rather than ignored, as
-- show refuses it.
cmdLabel :: Config -> Label -> IO Response
cmdLabel cfg opts =
  runDBResponse cfg $ do
    let word = opts.labelled
    label <- maybe (throwError (labelAlone word)) return opts.label
    for_ (indexedName word) (throwError . describedByName label)
    case readDescribed word of
      Just described -> describeActive cfg described opts.target label
      Nothing -> do
        labelled <- liftEither (readLabelled word)
        case labelled of
          LabelledAddress typed -> do
            unless (readsAsAddress typed) $
              throwError (namesNothing "label" "the label" "ADDRESS|TXID|PATH|portfolio|wallet LABEL" typed)
            refuseType "an address"
            named <- namedAddress (notHeldOrKept cfg typed) cfg opts.target.network typed
            case named of
              OwnAddress accId row -> addressAnswer accId =<< labelAddress row label
              -- The label of a contact says whose address it is
              BookAddress net contact -> do
                lift $ setContactLabel net (contactAddress contact) label
                contactAnswer net (contactAddress contact)
          LabelledPath kindM AddrExternal index -> do
            when (isJust kindM) $ refuseType "a path that starts with its type"
            portfolio <- requireActivePortfolio
            (accId, row) <- heldAddressAt cfg portfolio opts.target.network (kindM <|> opts.target.kind) AddrExternal index
            addressAnswer accId =<< labelAddress row label
          LabelledPath _ AddrInternal _ ->
            throwError changeTakesNoLabel
          LabelledTx named -> do
            refuseType "a transaction"
            when (isJust opts.target.network) $
              throwError "label takes no --network with a transaction: its label is the same in every portfolio that holds it"
            hash <- walletTxHashOf named
            -- One on hidden networks alone is refused before it is
            -- labelled. Otherwise it is labelled wherever it is, and
            -- shown where hw shows it.
            _ <- shownViews cfg named =<< transactionViews hash
            views <- labelTransaction hash label
            ResponseLabelledTxs (onShownNetworks cfg (placeNetwork . fst) views) <$> activePortfolioRef
  where
    refuseType what =
      when (isJust opts.target.kind) $
        throwError ("label takes no --type with " <> what)

-- | The refusal of a first word of label, tag or untag that is neither a
-- path nor a hash, and that reads as an address on no network
-- ('readsAsAddress'): it names nothing the command works on. The refusal
-- says so, then what the command takes: the user may have left out the
-- quotes of a label of two words, or put a tag first. It does not say
-- that the wallet has no such address, which is said of an address
-- (@notGeneratedOrKept@). The arguments are the command, what comes
-- after the first word, and the two as the usage line writes them.
namesNothing :: String -> String -> String -> Text -> String
namesNothing command after usage typed =
  cs typed
    <> " is not an address, a path or the id of a transaction. "
    <> command
    <> " takes what to "
    <> command
    <> " first, then "
    <> after
    <> ": hw "
    <> command
    <> " "
    <> usage

-- | The refusal of a word typed alone for label, which says what to type.
-- label once took such a word for the description of the active
-- portfolio, and a mistyped address whose label was left out replaced
-- that description (the type t'Label'). What the word was meant for
-- cannot be known, so the refusal follows what it reads as: the word
-- portfolio or wallet lacks its description, a word of the shape of
-- something label labels lacks its label ('labelledShapeOf'), and any
-- other word may be a description typed the old way, so the refusal
-- gives the command that sets it.
labelAlone :: String -> String
labelAlone word
  | Just described <- readDescribed word <|> indexedName word =
      "label needs the description after " <> word <> ": hw label " <> describedWord described <> " DESCRIPTION"
  | Just shape <- labelledShapeOf word =
      word <> " reads as " <> shape <> ". To label it, give the label after it: hw label " <> word <> " LABEL"
  | otherwise =
      "label needs what to label, then the label: hw label ADDRESS|TXID|PATH LABEL. To describe the active portfolio, run: hw label portfolio "
        <> shellWord word
        <> ". To describe its wallet, run: hw label wallet "
        <> shellWord word

-- | The refusal of a portfolio or a wallet named with its index for
-- label, as in hw label portfolio1 Savings. The views write these names,
-- so a user may type one. label describes the active portfolio and its
-- wallet only, by the bare words, as every command works in the active
-- portfolio. The refusal gives that command, with the description typed.
describedByName :: Text -> Described -> String
describedByName description = \case
  DescribedPortfolio ->
    "label describes the active portfolio, by the word portfolio alone: " <> command DescribedPortfolio <> ". To describe another portfolio, switch to it first"
  DescribedWallet ->
    "label describes the wallet of the active portfolio, by the word wallet alone: " <> command DescribedWallet <> ". To describe another wallet, switch to one of its portfolios first"
  where
    command described = "hw label " <> describedWord described <> " " <> shellWord (cs description)

-- | Set the description of the active portfolio, or of its wallet, to
-- the label typed after the word portfolio or wallet. An empty
-- description removes it. The answer shows the active portfolio, with
-- the description of its wallet too.
--
-- A description is of the whole portfolio, or of the whole wallet, so
-- --network and --type are refused with it.
describeActive :: Config -> Described -> Target -> Text -> ExceptT String (DB IO) Response
describeActive cfg described target description = do
  when (isJust target.network) $ throwError (refusedOption "--network")
  when (isJust target.kind) $ throwError (refusedOption "--type")
  liftEither $ checkDescription description
  portfolio <- requireActivePortfolio
  case described of
    DescribedPortfolio -> setPortfolioDescription portfolio.portfolioId description
    DescribedWallet -> setWalletDescription portfolio.portfolioWalletId description
  -- Read again, with the new description
  ResponsePortfolio <$> (viewOfPortfolio cfg =<< getPortfolio portfolio.portfolioId)
  where
    word = describedWord described
    refusedOption option = "label takes no " <> option <> " with " <> word <> ": a description is of the whole " <> word
