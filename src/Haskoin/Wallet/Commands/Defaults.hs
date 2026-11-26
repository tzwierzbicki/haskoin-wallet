{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

-- | setdefault and showdefault: the defaults the configuration file keeps.
--
-- setdefault writes a default to the configuration file inside its
-- transaction, as its last step (@writeDefaults@): when the file cannot
-- be written, the database is left as it was, so that the two never
-- disagree. It writes the network, the type, the unit, the view of the
-- listings and the networks hw shows. The active portfolio is no key of
-- the file: it is in the database, and switch changes it
-- ("Haskoin.Wallet.Commands.Portfolio"). showdefault reads both back, and
-- changes neither ('cmdShowDefault').
module Haskoin.Wallet.Commands.Defaults
  ( cmdSetDefault,
    cmdShowDefault,
  )
where

import Control.Monad (when)
import Control.Monad.Except (ExceptT (..), runExceptT, throwError)
import Control.Monad.IO.Class (liftIO)
import Data.Maybe (listToMaybe)
import Data.String.Conversions (cs)
import Data.Text (Text)
import qualified Data.Text as Text
import Haskoin
import Haskoin.Wallet.Amounts (unitName)
import Haskoin.Wallet.Cli.Command
import Haskoin.Wallet.Cli.Listing
import Haskoin.Wallet.Cli.Response
import Haskoin.Wallet.Commands.Run (runDBResponse)
import Haskoin.Wallet.Config
import Haskoin.Wallet.Config.View (viewName)
import Haskoin.Wallet.Database
import Haskoin.Wallet.Database.Portfolio
import Haskoin.Wallet.Keys
import qualified Toml

-- | Write keys of the configuration file of the data directory, as the
-- last step of a command: a file that cannot be written fails the
-- command, which undoes what it did in the database
writeDefaults :: Config -> [(Text, Toml.Value)] -> ExceptT String (DB IO) ()
writeDefaults cfg changes =
  ExceptT . liftIO . runExceptT $ setConfigKeys (configDataDirectory cfg) changes

-- | Write the defaults the options give to the configuration file: the
-- network, the type of address, the unit, the view of the listings and
-- whether they list their rows in the reverse order, the networks hw
-- shows, and whether hw keeps a journal for ledger. Each is written by
-- the name the file reads back, which is also the name the answer shows.
--
-- --journal and --no-journal only write the key here. The journal itself
-- is written or removed once the command is done (@journalAfter@), as
-- after any command that changes it. The answer says where the journal
-- is, or that it goes, in a t'JournalSwitch'. The portfolio the commands
-- work in is no default of the file: switch makes another one active.
--
-- --show-network and --hide-network change the networks hw shows
-- (@shownAfter@). The default network, the one --network names or the
-- one the file has, must be among them once the command is done
-- (@checkDefaultStaysShown@). So one command shows a network and makes it
-- the default, or hides the default network and names another. The
-- networks are written whole, in the order of the networks of a
-- portfolio, and the answer lists them.
cmdSetDefault :: Config -> SetDefault -> IO Response
cmdSetDefault cfg opts =
  runDBResponse cfg $ do
    shown <- liftEither $ shownAfter cfg opts.showNetworks opts.hideNetworks
    liftEither $ checkDefaultStaysShown cfg opts.network shown
    let named =
          [(defaultNetworkKey, cs net.name) | Just net <- [opts.network]]
            <> [(defaultTypeKey, kindName kind) | Just kind <- [opts.kind]]
            <> [(unitKey, unitName unit) | Just unit <- [opts.unit]]
            <> [(listingViewKey, viewName view) | Just view <- [opts.view]]
        -- The switches setdefault writes: true or false, not a name
        switched =
          [(listingReverseKey, order == ReverseOrder) | Just order <- [opts.listingOrder]]
            <> [(journalKey, kept) | Just kept <- [opts.journal]]
        changesNetworks = not (null opts.showNetworks && null opts.hideNetworks)
        names = [cs net.name | net <- shown] :: [Text]
    when (null named && null switched && not changesNetworks) $
      throwError "Give the default to set: --network, --type, --unit, --compact, --verbose, --tabular, --reverse, --no-reverse, --show-network, --hide-network, --journal or --no-journal"
    writeDefaults cfg $
      [(key, Toml.Text name) | (key, name) <- named]
        <> [(key, Toml.Bool switch) | (key, switch) <- switched]
        <> [(shownNetworksKey, Toml.List (Toml.Text <$> names)) | changesNetworks]
    let file = configFile (configDataDirectory cfg)
        written =
          named
            <> [(key, switchText switch) | (key, switch) <- switched]
            <> [(shownNetworksKey, shownNetworksText shown) | changesNetworks]
    return $ ResponseDefaults written file ((`JournalSwitch` journalFile cfg) <$> opts.journal)

-- | Show the defaults setdefault writes, as the configuration has them,
-- and the active portfolio, which switch changes. Each switch asks for
-- one of them, and no switch for all of them ('showsEverything'). A user
-- who wonders where a command works wants to see them together, and a
-- script asks for the one it reads.
--
-- A key the file does not have shows the value hw takes without it: the
-- value the commands work with, which is what the user wants to know.
-- The keys come in the order setdefault writes them, whatever the order
-- of the switches.
--
-- The active portfolio is no key of the file. It is read from the
-- database, and comes after the keys. --portfolio names it with its
-- wallet, since a portfolio is named by both. --wallet names the wallet
-- alone, for a user of several mnemonics who wants to know which one the
-- commands work with. A database without any portfolio has none, which
-- the answer says. It is no failure: the question has an answer.
cmdShowDefault :: Config -> ShowDefault -> IO Response
cmdShowDefault cfg opts =
  runDBResponse cfg $ do
    active <- activeShownNow
    let file = configFile (configDataDirectory cfg)
    return $ ResponseShownDefaults keys file active
  where
    included switchGiven = showsEverything opts || switchGiven
    keys =
      [(defaultNetworkKey, cs (configDefaultNetwork cfg).name) | included opts.askedNetwork]
        <> [(defaultTypeKey, kindName (configDefaultType cfg)) | included opts.askedType]
        <> [(unitKey, unitName (configUnit cfg)) | included opts.askedUnit]
        <> [(listingViewKey, viewName (configListingView cfg)) | included opts.askedView]
        <> [(listingReverseKey, switchText (configListingReverse cfg)) | included opts.askedReverse]
        <> [(shownNetworksKey, shownNetworksText (configShownNetworks cfg)) | included opts.askedShownNetworks]
        <> [(journalKey, switchText (configJournal cfg)) | included opts.askedJournal]
    -- --portfolio names the wallet too, so it wins over --wallet
    activeShownNow
      | included opts.askedPortfolio = ActivePortfolioShown <$> activePortfolioRef
      | opts.askedWallet = ActiveWalletShown . fmap refWallet <$> activePortfolioRef
      | otherwise = return ActiveNotAsked

-- | A switch of the configuration, as the file writes it and as the
-- answers of setdefault and showdefault give it: true or false
switchText :: Bool -> Text
switchText switch = if switch then "true" else "false"

-- | The networks hw shows, as the answers of setdefault and showdefault
-- give them: their names in a line, as in "btc, bch, btctest4"
shownNetworksText :: [Network] -> Text
shownNetworksText networks = Text.intercalate ", " [cs net.name | net <- networks]

-- | The networks hw shows once setdefault has shown the first ones given
-- and hidden the second ones: those of the configuration, with the ones
-- to show added and the ones to hide taken out, in the order of the
-- networks of a portfolio. Showing a network hw shows already, or hiding
-- a hidden one, changes nothing and is no error, since the file ends up
-- as asked. A network given to both options is refused: the user cannot
-- mean both.
shownAfter :: Config -> [Network] -> [Network] -> Either String [Network]
shownAfter cfg toShow toHide =
  case [net | net <- toShow, net `elem` toHide] of
    net : _ -> Left ("The network " <> net.name <> " is given to both --show-network and --hide-network")
    [] -> Right (inPortfolioOrder [net | net <- configShownNetworks cfg <> toShow, net `notElem` toHide])

-- | Refuse to leave the default network hidden: the one setdefault is
-- given with --network, or else the one of the configuration, must be
-- among the networks hw shows once the command is done. A command without
-- --network works on the default network, and would otherwise work on a
-- network hw shows nothing of. Each refusal gives the command that does
-- what the user meant.
checkDefaultStaysShown :: Config -> Maybe Network -> [Network] -> Either String ()
checkDefaultStaysShown cfg givenM shown = case givenM of
  Just net
    | net `notElem` shown ->
        Left $
          "The network "
            <> net.name
            <> " is hidden, and the default network must be one hw shows. To show it and make it the default, run: "
            <> showNetworkCommand net
            <> " --network "
            <> net.name
  Nothing
    | current `notElem` shown ->
        Left $
          "The network "
            <> current.name
            <> " is the default network, which hw always shows. To hide it, name another default in the same command"
            <> maybe "" (\other -> ", as in: hw setdefault --hide-network " <> current.name <> " --network " <> other.name) (listToMaybe shown)
  _ -> Right ()
  where
    current = configDefaultNetwork cfg
