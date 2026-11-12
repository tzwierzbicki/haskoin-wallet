{-# LANGUAGE OverloadedStrings #-}

-- | The views of a listing: how portfolios, addresses, balances, txs,
-- pendingtxs and coins show each element on the screen.
--
-- A listing has three views. The compact view gives each element in a few
-- lines, enough to tell the elements apart. The verbose view gives every
-- detail in padded fields. The table gives each element one row under a
-- header, which suits reading down a column, such as the amounts or the
-- balances, and gives up the details that do not fit on a row.
--
-- A user picks a view for one listing with --compact, --verbose or
-- --tabular. A user who always wants the same one sets the key
-- listing-view of config.json, with setdefault and the same option, and
-- the option then overrides it for one listing. The type lives in a module of its own because both the command
-- line ("Haskoin.Wallet.Cli.Parser") and the configuration
-- ("Haskoin.Wallet.Config") name it, and the configuration cannot depend
-- on the command line, which reads it for shell completion.
module Haskoin.Wallet.Cli.View
  ( View (..),
    viewName,
    readView,
    viewNames,
  )
where

import Data.List (find)
import Data.Text (Text)

-- | How a listing shows each element: in a few lines, with every detail
-- with --verbose, or on one row of a table with --tabular. Every listing
-- offers the three.
data View = Compact | Verbose | Table
  deriving (Eq, Show, Enum, Bounded)

-- | The name of a view, as the option that asks for it writes it and as
-- the key listing-view of config.json takes it. A table is named after
-- --tabular, so that the file and the command line read alike.
viewName :: View -> Text
viewName Compact = "compact"
viewName Verbose = "verbose"
viewName Table = "tabular"

-- | The view of a name, exactly as 'viewName' writes it. Nothing for any
-- other word, so that a typo in the configuration file is refused rather
-- than read as the compact view.
readView :: Text -> Maybe View
readView name = find ((== name) . viewName) [minBound .. maxBound]

-- | The names of every view, in the order the help gives them
viewNames :: [Text]
viewNames = viewName <$> [minBound .. maxBound]
