{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Where an answer is: the portfolio it is about, as a view names it, and
-- the paths of the addresses it shows.
--
-- A view names a portfolio and writes a path in the shortest way that
-- cannot be taken for another one. A database of one wallet names no
-- wallet. An answer seen from the active portfolio writes its paths alone,
-- and one seen from another portfolio says which (the types t'SeenFrom'
-- and t'PathNaming').
module Haskoin.Wallet.Cli.PrettyPrinter.Place
  ( portfolioWords,
    placeLine,
    accountRefLine,
    SeenFrom (..),
    seenPortfolioDoc,
    accountRefField,
    refText,
    PathNaming (..),
    activePaths,
    ownPathsAlone,
    pathsSeenFrom,
    listedPaths,
    ofPortfolio,
    ownSuffix,
    ownPathText,
  )
where

import Data.String.Conversions (cs)
import Haskoin
import Haskoin.Wallet.Cli.PrettyPrinter.Base (columns, field, styled, text, withStyle)
import Haskoin.Wallet.Cli.Style
import Haskoin.Wallet.Database.Portfolio
import Haskoin.Wallet.Keys (PoolPath (..), kindName, poolPathText)
import Numeric.Natural (Natural)
import Prettyprinter (Doc)

{- Where an answer is -}

-- | A portfolio as a view names it: "portfolio0" in a database of one
-- wallet, and with its wallet in a database of several, wallet0 too, as
-- in "portfolio0 of wallet0" ('WalletNaming'). Every line of a view names
-- a portfolio this way: the field of an answer, the title of a listing,
-- and a portfolio after a path. So a portfolio reads the same wherever
-- the user meets it, and a portfolio named alone is always of the one
-- wallet of the database. The listing of the portfolios names the wallet
-- of each on its own, and a sentence names both always ('refText').
portfolioWords :: WalletNaming -> PortfolioRef -> String
portfolioWords naming ref = cs (portfolioRefName naming ref)

-- | The line that says where the rows under it are: the portfolio, named
-- as the views name one, then the network
placeLine :: WalletNaming -> Place -> Doc Style
placeLine naming place =
  columns
    [ text (portfolioWords naming (placePortfolio place)) `withStyle` Account,
      text (placeNetwork place).name
    ]

-- | The line that names an account: its portfolio, named as the views
-- name one, its network and the type of its addresses, as --network and
-- --type pick it
accountRefLine :: WalletNaming -> AccountRef -> Doc Style
accountRefLine naming ref =
  columns
    [ text (portfolioWords naming (placePortfolio (accountRefPlace ref))) `withStyle` Account,
      text (accountRefNetwork ref).name,
      text (cs (kindName (accountRefKind ref)))
    ]

-- | The portfolio an answer about one thing shows it from, and the active
-- portfolio, if the database has one. A command given an address or a
-- transaction finds its portfolio by itself, whichever is active: a
-- transfer between two portfolios is of the one that pays it, which
-- signs it. So the answer may show the paths and the labels of another
-- portfolio than the active one, and it then says so
-- ('seenPortfolioDoc'). The alternative, showing a transfer as the
-- active portfolio sees it, was turned down: the view before signtx would
-- then show a payment received while the mnemonic signs one sent.
data SeenFrom = SeenFrom
  { seenPortfolio :: !PortfolioRef,
    seenActive :: !(Maybe PortfolioRef)
  }
  deriving (Eq, Show)

-- | The portfolio an answer is seen from, as its field names it:
-- "portfolio0", or "portfolio0, not the active portfolio1" when another
-- portfolio is active. Both are named as the views name a portfolio
-- ('portfolioWords'), so with several wallets the field reads "portfolio1
-- of wallet0, not the active portfolio0 of wallet1". The note is yellow.
-- In the view of a transaction it is in the first field, so the user
-- reads it before the paths. An answer that does not know the active
-- portfolio, as one read back from JSON, names none.
seenPortfolioDoc :: WalletNaming -> SeenFrom -> Doc Style
seenPortfolioDoc naming seen =
  (text (portfolioWords naming seen.seenPortfolio) `withStyle` Account) <> note
  where
    note = case seen.seenActive of
      Just active
        | seenFromAnother seen ->
            ", " <> styled OtherPortfolio ("not the active " <> portfolioWords naming active)
      _ -> mempty

-- | Whether an answer is seen from another portfolio than the active one.
-- An answer that does not know the active portfolio is not.
seenFromAnother :: SeenFrom -> Bool
seenFromAnother seen = maybe False (/= seen.seenPortfolio) seen.seenActive

-- | The field that says the account of an address: its portfolio, its
-- network and the type of its addresses. The portfolio says when it is not
-- the active one given ('seenPortfolioDoc'), and is named with its wallet
-- or not, as given.
accountRefField :: WalletNaming -> Natural -> Maybe PortfolioRef -> AccountRef -> Doc Style
accountRefField naming pad activeM ref =
  field pad "Account"
    <> columns
      [ seenPortfolioDoc naming (SeenFrom (placePortfolio (accountRefPlace ref)) activeM),
        text (accountRefNetwork ref).name,
        text (cs (kindName (accountRefKind ref)))
      ]

-- | A portfolio as a sentence names it, with its wallet always:
-- "portfolio1 of wallet0"
refText :: PortfolioRef -> String
refText ref = cs (portfolioName (refPortfolio ref)) <> " of " <> cs (walletName (refWallet ref))

{- The paths of another portfolio -}

-- | How a view writes the path of an address of the wallet. A path is
-- written alone, as in bip84\/0\/3, or with the portfolio that holds the
-- address after it, as in bip84\/0\/3 of portfolio1. That portfolio is
-- named as every line of the view names one ('portfolioWords'): with its
-- wallet in a database of several wallets, as in bip84\/0\/3 of
-- portfolio1 of wallet0.
--
-- A path alone is of the active portfolio, in a view of the active
-- portfolio. The commands read a path the same way. show, label and
-- --spend-from take a path in the active portfolio. So a path alone on the
-- screen is one the user can type back.
--
-- Every other path names its portfolio. Nobody then takes it for a path
-- to type, or for an address of the portfolio they work in. There are two
-- cases.
--
-- * A view of the active portfolio names the portfolio of an address of
--   another portfolio. A transaction pays such an address, or spends from
--   it (the type t'Haskoin.Wallet.Database.TxInfo.HeldElsewhere').
--
-- * A view of another portfolio than the active one names the portfolio
--   after every path. That covers the paths of its own portfolio, and
--   those of any other portfolio, the active one too. Such a view shows
--   paths of two portfolios. A path alone could be read as of either.
--
-- A view may not know the active portfolio, as an answer read back from
-- JSON. It writes the paths of its own portfolio alone.
--
-- The alternative was to keep the paths of the portfolio of the view
-- alone, whichever portfolio is active, as hw wrote them before. The
-- first line of such a view says its portfolio is not the active one
-- ('seenPortfolioDoc'). But a path is read and copied several lines
-- below that line.
data PathNaming = PathNaming
  { -- | The portfolio of the view, when its own paths name it: that of a
    -- view of another portfolio than the active one
    namingOwn :: !(Maybe PortfolioRef),
    -- | Whether the view names the wallet of a portfolio after a path,
    -- as its other lines do
    namingWallets :: !WalletNaming
  }

-- | How a view of the active portfolio writes its paths: its own alone.
-- The listings of one portfolio, addresses, balances, coins and txs, are
-- of the active one, and so are the reports of the commands that work in
-- it. A transaction of txs may pay an address of another portfolio, whose
-- path names it, with its wallet or not as given.
activePaths :: WalletNaming -> PathNaming
activePaths = PathNaming Nothing

-- | How a view that shows addresses of the active portfolio alone writes
-- their paths: each alone. addresses, balances and coins list such
-- addresses, and the report on the dust of a send-all shows such coins.
-- No path there is of another portfolio, so these views name no
-- portfolio and need not know how the wallets are named. A view that may
-- show an address of another portfolio, as that of a transaction, takes
-- 'activePaths' instead.
ownPathsAlone :: PathNaming
ownPathsAlone = activePaths WithoutWallets

-- | How an answer about one thing writes its paths, given how the views
-- name the wallets, the portfolio it is seen from and the active one: its
-- own alone, unless it is seen from another portfolio than the active one
pathsSeenFrom :: WalletNaming -> SeenFrom -> PathNaming
pathsSeenFrom naming seen = PathNaming (if seenFromAnother seen then Just seen.seenPortfolio else Nothing) naming

-- | How a transaction of a listing of every portfolio writes its paths,
-- given how the views name the wallets, the active portfolio, and the
-- portfolio of the transaction
listedPaths :: WalletNaming -> Maybe PortfolioRef -> PortfolioRef -> PathNaming
listedPaths naming activeM ref = pathsSeenFrom naming (SeenFrom ref activeM)

-- | A portfolio after a path: " of portfolio1", or " of portfolio1 of
-- wallet0" where the views name the wallets
ofPortfolio :: PathNaming -> PortfolioRef -> String
ofPortfolio paths ref = " of " <> portfolioWords paths.namingWallets ref

-- | What follows a path of the portfolio of the view: its portfolio when
-- its paths name it, or nothing
ownSuffix :: PathNaming -> String
ownSuffix paths = maybe "" (ofPortfolio paths) paths.namingOwn

-- | The path of an address of the portfolio of the view, as the view
-- writes it: with the type of its account in front, as in bip84\/0\/3
-- ('poolPathText'), and its portfolio after it when the paths of the view
-- name it
ownPathText :: PathNaming -> PoolPath -> String
ownPathText paths path = cs (poolPathText path) <> ownSuffix paths
