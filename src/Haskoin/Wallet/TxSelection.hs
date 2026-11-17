{-# LANGUAGE OverloadedRecordDot #-}

-- | The transactions a file keeps when tags narrow it down: the account
-- statement of report and the journal of exportjournal, with --tag,
-- --address-tag or --untagged.
--
-- A user sorts payments with tags: business on a transaction, rent on a
-- contact, kyc on the addresses of a group of coins. An accountant asks
-- for one of these groups, and is not to be handed the rest of the
-- wallet. So a statement and a journal can be of some transactions only.
--
-- = The transactions txs lists
--
-- The options are those of txs, and they keep the transactions txs lists
-- with them, in each portfolio. The user reads the list on the screen
-- first, and the file then holds the same one. A second rule here, close
-- to the first, would be a trap.
--
-- So the database answers, with the condition of txs
-- ("Haskoin.Wallet.Database.Tag".'Haskoin.Wallet.Database.Tag.txidsKept'),
-- and a selection is the transactions it kept. The modules that write
-- the files stay pure: they are given the selection, and ask it about
-- each transaction ('keepsTx').
--
-- = As each portfolio sees them
--
-- A transaction is kept as a portfolio sees it. The tags of an address
-- are of one portfolio, so a payment from a portfolio to another may be
-- kept for the first and not for the second. The statement has an
-- account for each portfolio and lists the payment where it is kept. The
-- journal has one entry for the payment and keeps it whole when either
-- side is kept, since half an entry does not add up.
--
-- = What a selection cannot state
--
-- The transactions left out moved money too. A file of some transactions
-- therefore states no balance: not the opening and closing balances of a
-- statement, and not the balances at the end of a journal. It says which
-- transactions it holds instead ('selectionStated'), so that no reader
-- takes it for the whole.
module Haskoin.Wallet.TxSelection
  ( TxSelection (..),
    keepsTx,
  )
where

import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import Haskoin
import Haskoin.Wallet.Database.Portfolio (PortfolioRef)

-- | Some of the transactions of the wallet, kept by their tags
data TxSelection = TxSelection
  { -- | What the transactions kept have in common, in the words that
    -- follow "transactions", as the first line of txs says it: "tagged
    -- business", "at addresses tagged kyc", "without a tag or a tagged
    -- contact"
    selectionStated :: !Text,
    -- | Each transaction kept: the portfolio that sees it, the name of
    -- its network, and its id. The network is part of it, since Bitcoin
    -- and Bitcoin Cash share the transactions made before they split.
    selectionKept :: !(Set (PortfolioRef, String, TxHash))
  }
  deriving (Eq, Show)

-- | Whether a selection keeps a transaction of a network, as a
-- portfolio sees it
keepsTx :: TxSelection -> PortfolioRef -> Network -> TxHash -> Bool
keepsTx selection ref net txid = (ref, net.name, txid) `Set.member` selectionKept selection
