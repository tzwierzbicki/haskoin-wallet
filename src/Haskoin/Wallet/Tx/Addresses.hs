{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedRecordDot #-}

-- | The addresses of a transaction, as hw lists them: those it spends
-- from, those it pays, and those that take its change.
--
-- Every view of a transaction lists the same addresses: the verbose view
-- on the screen ("Haskoin.Wallet.Cli.PrettyPrinter") and the sheet of the
-- addresses in a spreadsheet ("Haskoin.Wallet.Export.Sheet"). The lists are
-- made here, once, and a view only draws them. So a rule on what is
-- listed, such as every output of a payment that is not sent yet, or a
-- row for each output to an address paid several times ('paidOutputs'),
-- holds in each view.
--
-- The lists are read from one t'TxInfo'. They are from the side of one
-- portfolio, as its summary is: an address of another portfolio of the
-- database is among those of others, and says where it is.
module Haskoin.Wallet.Tx.Addresses
  ( TxAddress (..),
    HeldAddress (..),
    spentFrom,
    paidTo,
    paidOutputs,
    eachOutput,
  )
where

import Data.List (nub)
import qualified Data.Map.Strict as Map
import Data.Maybe (isJust)
import Data.Text (Text)
import Haskoin (Address, OutPoint (..))
import Haskoin.Wallet.Contacts (Contact)
import Haskoin.Wallet.Database.Portfolio (PortfolioRef)
import Haskoin.Wallet.Database.TxInfo (HeldElsewhere (..), MyInputs (..), MyOutputs (..), TxInfo (..), TxSummary (..), TxTags (..), TxType (..), outputsPaying, txInPreparation)
import Haskoin.Wallet.Keys (PoolPath)
import Haskoin.Wallet.Tags (ShownTag)
import Numeric.Natural (Natural)

-- | An address a transaction spends from or pays, as hw shows it
data TxAddress = TxAddress
  { txAddress :: !Address,
    -- | What it spends or receives. None for an address another wallet
    -- spends from in a transaction on the chain: that wallet's coins are
    -- nothing to this one.
    txAddressValue :: !(Maybe Natural),
    -- | Where an address of the wallet is, and its label: one of the
    -- portfolio of the view, or of another portfolio of the database
    txAddressHeld :: !(Maybe HeldAddress),
    -- | The labels the coins spent from an address of the portfolio show,
    -- each once. None for an address paid.
    txAddressCoinLabels :: ![Text],
    -- | The tags of an address of the portfolio of the view, all of
    -- them: the verbose view of a transaction shows them below the
    -- address, after its label
    txAddressShownTags :: ![ShownTag],
    -- | The contact of the address book the address is, for an address
    -- that is not of the wallet: its label and its tags
    txAddressContact :: !(Maybe Contact)
  }

-- | An address of the wallet in a transaction: where it is, and its
-- label. It is of the portfolio the view is seen from, or of another
-- portfolio of the database, which is then named.
data HeldAddress = HeldAddress
  { heldPath :: !PoolPath,
    heldLabel :: !Text,
    -- | The portfolio that holds it, when that is another one than the
    -- portfolio of the view
    heldIn :: !(Maybe PortfolioRef)
  }

-- | An address another portfolio holds, as an address of the wallet in a
-- transaction
heldElsewhereAddress :: HeldElsewhere -> HeldAddress
heldElsewhereAddress held = HeldAddress (elsewherePath held) (elsewhereLabel held) (Just (elsewherePortfolio held))

-- | The addresses a transaction spends from: those of the portfolio with
-- what they spend and the labels their coins show, then those of others.
-- Among these, an address of another portfolio of the database comes with
-- what it spends, since those coins are the user's too. An address of
-- another wallet comes without in a transaction on the chain, where its
-- coins are the payer's, and with what it spends in one in preparation
-- ('txInPreparation'): the user may be about to sign it, as a sweep,
-- or to send it, and checks what each address gives.
spentFrom :: TxInfo -> [TxAddress]
spentFrom info =
  [TxAddress a (Just v) (Just (HeldAddress p l Nothing)) (coinLabelsOf outpoints) (tagsAt info a) Nothing | (a, MyInputs v p l outpoints) <- Map.assocs (summaryMyInputs summary)]
    <> [fromOthers a v | (a, v) <- Map.assocs (summaryOtherInputs summary)]
  where
    summary = txInfoSummary info
    coinLabelsOf outpoints = nub [label | outpoint <- outpoints, Just label <- [Map.lookup outpoint.hash (txInfoInputLabels info)]]
    fromOthers a v = case Map.lookup a (txInfoElsewhere info) of
      Just held -> TxAddress a (Just v) (Just (heldElsewhereAddress held)) [] [] Nothing
      Nothing -> TxAddress a (if txInPreparation info then Just v else Nothing) Nothing [] [] (contactAt info a)

-- | The addresses a transaction pays, and the change it sends back to the
-- portfolio. What a payment sends back to the portfolio is its change. In
-- a payment the portfolio receives, the outputs to others are the change
-- of the payer, left out once it is on the chain. Between the addresses
-- of the portfolio, no output can be told to be the change. An address of
-- another portfolio of the database is among those of others, since the
-- payment leaves the portfolio, and says where it is.
--
-- A payment received that is in preparation ('txInPreparation') lists the
-- outputs to others too, after those of the portfolio. hw cannot tell
-- the payment of another wallet from a sweep, whose coins are the user's:
-- one sweep may pay the portfolio a tenth of them and a stranger the
-- rest, and the user who signs it must see every address it pays. An
-- external transaction pays others only.
paidTo :: TxInfo -> ([TxAddress], [TxAddress])
paidTo info =
  case summaryType summary of
    TxDebit -> (toOthers, toWallet)
    TxCredit
      | txInPreparation info -> (toWallet <> toOthers, [])
      | otherwise -> (toWallet, [])
    TxInternal -> (toWallet <> toOthers, [])
    TxExternal -> (toOthers, [])
  where
    summary = txInfoSummary info
    toWallet = [TxAddress a (Just v) (Just (HeldAddress p l Nothing)) [] (tagsAt info a) Nothing | (a, MyOutputs v p l) <- Map.assocs (summaryMyOutputs summary)]
    -- An address another portfolio holds is of the wallet, whatever the
    -- address book says of it
    toOthers =
      [ TxAddress a (Just v) held [] [] (if isJust held then Nothing else contactAt info a)
        | (a, v) <- Map.assocs (summaryOtherOutputs summary),
          let held = heldElsewhereAddress <$> Map.lookup a (txInfoElsewhere info)
      ]

-- | The outputs of a transaction to the addresses it pays, and those of
-- its change: the addresses of 'paidTo', with a row for each output
-- ('eachOutput'). It is what a view lists under To and Change. 'paidTo'
-- serves the views that name an address once, with all it receives.
paidOutputs :: TxInfo -> ([TxAddress], [TxAddress])
paidOutputs info = (concatMap (eachOutput info) to, concatMap (eachOutput info) change)
  where
    (to, change) = paidTo info

-- | The outputs that pay an address 'paidTo' lists: the address once for
-- each of them, in the order of the transaction, with what that output
-- pays. Nearly every address is paid by one output, and is its own row.
--
-- A transaction may pay an address in several outputs, each a coin of
-- its own: hw builds one when the user asks for it (--repeat-address),
-- and any wallet may write one in a file. One row with the total hid
-- that the transaction had more outputs than the view listed. A user
-- about to sign must see each output.
--
-- It is not for an address of 'spentFrom': the amounts are those of the
-- outputs to the address, and an address may be spent from and paid in
-- one transaction.
eachOutput :: TxInfo -> TxAddress -> [TxAddress]
eachOutput info row = case txAddressValue row of
  Just total -> [row {txAddressValue = Just amount} | amount <- outputsPaying (txInfoSummary info) (txAddress row) total]
  Nothing -> [row]

-- | The contact of the address book an address of others in a
-- transaction is, if it is one
contactAt :: TxInfo -> Address -> Maybe Contact
contactAt info addr = Map.lookup addr (txInfoContacts info)

-- | The tags of an address of the portfolio in a transaction, all of
-- them, or none for an address without tags
tagsAt :: TxInfo -> Address -> [ShownTag]
tagsAt info addr = Map.findWithDefault [] addr (txAddressTags (txInfoTags info))
