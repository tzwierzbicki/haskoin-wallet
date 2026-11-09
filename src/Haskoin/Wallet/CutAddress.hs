{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Cut addresses: an address with its middle hidden behind dots, which
-- show gives for a path that the wallet does not hold.
--
-- The cut form has one purpose: to check, on the offline computer, an
-- address that the online computer gave out. An online computer that was
-- tampered with can show an address of its own in place of one of the
-- account, and the payer would pay the attacker. The offline computer
-- holds the same account and derives the address at the same path. The
-- user compares the two.
--
-- The offline computer must not show that address whole, though. It never
-- gives out an address, so it cannot tell whether a path lies within the
-- gap that a wallet looks at when it recovers the seed. A user could type
-- any path, copy the address shown and have it paid, and no sync and no
-- recovery would find the coins. Nobody can pay a cut address: the
-- characters hidden leave over a million addresses that fit it, and only
-- one of them is of the account.
--
-- The ends shown carry at least 128 bits. To pass the check with an
-- address of its own, an attacker would have to find a key whose address
-- has the same ends, which takes about 2^128 tries. An address of a key
-- holds a hash of 160 bits, so this is a trade-off: the more the ends
-- show, the harder an address is to forge, and the fewer the addresses
-- that fit the cut form. 128 bits is out of reach of any search, and still
-- leaves enough hidden.
--
-- How many characters carry 128 bits depends on how the address is
-- written. Every address of an account starts with the same prefix, such
-- as bc1q, which carries nothing.
--
-- * bech32, the addresses of a BIP84 account: each character after bc1q
--   carries 5 bits. 13 at each end carry 26 × 5 = 130 bits. The 12 hidden
--   carry 60 bits, and the checksum, the last 6 characters, rules out 30
--   of them: about 2^30 addresses fit, a billion.
-- * Base58, the addresses of a BIP44 account on Bitcoin: each character
--   carries about 5.86 bits, but the first two carry only about 4.5
--   together, the version and the highest digit of the number after it.
--   12 at each end carry 4.5 + 22 × 5.86 = 133 bits. An address has 33 or
--   34 characters, so 9 or 10 are hidden. They carry 53 or 59 bits, and
--   the checksum of 32 bits rules out 32: 2^21 or 2^27 addresses fit, two
--   million or a hundred million.
-- * A cash address, the addresses of a Bitcoin Cash account: each
--   character after bitcoincash:q carries 5 bits, but for the first, which
--   carries 2, and the last before the checksum, which carries 3. 13 at
--   the start and 14 at the end carry 2 + 12 × 5 + 5 × 5 + 3 + 8 × 5 = 130
--   bits. The 14 hidden carry 70 bits, and the checksum of 8 characters
--   rules out 40: about 2^30 addresses fit.
--
-- Each end has a set number of characters, and the middle hides what is
-- left. A Base58 address one character shorter then hides one character
-- less, and its ends carry as many bits. A dot stands for each character
-- hidden. The cut form is then as long as the
-- address, and its ends line up with those of the address it is compared
-- with. No address has a dot, so a wallet refuses a cut address pasted as
-- an address.
--
-- An attacker who controls the online computer also picks the path it
-- names. One address it made can then match any of the addresses of the
-- account, at the path where it does: each doubling of the paths the user
-- would check halves the work. show sets no limit on the path, since a
-- limit would leave a large account with no check at all. The user is to
-- notice a path far past those the account gave out, which addresses
-- lists.
--
-- Turned down:
--
-- * A hash of the address, or words or a picture made from it. The user
--   cannot work it out from the address the online computer shows. That
--   computer would have to show it too, and it is the one not trusted.
-- * Typing the address on the offline computer, to compare it there. It
--   is slow and easy to get wrong, so nobody would do it.
-- * The whole address, under a limit on the path. Any address shown whole
--   can be paid, and a limit that a large account fits under still lets a
--   user pay an address past the gap.
module Haskoin.Wallet.CutAddress
  ( cutAddress,
  )
where

import Data.Text (Text)
import qualified Data.Text as Text
import Haskoin (Address (..), Network (..), addrToText)

-- | How many characters of an address its cut form shows at its start and
-- at its end
data Ends = Ends
  { -- | At the start, the prefix that every address of the account starts
    -- with included
    startShown :: !Int,
    -- | At the end
    endShown :: !Int
  }

-- | The ends of an address that carry at least 128 bits, as the network
-- writes the address: see the header of the module for the count of each
-- form. Only the two kinds of address that an account derives have them:
-- P2PKH and P2WPKH.
endsOf :: Network -> Address -> Maybe Ends
endsOf net addr = case addr of
  PubKeyAddress _ -> Just $ case net.cashAddrPrefix of
    -- The prefix, such as bitcoincash, then : and the q of the version
    Just prefix -> Ends (Text.length prefix + 2 + 13) 14
    Nothing -> Ends 12 12
  WitnessPubKeyAddress _ -> do
    -- The prefix, such as bc, then the separator 1 and the q of the
    -- witness version 0
    prefix <- net.bech32Prefix
    Just (Ends (Text.length prefix + 2 + 13) 13)
  _ -> Nothing

-- | The address with its middle hidden, a dot for each character, as the
-- network writes the address. There is none for an address of a kind that
-- no account derives.
cutAddress :: Network -> Address -> Maybe Text
cutAddress net addr = do
  ends <- endsOf net addr
  written <- addrToText net addr
  let hidden = Text.length written - ends.startShown - ends.endShown
  Just (Text.take ends.startShown written <> Text.replicate hidden "." <> Text.takeEnd ends.endShown written)
