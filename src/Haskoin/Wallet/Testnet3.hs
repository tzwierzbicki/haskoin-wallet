{-# LANGUAGE OverloadedRecordDot #-}
{-# OPTIONS_GHC -Wno-deprecations #-}

-- | The two testnet3 networks, which hw still works on and no longer
-- recommends.
--
-- haskoin-core 3.0 deprecates its values of these networks, 'btcTest' and
-- 'bchTest', and says that a later major version removes them. Bitcoin
-- Core deprecated testnet3 first, for testnet4. hw follows: a portfolio
-- keeps its accounts on both networks, so a user who has coins there
-- loses nothing, and hw says that the networks are deprecated where it
-- lists them ('isDeprecatedNetwork').
--
-- The compiler warns of every use of a deprecated value. This module is
-- the one place of hw that names the two: it is compiled without that
-- warning, and gives the networks under names of its own. Every other
-- module then keeps the warning, for whatever else is deprecated one day.
-- When haskoin-core removes the two values, hw removes the networks, or
-- this module describes them itself, and nothing else changes.
module Haskoin.Wallet.Testnet3
  ( btcTestnet3,
    bchTestnet3,
    isDeprecatedNetwork,
  )
where

import Haskoin (Network (..), bchTest, btcTest)

-- | Testnet3 of Bitcoin, which hw calls btctest
btcTestnet3 :: Network
btcTestnet3 = btcTest

-- | Testnet3 of Bitcoin Cash, which hw calls bchtest
bchTestnet3 :: Network
bchTestnet3 = bchTest

-- | Whether hw still works on the network, and says that it is deprecated
isDeprecatedNetwork :: Network -> Bool
isDeprecatedNetwork net = net.name `elem` [btcTestnet3.name, bchTestnet3.name]
