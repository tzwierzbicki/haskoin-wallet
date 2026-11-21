{-# LANGUAGE OverloadedRecordDot #-}
{-# OPTIONS_GHC -Wno-deprecations #-}

-- | The testnet3 of Bitcoin, which hw still works on and no longer
-- recommends.
--
-- haskoin-core 3.0 deprecates its value of this network, 'btcTest', and
-- says that a later major version removes it: Bitcoin Core deprecated
-- testnet3, for testnet4. hw follows: a portfolio keeps its accounts on
-- the network, so a user who has coins there loses nothing, and hw says
-- that the network is deprecated where it lists it
-- ('isDeprecatedNetwork'). The testnet3 of Bitcoin Cash is not deprecated:
-- Bitcoin Cash Node still runs it, and haskoin-core gives it as
-- @bchTest3@, named @bchtest3@.
--
-- The compiler warns of every use of a deprecated value. This module is
-- the one place of hw that names it: it is compiled without that warning,
-- and gives the network under a name of its own. Every other module then
-- keeps the warning, for whatever else is deprecated one day. When
-- haskoin-core removes the value, hw removes the network, or this module
-- describes it itself, and nothing else changes.
module Haskoin.Wallet.Testnet3
  ( btcTestnet3,
    isDeprecatedNetwork,
  )
where

import Haskoin (Network (..), btcTest)

-- | Testnet3 of Bitcoin, which hw calls btctest
btcTestnet3 :: Network
btcTestnet3 = btcTest

-- | Whether hw still works on the network, and says that it is deprecated
isDeprecatedNetwork :: Network -> Bool
isDeprecatedNetwork net = net.name == btcTestnet3.name
