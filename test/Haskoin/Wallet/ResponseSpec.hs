-- | The answers of hw in JSON, as --json writes them for scripts
module Haskoin.Wallet.ResponseSpec (spec) where

import Data.Default (def)
import Haskoin (btc, prepareContext)
import Haskoin.Util.Arbitrary (IdentityTests (..), JsonBox (..), testIdentity)
import Haskoin.Wallet.Arbitrary (arbitraryResponse)
import Test.Hspec

spec :: Spec
spec = prepareContext $ \ctx ->
  -- An answer reads back as it was written
  testIdentity def {jsonTests = [JsonBox (arbitraryResponse btc ctx)]}
