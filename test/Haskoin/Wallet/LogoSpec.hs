-- | The logo of hw: the name it spells, and the bits of that name which
-- the help and the reports draw as squares.
module Haskoin.Wallet.LogoSpec (spec) where

import Haskoin.Wallet.Logo
import Test.Hspec

spec :: Spec
spec = do
  describe "The logo" $ do
    it "spells the name of the program" $
      logoName `shouldBe` "hw"

  -- A letter is one byte of ASCII: h is 0x68 and w is 0x77
  describe "The bits of a name" $ do
    -- 0x68 is 0110 1000
    it "are those of the byte of h, the high bit first" $
      nameBits "h" `shouldBe` [[False, True, True, False, True, False, False, False]]

    -- 0x77 is 0111 0111
    it "are those of the byte of w, the high bit first" $
      nameBits "w" `shouldBe` [[False, True, True, True, False, True, True, True]]

    -- A is 0x41, 0100 0001, and B is 0x42, 0100 0010
    it "are a row for each letter, in the order of the name" $
      nameBits "AB"
        `shouldBe` [ [False, True, False, False, False, False, False, True],
                     [False, True, False, False, False, False, True, False]
                   ]
