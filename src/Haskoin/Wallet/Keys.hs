{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

-- | The keys of a wallet: from its mnemonic and passphrase to the signing
-- key of an account, along its BIP44 path, and the fingerprint of the
-- wallet; and the private keys of a sweep, as users write them and where
-- they come from.
module Haskoin.Wallet.Keys where

import Control.Applicative ((<|>))
import Data.Maybe (mapMaybe)
import Data.String.Conversions (cs)
import Data.Text (Text)
import qualified Data.Text as Text
import Haskoin
import Numeric.Natural (Natural)

data MnemonicPass = MnemonicPass
  { mnemonicWords :: !Text,
    mnemonicPass :: !Text
  }
  deriving (Eq, Show)

-- | The BIP44 path of an account: m/44'/coin'/account', with the coin
-- type of the network
bip44Deriv :: Network -> Natural -> HardPath
bip44Deriv net a = Deriv :| 44 :| net.bip44Coin :| fromIntegral a

-- | The index of an account from its public key: the last step of its path
xPubIndex :: XPubKey -> Natural
xPubIndex = fromIntegral . xPubChild

-- | The signing key of an account: the key at its BIP44 path under the
-- master key of the seed
signingKey :: Network -> Ctx -> MnemonicPass -> Natural -> Either String XPrvKey
signingKey net ctx mnem acc = do
  seed <- mnemonicToSeed (mnemonicPass mnem) (mnemonicWords mnem)
  return $ derivePath ctx (bip44Deriv net acc) (makeXPrvKey seed)

-- | The fingerprint of a wallet: that of the public key of its first
-- Bitcoin account
walletFingerprint :: Network -> Ctx -> MnemonicPass -> Either String Fingerprint
walletFingerprint net ctx mnem = do
  xPrvKey <- signingKey net ctx mnem 0
  return $ xPubFP ctx $ deriveXPubKey ctx xPrvKey

{- Private keys of a sweep -}

-- | Where the private keys of a sweep come from
data KeySource
  = -- | A file with one or more keys per line
    KeysInFile !FilePath
  | -- | Typed on the terminal without echo, asked for with --prvkeys -
    KeysOnTerminal
  deriving (Eq, Show)

-- | A private key as a user writes it: in WIF, compressed or not, of the
-- network; as a minikey; or as 64 hexadecimal digits. Whether the key is
-- meant for a compressed public key is not kept: a sweep tries both.
parseSecKey :: Network -> Text -> Maybe SecKey
parseSecKey net word = wif <|> mini <|> hex
  where
    wif = (.key) <$> fromWif net word
    mini = (.key) <$> fromMiniKey (cs word)
    hex = secKey =<< decodeHex word

-- | The private keys of a text, as a key file or a typed line holds them:
-- every word that is a key, on any line; a line starting with # is a
-- comment, and a line with words but no key is an error, so that a
-- mistyped key is not passed over.
parseSecKeys :: Network -> Text -> Either String [SecKey]
parseSecKeys net text = concat <$> mapM keysOf (zip [1 :: Int ..] (Text.lines text))
  where
    keysOf (number, line)
      | null (Text.words line) || "#" `Text.isPrefixOf` Text.stripStart line = Right []
      | null keys = Left $ "No private key on line " <> show number <> ": " <> Text.unpack line
      | otherwise = Right keys
      where
        keys = mapMaybe (parseSecKey net) (Text.words line)
