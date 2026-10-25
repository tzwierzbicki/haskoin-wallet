{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

-- | The keys of a seed: from its mnemonic and passphrase to the master key,
-- to the signing key of an account along its BIP44 path; the public side of
-- a seed, its fingerprint and the public keys of its accounts, which is all
-- a command keeps of it outside signing; and the private keys of a sweep,
-- as users write them and where they come from.
module Haskoin.Wallet.Keys where

import Control.Applicative ((<|>))
import Control.Exception (evaluate, try)
import Data.Maybe (mapMaybe)
import Data.String.Conversions (cs)
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Text.Normalize (NormalizationMode (NFKD), normalize)
import Haskoin
import Numeric.Natural (Natural)

{- The keys of an account -}

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

-- | The master key of a seed: the root of its accounts on every network.
-- The passphrase is normalized to NFKD, as BIP39 asks, so that it gives the
-- same seed whatever form its accented letters were typed in, and the seed
-- other BIP39 wallets derive.
masterKey :: MnemonicPass -> Either String XPrvKey
masterKey mnem = typedMasterKey mnem {mnemonicPass = normalizePassphrase (mnemonicPass mnem)}

-- | A passphrase as BIP39 hashes it: in the NFKD normal form, an accented
-- letter as its letter and its accent
normalizePassphrase :: Text -> Text
normalizePassphrase = normalize NFKD

-- | The master key from the passphrase as typed, not normalized: how hw
-- derived seeds before it followed BIP39 there, which differs for a
-- passphrase with accents. Only the check of an existing account tries it,
-- for the accounts hw made then.
typedMasterKey :: MnemonicPass -> Either String XPrvKey
typedMasterKey mnem = makeXPrvKey <$> mnemonicToSeed (mnemonicPass mnem) (mnemonicWords mnem)

-- | The signing key of an account under a master key: the key at its BIP44
-- path
accountKey :: Network -> Ctx -> XPrvKey -> Natural -> XPrvKey
accountKey net ctx master acc = derivePath ctx (bip44Deriv net acc) master

-- | The signing key of an account, from the mnemonic of its seed
signingKey :: Network -> Ctx -> MnemonicPass -> Natural -> Either String XPrvKey
signingKey net ctx mnem acc = (\master -> accountKey net ctx master acc) <$> masterKey mnem

-- | The public key of an account of a seed, or Nothing when the child key
-- at that index is invalid: haskoin-core throws for such a key, where BIP32
-- says to skip the index, as "Haskoin.Wallet.Address" does for an address
accountPubKey :: Network -> Ctx -> XPrvKey -> Natural -> IO (Maybe XPubKey)
accountPubKey net ctx master index = do
  derived <- try $ evaluate $ deriveXPubKey ctx $ accountKey net ctx master index
  return $ case derived of
    Left (DerivationException _) -> Nothing
    Right pub -> Just pub

-- | The fingerprint of a seed: the first four bytes of the hash of its
-- master public key, which PSBTs and output descriptors write before a key
-- of the seed, as in [bc560f95/44'/0'/0']. The same on every network, and
-- public: it is computed from the public key, never from the private one.
masterFingerprint :: Ctx -> XPubKey -> Fingerprint
masterFingerprint = xPubFP

{- The public side of a seed -}

-- | What a command keeps of a seed once it has the mnemonic: the
-- fingerprint, and the public key of any of its accounts, derived on
-- demand, Nothing at an index whose child key is invalid. The account
-- steps of BIP44 are hardened, so these public keys can only come from the
-- master private key, which stays inside this record and never comes out
-- of it. The private key of an account reaches a command in one way only:
-- as the signing key that "Haskoin.Wallet.Prompt" asks for.
data PublicSeed = PublicSeed
  { seedFingerprint :: !Fingerprint,
    seedAccountKey :: Network -> Natural -> IO (Maybe XPubKey)
  }

-- | The public side of a seed, from its mnemonic
seedOf :: Ctx -> MnemonicPass -> Either String PublicSeed
seedOf ctx mnem = seedOfMaster ctx <$> masterKey mnem

-- | The public side of a seed, from its master key
seedOfMaster :: Ctx -> XPrvKey -> PublicSeed
seedOfMaster ctx master =
  PublicSeed
    { seedFingerprint = masterFingerprint ctx (deriveXPubKey ctx master),
      seedAccountKey = \net index -> accountPubKey net ctx master index
    }

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
