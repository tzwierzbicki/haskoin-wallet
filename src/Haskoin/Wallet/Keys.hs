{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

-- | The keys of a seed: from its mnemonic and passphrase to the master key,
-- to the signing key of an account along the path of its kind, BIP44 or
-- BIP84; the public side of a seed, its fingerprint and the public keys of
-- its accounts, which is all a command keeps of it outside signing; and
-- the private keys of a sweep, as users write them and where they come
-- from.
module Haskoin.Wallet.Keys where

import Control.Applicative ((<|>))
import Control.Exception (evaluate, try)
import Crypto.KDF.PBKDF2 (Parameters (..), fastPBKDF2_SHA512)
import Data.String.Conversions (cs)
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Text.Encoding (encodeUtf8)
import Data.Text.Normalize (NormalizationMode (NFKD), normalize)
import Haskoin
import Haskoin.Wallet.Util (wordsOfLines)
import Numeric.Natural (Natural)

{- The kinds of accounts -}

-- | The kind of an account: the standard it follows, which sets the first
-- step of its path, the purpose of BIP43, and the kind of its addresses.
-- The two kinds share everything else: the coin type of the network, the
-- index, and the two chains of addresses below the account key.
data AccountKind
  = -- | BIP44: the account at m/44'/coin'/index', with legacy P2PKH
    -- addresses, which start with 1 on Bitcoin
    Bip44
  | -- | BIP84: the account at m/84'/coin'/index', with native SegWit
    -- P2WPKH addresses, which start with bc1q on Bitcoin and pay lower
    -- fees. Bitcoin Cash has no SegWit, so it has no such account.
    Bip84
  deriving (Eq, Ord, Show, Enum, Bounded)

-- | The purpose of a kind, the first step of the path of its accounts: 44
-- or 84, as the BIP of the kind is numbered
kindPurpose :: AccountKind -> Natural
kindPurpose Bip44 = 44
kindPurpose Bip84 = 84

-- | The kind of a purpose, for the two hw holds
kindOfPurpose :: Natural -> Maybe AccountKind
kindOfPurpose purpose = lookup purpose [(kindPurpose kind, kind) | kind <- [minBound .. maxBound]]

-- | The name of a kind, as the command line and the answers give it: bip44
-- or bip84
kindName :: AccountKind -> Text
kindName kind = "bip" <> cs (show (kindPurpose kind))

-- | The kind of a name, in any case: Nothing for a name that is none
kindNamed :: Text -> Maybe AccountKind
kindNamed name = lookup (Text.toLower name) [(kindName kind, kind) | kind <- [minBound .. maxBound]]

-- | The names of the kinds, as the help lists them
kindNames :: [Text]
kindNames = kindName <$> [minBound .. maxBound]

-- | The kind of coin an account spends, as the signer names it: P2PKH for
-- BIP44, P2WPKH for BIP84
kindCoinName :: AccountKind -> String
kindCoinName Bip44 = "P2PKH"
kindCoinName Bip84 = "P2WPKH"

-- | The kinds of accounts a network has: both on the networks of Bitcoin,
-- BIP44 alone on those of Bitcoin Cash, which has no SegWit
networkKinds :: Network -> [AccountKind]
networkKinds net = [kind | kind <- [minBound .. maxBound], kindOnNetwork net kind]

-- | Whether a network has accounts of the kind
kindOnNetwork :: Network -> AccountKind -> Bool
kindOnNetwork net kind = case kind of
  Bip44 -> True
  Bip84 -> net.segWit

-- | Refuse a kind of account the network does not have
checkKindOnNetwork :: Network -> AccountKind -> Either String ()
checkKindOnNetwork net kind
  | kindOnNetwork net kind = Right ()
  | otherwise = Left $ "A " <> cs (kindName kind) <> " account needs SegWit, which " <> net.name <> " does not have"

-- | The address of a key of an account of the kind: the P2PKH address of
-- the key for BIP44, its P2WPKH address for BIP84. The keys of an
-- extended key are compressed, so both addresses exist.
keyAddress :: AccountKind -> Ctx -> XPubKey -> Address
keyAddress Bip44 = xPubAddr
keyAddress Bip84 = xPubWitnessAddr

-- | The output script that pays a key of an account of the kind, the one
-- of its address: P2PKH for BIP44, P2WPKH for BIP84
keyScript :: AccountKind -> Ctx -> PublicKey -> ScriptOutput
keyScript kind ctx public = case kind of
  Bip44 -> PayPKHash hash
  Bip84 -> PayWitnessPKHash hash
  where
    hash = addressHash (marshal ctx public)

-- | Whether an output script is of a coin an account of the kind spends:
-- the script of one of its addresses
spendsScript :: AccountKind -> ScriptOutput -> Bool
spendsScript kind script = case (kind, script) of
  (Bip44, PayPKHash _) -> True
  (Bip84, PayWitnessPKHash _) -> True
  _ -> False

{- The keys of an account -}

data MnemonicPass = MnemonicPass
  { mnemonicWords :: !Text,
    mnemonicPass :: !Text
  }
  deriving (Eq, Show)

-- | The path of an account of a kind: m/purpose'/coin'/account', with the
-- purpose of the kind and the coin type of the network
accountDeriv :: AccountKind -> Network -> Natural -> HardPath
accountDeriv kind net a = Deriv :| fromIntegral (kindPurpose kind) :| net.bip44Coin :| fromIntegral a

-- | The index of an account from its public key: the last step of its path
xPubIndex :: XPubKey -> Natural
xPubIndex = fromIntegral . xPubChild

-- | The master key of a seed: the root of its accounts on every network.
-- The sentence and the passphrase are normalized to NFKD, as BIP39 asks,
-- so that the seed is the same whatever form accented letters were typed
-- in, and the one other BIP39 wallets derive.
masterKey :: MnemonicPass -> Either String XPrvKey
masterKey mnem = makeXPrvKey <$> mnemonicToSeed (mnemonicPass mnem) (mnemonicWords mnem)

-- | A passphrase as BIP39 hashes it: in the NFKD normal form, an accented
-- letter as its letter and its accent
normalizePassphrase :: Text -> Text
normalizePassphrase = normalize NFKD

-- | The master key from the passphrase as typed, not normalized: how hw
-- derived seeds before it followed BIP39 there, which differs for a
-- passphrase with accents. Only the check of an existing account tries it,
-- for the accounts hw made then. haskoin-core normalizes the passphrase
-- itself since its version 1.5.0, so the seed is derived here as it was
-- then: the PBKDF2 of BIP39, 2048 rounds of HMAC-SHA512, of the sentence
-- the entropy of the words gives back, with the salt "mnemonic" followed by
-- the passphrase as typed.
typedMasterKey :: MnemonicPass -> Either String XPrvKey
typedMasterKey mnem = do
  entropy <- fromMnemonic (mnemonicWords mnem)
  sentence <- toMnemonic entropy
  return $ makeXPrvKey $ fastPBKDF2_SHA512 (Parameters 2048 64) (encodeUtf8 sentence) ("mnemonic" <> encodeUtf8 (mnemonicPass mnem))

-- | The signing key of an account under a master key: the key at the path
-- of its kind
accountKey :: AccountKind -> Network -> Ctx -> XPrvKey -> Natural -> XPrvKey
accountKey kind net ctx master acc = derivePath ctx (accountDeriv kind net acc) master

-- | The signing key of an account, from the mnemonic of its seed
signingKey :: AccountKind -> Network -> Ctx -> MnemonicPass -> Natural -> Either String XPrvKey
signingKey kind net ctx mnem acc = (\master -> accountKey kind net ctx master acc) <$> masterKey mnem

-- | The public key of an account of a seed, or Nothing when the child key
-- at that index is invalid: haskoin-core throws for such a key, where BIP32
-- says to skip the index, as "Haskoin.Wallet.Database.Address" does for an address
accountPubKey :: AccountKind -> Network -> Ctx -> XPrvKey -> Natural -> IO (Maybe XPubKey)
accountPubKey kind net ctx master index = do
  derived <- try $ evaluate $ deriveXPubKey ctx $ accountKey kind net ctx master index
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
-- fingerprint, and the public key of any of its accounts, of either kind,
-- derived on demand, Nothing at an index whose child key is invalid. The
-- account steps are hardened, so these public keys can only come from the
-- master private key, which stays inside this record and never comes out
-- of it. The private key of an account reaches a command in one way only:
-- as the signing key that "Haskoin.Wallet.Cli.Prompt" asks for.
data PublicSeed = PublicSeed
  { seedFingerprint :: !Fingerprint,
    seedAccountKey :: AccountKind -> Network -> Natural -> IO (Maybe XPubKey)
  }

-- | The public side of a seed, from its mnemonic
seedOf :: Ctx -> MnemonicPass -> Either String PublicSeed
seedOf ctx mnem = seedOfMaster ctx <$> masterKey mnem

-- | The public side of the seed hw derived before it normalized the
-- passphrase, from the passphrase as typed ('typedMasterKey'): Nothing
-- when normalizing leaves the passphrase as it is, since that seed is then
-- the one of 'seedOf'
typedSeedOf :: Ctx -> MnemonicPass -> Either String (Maybe PublicSeed)
typedSeedOf ctx mnem
  | normalizePassphrase (mnemonicPass mnem) == mnemonicPass mnem = Right Nothing
  | otherwise = Just . seedOfMaster ctx <$> typedMasterKey mnem

-- | The public side of a seed, from its master key
seedOfMaster :: Ctx -> XPrvKey -> PublicSeed
seedOfMaster ctx master =
  PublicSeed
    { seedFingerprint = masterFingerprint ctx (deriveXPubKey ctx master),
      seedAccountKey = \kind net index -> accountPubKey kind net ctx master index
    }

{- Private keys of a sweep -}

-- | Where the private keys of a sweep come from
data KeySource
  = -- | A file with one or more keys per line
    KeysInFile !FilePath
  | -- | Typed on the terminal without echo, asked for with --key-file -
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
-- every word that is a key, on any line, as 'wordsOfLines' reads them
parseSecKeys :: Network -> Text -> Either String [SecKey]
parseSecKeys net = wordsOfLines "private key" (parseSecKey net)
