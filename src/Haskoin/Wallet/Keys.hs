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
import Control.Monad (filterM)
import Crypto.KDF.PBKDF2 (Parameters (..), fastPBKDF2_SHA512)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.String.Conversions (cs)
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Text.Encoding (encodeUtf8)
import Data.Text.Normalize (NormalizationMode (NFKD), normalize)
import Haskoin
import Haskoin.Crypto.Keys.Slip39 (EncryptedMasterSecret, decryptMasterSecret)
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

{- The kinds of accounts hw does not hold -}

-- | A kind of account hw does not hold, but looks for when it recovers a
-- seed: a wallet of another kind may have paid the seed at its path, and
-- the user is told of funds hw cannot spend, rather than shown an empty
-- wallet. Such an account is never created, and hw signs for none.
-- 'AccountKind' stays the kinds hw spends from.
data ForeignKind
  = -- | BIP49: the account at m/49'/coin'/index', with P2SH-P2WPKH
    -- addresses, SegWit wrapped in a script hash, which start with 3 on
    -- Bitcoin. Hardware wallets made such accounts before native SegWit.
    Bip49
  | -- | BIP86: the account at m/86'/coin'/index', with P2TR addresses,
    -- Taproot spent by its key alone, which start with bc1p on Bitcoin
    Bip86
  | -- | BIP44 with the coin type of Bitcoin, m/44'/0'/index', on Bitcoin
    -- Cash, whose own coin type is 145: the keys that held Bitcoin at the
    -- fork of 2017 hold Bitcoin Cash as well, and the wallets of that
    -- time spent it from there
    Bip44OfBitcoin
  deriving (Eq, Ord, Show, Enum, Bounded)

-- | The name of a foreign kind, as the answers give it: bip49, bip86, or
-- bip44 for the accounts of Bitcoin, whose path tells them from hw's own
foreignKindName :: ForeignKind -> Text
foreignKindName kind = case kind of
  Bip49 -> "bip49"
  Bip86 -> "bip86"
  Bip44OfBitcoin -> "bip44"

-- | The foreign kind of a name, which the JSON of an answer carries:
-- Nothing for a name that is none
foreignKindNamed :: Text -> Maybe ForeignKind
foreignKindNamed name = lookup name [(foreignKindName kind, kind) | kind <- [minBound .. maxBound]]

-- | A foreign kind as the answers describe it: its name, and what sets it
-- apart
foreignKindDescription :: ForeignKind -> String
foreignKindDescription kind = case kind of
  Bip49 -> "bip49, wrapped SegWit"
  Bip86 -> "bip86, Taproot"
  Bip44OfBitcoin -> "bip44 with the coin type of Bitcoin"

-- | The foreign kinds a seed may have been paid at on a network: BIP49
-- and BIP86 on the networks of Bitcoin, which have SegWit, and the BIP44
-- accounts of Bitcoin on Bitcoin Cash. The test networks of Bitcoin Cash
-- have the coin type of Bitcoin's test networks, so the accounts of
-- Bitcoin are their own there, and nothing else is looked for on them.
networkForeignKinds :: Network -> [ForeignKind]
networkForeignKinds net = [kind | kind <- [minBound .. maxBound], foreignKindOnNetwork net kind]

-- | Whether a seed may have been paid at accounts of the foreign kind on
-- the network. The accounts of Bitcoin are foreign where the coin type is
-- the one of Bitcoin Cash, 145: its test networks have the coin type 1 of
-- every test network, where the accounts of Bitcoin are hw's own.
foreignKindOnNetwork :: Network -> ForeignKind -> Bool
foreignKindOnNetwork net kind = case kind of
  Bip49 -> net.segWit
  Bip86 -> net.segWit
  Bip44OfBitcoin -> net.bip44Coin == bch.bip44Coin

-- | The path of an account of a foreign kind on a network:
-- m/purpose'/coin'/index', with the coin type of the network, or that of
-- Bitcoin for its BIP44 accounts
foreignDeriv :: ForeignKind -> Network -> Natural -> HardPath
foreignDeriv kind net a = Deriv :| purpose :| coin :| fromIntegral a
  where
    (purpose, coin) = case kind of
      Bip49 -> (49, net.bip44Coin)
      Bip86 -> (86, net.bip44Coin)
      Bip44OfBitcoin -> (44, btc.bip44Coin)

-- | The address of a key of an account of a foreign kind on a network:
-- P2SH-P2WPKH for BIP49, P2TR for BIP86, and P2PKH for the BIP44 accounts
-- of Bitcoin. Nothing on a network that has no such address: a Taproot
-- output has none on a network without SegWit, where no account of BIP86
-- is looked for ('foreignKindOnNetwork').
foreignAddress :: ForeignKind -> Network -> Ctx -> XPubKey -> Maybe Address
foreignAddress kind net ctx key = case kind of
  Bip49 -> Just (xPubCompatWitnessAddr ctx key)
  Bip86 -> taprootAddress net ctx key
  Bip44OfBitcoin -> Just (xPubAddr ctx key)

-- | The P2TR address of a key on a network, as BIP 86 derives it: the
-- output key is the x-only key tweaked by the tagged hash of itself, with
-- no script tree, which haskoin-core's taprootOutputKey computes as BIP
-- 341 defines it. haskoin-core gives the output an address on a network
-- that has SegWit, and none elsewhere.
taprootAddress :: Network -> Ctx -> XPubKey -> Maybe Address
taprootAddress net ctx key =
  outputAddress net ctx (taprootScriptOutput ctx (TaprootOutput key.key Nothing))

{- The keys of an account -}

-- | A mnemonic and its passphrase
data MnemonicPass = MnemonicPass
  { mnemonicWords :: !Text,
    mnemonicPass :: !Text
  }
  deriving (Eq)

-- | The words and the passphrase are not shown. Together they give every
-- key of the seed. A value that ends up in a log, in an error message or
-- in the output of a failed test must not reveal them.
instance Show MnemonicPass where
  showsPrec d _ =
    showParen (d > 10) $
      showString "MnemonicPass {mnemonicWords = <redacted>, mnemonicPass = <redacted>}"

-- | The path of an account of a kind: m/purpose'/coin'/account', with the
-- purpose of the kind and the coin type of the network
accountDeriv :: AccountKind -> Network -> Natural -> HardPath
accountDeriv kind net a = Deriv :| fromIntegral (kindPurpose kind) :| net.bip44Coin :| fromIntegral a

-- | Whether a path below the key of an account is of one of its
-- addresses: /0/n on its receiving chain, or /1/n on its change chain.
-- An account has no address anywhere else. hw refuses to show or sign a
-- transaction that names a key of the account at any other path: funds
-- paid there would be the account's, and yet never seen by it, which
-- scans the two chains alone.
isChainPath :: SoftPath -> Bool
isChainPath path = case pathToList path of
  [chain, _] -> chain == 0 || chain == 1
  _ -> False

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

{- The secret of a seed -}

-- | The secret of a seed as the user holds it, with its passphrase: the
-- mnemonic of BIP39, or the backup that the shares of SLIP-39 rebuild,
-- which the passphrase decrypts.
data SeedSecret
  = MnemonicSecret !MnemonicPass
  | SharesSecret !EncryptedMasterSecret !Text
  deriving (Eq)

-- | Nothing secret is shown. The mnemonic, the bytes of the backup and the
-- passphrase are left out. The kind of secret and the identifier of a
-- backup stay, since neither is secret.
instance Show SeedSecret where
  showsPrec d secret =
    showParen (d > 10) $ case secret of
      MnemonicSecret mnem -> showString "MnemonicSecret " . showsPrec 11 mnem
      SharesSecret backup _ -> showString "SharesSecret " . showsPrec 11 backup . showString " <redacted>"

-- | The master key of a seed, from its secret. A mnemonic gives the one of
-- BIP39 ('masterKey'). A backup in shares gives the one of SLIP-39: the
-- backup decrypted with the passphrase is the seed itself, which BIP32
-- derives the master key from, with no mnemonic in between. Any passphrase
-- decrypts it to a seed, so a wrong one is not told here.
--
-- The passphrase of a backup has printable ASCII characters only, as its
-- format asks: haskoin-core refuses any other, and hw with it. The prompt
-- tells the user before ('Haskoin.Wallet.Cli.Prompt.passphraseOf').
secretMasterKey :: SeedSecret -> Either String XPrvKey
secretMasterKey (MnemonicSecret mnem) = masterKey mnem
secretMasterKey (SharesSecret backup passphrase) =
  backupSeedMasterKey =<< decryptMasterSecret passphrase backup

-- | The master key of the seed a backup in shares holds. BIP32 takes a
-- seed of 16 to 64 bytes, and haskoin-core's makeXPrvKey throws for any
-- other length. The format of the shares allows a longer secret, which is
-- then no seed: it is refused here, with its length.
backupSeedMasterKey :: ByteString -> Either String XPrvKey
backupSeedMasterKey seed
  | bytes < 16 || bytes > 64 = Left ("The backup holds a secret of " <> show bytes <> " bytes, which is not a seed: a seed has 16 to 64 bytes")
  | otherwise = Right (makeXPrvKey seed)
  where
    bytes = BS.length seed

-- | The master key hw derived before it normalized passphrases
-- ('typedMasterKey'): Nothing when normalizing leaves the passphrase as it
-- is, and for a backup in shares, which hw never derived otherwise
secretTypedMasterKey :: SeedSecret -> Either String (Maybe XPrvKey)
secretTypedMasterKey (MnemonicSecret mnem)
  | normalizePassphrase (mnemonicPass mnem) == mnemonicPass mnem = Right Nothing
  | otherwise = Just <$> typedMasterKey mnem
secretTypedMasterKey (SharesSecret _ _) = Right Nothing

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
accountPubKey kind net ctx master index = pubKeyAtPath ctx master (accountDeriv kind net index)

-- | The public key at a hardened path under a master key, or Nothing when
-- a child key on the way is invalid
pubKeyAtPath :: Ctx -> XPrvKey -> HardPath -> IO (Maybe XPubKey)
pubKeyAtPath ctx master path = do
  derived <- try $ evaluate $ deriveXPubKey ctx $ derivePath ctx path master
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
-- fingerprint, and the public key of any of its accounts, of either kind
-- hw holds or of a foreign kind, derived on demand, Nothing at an index
-- whose child key is invalid. The account steps are hardened, so these
-- public keys can only come from the master private key, which stays
-- inside this record and never comes out of it. The private key of an
-- account reaches a command in one way only: as the signing key that
-- "Haskoin.Wallet.Cli.Prompt" asks for.
data PublicSeed = PublicSeed
  { seedFingerprint :: !Fingerprint,
    seedAccountKey :: AccountKind -> Network -> Natural -> IO (Maybe XPubKey),
    -- | The public key of an account of a foreign kind, which recover
    -- looks for and hw never holds
    seedForeignKey :: ForeignKind -> Network -> Natural -> IO (Maybe XPubKey)
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

-- | The public side of a seed, from its secret
secretSeed :: Ctx -> SeedSecret -> Either String PublicSeed
secretSeed ctx secret = seedOfMaster ctx <$> secretMasterKey secret

-- | The public side of the seed hw derived before it normalized the
-- passphrase, when the secret has one: see 'secretTypedMasterKey'
secretTypedSeed :: Ctx -> SeedSecret -> Either String (Maybe PublicSeed)
secretTypedSeed ctx secret = fmap (seedOfMaster ctx) <$> secretTypedMasterKey secret

-- | The public side of a seed, from its master key
seedOfMaster :: Ctx -> XPrvKey -> PublicSeed
seedOfMaster ctx master =
  PublicSeed
    { seedFingerprint = masterFingerprint ctx (deriveXPubKey ctx master),
      seedAccountKey = \kind net index -> accountPubKey kind net ctx master index,
      seedForeignKey = \kind net index -> pubKeyAtPath ctx master (foreignDeriv kind net index)
    }

-- | The accounts of a seed that have a key, of any kind hw holds on any
-- network, at the index the key carries: none when the seed does not have
-- it there. An account imported from its key alone, which says nothing of
-- its path, may have been given the wrong kind or network. This tells
-- which it has.
accountsWithKey :: PublicSeed -> XPubKey -> IO [(AccountKind, Network)]
accountsWithKey seed key =
  filterM (\(kind, net) -> (== Just key) <$> seedAccountKey seed kind net (xPubIndex key)) [(kind, net) | net <- allNets, kind <- networkKinds net]

-- | The accounts of a foreign kind of a seed that have a key, as
-- 'accountsWithKey' finds those of the kinds hw holds
foreignAccountsWithKey :: PublicSeed -> XPubKey -> IO [(ForeignKind, Network)]
foreignAccountsWithKey seed key =
  filterM (\(kind, net) -> (== Just key) <$> seedForeignKey seed kind net (xPubIndex key)) [(kind, net) | net <- allNets, kind <- networkForeignKinds net]

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
    hex = decodeSecKey =<< decodeHex word

-- | The private keys of a text, as a key file or a typed line holds them:
-- every word that is a key, on any line, as 'wordsOfLines' reads them
parseSecKeys :: Network -> Text -> Either String [SecKey]
parseSecKeys net = wordsOfLines "private key" (parseSecKey net)
