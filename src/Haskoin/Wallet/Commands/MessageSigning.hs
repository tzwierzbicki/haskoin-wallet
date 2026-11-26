{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

-- | signmsg and verifymsg: a message signed with the key of an address of
-- the wallet, and the check of a signed message, whoever signed it
-- ("Haskoin.Wallet.Keys.MessageSigning"). verifymsg needs no wallet.
module Haskoin.Wallet.Commands.MessageSigning
  ( cmdSignMsg,
    cmdVerifyMsg,
  )
where

import Control.Monad (unless)
import Control.Monad.Except (ExceptT (..), throwError)
import Control.Monad.IO.Class (liftIO)
import qualified Data.Aeson as Json
import Data.Aeson.Types (Parser, parseEither)
import Data.Bifunctor (first)
import Data.Foldable (for_)
import Data.String.Conversions (cs)
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Text.Encoding (encodeUtf8)
import Haskoin
import Haskoin.Wallet.Cli.Command
import Haskoin.Wallet.Cli.Prompt
import Haskoin.Wallet.Cli.Response
import Haskoin.Wallet.Cli.Wrap (warn)
import Haskoin.Wallet.Commands.Contact (readingNetworks)
import Haskoin.Wallet.Commands.Run (runDBResponse, runResponse)
import Haskoin.Wallet.Config
import Haskoin.Wallet.Database.Account
import Haskoin.Wallet.Database.Address
import Haskoin.Wallet.Database.Portfolio
import Haskoin.Wallet.FileIO
import Haskoin.Wallet.Keys.MessageSigning
import Haskoin.Wallet.Util

-- | Sign a message with the key of an address of the wallet. The answer is
-- the signed message, or the --output file its armored block went to.
--
-- An address tells its portfolio, whichever is active. The wallet must
-- hold it. One it has on several networks is the one on the --network,
-- or on the default network ('addressByText').
--
-- A path, such as bip84\/0\/3, names the address the public key of the
-- account derives there, in the active portfolio, on the --network or the
-- default network, as show derives it ('derivedAddressAt'). It needs no
-- row. The offline computer never gives out an address, since receive
-- runs online, and holds none: a path is how it signs. The signed message
-- names the address as typed, or, for a path, in the form the wallet
-- stores, so that the reader sees which address signed.
cmdSignMsg :: Config -> SignMsg -> IO Response
cmdSignMsg cfg opts =
  runDBResponse cfg $ do
    -- Before the mnemonic is asked, not after
    for_ opts.output checkFileToWrite
    (portfolio, acc, path, signer) <- case opts.address of
      NamedByAddress typed -> do
        unless (readsAsAddress typed) $ throwError (notToSignWith typed)
        (portfolio, accId, row) <- addressByTextOr (notHeldToSign typed) cfg opts.network typed
        acc <- getAccountById accId
        return (portfolio, acc, addrPath row, typed)
      NamedByPath kind addrType index -> do
        portfolio <- requireActivePortfolio
        (_, acc) <- chooseAccount cfg portfolio opts.network (Just kind)
        derived <- derivedAddressAt acc addrType index
        stored <- liftEither $ addrToTextE (accountNetwork acc) derived
        return (portfolio, acc, addrDeriv addrType :/ index, stored)
    let net = accountNetwork acc
    addr <- liftEither $ textToAddrE net signer
    -- Warned before the mnemonic is asked, so that the user can stop. On
    -- stderr, to keep the armored block and the --json output clean.
    liftIO $ mapM_ warn $ signingWarnings opts.message
    (_, accountPrvKey) <- askSigningKey terminal (accountKind acc) net (portfolioIndex portfolio.portfolioRow) (accountXPubKey acc)
    let addrKey = derivePath path accountPrvKey
    signature <- encodeSignature <$> liftEither (signMessage addr addrKey.key opts.message)
    case opts.output of
      Nothing -> return $ ResponseSignMsg opts.message signature signer
      Just file -> do
        liftEitherIO $ writeTextFile file (signedMessageText signer signature opts.message)
        return $ ResponseFile file

-- | The refusal of signmsg for a word that is no path and that reads as
-- an address on no network ('readsAsAddress'). It is not said to be an
-- address the wallet lacks (@notHeldToSign@): the path that refusal
-- sends to would not help with a word that is no address.
notToSignWith :: Text -> String
notToSignWith typed =
  cs typed <> " is not an address or a path. signmsg takes the address whose key signs, or its path, such as bip84/0/3, then the message: hw signmsg ADDRESS|PATH MESSAGE"

-- | The refusal of signmsg for an address the wallet does not hold. A
-- computer holds the addresses it gave out, and the offline one gives out
-- none, so the refusal points to the path, which needs no row.
notHeldToSign :: Text -> String
notHeldToSign typed =
  "The wallet has no address "
    <> cs typed
    <> ". A computer holds only the addresses it gave out. On another one, such as the offline computer, give the path of the address instead, as addresses shows it, such as bip84/0/3"

-- | Check a signed message: whether the key of the address signed it. The
-- wallet is not needed: the address can be anyone's. hw reads the address
-- on the network it is written for, shown or hidden, so the user gives no
-- --network (@signerAddress@).
cmdVerifyMsg :: Config -> VerifyMsg -> IO Response
cmdVerifyMsg _ opts =
  runResponse $ do
    (address, signature, message) <- case opts.source of
      Given address signature message -> return (address, signature, message)
      FromFile file -> signedMessageFromFile file
    addr <- liftEither $ signerAddress opts.network address
    valid <- liftEither $ verifyMessage addr signature message
    -- A message that is hard to copy exactly may be why the signature does
    -- not check out. On stderr, to keep the --json answer clean.
    unless valid $ liftIO $ mapM_ warn $ messageWarnings message
    return $ ResponseVerifyMsg message signature address valid

-- | The address a message is said to be signed by, read on the --network
-- when it is given. Without it, the address is read on a network that
-- can read it, whether hw shows that network or hides it.
--
-- The text of an address does not always name one network
-- ('readingNetworks'). This does not matter here. Every network that
-- reads the text reads the same key hash from it, and the message is
-- hashed the same way on each network ('signedMessageHash'). So the first
-- network that reads the address gives the answer that any other would.
-- Asking for --network, as hw did before, only made the user name a
-- network that changes nothing.
--
-- A hidden network reads the address as a shown one does. The check
-- reads nothing of the wallet and answers with nothing of a network: the
-- address is anyone's, and the answer is valid or not. hw first refused
-- an address that only hidden networks read, as it refuses what the
-- wallet has on hidden networks alone. The user then had to show a
-- network of the own wallet, and hide it again, to check the proof of
-- somebody else. For the same reason the --network of verifymsg may name
-- a hidden network ('Haskoin.Wallet.Cli.Command.networkNamedBy').
signerAddress :: Maybe Network -> Text -> Either String Address
signerAddress (Just net) typed = textToAddrE net typed
signerAddress Nothing typed = case readingNetworks typed of
  (_, addr) : _ -> Right addr
  [] -> Left (notAnAddress typed "any network")

-- | The address, the signature and the message of the output of signmsg
-- saved in a file: its --json answer when the file holds JSON, else its
-- armored block
signedMessageFromFile :: FilePath -> ExceptT String IO (Text, Text, Text)
signedMessageFromFile file = do
  content <- liftEitherIO $ readTextFile file
  if "{" `Text.isPrefixOf` Text.stripStart content
    then do
      answer <- liftEither $ first (couldNotRead file) $ parseEither signMsgAnswer =<< Json.eitherDecodeStrict' (encodeUtf8 content)
      case answer of
        Just signed -> return signed
        Nothing -> throwError $ "The file " <> file <> " holds an answer of hw, but not one of signmsg"
    else liftEither $ first (couldNotRead file) $ readSignedMessageBlock content
  where
    -- The fields of the answer of signmsg alone: the address, the
    -- signature and the message. Another answer is not read further, so
    -- that a file crafted as one has nothing more to be read for.
    signMsgAnswer :: Json.Value -> Parser (Maybe (Text, Text, Text))
    signMsgAnswer = Json.withObject "an answer of hw" $ \o -> do
      kind <- o Json..: "type"
      if kind == ("signmsg" :: Text)
        then (\message signature address -> Just (address, signature, message)) <$> o Json..: "message" <*> o Json..: "signature" <*> o Json..: "address"
        else return Nothing
