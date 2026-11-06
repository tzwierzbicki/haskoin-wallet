{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

-- | QR codes as a terminal draws them. A code is a square of dark and
-- light modules. A character cell is about twice as tall as it is wide, so
-- one cell draws two modules, one above the other, with a half block. A
-- module is then close to a square, and a whole code fits on a screen.
module Haskoin.Wallet.Cli.QrCode
  ( paymentUri,
    qrRows,
  )
where

import Codec.QRCode (ErrorLevel (L), TextEncoding (Iso8859_1OrUtf8WithoutECI), defaultQRCodeOptions, encodeAutomatic, toMatrix)
import Data.Maybe (isJust)
import Data.Text (Text)
import qualified Data.Text as Text
import Haskoin (Network (..))

{- What a code holds -}

-- | The payment URI of an address of the network, which its QR code holds.
-- A URI names its coin, so a wallet of another coin does not take the
-- address for one of its own, and a phone opens a wallet from it. It holds
-- nothing but the address: a label is private to the wallet.
--
-- BIP 173 asks for uppercase inside QR codes, and the cash address format
-- allows it, since uppercase takes the smaller alphanumeric mode. A Base58
-- address is case sensitive and stays as it is. A cash address is a URI as
-- it stands: its prefix is its scheme.
paymentUri :: Network -> Text -> Text
paymentUri net address
  | isJust net.cashAddrPrefix = Text.toUpper address
  | isBech32 = Text.toUpper bitcoinUri
  | otherwise = bitcoinUri
  where
    bitcoinUri = "bitcoin:" <> address
    isBech32 = maybe False (\prefix -> (prefix <> "1") `Text.isPrefixOf` address) net.bech32Prefix

{- Drawing a code -}

-- | The lines of characters that draw the QR code of the text, its quiet
-- zone included. There are none when the text is too long for a QR code.
--
-- A half block draws in the colour of the text, so a line reads as dark
-- modules on a light background only in the colours of the 'QrCode' style.
qrRows :: Text -> Maybe [Text]
qrRows payload = drawn . withQuietZone . toMatrix True False <$> encodeAutomatic options Iso8859_1OrUtf8WithoutECI payload
  where
    -- The lowest level of error correction: a screen has no stains or
    -- creases. The library raises it as far as the code keeps its size.
    options = defaultQRCodeOptions L

-- | The light margin the standard asks for around a code, in modules. A
-- scanner finds the code by it.
quietZone :: Int
quietZone = 4

-- | The modules of a code, True for a dark one, inside their quiet zone
withQuietZone :: [[Bool]] -> [[Bool]]
withQuietZone modules = edge <> (padded <$> modules) <> edge
  where
    padded row = margin <> row <> margin
    margin = replicate quietZone False
    edge = replicate quietZone (padded (False <$ concat (take 1 modules)))

-- | Modules drawn two rows to a line. A last row alone has a light row
-- under it.
drawn :: [[Bool]] -> [Text]
drawn (upper : lower : rest) = Text.pack (zipWith halfBlock upper lower) : drawn rest
drawn [upper] = [Text.pack (zipWith halfBlock upper (False <$ upper))]
drawn [] = []

-- | The character of two modules, the upper one first
halfBlock :: Bool -> Bool -> Char
halfBlock True True = '█'
halfBlock True False = '▀'
halfBlock False True = '▄'
halfBlock False False = ' '
