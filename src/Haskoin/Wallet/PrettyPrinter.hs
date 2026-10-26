{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}

module Haskoin.Wallet.PrettyPrinter where

import Control.Monad
import Data.List (intercalate, intersperse)
import Data.List.Split (chunksOf)
import qualified Data.Map.Strict as Map
import Data.String.Conversions (cs)
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Time.Format
import Data.Word
import Haskoin
import Haskoin.Wallet.Account
import Haskoin.Wallet.Amounts
import Haskoin.Wallet.Coin
import Haskoin.Wallet.Commands
import Haskoin.Wallet.Database
import Haskoin.Wallet.Sync
import Haskoin.Wallet.TxInfo
import Haskoin.Wallet.Util
import Numeric.Natural (Natural)
import System.Console.ANSI
import System.Exit
import System.IO

{- The printer -}

data Printer
  = PConcat !Printer !Printer
  | PNewline !Printer
  | PNest !Natural !Printer
  | PText [SGR] !String
  | PEmpty
  | PNonEmpty

instance Semigroup Printer where
  a <> PEmpty = a
  PEmpty <> b = b
  a <> b = PConcat a b

instance Monoid Printer where
  mempty = PEmpty

isEmptyPrinter :: Printer -> Bool
isEmptyPrinter prt =
  case prt of
    PEmpty -> True
    (PText _ s) -> null s
    (PNest _ n) -> isEmptyPrinter n
    (PNewline n) -> isEmptyPrinter n
    _ -> False

text :: String -> Printer
text = PText []

(<+>) :: Printer -> Printer -> Printer
p1 <+> p2
  | isEmptyPrinter p1 = p2
  | isEmptyPrinter p2 = p1
  | otherwise = p1 <> text " " <> p2

vcat :: [Printer] -> Printer
vcat = go . filter (not . isEmptyPrinter)
  where
    go [] = PEmpty
    go [x] = x
    go (x : xs) = x <> PNewline (go xs)

hsep :: [Printer] -> Printer
hsep = go . filter (not . isEmptyPrinter)
  where
    go [] = PEmpty
    go [x] = x
    go (x : xs) = x <+> go xs

nest :: Natural -> Printer -> Printer
nest = PNest

block :: Natural -> String -> String
block n str =
  case n `safeSubtract` fromIntegral (length str) of
    Just missing -> str <> replicate (fromIntegral missing) ' '
    _ -> str

rblock :: Natural -> String -> String
rblock n str =
  case n `safeSubtract` fromIntegral (length str) of
    Just missing -> replicate (fromIntegral missing) ' ' <> str
    _ -> str

{- Rendering -}

renderIO :: Printer -> IO ()
renderIO cp = go 0 0 cp >> putStrLn ""
  where
    go :: Natural -> Natural -> Printer -> IO Natural
    go l n p
      | isEmptyPrinter p = return l
      | otherwise =
          case p of
            PConcat p1 p2 -> do
              l2 <- go l n p1
              go l2 n p2
            PNewline p1 -> do
              putStrLn ""
              putStr $ replicate (fromIntegral n) ' '
              go n n p1
            PNest i p1 -> do
              putStr $ replicate (fromIntegral i) ' '
              go (l + i) (n + i) p1
            PText xs s -> do
              printFormat xs s
              return $ l + fromIntegral (length s)
            PEmpty -> return l
            PNonEmpty -> return l

printFormat :: [SGR] -> String -> IO ()
printFormat sgr str = do
  support <- hSupportsANSI stdout
  when support $ setSGR sgr
  putStr str
  when support $ setSGR []

{- Formats -}

formatTitle :: String -> Printer
formatTitle =
  PText
    [ SetConsoleIntensity BoldIntensity,
      SetColor Foreground Vivid White
    ]

formatAccount :: String -> Printer
formatAccount =
  PText
    [ SetConsoleIntensity BoldIntensity,
      SetColor Foreground Vivid Cyan
    ]

formatFilePath :: String -> Printer
formatFilePath =
  PText
    [ SetItalicized True,
      SetColor Foreground Vivid White
    ]

formatKey :: String -> Printer
formatKey = PText [SetColor Foreground Dull White]

formatValue :: String -> Printer
formatValue = PText [SetColor Foreground Vivid White]

formatLabel :: String -> Printer
formatLabel =
  PText
    [ SetItalicized True,
      SetColor Foreground Vivid White
    ]

formatDeriv :: String -> Printer
formatDeriv =
  PText
    [ SetItalicized True,
      SetColor Foreground Vivid Yellow
    ]

formatMnemonic :: Bool -> String -> Printer
formatMnemonic split =
  PText
    [ SetConsoleIntensity BoldIntensity,
      SetColor Foreground Vivid (if split then Yellow else Cyan)
    ]

formatAddress :: Bool -> String -> Printer
formatAddress False = PText [SetColor Foreground Dull White]
formatAddress True = PText [SetColor Foreground Vivid White]

formatTxHash :: String -> Printer
formatTxHash = PText [SetColor Foreground Dull White]

formatNoSigTxHash :: String -> Printer
formatNoSigTxHash = PText [SetColor Foreground Vivid Magenta]

formatBlockHash :: String -> Printer
formatBlockHash = PText [SetColor Foreground Dull White]

formatPosAmount :: String -> Printer
formatPosAmount = PText [SetColor Foreground Vivid Green]

formatNegAmount :: String -> Printer
formatNegAmount = PText [SetColor Foreground Vivid Red]

formatZeroAmount :: String -> Printer
formatZeroAmount = PText [SetColor Foreground Vivid White]

formatTrue :: String -> Printer
formatTrue = PText [SetColor Foreground Vivid Green]

formatFalse :: String -> Printer
formatFalse = PText [SetColor Foreground Vivid Red]

-- | What the user decided, apart from what hw found: a coin they locked
formatUserLock :: String -> Printer
formatUserLock = PText [SetColor Foreground Vivid Magenta]

formatError :: String -> Printer
formatError = PText [SetColor Foreground Vivid Red]

{- Printing responses -}

mnemonicPrinter :: Bool -> [Text] -> Printer
mnemonicPrinter split ws =
  vcat $
    fmap (mconcat . fmap formatWord) $
      chunksOf 4 $
        zip ([1 ..] :: [Natural]) ws
  where
    formatWord (i, w) =
      mconcat
        [ formatKey $ block 4 $ show i <> ".",
          formatMnemonic split $ block 10 $ cs w
        ]

partsPrinter :: [[Text]] -> [Printer]
partsPrinter [] = mempty
partsPrinter xs =
  concatMap formatPart $ zip ([1 ..] :: [Natural]) xs
  where
    formatPart (i, ws) =
      [ formatTitle $ "Private Mnemonic Part #" <> show i,
        nest 2 $ mnemonicPrinter True ws
      ]

keyPrinter :: Natural -> Text -> Printer
keyPrinter n txt = formatKey (block n (cs txt)) <> text ": "

amountPrinter :: AmountUnit -> Word64 -> Printer
amountPrinter unit = integerAmountPrinter unit . fromIntegral

integerAmountPrinter :: AmountUnit -> Integer -> Printer
integerAmountPrinter unit amnt
  | amnt == 0 = integerAmountPrinterWith formatZeroAmount unit amnt
  | amnt > 0 = integerAmountPrinterWith formatPosAmount unit amnt
  | otherwise = integerAmountPrinterWith formatNegAmount unit amnt

integerAmountPrinterWith ::
  (String -> Printer) -> AmountUnit -> Integer -> Printer
integerAmountPrinterWith f unit amnt =
  f (cs $ showAmount unit amnt) <+> unitPrinter unit amnt

unitPrinter :: AmountUnit -> Integer -> Printer
unitPrinter unit = text . cs . showUnit unit

naturalPrinter :: Natural -> Printer
naturalPrinter nat
  | nat == 0 = formatZeroAmount $ show nat
  | otherwise = formatPosAmount $ show nat

-- | The accounts synced, each with the results of its sync
syncResultsPrinter :: AmountUnit -> [SyncRes] -> Printer
syncResultsPrinter unit xs = vcat $ intercalate [text " "] (f <$> xs)
  where
    f (SyncRes acc spendable bh h t c r k n) =
      [ accountPrinter unit acc spendable,
        formatTitle "Sync Results:",
        nest 2 $ keyPrinter 12 "Best Block" <> formatBlockHash (cs $ blockHashToHex bh),
        nest 2 $ keyPrinter 12 "Best Height" <> formatValue (show h),
        nest 2 $ keyPrinter 12 "Tx updates" <> formatValue (show t),
        nest 2 $ keyPrinter 12 "Coin updates" <> formatValue (show c),
        nest 2 $ keyPrinter 12 "Sent again" <> formatValue (show r),
        nest 2 $ keyPrinter 12 "Conflicted" <> formatValue (show k),
        nest 2 $ keyPrinter 12 "Discovered" <> formatValue (show n)
      ]

-- | An account with its balances: those the store reports, and the one it
-- can spend now
accountPrinter :: AmountUnit -> DBAccount -> Word64 -> Printer
accountPrinter unit acc spendable =
  vcat
    [ keyPrinter 11 "Account" <> formatAccount (cs $ dBAccountName acc),
      mconcat [keyPrinter 11 "Fingerprint", formatValue $ maybe "unknown" cs (dBAccountFingerprint acc)],
      mconcat [keyPrinter 11 "Deriv", formatDeriv deriv],
      mconcat [keyPrinter 11 "Network", formatValue net],
      mconcat
        [keyPrinter 11 "External", formatValue $ show $ dBAccountExternal acc],
      mconcat
        [keyPrinter 11 "Internal", formatValue $ show $ dBAccountInternal acc],
      mconcat [keyPrinter 11 "Created", formatValue created],
      formatKey "Balances:",
      nest 2 $
        mconcat
          [ keyPrinter 11 "Confirmed",
            amountPrinter unit $ dBAccountBalanceConfirmed acc
          ],
      nest 2 $
        mconcat
          [ keyPrinter 11 "Unconfirmed",
            amountPrinter unit $ dBAccountBalanceUnconfirmed acc
          ],
      nest 2 $
        mconcat
          [ keyPrinter 11 "Spendable",
            amountPrinter unit spendable
          ],
      nest 2 $
        mconcat
          [ keyPrinter 11 "Coins",
            naturalPrinter $ fromIntegral $ dBAccountBalanceCoins acc
          ]
    ]
  where
    deriv = pathToStr $ accountPath acc
    net = (accountNetwork acc).name
    utctime = dBAccountCreated acc
    created = formatTime defaultTimeLocale "%d %b %Y" utctime

-- | An address with what it received and the balance it can spend now
addressPrinter :: AmountUnit -> Natural -> DBAddress -> Word64 -> Printer
addressPrinter unit pad addr spendable =
  vcat
    [ mconcat
        [ keyPrinter pad $ cs $ show (dBAddressIndex addr),
          formatAddress True $ cs $ dBAddressAddress addr
        ],
      if Text.null $ dBAddressLabel addr
        then PEmpty
        else
          nest 2 $
            mconcat
              [ keyPrinter 9 "Label",
                formatLabel $ cs $ dBAddressLabel addr
              ],
      nest 2 $
        mconcat
          [ keyPrinter 9 "Txs",
            naturalPrinter $ fromIntegral $ dBAddressBalanceTxs addr
          ],
      nest 2 $
        mconcat
          [ keyPrinter 9 "Received",
            amountPrinter unit $ dBAddressBalanceReceived addr
          ],
      nest 2 $
        mconcat
          [ keyPrinter 9 "Spendable",
            amountPrinter unit spendable
          ]
    ]

txInfoPrinter ::
  Network ->
  AmountUnit ->
  TxInfo ->
  Printer
txInfoPrinter net unit (TxInfo TxSummary {..} status) =
  vcat
    [ formatTitle (block 9 title) <> text ": " <> total,
      statusPrinter status,
      fee,
      changeInFee,
      internal,
      debit,
      credit
    ]
  where
    title =
      case summaryType of
        TxDebit -> "Debit"
        TxInternal -> "Internal"
        TxCredit -> "Credit"
    total = integerAmountPrinter unit summaryAmount
    fee = keyPrinter 9 "Fee" <> feePrinter unit summaryFee summaryFeeByte
    changeInFee =
      case summaryChangeInFee of
        Just change
          | change > 0 ->
              keyPrinter 9 "Change"
                <> amountPrinter unit (fromIntegral change)
                  <+> text "too small for an output, added to the fee"
        _ -> mempty
    internal
      | summaryType /= TxInternal = mempty
      | otherwise =
          vcat $
            [formatKey "From addresses:"]
              <> ( nest 2 . addrPrinter True
                     <$> Map.assocs (Map.map g summaryMyInputs)
                 )
              <> [formatKey "To addresses:"]
              <> ( nest 2
                     . addrPrinter True
                     <$> Map.assocs (Map.map f summaryMyOutputs)
                 )
    debit
      | summaryType /= TxDebit = mempty
      | otherwise =
          vcat $
            [formatKey "Sending to addresses:"]
              <> ( nest 2 . addrPrinter False
                     <$> Map.assocs
                       ( Map.map
                           (,Nothing,"")
                           summaryOtherOutputs
                       )
                 )
    credit
      | summaryType /= TxCredit = mempty
      | otherwise =
          vcat $
            [formatKey "My credited addresses:"]
              <> ( nest 2 . addrPrinter True
                     <$> Map.assocs (Map.map f summaryMyOutputs)
                 )
    f (MyOutputs v p l) = (v, Just p, cs l)
    g (MyInputs v p l _) = (v, Just p, cs l)
    addrPrinter isMine (a, (v, pathM, label)) =
      vcat
        [ formatAddress isMine (addrToString net a)
            <> text ":"
              <+> if isMine
                then integerAmountPrinterWith formatPosAmount unit (fromIntegral v)
                else integerAmountPrinterWith formatZeroAmount unit (fromIntegral v),
          case pathM of
            Just p ->
              nest 2 $
                formatKey "Path:" <+> formatDeriv (pathToStr p)
            _ -> mempty,
          if null label
            then mempty
            else nest 2 $ formatKey "Label:" <+> formatLabel label
        ]

-- | Where the transaction stands: its id when it has one, then its
-- confirmations on the chain, or its state in preparation
statusPrinter :: TxStatus -> Printer
statusPrinter (OnChain status) =
  vcat
    [ txHashPrinter (chainTxid status),
      if chainDead status
        then
          keyPrinter 9 "Status"
            <> formatFalse "dead"
              <+> text "(replaced, or dropped from the network)"
        else mempty,
      keyPrinter 9 "Confs" <> formatValue (show (chainConfirmations status))
    ]
statusPrinter (Pending status) =
  vcat
    [ maybe mempty txHashPrinter (pendingTxid status),
      keyPrinter 9 "NoSigHash"
        <> formatNoSigTxHash (cs $ txHashToHex (pendingNosigHash status)),
      keyPrinter 9 "Signed"
        <> text "This pending transaction is"
          <+> if pendingSigned status
            then formatTrue "signed"
            else formatFalse "not signed",
      if pendingOnline status
        then
          keyPrinter 9 "Online"
            <> text "This transaction is"
              <+> formatTrue "online"
        else mempty,
      if pendingConflicted status
        then
          keyPrinter 9 "Conflict"
            <> formatFalse "A coin it spends was spent by another transaction, or is gone,"
              <+> text "and it can no longer be sent; delete it with deletetx"
        else mempty
    ]

txHashPrinter :: TxHash -> Printer
txHashPrinter tid = keyPrinter 9 "TxHash" <> formatTxHash (cs $ txHashToHex tid)

feePrinter :: AmountUnit -> Natural -> Natural -> Printer
feePrinter unit fee feeBytes =
  integerAmountPrinterWith formatZeroAmount unit (fromIntegral fee)
    <+> text "("
    <> formatValue (show feeBytes)
      <+> text "sat/bytes"
    <> text ")"

coinPrinter :: Network -> AmountUnit -> JsonCoin -> Printer
coinPrinter net unit JsonCoin {..} =
  vcat
    [ formatKey (block 7 "TxHash")
        <> text ":"
          <+> formatTxHash (cs $ txHashToHex jsonCoinOutpoint.hash),
      formatKey (block 7 "Index")
        <> text ":"
          <+> formatValue (show jsonCoinOutpoint.index),
      formatKey (block 7 "Value")
        <> text ":"
          <+> amountPrinter unit jsonCoinValue,
      formatKey (block 7 "Address")
        <> text ":"
          <+> formatAddress True (addrToString net jsonCoinAddress),
      formatKey (block 7 "Confs")
        <> text ":"
          <+> formatValue (show jsonCoinConfirmations),
      formatKey (block 7 "Status")
        <> text ":"
          <+> status
    ]
  where
    status = case jsonCoinLock of
      Just LockedByTransaction -> formatFalse "Locked by a pending transaction"
      Just LockedByUser -> formatUserLock "Locked by the user"
      Nothing
        | jsonCoinSpendable -> formatTrue "Spendable"
        | otherwise -> formatFalse "Not spendable yet"

prettyPrinter :: AmountUnit -> Response -> IO ()
prettyPrinter unit =
  \case
    ResponseError err -> do
      renderIO . mconcat $ [formatError "Error: ", text $ cs err]
      exitFailure
    ResponseMnemonic orig mnem parts ->
      renderIO . vcat $
        [ formatTitle "System Entropy Source",
          nest 2 $ formatFilePath $ cs orig,
          formatTitle "Private Mnemonic",
          nest 2 $ mnemonicPrinter False mnem
        ]
          <> partsPrinter parts
    ResponseAccount acc spendable -> renderIO $ accountPrinter unit acc spendable
    ResponseAccounts [] -> renderIO $ text "There are no accounts to display"
    ResponseAccounts accs ->
      renderIO . vcat $ intersperse (text " ") $ uncurry (accountPrinter unit) <$> accs
    ResponseAccResult _ res txt -> do
      let p =
            if res
              then formatTrue "Success"
              else formatFalse "Failure"
      renderIO . vcat $ [formatKey "Result:" <+> p, text $ cs txt]
    ResponseFile f -> renderIO $ formatKey "File:" <+> formatFilePath f
    ResponseAddress _ addr spendable -> do
      let l = length $ show $ dBAddressIndex addr
      renderIO $ addressPrinter unit (fromIntegral l) addr spendable
    ResponseAddresses _ [] ->
      renderIO $ text "There are no addresses to display"
    ResponseAddresses _ addrs -> do
      let l = length $ show $ maximum $ dBAddressIndex . fst <$> addrs
      renderIO $ vcat $ uncurry (addressPrinter unit (fromIntegral l)) <$> addrs
    ResponseTxs _ [] ->
      renderIO $ text "There are no transactions to display"
    ResponseTxs acc txs -> do
      let net = accountNetwork acc
      renderIO $ vcat $ intersperse (text " ") $ txInfoPrinter net unit <$> txs
    ResponseTx acc txInfo -> do
      let net = accountNetwork acc
      renderIO $ txInfoPrinter net unit txInfo
    ResponseSentTx net h ->
      renderIO $
        vcat
          [ formatKey "Sent:" <+> formatTxHash (cs $ txHashToHex h),
            nest 2 $ formatKey "Network:" <+> formatValue net.name
          ]
    ResponseDeleteTx h c a ->
      renderIO $
        vcat
          [ formatKey "Deleted:" <+> formatNoSigTxHash (cs $ txHashToHex h),
            nest 2 $ formatKey "Freed coins:" <+> formatValue (show c),
            nest 2 $
              formatKey "Freed internal addresses:" <+> formatValue (show a)
          ]
    ResponseCoins _ [] ->
      renderIO $ text "There are no coins to display"
    ResponseCoins acc coins -> do
      let net = accountNetwork acc
      renderIO $ vcat $ intersperse (text " ") $ coinPrinter net unit <$> coins
    ResponseSignMsg m s a -> do
      renderIO $
        vcat
          [ text "-----BEGIN BITCOIN SIGNED MESSAGE-----",
            text $ cs m,
            text "-----BEGIN SIGNATURE-----",
            text $ cs a,
            text $ cs s,
            text "-----END BITCOIN SIGNED MESSAGE-----"
          ]
    ResponseSync xs -> renderIO $ syncResultsPrinter unit xs
    ResponseRecover net gap xs
      | null xs ->
          renderIO . text $
            "No account of the seed was paid on " <> net.name <> ": the search ended after " <> show gap <> " accounts in a row without a payment"
      | otherwise -> renderIO $ syncResultsPrinter unit xs
    ResponseVersion v dbv ->
      renderIO $
        vcat
          [ formatKey "Software version:" <+> formatValue (cs v),
            formatKey "Database version:" <+> formatValue (cs dbv)
          ]
