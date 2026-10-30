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

import Control.Applicative ((<|>))
import Control.Monad
import Data.Char (isAlphaNum, toUpper)
import Data.List (intercalate, intersperse)
import Data.List.Split (chunksOf)
import qualified Data.Map.Strict as Map
import Data.Maybe (isJust)
import Data.String.Conversions (cs)
import Data.Text (Text)
import qualified Data.Text as Text
import Haskoin.Wallet.FeeRates (FeeRates (..), levelName, rateAt)
import Haskoin.Wallet.MessageSigning (signedMessageBlock)
import Data.Time (UTCTime, diffUTCTime, getCurrentTime)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import Data.Time.Format
import Data.Word
import Haskoin
import Haskoin.Wallet.Account
import Haskoin.Wallet.Address (CoinBalance (..), addrPath, coinBalanceUnspendable, pathText)
import Haskoin.Wallet.Amounts
import Haskoin.Wallet.Coin
import Haskoin.Wallet.Database
import Haskoin.Wallet.Keys (KeySource (..))
import Haskoin.Wallet.Parser (BumpFee (..), Command (..), PrepareSendAll (..), PrepareSweep (..), PrepareTx (..), Signing (..))
import Haskoin.Wallet.Response
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

-- | Show the printer on stdout
renderIO :: Printer -> IO ()
renderIO = renderOn stdout

-- | Show the printer on the handle: stdout for an answer, stderr for what
-- a command shows before it asks the user
renderOn :: Handle -> Printer -> IO ()
renderOn h cp = go 0 0 cp >> hPutStrLn h ""
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
              hPutStrLn h ""
              hPutStr h $ replicate (fromIntegral n) ' '
              go n n p1
            PNest i p1 -> do
              hPutStr h $ replicate (fromIntegral i) ' '
              go (l + i) (n + i) p1
            PText xs s -> do
              printFormat h xs s
              return $ l + fromIntegral (length s)
            PEmpty -> return l
            PNonEmpty -> return l

printFormat :: Handle -> [SGR] -> String -> IO ()
printFormat h sgr str = do
  support <- hSupportsANSI h
  when support $ hSetSGR h sgr
  hPutStr h str
  when support $ hSetSGR h []

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

-- | A time as the listings show it, to the second, in UTC
timeText :: UTCTime -> String
timeText = formatTime defaultTimeLocale "%d %b %Y %H:%M:%S UTC"

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

-- | An address after its derivation path, which tells receiving and change
-- addresses apart, with its label and the given details below it. The path
-- is padded to the given width, that of the longest path listed
-- ('pathWidth'). Each detail is a name and a value, the names padded to the
-- longest one.
addressLines :: Natural -> DBAddress -> [(Text, Printer)] -> Printer
addressLines pad addr details =
  vcat $
    (keyPrinter pad (pathText (addrPath addr)) <> formatAddress True (cs $ dBAddressAddress addr))
      : [nest 2 (keyPrinter width name <> value) | (name, value) <- labelled <> details]
  where
    label = dBAddressLabel addr
    labelled = [("Label", formatLabel (cs label)) | not (Text.null label)]
    width = fromIntegral $ maximum (Text.length . fst <$> ("Label", PEmpty) : details)

-- | The width of the longest derivation path of the addresses listed
pathWidth :: [DBAddress] -> Natural
pathWidth addrs = fromIntegral $ maximum (0 : (Text.length . pathText . addrPath <$> addrs))

-- | An address as addrs shows it: its transactions, what it received, and
-- the balance it can spend now
addressPrinter :: AmountUnit -> Natural -> DBAddress -> Word64 -> Printer
addressPrinter unit pad addr spendable =
  addressLines
    pad
    addr
    [ ("Txs", naturalPrinter $ fromIntegral $ dBAddressBalanceTxs addr),
      ("Received", amountPrinter unit $ dBAddressBalanceReceived addr),
      ("Spendable", amountPrinter unit spendable)
    ]

-- | An address as balances shows it: the balance of its coins
coinBalancePrinter :: AmountUnit -> Natural -> DBAddress -> CoinBalance -> Printer
coinBalancePrinter unit pad addr balance =
  addressLines
    pad
    addr
    [ ("Total", amountPrinter unit $ coinBalanceTotal balance),
      ("Spendable", amountPrinter unit $ coinBalanceSpendable balance),
      ("Unspendable", amountPrinter unit $ coinBalanceUnspendable balance)
    ]

-- | Addresses as balances lists them, their paths aligned. preparetx
-- --changeback shows the addresses it can send the change back to this way.
coinBalancesPrinter :: AmountUnit -> [(DBAddress, CoinBalance)] -> Printer
coinBalancesPrinter unit addrs = vcat $ uncurry (coinBalancePrinter unit (pathWidth (fst <$> addrs))) <$> addrs

-- | A transaction of a listing, on the network of the account listed
txInfoPrinter :: Network -> AmountUnit -> TxInfo -> Printer
txInfoPrinter = transactionPrinter False

-- | The one transaction a command answers with, naming its network. The
-- user sees which coins it moves: private keys, coins and the legacy
-- addresses of Bitcoin Cash read the same on Bitcoin, and a sweep outside
-- the wallet is on the network of its file.
txAnswerPrinter :: Network -> AmountUnit -> TxInfo -> Printer
txAnswerPrinter = transactionPrinter True

-- | A transaction, with its network when the first argument says so. Every
-- transaction shows the same sections, each when it has addresses: the
-- addresses it spends from, those it sends to, and for a payment, the
-- change it sends back to the wallet. The addresses of the wallet show
-- their derivation paths and labels. An address another wallet spends
-- from shows no amount: that wallet's coins are nothing to this one, and
-- the change it keeps is left out.
transactionPrinter ::
  Bool ->
  Network ->
  AmountUnit ->
  TxInfo ->
  Printer
transactionPrinter showNetwork net unit (TxInfo TxSummary {..} status) =
  vcat
    [ formatTitle (block 9 title) <> text ": " <> total,
      network,
      statusPrinter status,
      fee,
      changeInFee,
      section "Spent from:" spentFrom,
      section "Sending to:" sendingTo,
      section "Change to:" changeTo
    ]
  where
    title =
      case summaryType of
        TxDebit -> "Debit"
        TxInternal -> "Internal"
        TxCredit -> "Credit"
    total = integerAmountPrinter unit summaryAmount
    network
      | showNetwork = keyPrinter 9 "Network" <> formatValue net.name
      | otherwise = mempty
    fee = keyPrinter 9 "Fee" <> feePrinter unit summaryFee summaryFeeByte
    changeInFee =
      case summaryChangeInFee of
        Just change
          | change > 0 ->
              keyPrinter 9 "Change"
                <> amountPrinter unit (fromIntegral change)
                  <+> text "too small for an output, added to the fee"
        _ -> mempty
    -- Every input, those of the wallet first
    spentFrom =
      [walletAddress formatZeroAmount a v p (cs l) | (a, MyInputs v p l _) <- Map.assocs summaryMyInputs]
        <> [formatAddress False (addrToString net a) | a <- Map.keys summaryOtherInputs]
    toWallet = [walletAddress formatPosAmount a v p (cs l) | (a, MyOutputs v p l) <- Map.assocs summaryMyOutputs]
    toOthers = [otherAddress a v | (a, v) <- Map.assocs summaryOtherOutputs]
    -- What a payment sends back to the wallet is its change. In a payment
    -- the wallet receives, the outputs to others are the change of the
    -- payer, left out. Between the addresses of the wallet, no output can
    -- be told to be the change.
    (sendingTo, changeTo) =
      case summaryType of
        TxDebit -> (toOthers, toWallet)
        TxCredit -> (toWallet, [])
        TxInternal -> (toWallet <> toOthers, [])
    section heading rows
      | null rows = mempty
      | otherwise = vcat (formatKey heading : (nest 2 <$> rows))
    -- An address of the wallet and its value, with its path and its label
    walletAddress amountFormat a v p label =
      vcat
        [ formatAddress True (addrToString net a) <> text ":" <+> integerAmountPrinterWith amountFormat unit (fromIntegral v),
          nest 2 $ formatKey "Path:" <+> formatDeriv (pathToStr p),
          if null label
            then mempty
            else nest 2 $ formatKey "Label:" <+> formatLabel label
        ]
    -- An address of another wallet and its value
    otherAddress a v =
      formatAddress False (addrToString net a) <> text ":" <+> integerAmountPrinterWith formatZeroAmount unit (fromIntegral v)

-- | Where the transaction stands: its id when it has one, then its
-- confirmations on the chain, or its state in preparation
statusPrinter :: TxStatus -> Printer
statusPrinter (OnChain status) =
  vcat
    [ txHashPrinter (chainTxid status),
      -- In UTC: the time the store first saw the transaction, in its mempool
      -- or in a block; none for a summary the next sync has to fetch again
      case chainTime status of
        Just time -> keyPrinter 9 "Time" <> formatValue (timeText time)
        Nothing -> mempty,
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
      case pendingCreated status of
        Just time -> keyPrinter 9 "Created" <> formatValue (timeText time)
        Nothing -> mempty,
      vcat [keyPrinter 9 "Replaces" <> formatTxHash (cs $ txHashToHex replaced) | replaced <- pendingReplaces status],
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

-- | The coins a transaction just prepared leaves in the account, one a
-- line, each with why: the coin as --coin and lockcoin take it, and its
-- value
leftCoinsPrinter :: AmountUnit -> [LeftCoin] -> Printer
leftCoinsPrinter unit left =
  vcat $ formatTitle "Staying in the account:" : (nest 2 . leftCoinPrinter <$> left)
  where
    leftCoinPrinter coin =
      formatTxHash (coinText coin.leftCoinOutpoint)
        <+> amountPrinter unit coin.leftCoinValue
        <> text ","
          <+> text (leftReasonText coin.leftCoinReason)

coinPrinter :: Network -> AmountUnit -> JsonCoin -> Printer
coinPrinter net unit JsonCoin {..} =
  vcat
    [ -- As --coin, lockcoin and unlockcoin take it
      formatKey (block 7 "Coin")
        <> text ":"
          <+> formatTxHash (coinText jsonCoinOutpoint),
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
prettyPrinter unit res = do
  responsePrinter unit res
  when (responseFailed res) exitFailure

responsePrinter :: AmountUnit -> Response -> IO ()
responsePrinter unit =
  \case
    ResponseError err ->
      renderIO . mconcat $ [formatError "Error: ", text $ cs err]
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
    ResponseAddress _ addr spendable ->
      renderIO $ addressPrinter unit (pathWidth [addr]) addr spendable
    ResponseAddresses _ [] ->
      renderIO $ text "There are no addresses to display"
    ResponseAddresses _ addrs ->
      renderIO $ vcat $ uncurry (addressPrinter unit (pathWidth (fst <$> addrs))) <$> addrs
    ResponseBalances _ [] ->
      renderIO $ text "There are no addresses with coins to display"
    ResponseBalances _ addrs -> renderIO $ coinBalancesPrinter unit addrs
    ResponseTxs _ [] ->
      renderIO $ text "There are no transactions to display"
    ResponseTxs acc txs -> do
      let net = accountNetwork acc
      renderIO $ vcat $ intersperse (text " ") $ txInfoPrinter net unit <$> txs
    ResponsePendingTxs asked [] ->
      renderIO . text $ case asked of
        Just acc -> "There are no pending transactions in the account " <> cs (dBAccountName acc)
        Nothing -> "There are no pending transactions"
    -- Listing every account, each transaction names its own
    ResponsePendingTxs asked held ->
      renderIO $ vcat $ intersperse (text " ") $ pendingPrinter <$> held
      where
        pendingPrinter (acc, info) =
          vcat
            [ case asked of
                Nothing -> keyPrinter 9 "Account" <> formatAccount (cs (dBAccountName acc))
                Just _ -> mempty,
              txInfoPrinter (accountNetwork acc) unit info
            ]
    ResponseTx acc txInfo -> do
      let net = accountNetwork acc
      renderIO $ txAnswerPrinter net unit txInfo
    -- The coins left in the account below the transaction, apart from it
    ResponsePrepared acc txInfo left -> do
      let net = accountNetwork acc
      renderIO $ txAnswerPrinter net unit txInfo
      unless (null left) $ putStrLn "" >> renderIO (leftCoinsPrinter unit left)
    ResponseOutsideTx net txInfo -> renderIO $ txAnswerPrinter net unit txInfo
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
    ResponseSignMsg m s a ->
      renderIO . vcat $ text . cs <$> signedMessageBlock a s m
    ResponseVerifyMsg _ _ addr valid ->
      renderIO . vcat $
        if valid
          then
            [ formatKey "Signature:" <+> formatTrue "Valid",
              text $ "The key of " <> cs addr <> " signed this message"
            ]
          else
            [ formatKey "Signature:" <+> formatFalse "Invalid",
              text $ "This is not a signature of this message by the key of " <> cs addr
            ]
    ResponseSync xs -> renderIO $ syncResultsPrinter unit xs
    ResponseRecover net gap xs
      | null xs ->
          renderIO . text $
            "No account of the seed was paid on " <> net.name <> ": the search ended after " <> show gap <> " accounts in a row without a payment"
      | otherwise -> renderIO $ syncResultsPrinter unit xs
    ResponseFees net rates -> do
      now <- getCurrentTime
      renderIO $ vcat [formatKey "Network:" <+> formatValue (cs net.name), feeRatesPrinter now rates]
    ResponseVersion v dbv ->
      renderIO $
        vcat
          [ formatKey "Software version:" <+> formatValue (cs v),
            formatKey "Database version:" <+> formatValue (cs dbv)
          ]

-- | The fee rates recent blocks paid, with how fresh they are: the age of
-- the latest of those blocks, which also tells when the network is slow
feeRatesPrinter :: UTCTime -> FeeRates -> Printer
feeRatesPrinter now rates =
  vcat
    [ text $ "Fee rates paid in " <> blocks <> " (satoshis per byte):",
      nest 2 . vcat $
        [ formatKey (block 8 (levelTitle level <> ":")) <+> formatValue (show (rateAt level rates))
          | level <- [minBound .. maxBound]
        ],
      text $ "The latest of these blocks was mined " <> agoText now (posixSecondsToUTCTime (fromIntegral rates.ratesLatestTime)) <> "."
    ]
  where
    blocks
      | rates.ratesFromHeight == rates.ratesToHeight = "block " <> show rates.ratesToHeight
      | otherwise = "blocks " <> show rates.ratesFromHeight <> " to " <> show rates.ratesToHeight
    levelTitle level = case Text.unpack (levelName level) of
      c : rest -> toUpper c : rest
      [] -> []

-- | How long before now a time was, in round words: "3 minutes ago",
-- "2 hours ago", "5 days ago". A time to come reads as just past.
agoText :: UTCTime -> UTCTime -> String
agoText now time
  | seconds < 60 = "less than a minute ago"
  | minutes < 120 = count minutes "minute"
  | hours < 48 = count hours "hour"
  | otherwise = count days "day"
  where
    seconds = max 0 (floor (diffUTCTime now time)) :: Integer
    minutes = seconds `div` 60
    hours = minutes `div` 60
    days = hours `div` 24
    count n unit = show n <> " " <> unit <> (if n == 1 then "" else "s") <> " ago"

{- Next steps -}

-- | What the user does next after a command of a payment or a sweep,
-- preparetx, preparesendall, preparesweep, signtx, signsweep, sendtx or
-- sync, below its answer. In the pretty output only: a script reading the JSON answer knows
-- its next step.
printNextSteps :: Command -> Response -> IO ()
printNextSteps cmd res = do
  let steps = nextStepsPrinter cmd res
  unless (isEmptyPrinter steps) $ putStrLn "" >> renderIO steps

-- | The steps the user may take next, after a command of a payment or a
-- sweep
nextStepsPrinter :: Command -> Response -> Printer
nextStepsPrinter cmd res =
  case (cmd, res) of
    (CommandPrepareTx opts, ResponsePrepared _ info _)
      | Just nosig <- nosigOf info -> afterPreparing opts.output nosig
    (CommandPrepareSendAll opts, ResponsePrepared _ info _)
      | Just nosig <- nosigOf info -> afterPreparing opts.output nosig
    (CommandBumpFee opts, ResponsePrepared _ info _)
      | Just nosig <- nosigOf info -> afterPreparing opts.output nosig
    -- The keys of a file at hand for the sweep are the ones to sign it with
    (CommandPrepareSweep opts, ResponseTx _ info)
      | Just nosig <- nosigOf info ->
          stepsPrinter
            [ ("Next, sign it with the private keys of the coins", "hw signsweep " <> nosig <> keysOption opts.keys),
              case opts.output of
                Nothing -> ("Or export it, to sign it where the keys are", "hw exporttx " <> nosig <> " FILE")
                Just file -> ("Or sign its file where the keys are", "hw signsweep --input " <> shellWord file)
            ]
    -- A sweep outside the wallet is in its file only
    (CommandPrepareSweep opts, ResponseOutsideTx _ _)
      | Just file <- opts.output ->
          stepsPrinter [("Next, sign its file with the private keys of the coins", "hw signsweep --input " <> shellWord file <> keysOption opts.keys)]
    (CommandSignTx signing _, ResponseTx _ info) -> afterSigning signing info
    (CommandSignSweep signing _, ResponseTx _ info) -> afterSigning signing info
    (CommandSignSweep signing _, ResponseOutsideTx _ info) -> afterSigning signing info
    (CommandSendTx {}, ResponseTx acc _) ->
      stepsPrinter [("Next, sync the account to follow the transaction", "hw sync" <> accountOption [acc])]
    (CommandSync {}, ResponseSync results) ->
      case filter ((> 0) . syncResConflicted) results of
        [] -> stepsPrinter [("Next, see the transactions", "hw txs" <> accountOption (syncResAccount <$> results))]
        conflicted ->
          stepsPrinter
            [ ("Next, find the pending transaction in conflict, which can no longer be sent", "hw pendingtxs" <> accountOption (syncResAccount <$> conflicted)),
              ("Then delete it, to free its coins", "hw deletetx NOSIGHASH")
            ]
    _ -> PEmpty
  where
    nosigOf info = case txInfoStatus info of
      Pending status -> Just (cs (txHashToHex (pendingNosigHash status)))
      OnChain _ -> Nothing
    -- A payment of the wallet is signed with its mnemonic, here or on an
    -- offline computer
    afterPreparing output nosig =
      stepsPrinter
        [ ("Next, sign it", "hw signtx " <> nosig),
          case output of
            Nothing -> ("Or export it, to sign it on an offline computer", "hw exporttx " <> nosig <> " FILE")
            Just file -> ("Or sign its file on an offline computer", "hw signtx --input " <> shellWord file)
        ]
    -- A transaction signed in the wallet is sent by its nosigHash, one
    -- signed from a file by the file signed: the --output file, or the
    -- --input file it replaced
    afterSigning signing info
      | Just nosig <- nosigOf info, isJust signing.nosigHash =
          stepsPrinter [("Next, send it to the network", "hw sendtx " <> nosig)]
      | Just file <- signing.output <|> signing.input =
          stepsPrinter [("Next, send its file from the online computer", "hw sendtx --input " <> shellWord file)]
      | otherwise = PEmpty
    -- Without a file, signsweep asks for the keys on the terminal
    keysOption (Just (KeysInFile file)) = " --prvkeys " <> shellWord file
    keysOption _ = ""
    -- The account of the next command, named when there is one
    accountOption [acc] = " --account " <> shellWord (cs (dBAccountName acc))
    accountOption _ = " --account NAME"

-- | Steps, each with what it does and the command that does it
stepsPrinter :: [(String, String)] -> Printer
stepsPrinter steps = vcat [vcat [formatKey (what <> ":"), nest 2 (formatValue command)] | (what, command) <- steps]

-- | A word of a command typed in a shell, quoted when the shell would read
-- it as more than one word or change it: an account name with a space, for
-- instance
shellWord :: String -> String
shellWord word
  | not (null word) && all plain word = word
  | otherwise = "'" <> concatMap quoted word <> "'"
  where
    plain c = isAlphaNum c || c `elem` ("-_./:@%+=," :: String)
    quoted '\'' = "'\\''"
    quoted c = [c]
