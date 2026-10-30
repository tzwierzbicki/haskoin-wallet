{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}

-- | The answers of the commands, as hw prints them for a person, and the
-- next steps of a payment. Each part of an answer names what it is with a
-- 'Style', as the help does: a field, an amount, an account, a command to
-- type. "Haskoin.Wallet.Style" says how each one looks, so that the help
-- and the answers speak the same colours.
--
-- An answer is a document of lines. A line that may be missing, such as
-- the label of an address, comes as a list of none or one line.
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
import qualified Data.Text.IO as Text
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
import Haskoin.Wallet.FeeRates (FeeRates (..), levelName, rateAt)
import Haskoin.Wallet.Keys (KeySource (..))
import Haskoin.Wallet.MessageSigning (signedMessageBlock)
import Haskoin.Wallet.Parser (BumpFee (..), Command (..), PrepareSendAll (..), PrepareSweep (..), PrepareTx (..), Signing (..), program)
import Haskoin.Wallet.Parser.Help (commandName)
import Haskoin.Wallet.Parser.Run (commandNamed)
import Haskoin.Wallet.Response
import Haskoin.Wallet.Style
import Haskoin.Wallet.Sync
import Haskoin.Wallet.TxInfo
import Haskoin.Wallet.Util
import Numeric.Natural (Natural)
import Prettyprinter (Doc, annotate, hcat, hsep, indent, pretty, vcat, (<+>))
import System.Exit
import System.IO

{- Rendering -}

-- | Print the document on stdout
renderIO :: Doc Style -> IO ()
renderIO = renderOn stdout

-- | Print the document on the handle: stdout for an answer, stderr for
-- what a command shows before it asks the user. It is styled on a
-- terminal, and plain otherwise.
renderOn :: Handle -> Doc Style -> IO ()
renderOn h doc = do
  output <- outputOf h
  putDoc h output doc

{- The parts of an answer -}

-- | Text as it is, unstyled
text :: String -> Doc Style
text = pretty

-- | Text in a style
styled :: Style -> String -> Doc Style
styled style = annotate style . pretty

-- | The name of a field padded to the width, with its colon, before its
-- value: "Fee      : "
field :: Natural -> Text -> Doc Style
field n name = styled Field (block n (cs name) <> ":") <> " "

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

-- | A time as the listings show it, to the second, in UTC
timeText :: UTCTime -> String
timeText = formatTime defaultTimeLocale "%d %b %Y %H:%M:%S UTC"

-- | A count, such as of transactions or coins
countPrinter :: Natural -> Doc Style
countPrinter = text . show

-- | A number of confirmations: none yet waits for the network
confirmationsPrinter :: Natural -> Doc Style
confirmationsPrinter 0 = styled Waiting "0"
confirmationsPrinter n = text (show n)

{- Amounts -}

-- | An amount the wallet holds or gets, in green unless zero
amountPrinter :: AmountUnit -> Word64 -> Doc Style
amountPrinter unit = integerAmountPrinter unit . fromIntegral

-- | An amount coloured by its sign: green when it comes in, red when it
-- goes out
integerAmountPrinter :: AmountUnit -> Integer -> Doc Style
integerAmountPrinter unit amnt
  | amnt > 0 = amountIn (Just Incoming) unit amnt
  | amnt < 0 = amountIn (Just Outgoing) unit amnt
  | otherwise = amountIn Nothing unit amnt

-- | An amount neither in nor out, such as a fee: unstyled
neutralAmount :: AmountUnit -> Integer -> Doc Style
neutralAmount = amountIn Nothing

-- | The number of an amount in the style, if any, then its unit
amountIn :: Maybe Style -> AmountUnit -> Integer -> Doc Style
amountIn style unit amnt = maybe id annotate style (pretty (showAmount unit amnt)) <+> pretty (showUnit unit amnt)

{- Printing responses -}

-- | The words of a mnemonic, four on a line, each after its number
mnemonicPrinter :: [Text] -> Doc Style
mnemonicPrinter ws =
  vcat $ hcat . fmap formatWord <$> chunksOf 4 (zip ([1 ..] :: [Natural]) ws)
  where
    formatWord (i, w) = styled Field (block 4 (show i <> ".")) <> styled Secret (block 10 (cs w))

partsPrinter :: [[Text]] -> [Doc Style]
partsPrinter xs =
  concatMap formatPart $ zip ([1 ..] :: [Natural]) xs
  where
    formatPart (i, ws) =
      [ styled Title ("Private Mnemonic Part #" <> show i),
        indent 2 $ mnemonicPrinter ws
      ]

-- | The accounts synced, each with the results of its sync
syncResultsPrinter :: AmountUnit -> [SyncRes] -> Doc Style
syncResultsPrinter unit xs = vcat $ intercalate [mempty] (f <$> xs)
  where
    f (SyncRes acc spendable bh h t c r k n) =
      [ accountPrinter unit acc spendable,
        styled Title "Sync Results:",
        indent 2 $ field 12 "Best Block" <> styled Muted (cs $ blockHashToHex bh),
        indent 2 $ field 12 "Best Height" <> text (show h),
        indent 2 $ field 12 "Tx updates" <> text (show t),
        indent 2 $ field 12 "Coin updates" <> text (show c),
        indent 2 $ field 12 "Sent again" <> text (show r),
        indent 2 $ field 12 "Conflicted" <> conflicted k,
        indent 2 $ field 12 "Discovered" <> text (show n)
      ]
    -- A conflicted transaction can no longer be sent
    conflicted 0 = text "0"
    conflicted k = styled Failure (show k)

-- | An account with its balances: those the store reports, and the one it
-- can spend now
accountPrinter :: AmountUnit -> DBAccount -> Word64 -> Doc Style
accountPrinter unit acc spendable =
  vcat
    [ field 11 "Account" <> pretty (dBAccountName acc) `withStyle` Account,
      field 11 "Fingerprint" <> maybe (styled Muted "unknown") pretty (dBAccountFingerprint acc),
      field 11 "Deriv" <> text deriv,
      field 11 "Network" <> text net,
      field 11 "External" <> text (show $ dBAccountExternal acc),
      field 11 "Internal" <> text (show $ dBAccountInternal acc),
      field 11 "Created" <> text created,
      styled Field "Balances:",
      indent 2 $ field 11 "Confirmed" <> amountPrinter unit (dBAccountBalanceConfirmed acc),
      indent 2 $ field 11 "Unconfirmed" <> amountPrinter unit (dBAccountBalanceUnconfirmed acc),
      indent 2 $ field 11 "Spendable" <> amountPrinter unit spendable,
      indent 2 $ field 11 "Coins" <> countPrinter (fromIntegral $ dBAccountBalanceCoins acc)
    ]
  where
    deriv = pathToStr $ accountPath acc
    net = (accountNetwork acc).name
    utctime = dBAccountCreated acc
    created = formatTime defaultTimeLocale "%d %b %Y" utctime

-- | The document in the style
withStyle :: Doc Style -> Style -> Doc Style
withStyle doc style = annotate style doc

-- | An address after its derivation path, which tells receiving and change
-- addresses apart, with its label and the given details below it. The path
-- is padded to the given width, that of the longest path listed
-- ('pathWidth'). Each detail is a name and a value, the names padded to the
-- longest one.
addressLines :: Natural -> DBAddress -> [(Text, Doc Style)] -> Doc Style
addressLines pad addr details =
  vcat $
    (field pad (pathText (addrPath addr)) <> pretty (dBAddressAddress addr))
      : [indent 2 (field width name <> value) | (name, value) <- labelled <> details]
  where
    label = dBAddressLabel addr
    labelled = [("Label", pretty label `withStyle` UserText) | not (Text.null label)]
    width = fromIntegral $ maximum (Text.length . fst <$> ("Label", mempty) : details)

-- | The width of the longest derivation path of the addresses listed
pathWidth :: [DBAddress] -> Natural
pathWidth addrs = fromIntegral $ maximum (0 : (Text.length . pathText . addrPath <$> addrs))

-- | An address as addrs shows it: its transactions, what it received, and
-- the balance it can spend now
addressPrinter :: AmountUnit -> Natural -> DBAddress -> Word64 -> Doc Style
addressPrinter unit pad addr spendable =
  addressLines
    pad
    addr
    [ ("Txs", countPrinter $ fromIntegral $ dBAddressBalanceTxs addr),
      ("Received", amountPrinter unit $ dBAddressBalanceReceived addr),
      ("Spendable", amountPrinter unit spendable)
    ]

-- | An address as balances shows it: the balance of its coins. What it
-- cannot spend yet waits.
coinBalancePrinter :: AmountUnit -> Natural -> DBAddress -> CoinBalance -> Doc Style
coinBalancePrinter unit pad addr balance =
  addressLines
    pad
    addr
    [ ("Total", amountPrinter unit $ coinBalanceTotal balance),
      ("Spendable", amountPrinter unit $ coinBalanceSpendable balance),
      ("Unspendable", unspendable (coinBalanceUnspendable balance))
    ]
  where
    unspendable 0 = neutralAmount unit 0
    unspendable v = amountIn (Just Waiting) unit (fromIntegral v)

-- | Addresses as balances lists them, their paths aligned. preparetx
-- --changeback shows the addresses it can send the change back to this way.
coinBalancesPrinter :: AmountUnit -> [(DBAddress, CoinBalance)] -> Doc Style
coinBalancesPrinter unit addrs = vcat $ uncurry (coinBalancePrinter unit (pathWidth (fst <$> addrs))) <$> addrs

-- | A transaction of a listing, on the network of the account listed
txInfoPrinter :: Network -> AmountUnit -> TxInfo -> Doc Style
txInfoPrinter = transactionPrinter False

-- | The one transaction a command answers with, naming its network. The
-- user sees which coins it moves: private keys, coins and the legacy
-- addresses of Bitcoin Cash read the same on Bitcoin, and a sweep outside
-- the wallet is on the network of its file.
txAnswerPrinter :: Network -> AmountUnit -> TxInfo -> Doc Style
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
  Doc Style
transactionPrinter showNetwork net unit (TxInfo TxSummary {..} status) =
  vcat $
    [styled Title (block 9 title) <> ": " <> total]
      <> network
      <> statusLines status
      <> [fee]
      <> changeInFee
      <> section "Spent from:" spentFrom
      <> section "Sending to:" sendingTo
      <> section "Change to:" changeTo
  where
    title =
      case summaryType of
        TxDebit -> "Debit"
        TxInternal -> "Internal"
        TxCredit -> "Credit"
    total = integerAmountPrinter unit summaryAmount
    network = [field 9 "Network" <> text net.name | showNetwork]
    fee = field 9 "Fee" <> feePrinter unit summaryFee summaryFeeByte
    -- Change too small for an output is lost to the fee: it comes neither
    -- in nor out
    changeInFee =
      case summaryChangeInFee of
        Just change
          | change > 0 ->
              [ field 9 "Change"
                  <> neutralAmount unit (fromIntegral change)
                  <+> "too small for an output, added to the fee"
              ]
        _ -> []
    -- Every input, those of the wallet first
    spentFrom =
      [walletAddress Nothing a v p (cs l) | (a, MyInputs v p l _) <- Map.assocs summaryMyInputs]
        <> [text (addrToString net a) | a <- Map.keys summaryOtherInputs]
    toWallet = [walletAddress (Just Incoming) a v p (cs l) | (a, MyOutputs v p l) <- Map.assocs summaryMyOutputs]
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
      | null rows = []
      | otherwise = [vcat (styled Field heading : (indent 2 <$> rows))]
    -- An address of the wallet and its value, with its path and its label
    walletAddress amountStyle a v p label =
      vcat $
        [ text (addrToString net a) <> ":" <+> amountIn amountStyle unit (fromIntegral v),
          indent 2 $ styled Field "Path:" <+> text (pathToStr p)
        ]
          <> [indent 2 $ styled Field "Label:" <+> styled UserText label | not (null label)]
    -- An address of another wallet and its value
    otherAddress a v =
      text (addrToString net a) <> ":" <+> neutralAmount unit (fromIntegral v)

-- | Where the transaction stands: its id when it has one, then its
-- confirmations on the chain, or its state in preparation
statusLines :: TxStatus -> [Doc Style]
statusLines (OnChain status) =
  [txHashPrinter (chainTxid status)]
    -- In UTC: the time the store first saw the transaction, in its mempool
    -- or in a block; none for a summary the next sync has to fetch again
    <> [field 9 "Time" <> text (timeText time) | Just time <- [chainTime status]]
    <> [ field 9 "Status" <> styled Failure "dead" <+> "(replaced, or dropped from the network)"
       | chainDead status
       ]
    <> [field 9 "Confs" <> confirmationsPrinter (chainConfirmations status)]
statusLines (Pending status) =
  [txHashPrinter txid | Just txid <- [pendingTxid status]]
    <> [field 9 "NoSigHash" <> styled NoSigHash (cs $ txHashToHex (pendingNosigHash status))]
    <> [field 9 "Created" <> text (timeText time) | Just time <- [pendingCreated status]]
    <> [field 9 "Replaces" <> text (cs $ txHashToHex replaced) | replaced <- pendingReplaces status]
    -- An unsigned transaction waits for signtx
    <> [ field 9 "Signed"
           <> "This pending transaction is"
           <+> if pendingSigned status
             then styled Good "signed"
             else styled Waiting "not signed"
       ]
    <> [field 9 "Online" <> "This transaction is" <+> styled Good "online" | pendingOnline status]
    <> [ field 9 "Conflict"
           <> styled Failure "A coin it spends was spent by another transaction, or is gone,"
           <+> "and it can no longer be sent; delete it with"
           <+> commandWord "deletetx"
       | pendingConflicted status
       ]

txHashPrinter :: TxHash -> Doc Style
txHashPrinter tid = field 9 "TxHash" <> text (cs $ txHashToHex tid)

feePrinter :: AmountUnit -> Natural -> Natural -> Doc Style
feePrinter unit fee feeBytes =
  neutralAmount unit (fromIntegral fee) <+> "(" <> text (show feeBytes) <+> "sat/bytes" <> ")"

-- | The coins a transaction just prepared leaves in the account, one a
-- line, each with why: the coin as --coin and lockcoin take it, and its
-- value
leftCoinsPrinter :: AmountUnit -> [LeftCoin] -> Doc Style
leftCoinsPrinter unit left =
  vcat $ styled Title "Staying in the account:" : (indent 2 . leftCoinPrinter <$> left)
  where
    leftCoinPrinter coin =
      text (coinText coin.leftCoinOutpoint)
        <+> neutralAmount unit (fromIntegral coin.leftCoinValue)
        <> ","
        <+> text (leftReasonText coin.leftCoinReason)

coinPrinter :: Network -> AmountUnit -> JsonCoin -> Doc Style
coinPrinter net unit JsonCoin {..} =
  vcat
    [ -- As --coin, lockcoin and unlockcoin take it
      coinField "Coin" <+> text (coinText jsonCoinOutpoint),
      coinField "Value" <+> amountPrinter unit jsonCoinValue,
      coinField "Address" <+> text (addrToString net jsonCoinAddress),
      coinField "Confs" <+> confirmationsPrinter jsonCoinConfirmations,
      coinField "Status" <+> status
    ]
  where
    coinField name = styled Field (block 7 name <> ":")
    status = case jsonCoinLock of
      Just LockedByTransaction -> styled Waiting "Locked by a pending transaction"
      Just LockedByUser -> styled Waiting "Locked by the user"
      Nothing
        | jsonCoinSpendable -> styled Good "Spendable"
        | otherwise -> styled Waiting "Not spendable yet"

prettyPrinter :: AmountUnit -> Response -> IO ()
prettyPrinter unit res = do
  responsePrinter unit res
  when (responseFailed res) exitFailure

responsePrinter :: AmountUnit -> Response -> IO ()
responsePrinter unit =
  \case
    ResponseError err ->
      renderIO $ styled Failure "Error:" <+> pretty err
    ResponseMnemonic orig mnem parts ->
      renderIO . vcat $
        [ styled Title "System Entropy Source",
          indent 2 $ styled UserText (cs orig),
          styled Title "Private Mnemonic",
          indent 2 $ mnemonicPrinter mnem
        ]
          <> partsPrinter parts
    ResponseAccount acc spendable -> renderIO $ accountPrinter unit acc spendable
    ResponseAccounts [] -> renderIO $ text "There are no accounts to display"
    ResponseAccounts accs ->
      renderIO . vcat $ intersperse mempty $ uncurry (accountPrinter unit) <$> accs
    ResponseAccResult _ res txt -> do
      let p =
            if res
              then styled Good "Success"
              else styled Failure "Failure"
      renderIO . vcat $ [styled Field "Result:" <+> p, pretty txt]
    ResponseFile f -> renderIO $ styled Field "File:" <+> styled UserText f
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
      renderIO $ vcat $ intersperse mempty $ txInfoPrinter net unit <$> txs
    ResponsePendingTxs asked [] ->
      renderIO . text $ case asked of
        Just acc -> "There are no pending transactions in the account " <> cs (dBAccountName acc)
        Nothing -> "There are no pending transactions"
    -- Listing every account, each transaction names its own
    ResponsePendingTxs asked held ->
      renderIO $ vcat $ intersperse mempty $ pendingPrinter <$> held
      where
        pendingPrinter (acc, info) =
          vcat $
            [field 9 "Account" <> pretty (dBAccountName acc) `withStyle` Account | Nothing <- [asked]]
              <> [txInfoPrinter (accountNetwork acc) unit info]
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
          [ styled Field "Sent:" <+> text (cs $ txHashToHex h),
            indent 2 $ styled Field "Network:" <+> text net.name
          ]
    ResponseDeleteTx h c a ->
      renderIO $
        vcat
          [ styled Field "Deleted:" <+> styled NoSigHash (cs $ txHashToHex h),
            indent 2 $ styled Field "Freed coins:" <+> text (show c),
            indent 2 $ styled Field "Freed internal addresses:" <+> text (show a)
          ]
    ResponseCoins _ [] ->
      renderIO $ text "There are no coins to display"
    ResponseCoins acc coins -> do
      let net = accountNetwork acc
      renderIO $ vcat $ intersperse mempty $ coinPrinter net unit <$> coins
    -- As it is, byte for byte: verifymsg --input reads it back, and other
    -- wallets check the message exactly
    ResponseSignMsg m s a ->
      Text.putStr . Text.unlines $ signedMessageBlock a s m
    ResponseVerifyMsg _ _ addr valid ->
      renderIO . vcat $
        if valid
          then
            [ styled Field "Signature:" <+> styled Good "Valid",
              text $ "The key of " <> cs addr <> " signed this message"
            ]
          else
            [ styled Field "Signature:" <+> styled Failure "Invalid",
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
      renderIO $ vcat [styled Field "Network:" <+> text net.name, feeRatesPrinter now rates]
    ResponseVersion v dbv ->
      renderIO $
        vcat
          [ styled Field "Software version:" <+> pretty v,
            styled Field "Database version:" <+> pretty dbv
          ]

-- | The fee rates recent blocks paid, with how fresh they are: the age of
-- the latest of those blocks, which also tells when the network is slow.
-- A latest block mined an hour ago or more waits: its rates may be too
-- low, as the help of fees says.
feeRatesPrinter :: UTCTime -> FeeRates -> Doc Style
feeRatesPrinter now rates =
  vcat
    [ text $ "Fee rates paid in " <> blocks <> " (satoshis per byte):",
      indent 2 . vcat $
        [ styled Field (block 8 (levelTitle level <> ":")) <+> text (show (rateAt level rates))
          | level <- [minBound .. maxBound]
        ],
      "The latest of these blocks was mined" <+> age <> "."
    ]
  where
    latest = posixSecondsToUTCTime (fromIntegral rates.ratesLatestTime)
    age
      | diffUTCTime now latest >= 3600 = styled Waiting (agoText now latest)
      | otherwise = text (agoText now latest)
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
printNextSteps cmd res =
  forM_ (nextStepsPrinter cmd res) $ \steps -> putStrLn "" >> renderIO steps

-- | The steps the user may take next, after a command of a payment or a
-- sweep, if any. Each step is a command line, styled as the help shows
-- commands, options and metavars.
nextStepsPrinter :: Command -> Response -> Maybe (Doc Style)
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
            [ ("Next, sign it with the private keys of the coins", hwLine "signsweep" (nosigWord nosig : keysArgs opts.keys)),
              case opts.output of
                Nothing -> ("Or export it, to sign it where the keys are", hwLine "exporttx" [nosigWord nosig, metavarWord "FILE"])
                Just file -> ("Or sign its file where the keys are", hwLine "signsweep" [optionWord "--input", fileWord file])
            ]
    -- A sweep outside the wallet is in its file only
    (CommandPrepareSweep opts, ResponseOutsideTx _ _)
      | Just file <- opts.output ->
          stepsPrinter [("Next, sign its file with the private keys of the coins", hwLine "signsweep" ([optionWord "--input", fileWord file] <> keysArgs opts.keys))]
    (CommandSignTx signing _, ResponseTx _ info) -> afterSigning signing info
    (CommandSignSweep signing _, ResponseTx _ info) -> afterSigning signing info
    (CommandSignSweep signing _, ResponseOutsideTx _ info) -> afterSigning signing info
    (CommandSendTx {}, ResponseTx acc _) ->
      stepsPrinter [("Next, sync the account to follow the transaction", hwLine "sync" (accountArgs [acc]))]
    (CommandSync {}, ResponseSync results) ->
      case filter ((> 0) . syncResConflicted) results of
        [] -> stepsPrinter [("Next, see the transactions", hwLine "txs" (accountArgs (syncResAccount <$> results)))]
        conflicted ->
          stepsPrinter
            [ ("Next, find the pending transaction in conflict, which can no longer be sent", hwLine "pendingtxs" (accountArgs (syncResAccount <$> conflicted))),
              ("Then delete it, to free its coins", hwLine "deletetx" [metavarWord "NOSIGHASH"])
            ]
    _ -> Nothing
  where
    nosigOf info = case txInfoStatus info of
      Pending status -> Just (cs (txHashToHex (pendingNosigHash status)))
      OnChain _ -> Nothing
    -- A payment of the wallet is signed with its mnemonic, here or on an
    -- offline computer
    afterPreparing output nosig =
      stepsPrinter
        [ ("Next, sign it", hwLine "signtx" [nosigWord nosig]),
          case output of
            Nothing -> ("Or export it, to sign it on an offline computer", hwLine "exporttx" [nosigWord nosig, metavarWord "FILE"])
            Just file -> ("Or sign its file on an offline computer", hwLine "signtx" [optionWord "--input", fileWord file])
        ]
    -- A transaction signed in the wallet is sent by its nosigHash, one
    -- signed from a file by the file signed: the --output file, or the
    -- --input file it replaced
    afterSigning signing info
      | Just nosig <- nosigOf info, isJust signing.nosigHash =
          stepsPrinter [("Next, send it to the network", hwLine "sendtx" [nosigWord nosig])]
      | Just file <- signing.output <|> signing.input =
          stepsPrinter [("Next, send its file from the online computer", hwLine "sendtx" [optionWord "--input", fileWord file])]
      | otherwise = Nothing
    -- Without a file, signsweep asks for the keys on the terminal
    keysArgs (Just (KeysInFile file)) = [optionWord "--prvkeys", fileWord file]
    keysArgs _ = []
    -- The account of the next command, named when there is one
    accountArgs [acc] = [optionWord "--account", styled Account (shellWord (cs (dBAccountName acc)))]
    accountArgs _ = [optionWord "--account", metavarWord "NAME"]

-- | Steps, each with what it does and the command that does it
stepsPrinter :: [(String, Doc Style)] -> Maybe (Doc Style)
stepsPrinter steps = Just $ vcat [vcat [styled Field (what <> ":"), indent 2 command] | (what, command) <- steps]

-- | A command line of hw, "hw signtx NOSIGHASH", the command in the colour
-- of its kind as the help shows it
hwLine :: String -> [Doc Style] -> Doc Style
hwLine name args = hsep ("hw" : commandWord name : args)

-- | A command of hw, in the colour of its kind: the colour of the offline
-- commands for one that asks for a mnemonic or private keys
commandWord :: String -> Doc Style
commandWord name = maybe (styled Command name) commandName (commandNamed program name)

optionWord :: String -> Doc Style
optionWord = styled Option

-- | What the user has to fill in, as the help shows it
metavarWord :: String -> Doc Style
metavarWord = styled Metavar

nosigWord :: String -> Doc Style
nosigWord = styled NoSigHash

fileWord :: String -> Doc Style
fileWord = styled UserText . shellWord

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
