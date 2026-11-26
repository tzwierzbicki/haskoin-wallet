{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

-- | The command line of the payments: preparetx, preparesendall and fees.
-- "Haskoin.Wallet.Commands.Payment" runs them.
--
-- The options of a payment are many: the recipients, the fee, the coins
-- to spend and the change. They are read as one block ('paymentOptions',
-- 'sendAllOptions'), which send and sendall take whole
-- ("Haskoin.Wallet.Cli.Parser.Send"). So a payment takes the same options
-- whether it is prepared for the offline computer or sent in one command.
module Haskoin.Wallet.Cli.Parser.Payment
  ( prepareTxCommand,
    paymentOptions,
    prepareSendAllCommand,
    sendAllOptions,
    feesCommand,
  )
where

import Control.Applicative (Alternative (..), optional)
import Data.List (find, intercalate)
import Data.String.Conversions (cs)
import Data.Text (Text)
import Haskoin
import Haskoin.Wallet.Cli.Command (ChangeBack (..), Command (..), PrepareSendAll (..), PrepareTx (..), readChangeBack)
import Haskoin.Wallet.Cli.Completion (RecipientForm (..), activeAddresses, addressesToSpendFrom, coinsToSpend, recipientContacts, tagNames)
import Haskoin.Wallet.Cli.Parser.Args
import Haskoin.Wallet.Cli.Parser.Options (coinOptionFields, coinReader, dustCoinsOption, feeOption, networkMaybeOption, networkOptionWith, outputFileMaybeOption, readKind, recipientsPayOption, tagOption, tagReader, txLabelOption, underFee, yesOption)
import Haskoin.Wallet.Database.Address (AddressNamed, readAddressNamed)
import Haskoin.Wallet.Keys (AccountKind (..), kindNames)
import Haskoin.Wallet.ShortId (CoinRef (..))
import Haskoin.Wallet.Tags (Tag)
import Haskoin.Wallet.Tx.CoinSelection (CoinSelection (..), FeePayer (..))
import Haskoin.Wallet.Util (readNatural)
import Numeric.Natural (Natural)
import Text.RawString.QQ (r)

{- preparetx -}

-- | preparetx, which prepares a payment without signing it
prepareTxCommand :: CommandSpec Command
prepareTxCommand =
  CommandSpec
    { name = "preparetx",
      offline = False,
      summary = "Prepare a payment, without signing it",
      details =
        [r|
A payment takes three steps. preparetx is the first: it builds the
transaction and does not sign it. It needs no mnemonic, so it can run on an
online computer. signtx then signs the transaction, on an offline computer if
you like, and broadcast sends it to the network. `send` does the three steps
in one command, with the same options, on a computer where you type the
mnemonic.

hw keeps the new transaction as a pending transaction, which pendingtxs
lists. --output also writes it to a file, a PSBT (BIP 174), that you can carry
to an offline computer. Until the transaction is signed, hw names it by its
nosigHash, a hash of the transaction without its signatures. Once it is
signed, hw shows it by its txid instead. The commands take either hash, in
full or by its short id, which is its first 8 digits. While the transaction
is pending, its coins are locked: no other payment spends them. deletetx
deletes the transaction and frees its coins.

--label and --tag record what the payment is for, at the moment you make it.
A label is a text of your own. A tag is a word you put on transactions and
addresses to group them, such as rent. --tag TAG puts the tag on the
transaction, as the command `tag` does afterwards with the hash of the
transaction. Repeat --tag for several tags. hw creates a tag that does not
exist yet. The label and the tags stay with the transaction after it is
signed, sent and found by a sync. The --output file carries the label but not
the tags, which stay in the wallet that prepared the transaction. --tag does
not choose the coins to spend. --spend-from-tag, explained below, does.

On Bitcoin, preparetx marks the transaction as replaceable (BIP 125). Until
the transaction confirms, bumpfee can replace it with one that pays a higher
fee. Bitcoin Cash does not replace transactions.

preparetx selects the coins to spend among all the coins of the active
portfolio on the network of the payment. One transaction can spend bip44 and
bip84 coins together. A bip84 coin, which is SegWit, costs less fee to spend,
and the selection counts each coin at its own cost. --strategy sets how the
coins are selected.

Coin control lets you decide which coins the payment spends. Its options are
of two kinds, choosers and filters.

A chooser names coins to spend. --coin names one coin. --spend-from names the
coins at an address. --spend-from-tag names the coins at the addresses that
have a tag. The choosers add up: the chosen coins are those of every --coin,
of every --spend-from and of every --spend-from-tag. With a chooser,
preparetx spends every chosen coin that can be spent, and no other coin,
whatever --strategy says. The chosen coins must be in the active portfolio.
hw refuses a coin or an address of another portfolio, and names the portfolio
that holds it. --spend-from takes an address, or the path of an address of
the active portfolio. Write the path with its type, as the views show it,
such as bip44/1/0.

--spend-from-tag TAG chooses the coins at every address of the portfolio that
has the tag. It works as --spend-from does, for a whole group of addresses at
once. Repeat it for several tags, and the coins of all of them are chosen.
`coins` --tag TAG lists the coins of a tag. hw refuses a word that is not a
tag. It also refuses a tag that no address of the portfolio has on the
network of the payment.

A tag can lock its coins, with hw tag TAG --lock-coins. A payment then does
not spend the coins at the addresses that have that tag. --spend-from-tag TAG
is the only way to spend them. --coin and --spend-from do not open the lock.
When they choose a coin that a tag locks, preparetx does not spend the coin,
and the answer lists it with the reason.

A filter narrows the coins that may be spent, and never adds a coin. The
filters are --min-conf and --coin-type. Without a chooser, a filter applies to
every coin of the portfolio, and the strategy selects among the coins that
pass it. With a chooser, a filter applies to the chosen coins only. So
--coin-type bip44 with --spend-from spends only the coins at that address,
not every bip44 coin of the portfolio. With --spend-from-tag, it spends only
the bip44 coins of the tag. hw refuses a coin of the other type that --coin
or --spend-from names, since the two options disagree about it.

With a chooser and --select, preparetx spends only the coins the payment
needs among the chosen coins, instead of all of them. The strategy then
selects among the chosen coins, as it selects among all the coins of the
portfolio when there is no chooser. It leaves a chosen coin that is worth
less than the fee to spend it. With or without --select, the answer lists the
chosen coins that cannot be spent, because they are locked or have too few
confirmations, each with the reason. A coin that --select left because the
payment did not need it is not in that list. When no chosen coin can be
spent, preparetx prepares nothing.

The change goes to a new change address of the portfolio. The type of that
address is the first of these that applies:

  1. The type that --change-type asks for.
  2. The type of the coins the payment spends, when they all have one type.
     A payment then stays within one type of address.
  3. The default-type of the configuration file, or bip44, when the payment
     spends coins of both types.

On Bitcoin Cash every coin is bip44, so the change is too, and hw refuses
--change-type bip84.

The new change address has no tag, whatever tags the coins spent have. With
--tag-change, the change address gets the tags that all the coins the payment
spends share. A coin has the tags of its address. The change of a payment
that spends the coins of one group then stays in that group:

  hw preparetx ADDRESS:0.01 --spend-from-tag kyc --tag-change

`balances` --tag kyc still counts that change, and the next --spend-from-tag
kyc spends it. --tag-change also works without coin control: the change
address gets the tags that the selected coins share. A tag that one of the
coins lacks is left out. So when a payment mixes the coins of a group with
other coins, its change leaves the group, and hw says that --tag-change put
no tag on it. The answer shows the tags under the change address. untag takes
a tag off, and deletetx takes the tags off when
it deletes the transaction. hw refuses --tag-change with --change-back, since
the address of --change-back keeps the tags it has. For the payments you made
before, `tag` --with-change tags their change.

--change-back ADDRESS sends the change to that address instead of a new
change address. Use it to keep the funds at an address you chose: some banks
accept payments only from addresses they have checked. The address must be an
address of the active portfolio on the network of the payment. It can be a
receiving address or a change address, of either type. You can give its path
instead, such as bip84/1/0. The address may have been used already, or be the
change address of another pending transaction, since you chose it. hw refuses
any other address: change sent elsewhere would be a payment. An address has a
type of its own, so --change-back ADDRESS takes no --change-type. Every form
of --change-back sends the change to an address you already have. That
address keeps the tags it has, and gets no new tag.

--change-back largest sends the change back to the address of the largest
coin the payment spends. You cannot name that address yourself, since you do
not know in advance which coins the strategy selects. Only the coins that
have the type of the change count. That type follows the rule above, so that
the fee is counted for the right kind of output. When two coins have the same
value, hw takes the one at the lower path, and a receiving address before a
change address. With --change-type, hw refuses a payment that spends no coin
of that type, and prepares nothing. A payment that has no change sends
nothing back, and hw never refuses it for that. With --spend-from-tag, the
change goes back to an address that has the tag. So --spend-from-tag bank
--change-back largest pays from the addresses a bank has checked, and keeps
the change at one of them.

--change-back smallest works the same way with the smallest coin. It sends
the change back to the address of the smallest coin the payment spends, by
the same rules.

Nodes do not relay an output that is too small. The limit is 546 satoshis for
a legacy P2PKH output, and 294 for a native SegWit P2WPKH output. hw refuses
a payment below the limit, and adds change below the limit to the fee.

preparetx gives the transaction a lock time: the height of the best block the
wallet knows. The transaction cannot be mined into a block below that height.
Most wallets set the lock time this way, so the transaction looks like
theirs. One time in ten, the lock time is up to 99 blocks lower, so that it
does not show how long the signing took. When the best block was mined more
than eight hours ago, the wallet is behind the chain, and a sync may find
coins that were spent or received since. preparetx then asks whether to
prepare the transaction anyway, with a lock time of 0.

A fee rate above 100 satoshis per virtual byte is more often a typo than a
choice, so preparetx asks before it takes one.

preparetx also asks before a payment spends coins that do not all have the
same tags. A tag names a group of coins, such as kyc for the coins bought at
an exchange. The selection picks coins by their value, whatever their tags,
so a payment may spend coins of several groups. A transaction shows that its
coins have one owner, so such a payment joins the groups for good. preparetx
names each group, with its coins and what they are worth, and asks before it
prepares anything. The coins without a tag are a group too. With any answer
but yes, preparetx prepares nothing. To pay with the coins of one tag only,
use --spend-from-tag: preparetx then asks only when a coin spent does not
have a tag that the option names.

--yes answers yes to these three questions in advance, for scripts.
|],
      args = CommandPrepareTx <$> paymentOptions outputFileMaybeOption txLabelOption
    }

-- | The options of a payment, which preparetx and send share. They are
-- written once, so that the two commands read the same options in the
-- same way, and send builds the payment preparetx would build.
--
-- The two options given are the ones that differ. preparetx writes its
-- transaction to an --output file, and send has no such option. The help
-- of --label says where the label goes, which is not to a file for send.
-- --tag is the same for both (@txTagsOption@).
paymentOptions :: Args (Maybe FilePath) -> Args (Maybe Text) -> Args PrepareTx
paymentOptions fileOption txLabel =
  PrepareTx
    <$> some paymentArg
    <*> paymentNetworkOption
    <*> underFee feeOption
    <*> underFee rcptPayOption
    <*> underCoins minConfOption
    <*> underCoins coinTypeOption
    <*> underCoins coinSelectionOption
    <*> fileOption
    <*> txLabel
    <*> txTagsOption
    <*> underCoins (many coinOption)
    <*> underCoins (many spendFromOption)
    <*> underCoins (many spendFromTagOption)
    <*> underCoins selectOption
    <*> underFee prepareYesOption
    <*> underChange changeBackOption
    <*> underChange changeTypeOption
    <*> underChange tagChangeOption

-- | The options that choose the coins a payment spends, under their
-- subheading of the help
underCoins :: Args a -> Args a
underCoins = optionGroup "Coins"

-- | The options that say where the change of a payment goes, under their
-- subheading of the help
underChange :: Args a -> Args a
underChange = optionGroup "Change"

-- | The network of a payment, which the coins chosen tell when they are
-- given
paymentNetworkOption :: Args (Maybe Network)
paymentNetworkOption =
  networkOptionWith "The payment spends the coins of the active portfolio on this network. Without it, the network is that of the coins and addresses that --coin and --spend-from name, or else the default-network of the configuration file, or btc. With --coin or --spend-from, you need it only to tell apart the test networks, which share their keys."

-- | A filter of coin control: the one type of coins a payment may spend.
-- It narrows every coin of the portfolio, or the coins the choosers
-- chose, and never adds one. The help says so, since a user may read it
-- as one more way to choose coins, adding up with --spend-from.
coinTypeOption :: Args (Maybe AccountKind)
coinTypeOption =
  optional . option (eitherReader (readKind . cs)) $
    long "coin-type"
      <> metavar "TYPE"
      <> completeWith (cs <$> kindNames)
      <> help "Filter: spend only the coins of this type, bip44 or bip84. It applies to every coin of the portfolio, or to the coins that --coin, --spend-from and --spend-from-tag choose. hw refuses a coin of the other type that --coin or --spend-from names. Without it, the payment spends coins of both types."

-- | Where the change of preparetx goes in place of a new change address:
-- an address of the portfolio, or that of the largest or of the smallest
-- coin spent. The shell is offered the keywords with the addresses.
changeBackOption :: Args (Maybe ChangeBack)
changeBackOption =
  optional . option (eitherReader readChangeBack) $
    long "change-back"
      <> metavar "ADDRESS|PATH|largest|smallest"
      <> completeWith ["largest", "smallest"]
      <> completer (mkCompleter activeAddresses)
      <> help "Send the change to this address instead of a new change address. A path names the address too, such as bip84/1/0. It must be an address of the active portfolio, on the network of the payment. It may have been used already, or be the change address of another pending transaction. The word largest sends the change back to the address of the largest coin the payment spends, and smallest to the address of the smallest one. Only the coins that have the type of the change count. Without it, the change goes to a new change address."

-- | Whether the new change address of a payment takes the tags of the
-- coins spent. It is a switch the user sets for a payment, and not what
-- every payment does: only the user knows whether a tag says something of
-- the coins, which their change is still of, or of the address alone.
tagChangeOption :: Args Bool
tagChangeOption =
  switch $
    long "tag-change"
      <> help "Tag the new change address with the tags that all the coins the payment spends share, so that the change of a group of coins stays in the group. A coin has the tags of its address. A tag that one of the coins lacks is left out. Without it, the change address gets no tag. hw refuses it with --change-back, whose address keeps its tags."

-- | The type of the change of preparetx, when the command says
changeTypeOption :: Args (Maybe AccountKind)
changeTypeOption =
  optional . option (eitherReader (readKind . cs)) $
    long "change-type"
      <> metavar "TYPE"
      <> completeWith (cs <$> kindNames)
      <> help "The type of address the change goes to, bip44 or bip84. It is the type of the new change address, or of the coin that --change-back largest or smallest picks. Without it, the change has the type of the coins spent when they all have one type, or else the default-type of the configuration file, or bip44."

-- | A recipient of preparetx, ADDRESS:AMOUNT
paymentArg :: Args Text
paymentArg =
  strArgument $
    metavar "ADDRESS:AMOUNT"
      <> completer (mkCompleter (recipientContacts WithAmount))
      <> help
        [r|
A recipient and the amount it gets. Repeat for several recipients. The amount
is in the unit of --unit or of config.toml, whole coins by default, unless it
ends with btc, bch, bit, bits, sat or sats. ADDRESS:0.00001, ADDRESS:10bits and
ADDRESS:1000sat pay the same. btc is the coin of Bitcoin and bch that of
Bitcoin Cash: an amount in the coin of the other chain is refused, as
ADDRESS:0.5bch on btc. Whole coins take up to 8 decimals, bits up to 2, and
satoshis none. Separators such as 1'000 are refused. A Bitcoin Cash address
can keep its prefix, as in bitcoincash:qp...:0.001. The Tab key completes the
address of a contact of the address book, when you type the start of the
address or, in bash, the start of its label.
|]

-- | A chooser of coin control: an address of the portfolio whose coins to
-- spend. Its coins add up with those of the other choosers.
spendFromOption :: Args AddressNamed
spendFromOption =
  option (eitherReader readAddressNamed) $
    long "spend-from"
      <> metavar "ADDRESS|PATH"
      <> completer (mkCompleter addressesToSpendFrom)
      <> help "Chooser: spend the coins at this address of the active portfolio. A path names the address too, such as bip44/1/0. Repeat for several addresses. Their coins add up with the coins of --coin and --spend-from-tag."

-- | A chooser of coin control: a tag whose addresses have their coins
-- spent. It is --spend-from for a whole group of addresses, named once.
-- Its coins add up with those of the other choosers.
--
-- The tag has an option of its own, and is not one more word --spend-from
-- reads. A mistyped address would otherwise be looked for as a tag, and
-- the refusal would be about a tag the user never meant. The shell also
-- offers tags here and addresses there, each list short.
spendFromTagOption :: Args Tag
spendFromTagOption =
  option tagReader $
    long "spend-from-tag"
      <> metavar "TAG"
      <> completer (mkCompleter tagNames)
      <> help "Chooser: spend the coins at every address of the portfolio that has this tag. Repeat for several tags: the coins of all of them are chosen. They add up with the coins of --coin and --spend-from. It is the only way to spend the coins of a tag that locks its coins."

-- | Whether the strategy selects among the coins the choosers chose,
-- instead of every one of them being spent. It is neither a chooser nor a
-- filter: it changes what is done with the coins chosen.
selectOption :: Args Bool
selectOption =
  switch $
    long "select"
      <> help "With --coin, --spend-from or --spend-from-tag, spend only the coins the payment needs among the chosen coins, instead of all of them. --strategy selects them."

-- | A chooser of coin control: a coin to spend, as coins lists it, in full
-- or by its short id. It adds up with the coins of the other choosers.
coinOption :: Args CoinRef
coinOption =
  option coinReader $
    coinOptionFields "Chooser: spend this coin. A chooser names coins to spend, and the coins of all the choosers add up: --coin, --spend-from and --spend-from-tag. Give the coin as `coins` lists it, by its short id or with its txid in full. Repeat for several coins."
      <> completer (mkCompleter coinsToSpend)

-- | Whether preparetx, send, preparesendall and sendall take a high fee
-- rate, prepare while the wallet is behind the chain, and spend together
-- coins that do not all have the same tags, without asking
prepareYesOption :: Args Bool
prepareYesOption = yesOption "Answer yes in advance to the three questions the command may ask: about a fee rate above 100 satoshis per virtual byte, about a wallet that is behind the chain, and about coins that do not all have the same tags"

-- | Whether the recipients of a payment pay its fee (--recipients-pay)
rcptPayOption :: Args FeePayer
rcptPayOption = recipientsPayOption "The recipients pay the fee, in equal shares taken from their amounts"

-- | A filter of coin control: the confirmations a coin needs to be spent.
-- Like --coin-type, it narrows every coin of the portfolio, or the coins
-- the choosers chose. A chosen coin it leaves out is listed with why,
-- not refused, since a coin gains confirmations while the user waits.
minConfOption :: Args Natural
minConfOption =
  option (maybeReader $ readNatural . cs) $
    short 'm'
      <> long "min-conf"
      <> metavar "INT"
      <> value 0
      <> showConfig
      <> help "Filter: spend only the coins that have at least this many confirmations. A filter narrows the coins that may be spent and never adds one. It applies to every coin of the portfolio, or to the coins that --coin, --spend-from and --spend-from-tag choose. At 0, the payment may spend unconfirmed coins that the wallet's own transactions made: its change, and what its portfolios pay each other. It never spends an unconfirmed payment from somebody else."

-- | --strategy of preparetx: how the coins of the payment are chosen
coinSelectionOption :: Args CoinSelection
coinSelectionOption =
  option (eitherReader f) $
    long "strategy"
      <> metavar "STRATEGY"
      <> value LeastWaste
      <> showConfigWith strategyName
      <> completeWith (strategyName <$> strategies)
      <> help
        [r|
How hw selects the coins to spend. least-waste tries the other strategies,
and an exact match that needs no change. It keeps the coins that waste the
least fee, now and later. random-draw spends coins in random order.
largest-first spends the fewest coins, for the lowest fee now. smallest-first
spends the smallest coins first, to consolidate them. With --coin,
--spend-from or --spend-from-tag, the payment spends every chosen coin and
the strategy does not apply, unless --select is given.
|]
  where
    strategies = [LeastWaste, RandomDraw, LargestFirst, SmallestFirst]
    f s =
      maybeToEither ("Invalid strategy. Select one of the following: " <> names) $
        find ((== s) . strategyName) strategies
    names = intercalate ", " (strategyName <$> strategies)

-- | The name of a coin selection strategy on the command line
strategyName :: CoinSelection -> String
strategyName LeastWaste = "least-waste"
strategyName RandomDraw = "random-draw"
strategyName LargestFirst = "largest-first"
strategyName SmallestFirst = "smallest-first"

-- | The tags of a transaction to prepare or to send: --tag, once for
-- each tag. The four commands that build a payment take it, with one
-- help. The tags are kept in the wallet and in no file, so the help has
-- nothing to say that differs between a command with an --output file
-- and one without, as the help of --label has.
--
-- The moment a user pays is the moment they know what the payment is for. A
-- tag put later, with the command tag and the id of the transaction, is the
-- step that is skipped.
--
-- The help tells --tag from --spend-from-tag, which is also a tag on the
-- line of a payment. The first says what the payment is. The second
-- chooses the coins that pay it.
txTagsOption :: Args [Tag]
txTagsOption =
  many . tagOption $
    "A tag for the transaction, to say what the payment is for. The command `tag` puts one in the same way afterwards. Repeat for several tags. hw creates a tag that does not exist yet. The tag stays on the transaction once the network has it. It does not choose the coins to spend: --spend-from-tag does."

{- preparesendall -}

-- | preparesendall, which prepares a payment of all the coins of a
-- portfolio
prepareSendAllCommand :: CommandSpec Command
prepareSendAllCommand =
  CommandSpec
    { name = "preparesendall",
      offline = False,
      summary = "Prepare a payment of all the coins of a portfolio",
      details =
        [r|
preparesendall prepares a transaction that sends every coin the active
portfolio can spend now on one network. It sends the coins of both types of
address, and the transaction has no change. The amount sent is the spendable
balance that `portfolios` shows for that network. Like preparetx,
preparesendall needs no mnemonic and does not sign. hw keeps the transaction
as a pending transaction, for signtx to sign. sendall prepares the same
transaction, signs it and sends it in one command, with the same options, on
a computer where you type the mnemonic.

A recipient written with an amount, ADDRESS:AMOUNT, gets exactly that amount.
The recipients written without an amount share the rest in equal parts, and
they pay the fee. So give at least one recipient without an amount. When the
rest does not cover the fee, preparesendall prepares nothing. It never
reduces a fixed amount.

A dust coin is a coin worth no more than the fee to spend it. Spending it
lowers what the recipients get. Whether a coin is dust depends on the fee
rate. preparesendall lists the dust coins, explains what happens when you
spend them or leave them, and asks whether to spend them. Any answer but yes
leaves them in the portfolio. --spend-dust or --leave-dust answers in
advance, for scripts.

Coin control limits the coins that preparesendall sends. Its options are of
two kinds, choosers and filters, as for preparetx.

A chooser names coins to send. --coin names one coin, --spend-from the coins
at an address, and --spend-from-tag the coins at the addresses that have a
tag. The choosers add up. With a chooser, preparesendall sends only the
chosen coins: every one that can be spent, dust coins included. --spend-dust
and --leave-dust do not apply then. The chosen coins must be in the active
portfolio.

A filter narrows the coins sent, and never adds a coin. The filters are
--min-conf and --coin-type. A filter applies to every coin of the portfolio,
or to the chosen coins when you give a chooser.

For example, to move every legacy coin of a portfolio to a native SegWit
address of the same portfolio, give --coin-type bip44 and an address that
`receive` --type bip84 gave. To empty the addresses that have a tag, give
--spend-from-tag TAG. A tag can lock its coins. The coins of such a tag stay
in the portfolio, unless --spend-from-tag names that tag.

The answer lists every coin that the transaction leaves in the portfolio,
with the reason: dust, locked by you, by a tag or by a pending transaction,
not spendable yet, not chosen with --coin, --spend-from or --spend-from-tag,
or not of the type --coin-type sends.

preparesendall gives the transaction a lock time, the height of the best
block the wallet knows, as preparetx does. When that block was mined more
than eight hours ago, the wallet is behind the chain. preparesendall then
asks whether to prepare the transaction anyway, with a lock time of 0.

A fee rate above 100 satoshis per virtual byte is more often a typo than a
choice, so preparesendall asks before it takes one.

When the coins it sends do not all have the same tags, preparesendall names
each group of coins and asks before it prepares anything. A transaction shows
that its coins have one owner, so it joins the groups for good. To send the
coins of one tag only, use --spend-from-tag: preparesendall then asks only
when a coin sent does not have a tag that the option names.

--yes answers yes to the last three questions in advance, for scripts.
--spend-dust or --leave-dust answers the question about the dust coins.
|],
      args = CommandPrepareSendAll <$> sendAllOptions outputFileMaybeOption txLabelOption
    }

-- | The options of a payment of all the coins, which preparesendall and
-- sendall share. They are written once, so that the two commands read the
-- same options in the same way, and sendall builds the transaction
-- preparesendall would build.
--
-- The two options given are the ones that differ, as for preparetx and
-- send ('paymentOptions'). preparesendall writes its transaction to an
-- --output file, and sendall has no such option. The help of --label says
-- where the label goes, which is not to a file for sendall.
sendAllOptions :: Args (Maybe FilePath) -> Args (Maybe Text) -> Args PrepareSendAll
sendAllOptions fileOption txLabel =
  PrepareSendAll
    <$> some sendAllRecipientArg
    <*> paymentNetworkOption
    <*> underFee feeOption
    <*> underCoins minConfOption
    <*> underCoins coinTypeOption
    <*> fileOption
    <*> txLabel
    <*> txTagsOption
    <*> underCoins (many coinOption)
    <*> underCoins (many spendFromOption)
    <*> underCoins (many spendFromTagOption)
    <*> underCoins (dustCoinsOption "in the portfolio")
    <*> underFee prepareYesOption

-- | A recipient of preparesendall: an address alone, which shares what
-- remains, or an address with its amount
sendAllRecipientArg :: Args Text
sendAllRecipientArg =
  strArgument $
    metavar "ADDRESS[:AMOUNT]"
      <> completer (mkCompleter (recipientContacts AddressAlone))
      <> help
        [r|
A recipient. An address alone shares what remains with the other recipients
that have no amount. ADDRESS:AMOUNT gets a fixed amount, written as for
preparetx. Repeat for several recipients, and give at least one without an
amount. The Tab key completes the address of a contact of the address book,
when you type the start of the address or, in bash, the start of its label.
|]

{- fees -}

-- | fees, which shows the fee rates of recent blocks
feesCommand :: CommandSpec Command
feesCommand =
  CommandSpec
    { name = "fees",
      offline = False,
      summary = "Show the fee rates of recent blocks",
      details =
        [r|
fees shows three fee rates, which it reads from the latest six blocks that
hold fee-paying transactions:

  low      a quarter of the virtual bytes in those blocks paid less
  median   half of the virtual bytes paid less
  high     nine tenths of the virtual bytes paid less

The rates are in satoshis per virtual byte, the unit in which the network
charges fees. In that unit, a byte of a signature in a witness counts as a
quarter of a byte.

A transaction that pays the high rate almost surely gets into the next block.
One that pays the low rate usually does when the network is calm. The age of
the latest block tells how fresh the rates are. When the latest block was
mined an hour ago, the network is slow, and the rates may be too low.

The rates show what got into blocks, not how long the transactions waited.
When many transactions arrive suddenly after a quiet period, the rates are
too low for a while. If a payment is then stuck, bumpfee replaces it with one
that pays a higher fee.

preparetx, preparesendall, bumpfee and preparesweep show the same rates, and
ask which one to take when you give no --fee-rate. --fee-rate low, median or
high takes one of the rates without asking.
|],
      args = CommandFees <$> networkMaybeOption
    }
