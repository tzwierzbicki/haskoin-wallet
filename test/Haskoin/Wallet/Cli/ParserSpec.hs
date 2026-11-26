{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

-- | The command line of hw, before anything runs: the commands of its help
-- and their groups, the usage lines its help shows, the options that say
-- where a command works, what label and show take, the commands its texts
-- name, and what the arguments ask for
module Haskoin.Wallet.Cli.ParserSpec (spec) where

import Data.Either (isLeft, isRight)
import Data.Maybe (isJust)
import Data.Time (fromGregorian)
import Haskoin (bch, btc, txHashToHex)
import Haskoin.Wallet.Cli.Listing (ListingOrder (..), Paging (..))
import Haskoin.Wallet.Database.Tag (TagFilter (..), TxTagFilter (..), noTxTagFilter)
import Haskoin.Wallet.Cli.Parser
  ( BumpFee (..),
    ChangeBack (..),
    CheckMnemonic (..),
    Command (..),
    DustCoins (..),
    Described (..),
    Label (..),
    Labelled (..),
    ListingOutput (..),
    MnemonicSheetOptions (..),
    NewPortfolio (..),
    PrepareSendAll (..),
    PrepareSweep (..),
    PrepareTx (..),
    Receive (..),
    Recover (..),
    RenameTag (..),
    ReportKind (..),
    ReportOptions (..),
    Shown (..),
    SignMsg (..),
    TagOptions (..),
    Untag (..),
    Target (..),
    program,
    readCoinRef,
    indexedName,
    labelledShapeOf,
    readDescribed,
    readLabelled,
    readShown,
    wrapsLines,
  )
import Haskoin.Wallet.Cli.Parser.Args (Args (..), CommandGroup (..), CommandSpec (..), Name (..), OptDoc (..), Program (..))
import Haskoin.Wallet.Cli.Parser.Completion (completionParts, escapedForBash, keptByBash)
import Haskoin.Wallet.Cli.Parser.Help (mentionedCommands, usageParts)
import Haskoin.Wallet.Cli.Parser.Run (Request (..), commandNamed, pagerOff, requestOf)
import Haskoin.Wallet.WalletFile (ImportScope (..))
import Haskoin.Wallet.Report.Price (Price (..))
import Haskoin.Wallet.Cli.Style (renderPlain)
import Haskoin.Wallet.Config.View (View (..))
import Haskoin.Wallet.Database.Address (AddrType (..), AddressNamed (..))
import Haskoin.Wallet.Keys (AccountKind (..))
import Haskoin.Wallet.ShortId (HashRef (..), coinRefText, shortIdText)
import Haskoin.Wallet.Sync (SyncMode (..))
import Haskoin.Wallet.Tags (TagColor (..), readTag, tagText)
import Haskoin.Wallet.TestUtils (coldTag, parsedLine, rentTag)
import Haskoin.Wallet.Tx.CoinSelection (CoinSelection (..), FeePayer (..))
import Haskoin.Wallet.Tx.FeeRates (FeeRateChoice (..))
import Test.Hspec

spec :: Spec
spec = do
  describe "The commands of the help" $ do
    -- The groups and the order of the README, which follows the
    -- specification
    it "come in the groups of the README, each command in its order" $
      [(group.title, [command.name | command <- group.commands]) | group <- program.groups]
        `shouldBe` [ ( "Wallets and portfolios",
                       ["newportfolio", "recover", "checkmnemonic", "mnemonicsheet", "exportwallet", "importwallet", "portfolios", "switch", "setconfig", "showconfig", "deleteportfolio", "deletewallet", "sync"]
                     ),
                     ("Addresses", ["receive", "show", "addresses", "balances", "label"]),
                     ("Coins", ["coins", "lockcoin", "unlockcoin"]),
                     ("Payments", ["fees", "send", "sendall", "preparetx", "preparesendall", "signtx", "broadcast", "bumpfee"]),
                     ("Transactions", ["txs", "pendingtxs", "exporttx", "importtx", "deletetx"]),
                     ("Tags", ["tag", "untag", "tags", "renametag", "deletetag"]),
                     ("Address book", ["newcontact", "contacts", "deletecontact"]),
                     ("Sweeps", ["sweep", "preparesweep", "signsweep"]),
                     ("Messages", ["signmsg", "verifymsg"]),
                     ("The wallet", ["backup", "showbackups", "restoredatabase", "exportsheet", "exportjournal", "report"])
                   ]

    it "leave out the commands that portfolios replaced" $
      filter (isJust . commandNamed program) ["mnemonic", "createacc", "renameacc", "deleteacc", "importacc", "exportacc", "accounts"]
        `shouldBe` []

    -- A command line for each command, with what it needs, and the
    -- --wallet and --portfolio of the commands that keep them
    it "each read a command line with the arguments it needs" $ do
      map fst everyCommandLine `shouldBe` concatMap (map (.name) . (.commands)) program.groups
      [name | (name, args) <- everyCommandLine, not (isRight (parsedLine (name : args)))] `shouldBe` []

  describe "The options that say where a command works" $ do
    -- The commands work in the active portfolio. --portfolio is left to
    -- switch, to exportwallet, which writes one portfolio with it, and to
    -- the commands that name a portfolio of a file of a wallet.
    -- showconfig has a --portfolio switch too, which names no portfolio:
    -- it asks which one is active. report writes the statement of one
    -- portfolio with it. deleteportfolio takes its portfolio as an
    -- argument instead.
    it "give --portfolio only to switch, showconfig, report and the commands that read or write a file of a wallet" $
      filter (takesOption "portfolio") commandNames
        `shouldBe` ["exportwallet", "importwallet", "switch", "showconfig", "show", "report"]

    -- --wallet is left to switch, to exportwallet, which writes one
    -- wallet with it, and to the commands that read a file of a wallet,
    -- which name one wallet of the file with it. The --wallet switch of
    -- showconfig names no wallet: it asks which one the active portfolio
    -- is of. report writes the statement of one wallet with it.
    -- deletewallet takes its wallet as an argument instead, as
    -- deleteportfolio takes its portfolio.
    it "give --wallet only to switch, showconfig, report and the commands that read or write a file of a wallet" $
      filter (takesOption "wallet") commandNames `shouldBe` ["exportwallet", "importwallet", "switch", "showconfig", "show", "report"]

    -- Each of them asks before it deletes, and --yes answers for a script
    it "give --yes to the five commands that delete from the database" $
      filter (not . takesOption "yes") ["deleteportfolio", "deletewallet", "deletetx", "deletetag", "deletecontact"] `shouldBe` []

    -- -w is --sweep-from on preparesweep
    it "give --wallet no short name" $
      [doc.names | command <- allCommands, doc <- command.args.rows, Long "wallet" `elem` doc.names, doc.names /= [Long "wallet"]]
        `shouldBe` []

    it "read the wallet --wallet names by its index" $
      parsedLine ["switch", "--wallet", "1", "--portfolio", "0"] `shouldBe` Right (CommandSwitch (Just 1) (Just 0))

    it "read the portfolio --portfolio or -p names by its index, or by its name" $ do
      let toPortfolio2 = Right (CommandSwitch Nothing (Just 2))
      parsedLine ["switch", "--portfolio", "2"] `shouldBe` toPortfolio2
      parsedLine ["switch", "-p", "2"] `shouldBe` toPortfolio2
      parsedLine ["switch", "--portfolio", "portfolio2"] `shouldBe` toPortfolio2

    it "read the wallet --wallet names by its name too" $
      parsedLine ["switch", "--wallet", "wallet1", "-p", "0"] `shouldBe` Right (CommandSwitch (Just 1) (Just 0))

    -- The portfolio is the one value switch needs, so it goes without an
    -- option, in the wallet of the active portfolio without --wallet
    it "make switch take the portfolio as its argument, without --portfolio" $
      parsedLine ["switch", "1"] `shouldBe` Right (CommandSwitch Nothing (Just 1))

    it "make switch take the portfolio of its argument by its name too" $
      parsedLine ["switch", "portfolio1"] `shouldBe` Right (CommandSwitch Nothing (Just 1))

    it "make switch take --wallet after the portfolio of its argument, or before it" $ do
      parsedLine ["switch", "0", "--wallet", "1"] `shouldBe` Right (CommandSwitch (Just 1) (Just 0))
      parsedLine ["switch", "--wallet", "1", "0"] `shouldBe` Right (CommandSwitch (Just 1) (Just 0))

    -- The argument and --portfolio are one value, given once
    it "refuse switch with both a portfolio as its argument and --portfolio" $
      parsedLine ["switch", "1", "--portfolio", "2"] `shouldBe` Left "Invalid option `--portfolio'"

    it "refuse a portfolio of switch that is neither an index nor a name" $
      parsedLine ["switch", "one"] `shouldBe` Left "The portfolio one is not an index, such as 0, or a name, such as portfolio0"

    it "refuse a --portfolio value that is neither an index nor a name" $
      parsedLine ["switch", "--portfolio", "one"] `shouldBe` Left "option --portfolio: The --portfolio value one is not an index, such as 0, or a name, such as portfolio0"

    it "read the network of --network or -n, and the type of --type or -t" $ do
      let onBch = Right (CommandReceive (Receive (Target (Just bch) (Just Bip44)) Nothing []))
      parsedLine ["receive", "--network", "bch", "--type", "bip44"] `shouldBe` onBch
      parsedLine ["receive", "-n", "bch", "-t", "bip44"] `shouldBe` onBch

    it "refuse a type other than bip44 and bip84" $
      parsedLine ["receive", "--type", "bip49"] `shouldBe` Left "option --type: The type of an account is bip44 or bip84"

    -- 2^31 = 2147483648, the number of hardened indexes of BIP32
    it "take the index 2147483647, the highest an account can have" $
      parsedLine ["switch", "--portfolio", "2147483647"] `shouldBe` Right (CommandSwitch Nothing (Just 2147483647))

    it "refuse a portfolio of an index of 2^31 or more" $
      parsedLine ["switch", "--portfolio", "2147483648"]
        `shouldBe` Left "option --portfolio: The --portfolio value 2147483648 is too large: the index of a portfolio is below 2147483648, which is 2^31"

    it "refuse a portfolio of an index of 2^31 or more as the argument of switch" $
      parsedLine ["switch", "2147483648"]
        `shouldBe` Left "The portfolio 2147483648 is too large: the index of a portfolio is below 2147483648, which is 2^31"

    it "refuse a wallet of an index of 2^31 or more" $
      parsedLine ["switch", "--wallet", "2147483648", "--portfolio", "0"]
        `shouldBe` Left "option --wallet: The --wallet value 2147483648 is too large: the index of a wallet is below 2147483648, which is 2^31"

    -- 2^64 + 1 would wrap around to 1 as a number of 64 bits
    it "refuse a portfolio of an index of 2^64 + 1, which would wrap around to portfolio1" $
      parsedLine ["deleteportfolio", "18446744073709551617"]
        `shouldBe` Left "The portfolio 18446744073709551617 is too large: the index of a portfolio is below 2147483648, which is 2^31"

    it "leave out --account, which the active portfolio replaces" $
      parsedLine ["receive", "--account", "main"] `shouldBe` Left "Invalid option `--account'"

    -- switch takes portfolio0 of the wallet then ('switchTo')
    it "make switch take --wallet alone, without a portfolio" $
      parsedLine ["switch", "--wallet", "1"] `shouldBe` Right (CommandSwitch (Just 1) Nothing)

    it "make show --portfolio name a portfolio of a file of a wallet" $
      parsedLine ["show", "wallet.jsonl", "--portfolio", "1"]
        `shouldBe` Right (CommandShow (Target Nothing Nothing) (ImportScope Nothing (Just 1)) (Just (ShownFile "wallet.jsonl")))

    it "make show --wallet name a wallet of a file of a wallet, by its fingerprint" $
      parsedLine ["show", "wallet.jsonl", "--wallet", "bc560f95", "--portfolio", "1"]
        `shouldBe` Right (CommandShow (Target Nothing Nothing) (ImportScope (Just "bc560f95") (Just 1)) (Just (ShownFile "wallet.jsonl")))

    it "make importwallet read the whole file without options" $
      parsedLine ["importwallet", "all.jsonl"] `shouldBe` Right (CommandImportWallet (ImportScope Nothing Nothing) "all.jsonl")

    it "make importwallet take one wallet of the file by its fingerprint with --wallet" $
      parsedLine ["importwallet", "all.jsonl", "--wallet", "bc560f95"]
        `shouldBe` Right (CommandImportWallet (ImportScope (Just "bc560f95") Nothing) "all.jsonl")

    it "make importwallet take one portfolio of one wallet of the file with --wallet and --portfolio" $
      parsedLine ["importwallet", "all.jsonl", "--wallet", "bc560f95", "--portfolio", "1"]
        `shouldBe` Right (CommandImportWallet (ImportScope (Just "bc560f95") (Just 1)) "all.jsonl")

    it "read the fingerprint of the --wallet of importwallet in capitals too" $
      parsedLine ["importwallet", "all.jsonl", "--wallet", "BC560F95"]
        `shouldBe` Right (CommandImportWallet (ImportScope (Just "bc560f95") Nothing) "all.jsonl")

    -- The other commands name a wallet by its index, which a file does
    -- not hold
    it "refuse an index as the --wallet of importwallet, and say that a file names a wallet by its fingerprint" $
      parsedLine ["importwallet", "all.jsonl", "--wallet", "0"]
        `shouldBe` Left "option --wallet: The --wallet value 0 is not a fingerprint. A file names each wallet by its fingerprint, 8 hexadecimal digits such as bc560f95, and not by an index: show lists the wallets of a file"

  describe "The options of the commands of portfolios and payments" $ do
    it "make newportfolio ask for a mnemonic the user has with --existing" $
      parsedLine ["newportfolio", "--existing"]
        `shouldBe` Right (CommandNewPortfolio (NewPortfolio True False Nothing False Nothing Nothing False False))

    it "make newportfolio refuse a mnemonic of no wallet of the database with --existing --match" $
      parsedLine ["newportfolio", "--existing", "--match"]
        `shouldBe` Right (CommandNewPortfolio (NewPortfolio True True Nothing False Nothing Nothing False False))

    it "make newportfolio print the words of the new mnemonic on one line with --one-line" $
      parsedLine ["newportfolio", "--one-line"]
        `shouldBe` Right (CommandNewPortfolio (NewPortfolio False False Nothing False Nothing Nothing True False))

    it "make newportfolio print the master private key of the new mnemonic with --show-master-key" $
      parsedLine ["newportfolio", "--show-master-key"]
        `shouldBe` Right (CommandNewPortfolio (NewPortfolio False False Nothing False Nothing Nothing False True))

    it "make checkmnemonic check the mnemonic and print no key, without an option" $
      parsedLine ["checkmnemonic"] `shouldBe` Right (CommandCheckMnemonic (CheckMnemonic False))

    it "make checkmnemonic print the master private key of the wallet with --show-master-key" $
      parsedLine ["checkmnemonic", "--show-master-key"] `shouldBe` Right (CommandCheckMnemonic (CheckMnemonic True))

    it "make mnemonicsheet write the sheet of the default mnemonic to the file named, without an option" $
      parsedLine ["mnemonicsheet", "sheet.pdf"] `shouldBe` Right (CommandMnemonicSheet (MnemonicSheetOptions "sheet.pdf" Nothing Nothing Nothing))

    it "give mnemonicsheet the --entropy, --shares and --threshold of newportfolio" $
      parsedLine ["mnemonicsheet", "shares.pdf", "--entropy", "32", "--shares", "3", "--threshold", "2"]
        `shouldBe` Right (CommandMnemonicSheet (MnemonicSheetOptions "shares.pdf" (Just 32) (Just 3) (Just 2)))

    it "refuse mnemonicsheet without a file to write" $
      either (const "refused") show (parsedLine ["mnemonicsheet"]) `shouldBe` "refused"

    -- The specification: recover stops after 10 portfolios in a row where
    -- it found nothing
    it "give recover a gap of 10 portfolios without --portfolio-gap" $
      parsedLine ["recover"] `shouldBe` Right (CommandRecover (Recover 10))

    it "give recover the gap of --portfolio-gap" $
      parsedLine ["recover", "--portfolio-gap", "20"] `shouldBe` Right (CommandRecover (Recover 20))

    -- Neither option given: every portfolio of every wallet
    it "make exportwallet name no wallet and no portfolio without options" $
      parsedLine ["exportwallet", "all.jsonl"] `shouldBe` Right (CommandExportWallet Nothing Nothing "all.jsonl")

    it "make exportwallet take a wallet alone by --wallet, to write that wallet" $
      parsedLine ["exportwallet", "one.jsonl", "--wallet", "1"] `shouldBe` Right (CommandExportWallet (Just 1) Nothing "one.jsonl")

    -- The wallet is then the one of the active portfolio, as for switch
    it "make exportwallet take a portfolio alone by --portfolio" $
      parsedLine ["exportwallet", "one.jsonl", "--portfolio", "2"] `shouldBe` Right (CommandExportWallet Nothing (Just 2) "one.jsonl")

    it "make exportwallet take a wallet and a portfolio together" $
      parsedLine ["exportwallet", "one.jsonl", "--wallet", "1", "--portfolio", "2"]
        `shouldBe` Right (CommandExportWallet (Just 1) (Just 2) "one.jsonl")

    it "make deletewallet take the wallet by its index, as its argument" $
      parsedLine ["deletewallet", "1"] `shouldBe` Right (CommandDeleteWallet 1 False)

    it "make deletewallet take the wallet by the name portfolios shows too" $
      parsedLine ["deletewallet", "wallet1"] `shouldBe` Right (CommandDeleteWallet 1 False)

    it "make deletewallet answer its question with --yes" $
      parsedLine ["deletewallet", "1", "--yes"] `shouldBe` Right (CommandDeleteWallet 1 True)

    it "refuse a word that names no wallet as the argument of deletewallet" $
      parsedLine ["deletewallet", "savings"] `shouldBe` Left "The wallet savings is not an index, such as 0, or a name, such as wallet0"

    -- A command that deletes never falls back on a default: the usage line
    -- shows its target without brackets, and the parser refuses it alone
    it "refuse deletewallet without its wallet, which it never takes from the active portfolio" $
      parsedLine ["deletewallet"] `shouldBe` Left "Missing: WALLET"

    it "make deleteportfolio take the portfolio by its index, as its argument" $
      parsedLine ["deleteportfolio", "2"] `shouldBe` Right (CommandDeletePortfolio 2 False)

    it "make deleteportfolio take the portfolio by the name portfolios shows too" $
      parsedLine ["deleteportfolio", "portfolio2"] `shouldBe` Right (CommandDeletePortfolio 2 False)

    it "make deleteportfolio answer its question with --yes" $
      parsedLine ["deleteportfolio", "2", "--yes"] `shouldBe` Right (CommandDeletePortfolio 2 True)

    it "refuse a word that names no portfolio as the argument of deleteportfolio" $
      parsedLine ["deleteportfolio", "travel"] `shouldBe` Left "The portfolio travel is not an index, such as 0, or a name, such as portfolio0"

    it "refuse deleteportfolio without its portfolio, which it never takes from the active portfolio" $
      parsedLine ["deleteportfolio"] `shouldBe` Left "Missing: PORTFOLIO"

    it "make --change-back of preparetx take an address" $
      (.changeBack) <$> preparedWith ["--change-back", "1KEn7jEXa7KCLeZy59dka5qRJBLnPMmrLj"]
        `shouldBe` Right (Just (BackToNamed (NamedByAddress "1KEn7jEXa7KCLeZy59dka5qRJBLnPMmrLj")))

    it "make --change-back of preparetx take the path of an address, with its type" $
      (.changeBack) <$> preparedWith ["--change-back", "bip84/1/0"]
        `shouldBe` Right (Just (BackToNamed (NamedByPath Bip84 AddrInternal 0)))

    it "read largest after --change-back of preparetx as the largest coin spent, not as an address" $
      (.changeBack) <$> preparedWith ["--change-back", "largest"]
        `shouldBe` Right (Just BackToLargestCoin)

    it "read smallest after --change-back of preparetx as the smallest coin spent, not as an address" $
      (.changeBack) <$> preparedWith ["--change-back", "smallest"]
        `shouldBe` Right (Just BackToSmallestCoin)

    it "read each --spend-from-tag of preparetx as a tag whose coins to spend" $
      map tagText . (.spendFromTags) <$> preparedWith ["--spend-from-tag", "kyc", "--spend-from-tag", "cold"]
        `shouldBe` Right ["kyc", "cold"]

    it "read --tag and --address-tag of txs as the two kinds of tags to keep the transactions of" $
      parsedLine ["txs", "--tag", "rent", "--address-tag", "kyc", "--address-tag", "cold"]
        `shouldBe` (\rent kyc cold -> CommandTxs Nothing (TxTagFilter [rent] [kyc, cold] False) (OnScreen Nothing) (Paging Nothing Nothing Nothing)) <$> readTag "rent" <*> readTag "kyc" <*> readTag "cold"

    it "read --untagged of txs as the transactions left to sort, besides the tags to keep" $
      parsedLine ["txs", "--untagged", "--tag", "rent"]
        `shouldBe` (\rent -> CommandTxs Nothing (TxTagFilter [rent] [] True) (OnScreen Nothing) (Paging Nothing Nothing Nothing)) <$> readTag "rent"

    it "read --untagged of the other listings as the rows without a tag" $ do
      let untagged = TagFilter [] True
          whole = Target Nothing Nothing
          page = Paging Nothing Nothing Nothing
      parsedLine ["addresses", "--untagged"] `shouldBe` Right (CommandAddresses whole False untagged (OnScreen Nothing) page)
      parsedLine ["balances", "--untagged"] `shouldBe` Right (CommandBalances whole untagged (OnScreen Nothing) page)
      parsedLine ["coins", "--untagged"] `shouldBe` Right (CommandCoins whole False untagged (OnScreen Nothing) page)
      parsedLine ["contacts", "--untagged"] `shouldBe` Right (CommandContacts Nothing untagged (OnScreen Nothing) page)
      parsedLine ["pendingtxs", "--untagged"] `shouldBe` Right (CommandPendingTxs Nothing (TxTagFilter [] [] True) (OnScreen Nothing) page)

    it "read --untagged beside --tag of a listing as both to keep" $
      parsedLine ["coins", "--tag", "kyc", "--untagged"]
        `shouldBe` (\kyc -> CommandCoins (Target Nothing Nothing) False (TagFilter [kyc] True) (OnScreen Nothing) (Paging Nothing Nothing Nothing)) <$> readTag "kyc"

    it "refuse a tag with a slash before the command runs, with the rule it breaks" $
      parsedLine ["addresses", "--tag", "bank/kyc"]
        `shouldBe` Left "option --tag: \"bank/kyc\" is not a tag: a tag is made of the letters a to z, digits, - and _"

    it "refuse a colour of a tag that is none of the six, with the colours to choose from" $
      parsedLine ["tag", "kyc", "--color", "pink"]
        `shouldBe` Left "option --color: \"pink\" is not a colour of a tag. Choose one of red, green, yellow, blue, magenta, cyan, or none to remove the colour."

    -- The user typed the tag first, then the address
    it "tell the order of the words of tag when an address comes where a tag goes" $
      parsedLine ["tag", "kyc", "1KEn7jEXa7KCLeZy59dka5qRJBLnPMmrLj"]
        `shouldBe` Left "1KEn7jEXa7KCLeZy59dka5qRJBLnPMmrLj reads as an address, not as a tag. What to tag comes first, then the tags: ADDRESS|TXID|PATH TAG"

    it "read the words of renametag as the tag, then its new name, both in small letters" $
      parsedLine ["renametag", "KYC", "Verified"]
        `shouldBe` (\kyc verified -> CommandRenameTag (RenameTag kyc verified False)) <$> readTag "kyc" <*> readTag "verified"

    it "read --merge of renametag as the wish to merge the tag into the second one" $
      parsedLine ["renametag", "saving", "savings", "--merge"]
        `shouldBe` (\saving savings -> CommandRenameTag (RenameTag saving savings True)) <$> readTag "saving" <*> readTag "savings"

    it "refuse a new name of renametag with a slash before the command runs, with the rule it breaks" $
      parsedLine ["renametag", "kyc", "bank/kyc"]
        `shouldBe` Left "\"bank/kyc\" is not a tag: a tag is made of the letters a to z, digits, - and _"

    it "refuse a new name of renametag made of two words, with how to join them" $
      parsedLine ["renametag", "kyc", "know customer"]
        `shouldBe` Left "\"know customer\" has a space: the name of a tag is one word. Join its words with - or _, as in know-customer."

    it "read the words of tag as what to tag, then the tags" $
      parsedLine ["tag", "bip84/0/3", "kyc", "cold", "--color", "RED"]
        `shouldBe` (\kyc cold -> CommandTag (TagOptions "bip84/0/3" [kyc, cold] (Just (Just TagRed)) False False False (Target Nothing Nothing))) <$> readTag "kyc" <*> readTag "cold"

    it "read --with-change of tag, which asks for the change down the chain too" $
      parsedLine ["tag", "bip84/0/3", "kyc", "--with-change"]
        `shouldBe` (\kyc -> CommandTag (TagOptions "bip84/0/3" [kyc] Nothing False False True (Target Nothing Nothing))) <$> readTag "kyc"

    it "read --lock-coins of tag after a tag alone, which makes the tag lock its coins" $
      parsedLine ["tag", "savings", "--lock-coins"]
        `shouldBe` Right (CommandTag (TagOptions "savings" [] Nothing True False False (Target Nothing Nothing)))

    it "read --unlock-coins of tag after a tag alone, which stops the tag from locking its coins" $
      parsedLine ["tag", "savings", "--unlock-coins"]
        `shouldBe` Right (CommandTag (TagOptions "savings" [] Nothing False True False (Target Nothing Nothing)))

    it "read --with-change of untag" $
      parsedLine ["untag", "bip84/0/3", "kyc", "--with-change"]
        `shouldBe` (\kyc -> CommandUntag (Untag "bip84/0/3" [kyc] True (Target Nothing Nothing))) <$> readTag "kyc"

    it "make --spend-from of preparetx take the path of an address, with its type" $
      (.spendFrom) <$> preparedWith ["--spend-from", "bip44/1/0", "--spend-from", "bip84/0/2"]
        `shouldBe` Right [NamedByPath Bip44 AddrInternal 0, NamedByPath Bip84 AddrExternal 2]

    -- preparetx has no --type, and spends both types of address
    it "refuse a --spend-from path without its type, naming the two forms with a type" $
      preparedWith ["--spend-from", "/1/0"]
        `shouldBe` Left "option --spend-from: The path /1/0 needs the type of its account in front, as the views write it: bip44/1/0 or bip84/1/0"

    it "refuse a --spend-from word of the shape of a path that is no path, with the two forms of a path" $
      preparedWith ["--spend-from", "m/1/0"]
        `shouldBe` Left "option --spend-from: The path m/1/0 is not that of an address: /0/i is the receiving address i, and /1/i the change address i"

    it "make --spend-from of preparesendall take the path of an address too" $
      case parsedLine ["preparesendall", "1JCq8Aa9d9rg4T4XV93RV3DMxd5u7GkSSU", "--spend-from", "bip44/1/0"] of
        Right (CommandPrepareSendAll opts) -> opts.spendFrom `shouldBe` [NamedByPath Bip44 AddrInternal 0]
        other -> expectationFailure ("Not a preparesendall: " <> show other)

    it "make signmsg take the path of the address that signs, with its type" $
      case parsedLine ["signmsg", "bip84/0/3", "Hello"] of
        Right (CommandSignMsg opts) -> opts.address `shouldBe` NamedByPath Bip84 AddrExternal 3
        other -> expectationFailure ("Not a signmsg: " <> show other)

    it "make signmsg take an address, as typed" $
      case parsedLine ["signmsg", "1KEn7jEXa7KCLeZy59dka5qRJBLnPMmrLj", "Hello"] of
        Right (CommandSignMsg opts) -> opts.address `shouldBe` NamedByAddress "1KEn7jEXa7KCLeZy59dka5qRJBLnPMmrLj"
        other -> expectationFailure ("Not a signmsg: " <> show other)

    it "refuse --change-back without its address" $
      preparedWith ["--change-back"] `shouldBe` Left "The option `--change-back` expects an argument."

    it "make --coin-type and --change-type take bip44 or bip84" $
      ((\opts -> (opts.coinType, opts.changeType)) <$> preparedWith ["--coin-type", "bip84", "--change-type", "bip44"])
        `shouldBe` Right (Just Bip84, Just Bip44)

    it "refuse a --coin-type other than bip44 and bip84" $
      preparedWith ["--coin-type", "bip49"] `shouldBe` Left "option --coin-type: The type of an account is bip44 or bip84"

    it "leave out --new-change: the change goes to a new address unless --change-back says" $
      preparedWith ["--new-change"] `shouldBe` Left "Invalid option `--new-change'"

  describe "The options of sync" $ do
    it "sync what changed on every network, with no option" $
      parsedLine ["sync"] `shouldBe` Right (CommandSync Nothing Incremental)

    it "sync everything again with --full" $
      parsedLine ["sync", "--full"] `shouldBe` Right (CommandSync Nothing Full)

    it "follow the new chain of the network named with --new-chain" $
      parsedLine ["sync", "--network", "bch", "--new-chain"] `shouldBe` Right (CommandSync (Just bch) NewChain)

    -- A sync on a new chain is a full one already
    it "follow a new chain with --new-chain and --full together" $
      parsedLine ["sync", "--network", "bch", "--full", "--new-chain"] `shouldBe` Right (CommandSync (Just bch) NewChain)

  describe "The options of send" $ do
    -- Every option of a payment but --coin, each with a value that is not
    -- its default. A coin is checked apart, by its text: a short id has
    -- no other form to write here.
    let everyOption =
          [ "--network", "bch",
            "--fee-rate", "7",
            "--recipients-pay",
            "--min-conf", "2",
            "--coin-type", "bip44",
            "--strategy", "largest-first",
            "--label", "rent of October",
            "--tag", "rent",
            "--spend-from", "bip44/0/3",
            "--select",
            "--yes",
            "--change-back", "largest",
            "--change-type", "bip84",
            "--tag-change",
            "--repeat-address"
          ]
        everyOptionRead =
          PrepareTx
            { recipients = ["1JCq8Aa9d9rg4T4XV93RV3DMxd5u7GkSSU:0.001"],
              repeatAddress = True,
              network = Just bch,
              feeRate = Just (FeeRateOf 7),
              feePayer = RecipientsPay,
              minConf = 2,
              coinType = Just Bip44,
              strategy = LargestFirst,
              output = Nothing,
              label = Just "rent of October",
              tags = [rentTag],
              coins = [],
              spendFrom = [NamedByPath Bip44 AddrExternal 3],
              spendFromTags = [],
              select = True,
              yes = True,
              changeBack = Just BackToLargestCoin,
              changeType = Just Bip84,
              tagChange = True
            }

    it "are each read into the payment it describes" $
      parsedLine (["send", payment] <> everyOption) `shouldBe` Right (CommandSend everyOptionRead)

    it "are read by preparetx into the same payment" $
      parsedLine (["preparetx", payment] <> everyOption) `shouldBe` Right (CommandPrepareTx everyOptionRead)

    it "read each coin of --coin, by its short id or with its txid in full" $
      case parsedLine ["send", payment, "--coin", "a1075db5:1", "--coin", "a1075db5c0ffee00000000000000000000000000000000000000000000000001:0"] of
        Right (CommandSend opts) -> map coinRefText opts.coins `shouldBe` ["a1075db5:1", "a1075db5c0ffee00000000000000000000000000000000000000000000000001:0"]
        other -> expectationFailure ("Not a send: " <> show other)

    -- No fee rate, the sender pays, any confirmations, coins of both types,
    -- the strategy that wastes the least, and a new change address
    it "have the defaults of preparetx when they are left out" $
      parsedLine ["send", payment]
        `shouldBe` Right
          ( CommandSend
              PrepareTx
                { recipients = ["1JCq8Aa9d9rg4T4XV93RV3DMxd5u7GkSSU:0.001"],
                  repeatAddress = False,
                  network = Nothing,
                  feeRate = Nothing,
                  feePayer = SenderPays,
                  minConf = 0,
                  coinType = Nothing,
                  strategy = LeastWaste,
                  output = Nothing,
                  label = Nothing,
                  tags = [],
                  coins = [],
                  spendFrom = [],
                  spendFromTags = [],
                  select = False,
                  yes = False,
                  changeBack = Nothing,
                  changeType = Nothing,
                  tagChange = False
                }
          )

    it "read each --tag as a tag of the transaction, in the order typed and in small letters" $
      case parsedLine ["send", payment, "--tag", "Rent", "--tag", "flat-2"] of
        Right (CommandSend opts) -> map tagText opts.tags `shouldBe` ["rent", "flat-2"]
        other -> expectationFailure ("Not a send: " <> show other)

    -- --tag says what the payment is for, and --spend-from-tag which
    -- coins pay it
    it "read --tag as no coin to spend, and --spend-from-tag as no tag of the transaction" $
      case parsedLine ["send", payment, "--tag", "rent", "--spend-from-tag", "cold"] of
        Right (CommandSend opts) -> (map tagText opts.tags, map tagText opts.spendFromTags) `shouldBe` (["rent"], ["cold"])
        other -> expectationFailure ("Not a send: " <> show other)

    -- The second word typed alone would be read as a recipient, so the
    -- refusal shows the option repeated
    it "refuse a --tag of two words before the command runs, and show --tag repeated for two tags" $
      parsedLine ["send", payment, "--tag", "my rent"]
        `shouldBe` Left "option --tag: \"my rent\" has a space: a tag is one word. Repeat --tag for several tags, as in --tag my --tag rent."

    -- send writes no file: the transaction it makes is sent, not carried
    it "leave out --output" $
      parsedLine ["send", payment, "--output", "unsigned.psbt"] `shouldBe` Left "Invalid option `--output'"

    it "need a recipient" $
      parsedLine ["send"] `shouldBe` Left "Missing: ADDRESS:AMOUNT"

  describe "The options of sendall" $ do
    -- Every option of a payment of all the coins but --coin, each with a
    -- value that is not its default. A coin is checked apart, by its text.
    let recipient = "1JCq8Aa9d9rg4T4XV93RV3DMxd5u7GkSSU"
        everyOption =
          [ "--network", "bch",
            "--fee-rate", "7",
            "--min-conf", "2",
            "--coin-type", "bip44",
            "--label", "closing the account",
            "--tag", "cold",
            "--spend-from", "bip44/0/3",
            "--leave-dust",
            "--yes",
            "--repeat-address"
          ]
        everyOptionRead =
          PrepareSendAll
            { recipients = ["1JCq8Aa9d9rg4T4XV93RV3DMxd5u7GkSSU"],
              repeatAddress = True,
              network = Just bch,
              feeRate = Just (FeeRateOf 7),
              minConf = 2,
              coinType = Just Bip44,
              output = Nothing,
              label = Just "closing the account",
              tags = [coldTag],
              coins = [],
              spendFrom = [NamedByPath Bip44 AddrExternal 3],
              spendFromTags = [],
              dustCoins = Just LeaveDust,
              yes = True
            }

    it "are each read into the payment of all the coins it describes" $
      parsedLine (["sendall", recipient] <> everyOption) `shouldBe` Right (CommandSendAll everyOptionRead)

    it "are read by preparesendall into the same payment" $
      parsedLine (["preparesendall", recipient] <> everyOption) `shouldBe` Right (CommandPrepareSendAll everyOptionRead)

    it "read --spend-dust as the answer that spends the coins worth less than their fee" $
      case parsedLine ["sendall", recipient, "--spend-dust"] of
        Right (CommandSendAll opts) -> opts.dustCoins `shouldBe` Just SpendDust
        other -> expectationFailure ("Not a sendall: " <> show other)

    it "are read by sweep into the sweep that preparesweep reads from the same options" $ do
      let options = ["--key-file", "keys.txt", "--sweep-to", recipient, "--network", "bch", "--leave-dust", "--label", "old wallet", "--fee-rate", "10", "--yes"]
      case (parsedLine ("sweep" : options), parsedLine ("preparesweep" : options)) of
        (Right (CommandSweep swept), Right (CommandPrepareSweep prepared)) -> swept `shouldBe` prepared
        other -> expectationFailure ("Not a sweep and a preparesweep: " <> show other)

    it "read --repeat-address of sweep and of preparesweep as the switch that takes a destination twice, off without it" $ do
      let repeatsOf command options = case parsedLine ([command, "--sweep-to", recipient] <> options) of
            Right (CommandSweep opts) -> Right opts.repeatAddress
            Right (CommandPrepareSweep opts) -> Right opts.repeatAddress
            other -> Left ("Not a sweep: " <> show other)
      repeatsOf "sweep" ["--repeat-address"] `shouldBe` Right True
      repeatsOf "sweep" [] `shouldBe` Right False
      repeatsOf "preparesweep" ["--sweep-from", recipient, "--repeat-address"] `shouldBe` Right True
      repeatsOf "preparesweep" ["--sweep-from", recipient] `shouldBe` Right False

    it "read --leave-dust and --spend-dust of preparesweep as the answer about the coins worth less than their fee, and neither as no answer" $ do
      let dustOf options = case parsedLine (["preparesweep", "--sweep-from", recipient, "--sweep-to", recipient] <> options) of
            Right (CommandPrepareSweep opts) -> Right opts.dustCoins
            other -> Left ("Not a preparesweep: " <> show other)
      dustOf ["--leave-dust"] `shouldBe` Right (Just LeaveDust)
      dustOf ["--spend-dust"] `shouldBe` Right (Just SpendDust)
      dustOf [] `shouldBe` Right Nothing

    it "read each coin of --coin, by its short id or with its txid in full" $
      case parsedLine ["sendall", recipient, "--coin", "a1075db5:1", "--coin", "a1075db5c0ffee00000000000000000000000000000000000000000000000001:0"] of
        Right (CommandSendAll opts) -> map coinRefText opts.coins `shouldBe` ["a1075db5:1", "a1075db5c0ffee00000000000000000000000000000000000000000000000001:0"]
        other -> expectationFailure ("Not a sendall: " <> show other)

    it "take a recipient with a fixed amount beside one without" $
      case parsedLine ["sendall", "1JCq8Aa9d9rg4T4XV93RV3DMxd5u7GkSSU:0.001", "1KEn7jEXa7KCLeZy59dka5qRJBLnPMmrLj"] of
        Right (CommandSendAll opts) -> opts.recipients `shouldBe` ["1JCq8Aa9d9rg4T4XV93RV3DMxd5u7GkSSU:0.001", "1KEn7jEXa7KCLeZy59dka5qRJBLnPMmrLj"]
        other -> expectationFailure ("Not a sendall: " <> show other)

    -- No fee rate, any confirmations, coins of both types, and a question
    -- about the coins worth less than their fee
    it "have the defaults of preparesendall when they are left out" $
      parsedLine ["sendall", recipient]
        `shouldBe` Right
          ( CommandSendAll
              PrepareSendAll
                { recipients = ["1JCq8Aa9d9rg4T4XV93RV3DMxd5u7GkSSU"],
                  repeatAddress = False,
                  network = Nothing,
                  feeRate = Nothing,
                  minConf = 0,
                  coinType = Nothing,
                  output = Nothing,
                  label = Nothing,
                  tags = [],
                  coins = [],
                  spendFrom = [],
                  spendFromTags = [],
                  dustCoins = Nothing,
                  yes = False
                }
          )

    it "read each --tag as a tag of the transaction, in the order typed" $
      case parsedLine ["sendall", recipient, "--tag", "cold", "--tag", "moving"] of
        Right (CommandSendAll opts) -> map tagText opts.tags `shouldBe` ["cold", "moving"]
        other -> expectationFailure ("Not a sendall: " <> show other)

    -- sendall writes no file: the transaction it makes is sent, not carried
    it "leave out --output" $
      parsedLine ["sendall", recipient, "--output", "unsigned.psbt"] `shouldBe` Left "Invalid option `--output'"

    it "need a recipient" $
      parsedLine ["sendall"] `shouldBe` Left "Missing: ADDRESS[:AMOUNT]"

  describe "The options of bumpfee" $ do
    let bumpedWith options = case parsedLine (["bumpfee", "a1075db5"] <> options) of
          Right (CommandBumpFee opts) -> Right opts
          Right other -> Left ("Not a bumpfee: " <> show other)
          Left err -> Left err
    it "have the recipients pay what the change cannot with --recipients-pay" $
      ((.recipientsPay) <$> bumpedWith ["--recipients-pay"]) `shouldBe` Right RecipientsPay

    it "leave it to the sender to pay by default" $
      ((.recipientsPay) <$> bumpedWith []) `shouldBe` Right SenderPays

    -- A short letter keeps one meaning across the commands
    it "take -r for --recipients-pay, as preparetx does" $
      ((.recipientsPay) <$> bumpedWith ["-r"]) `shouldBe` Right RecipientsPay

  describe "The options of exportjournal" $ do
    it "name the file alone for the journal of every transaction" $
      parsedLine ["exportjournal", "hw.ledger"] `shouldBe` Right (CommandExportJournal "hw.ledger" noTxTagFilter)

    it "name the tags to keep the transactions of, as txs does" $
      parsedLine ["exportjournal", "business.ledger", "--tag", "business", "--address-tag", "kyc", "--untagged"]
        `shouldBe` (\business kyc -> CommandExportJournal "business.ledger" (TxTagFilter [business] [kyc] True)) <$> readTag "business" <*> readTag "kyc"

  describe "The options of exportsheet" $ do
    it "name the file alone for the spreadsheet of every row" $
      parsedLine ["exportsheet", "wallet.ods"] `shouldBe` Right (CommandExportSheet "wallet.ods" (TagFilter [] False))

    it "name the tags to keep the rows of, and the rows without a tag" $
      parsedLine ["exportsheet", "business.ods", "--tag", "business", "--tag", "rent", "--untagged"]
        `shouldBe` (\business rent -> CommandExportSheet "business.ods" (TagFilter [business, rent] True)) <$> readTag "business" <*> readTag "rent"

    -- A tab of addresses keeps its rows by the tags of the address with
    -- --tag already
    it "have no --address-tag" $
      parsedLine ["exportsheet", "business.ods", "--address-tag", "kyc"] `shouldBe` Left "Invalid option `--address-tag'"

  describe "The options of report" $ do
    it "name the file, the report and the day the report is for" $
      parsedLine ["report", "balances.pdf", "--balances", "--date", "2025-12-31"]
        `shouldBe` Right (CommandReport (ReportOptions "balances.pdf" BalanceReport (Just (fromGregorian 2025 12 31)) Nothing Nothing Nothing Nothing Nothing [] Nothing Nothing Nothing Nothing [] noTxTagFilter))

    it "leave the day out for a report of now" $
      parsedLine ["report", "balances.pdf", "--balances"] `shouldBe` Right (CommandReport (ReportOptions "balances.pdf" BalanceReport Nothing Nothing Nothing Nothing Nothing Nothing [] Nothing Nothing Nothing Nothing [] noTxTagFilter))

    it "name the holder of the wallets" $
      parsedLine ["report", "balances.pdf", "--balances", "--holder", "Jane Doe"]
        `shouldBe` Right (CommandReport (ReportOptions "balances.pdf" BalanceReport Nothing Nothing Nothing Nothing Nothing (Just "Jane Doe") [] Nothing Nothing Nothing Nothing [] noTxTagFilter))

    -- 420.50 is the number 42050 with two decimals
    it "take a price for each network, with their currency in capitals and their source" $
      parsedLine ["report", "balances.pdf", "--balances", "--price", "btc=87500", "--price", "bch=420.50", "--currency", "chf", "--price-source", "The tax office"]
        `shouldBe` Right (CommandReport (ReportOptions "balances.pdf" BalanceReport Nothing Nothing Nothing Nothing Nothing Nothing [(btc, Price 87500 0), (bch, Price 42050 2)] (Just "CHF") (Just "The tax office") Nothing Nothing [] noTxTagFilter))

    it "refuse a price without its network" $
      parsedLine ["report", "balances.pdf", "--balances", "--price", "87500"]
        `shouldBe` Left "option --price: The price 87500 does not read: give a network and its price, such as btc=87500"

    it "refuse a price with a comma" $
      parsedLine ["report", "balances.pdf", "--balances", "--price", "btc=87,500"]
        `shouldBe` Left "option --price: The price 87,500 does not read: give a number with a point before its decimals, such as 87500 or 87500.50"

    it "refuse a currency that is not a code of three letters" $
      parsedLine ["report", "balances.pdf", "--balances", "--currency", "francs"]
        `shouldBe` Left "option --currency: The currency francs does not read: give its code of three letters, such as CHF, EUR or USD"

    it "must say which report to write" $
      parsedLine ["report", "balances.pdf"] `shouldBe` Left "Missing: (--balances | --transactions | --payment TXID)"

    it "name the account statement, and the first and the last day of its period" $
      parsedLine ["report", "2025.pdf", "--transactions", "--from", "2025-01-01", "--to", "2025-12-31"]
        `shouldBe` Right (CommandReport (ReportOptions "2025.pdf" AccountReport Nothing (Just (fromGregorian 2025 1 1)) (Just (fromGregorian 2025 12 31)) Nothing Nothing Nothing [] Nothing Nothing Nothing Nothing [] noTxTagFilter))

    it "leave the period out for the account statement of the month that is running" $
      parsedLine ["report", "month.pdf", "--transactions"] `shouldBe` Right (CommandReport (ReportOptions "month.pdf" AccountReport Nothing Nothing Nothing Nothing Nothing Nothing [] Nothing Nothing Nothing Nothing [] noTxTagFilter))

    it "name the one wallet and the one portfolio a report covers" $
      parsedLine ["report", "savings.pdf", "--balances", "--wallet", "1", "--portfolio", "2"]
        `shouldBe` Right (CommandReport (ReportOptions "savings.pdf" BalanceReport Nothing Nothing Nothing (Just 1) (Just 2) Nothing [] Nothing Nothing Nothing Nothing [] noTxTagFilter))

    it "name the tags an account statement keeps the transactions of, those of their addresses, and the transactions left to sort" $
      case parsedLine ["report", "business.pdf", "--transactions", "--tag", "business", "--tag", "rent", "--address-tag", "kyc", "--untagged"] of
        Right (CommandReport opts) -> Right opts.kept `shouldBe` ((\business rent kyc -> TxTagFilter [business, rent] [kyc] True) <$> readTag "business" <*> readTag "rent" <*> readTag "kyc")
        other -> expectationFailure (show other)

    it "name the tag a balance statement keeps the addresses of, and the addresses without a tag" $
      case parsedLine ["report", "kyc.pdf", "--balances", "--tag", "kyc", "--untagged"] of
        Right (CommandReport opts) -> Right (opts.kind, opts.kept) `shouldBe` ((\kyc -> (BalanceReport, TxTagFilter [kyc] [] True)) <$> readTag "kyc")
        other -> expectationFailure (show other)

    it "take one report at a time" $
      parsedLine ["report", "both.pdf", "--balances", "--transactions"] `shouldSatisfy` isLeft

    it "name the payment confirmation and its transaction, by its short id" $
      case parsedLine ["report", "rent.pdf", "--payment", "a1075db5"] of
        Right (CommandReport opts) | PaymentReport (ShortHash short) <- opts.kind -> (opts.file, shortIdText short) `shouldBe` ("rent.pdf", "a1075db5")
        other -> expectationFailure (show other)

    it "take the transaction of a payment confirmation by its txid in full" $
      case parsedLine ["report", "rent.pdf", "--payment", "ab00000000000000000000000000000000000000000000000000000000000001"] of
        Right (CommandReport opts) | PaymentReport (FullHash tid) <- opts.kind -> txHashToHex tid `shouldBe` "ab00000000000000000000000000000000000000000000000000000000000001"
        other -> expectationFailure (show other)

    it "name the other side of a payment, its purpose and each address to list" $
      case parsedLine ["report", "rent.pdf", "--payment", "a1075db5", "--counterparty", "ACME Ltd", "--purpose", "Invoice 42", "--address", "1JCq8Aa9d9rg4T4XV93RV3DMxd5u7GkSSU", "--address", "1PxH6Yutj49mRAabGvcTxnLkFZCuXDXvRJ"] of
        Right (CommandReport opts) ->
          (opts.counterparty, opts.purpose, opts.addresses)
            `shouldBe` (Just "ACME Ltd", Just "Invoice 42", ["1JCq8Aa9d9rg4T4XV93RV3DMxd5u7GkSSU", "1PxH6Yutj49mRAabGvcTxnLkFZCuXDXvRJ"])
        other -> expectationFailure (show other)

    it "refuse a payment named by a word that is neither a txid nor a short id" $
      parsedLine ["report", "rent.pdf", "--payment", "rent"]
        `shouldBe` Left "option --payment: The hash rent is not valid: give its 64 digits, or its short id, its first 8 digits, as the listings show it"

    it "take a payment confirmation alone, without a statement" $
      parsedLine ["report", "both.pdf", "--balances", "--payment", "a1075db5"] `shouldSatisfy` isLeft

    it "refuse a first day of a period that does not read as a day" $
      parsedLine ["report", "2025.pdf", "--transactions", "--from", "01-01-2025"]
        `shouldBe` Left "option --from: The date 01-01-2025 does not read: give a day as its year, its month and its day, such as 2025-12-31"

    it "refuse a date with the day first" $
      parsedLine ["report", "balances.pdf", "--balances", "--date", "31-12-2025"]
        `shouldBe` Left "option --date: The date 31-12-2025 does not read: give a day as its year, its month and its day, such as 2025-12-31"

    it "refuse a day the calendar does not have" $
      parsedLine ["report", "balances.pdf", "--balances", "--date", "2025-02-30"]
        `shouldBe` Left "option --date: The date 2025-02-30 does not read: give a day as its year, its month and its day, such as 2025-12-31"

    it "refuse a month or a day of one digit, which could be read both ways" $
      parsedLine ["report", "balances.pdf", "--balances", "--date", "2025-1-5"]
        `shouldBe` Left "option --date: The date 2025-1-5 does not read: give a day as its year, its month and its day, such as 2025-12-31"

  describe "The usage line of a command" $ do
    it "puts an option that may be left out in brackets, its names separated by a bar" $
      usageOf "receive" `shouldBe` "[-n|--network NETWORK] [-t|--type TYPE] [-l|--label LABEL] [--tag TAG]..."

    -- The recipients and the coins may be repeated, and the strategy has a
    -- default
    it "lists the options of preparetx as they are typed" $
      usageOf "preparetx"
        `shouldBe` "ADDRESS:AMOUNT... [--repeat-address] [-n|--network NETWORK] [-f|--fee-rate RATE] [-r|--recipients-pay] [-m|--min-conf INT] [--coin-type TYPE] [--strategy STRATEGY] [-o|--output FILENAME] [-l|--label LABEL] [--tag TAG]... [-c|--coin TXID:INDEX]... [--spend-from ADDRESS|PATH]... [--spend-from-tag TAG]... [--select] [-y|--yes] [--change-back ADDRESS|PATH|largest|smallest] [--change-type TYPE] [--tag-change]"

    -- The options of preparetx, in its order, without --output
    it "lists the options of send as those of preparetx, without --output" $
      usageOf "send"
        `shouldBe` "ADDRESS:AMOUNT... [--repeat-address] [-n|--network NETWORK] [-f|--fee-rate RATE] [-r|--recipients-pay] [-m|--min-conf INT] [--coin-type TYPE] [--strategy STRATEGY] [-l|--label LABEL] [--tag TAG]... [-c|--coin TXID:INDEX]... [--spend-from ADDRESS|PATH]... [--spend-from-tag TAG]... [--select] [-y|--yes] [--change-back ADDRESS|PATH|largest|smallest] [--change-type TYPE] [--tag-change]"

    it "lists the options of bumpfee, --recipients-pay after the fee rate" $
      usageOf "bumpfee" `shouldBe` "TXID [-f|--fee-rate RATE] [-r|--recipients-pay] [-o|--output FILENAME] [-y|--yes]"

    it "follows what may be given several times with ..., after the brackets of an optional one" $
      usageOf "preparesendall"
        `shouldBe` "ADDRESS[:AMOUNT]... [--repeat-address] [-n|--network NETWORK] [-f|--fee-rate RATE] [-m|--min-conf INT] [--coin-type TYPE] [-o|--output FILENAME] [-l|--label LABEL] [--tag TAG]... [-c|--coin TXID:INDEX]... [--spend-from ADDRESS|PATH]... [--spend-from-tag TAG]... [--spend-dust | --leave-dust] [-y|--yes]"

    -- The options of preparesendall, in its order, without --output
    it "lists the options of sendall as those of preparesendall, without --output" $
      usageOf "sendall"
        `shouldBe` "ADDRESS[:AMOUNT]... [--repeat-address] [-n|--network NETWORK] [-f|--fee-rate RATE] [-m|--min-conf INT] [--coin-type TYPE] [-l|--label LABEL] [--tag TAG]... [-c|--coin TXID:INDEX]... [--spend-from ADDRESS|PATH]... [--spend-from-tag TAG]... [--spend-dust | --leave-dust] [-y|--yes]"

    it "offers txs --compact, --verbose, --tabular and --csv as a choice, since a table has no room for every detail and a file is not the screen" $
      usageOf "txs" `shouldBe` "[-n|--network NETWORK] [--tag TAG]... [--address-tag TAG]... [--untagged] [--compact | (-v|--verbose) | --tabular | --csv FILENAME] [--after TXID] [--max INT] [--reverse | --no-reverse]"

    it "offers addresses --compact, --verbose, --tabular and --csv as a choice too" $
      usageOf "addresses" `shouldBe` "[-n|--network NETWORK] [-t|--type TYPE] [--change] [--tag TAG]... [--untagged] [--compact | (-v|--verbose) | --tabular | --csv FILENAME] [--after ADDRESS] [--max INT] [--reverse | --no-reverse]"

    it "offers portfolios --compact, --verbose, --tabular and --csv as a choice" $
      usageOf "portfolios" `shouldBe` "[--compact | (-v|--verbose) | --tabular | --csv FILENAME]"

    it "offers coins --compact, --verbose, --tabular and --csv as a choice" $
      usageOf "coins" `shouldBe` "[-n|--network NETWORK] [-t|--type TYPE] [--locked] [--tag TAG]... [--untagged] [--compact | (-v|--verbose) | --tabular | --csv FILENAME] [--after TXID:INDEX] [--max INT] [--reverse | --no-reverse]"

    it "offers pendingtxs --compact, --verbose, --tabular and --csv as a choice" $
      usageOf "pendingtxs" `shouldBe` "[-n|--network NETWORK] [--tag TAG]... [--address-tag TAG]... [--untagged] [--compact | (-v|--verbose) | --tabular | --csv FILENAME] [--after HASH] [--max INT] [--reverse | --no-reverse]"

    it "offers balances --compact, --verbose, --tabular and --csv as a choice" $
      usageOf "balances" `shouldBe` "[-n|--network NETWORK] [-t|--type TYPE] [--tag TAG]... [--untagged] [--compact | (-v|--verbose) | --tabular | --csv FILENAME] [--after ADDRESS] [--max INT] [--reverse | --no-reverse]"

    -- A listing that asks for no order takes that of the configuration
    -- on the screen
    it "reads --reverse of a listing as the reverse order, --no-reverse as the usual one, and neither as no order asked" $ do
      parsedLine ["txs", "--reverse"] `shouldBe` Right (CommandTxs Nothing noTxTagFilter (OnScreen Nothing) (Paging Nothing Nothing (Just ReverseOrder)))
      parsedLine ["txs", "--no-reverse"] `shouldBe` Right (CommandTxs Nothing noTxTagFilter (OnScreen Nothing) (Paging Nothing Nothing (Just UsualOrder)))
      parsedLine ["txs"] `shouldBe` Right (CommandTxs Nothing noTxTagFilter (OnScreen Nothing) (Paging Nothing Nothing Nothing))

    it "refuses --reverse and --no-reverse together" $
      parsedLine ["txs", "--reverse", "--no-reverse"] `shouldBe` Left "Invalid option `--no-reverse'"

    -- -r is --recipients-pay in preparetx, send and bumpfee
    it "gives --reverse no short letter" $
      parsedLine ["txs", "-r"] `shouldBe` Left "Invalid option `-r'"

    -- portfolios lists the wallets and their portfolios by their numbers,
    -- and takes no --after and no --max either
    it "gives portfolios no --reverse" $
      parsedLine ["portfolios", "--reverse"] `shouldBe` Left "Invalid option `--reverse'"

    it "puts the word of show, which may be left out, before its options" $
      usageOf "show" `shouldBe` "[PATH|ADDRESS|HASH|TXID:INDEX|FILENAME] [-n|--network NETWORK] [-t|--type TYPE] [--wallet FINGERPRINT] [-p|--portfolio INT]"

    -- The portfolio may be left out when --wallet is given
    it "puts the portfolio of switch, its argument or --portfolio, as an optional choice before the --wallet it may take" $
      usageOf "switch" `shouldBe` "[PORTFOLIO | (-p|--portfolio INT)] [--wallet INT]"

    it "puts a choice in parentheses, and a required option with two names too" $
      usageOf "verifymsg" `shouldBe` "(ADDRESS SIGNATURE MESSAGE | (-i|--input FILENAME)) [-n|--network NETWORK]"

    it "shows an option that has to be given at least once as required" $
      usageOf "preparesweep"
        `shouldBe` "[-n|--network NETWORK] [-k|--key-file FILENAME] [-w|--sweep-from ADDRESS]... [--address-file FILENAME] (-t|--sweep-to ADDRESS[:AMOUNT])... [--repeat-address] [-c|--coin TXID:INDEX]... [--spend-dust | --leave-dust] [-o|--output FILENAME] [-l|--label LABEL] [-f|--fee-rate RATE] [-y|--yes]"

    -- The options of preparesweep, in its order, without --output and
    -- without the three that name coins without their keys
    it "lists the options of sweep as those of preparesweep, without --output, --sweep-from, --address-file and --coin" $
      usageOf "sweep"
        `shouldBe` "[-n|--network NETWORK] [-k|--key-file FILENAME] (-t|--sweep-to ADDRESS[:AMOUNT])... [--repeat-address] [--spend-dust | --leave-dust] [-l|--label LABEL] [-f|--fee-rate RATE] [-y|--yes]"

  describe "What label labels" $ do
    it "is a transaction for a word of 64 hexadecimal digits" $
      case readLabelled "ab00000000000000000000000000000000000000000000000000000000000001" of
        Right (LabelledTx (FullHash tid)) -> txHashToHex tid `shouldBe` "ab00000000000000000000000000000000000000000000000000000000000001"
        other -> expectationFailure $ "Not a transaction: " <> show other

    it "is a transaction by its short id, for a word of 8 hexadecimal digits" $
      case readLabelled "a1075db5" of
        Right (LabelledTx (ShortHash short)) -> shortIdText short `shouldBe` "a1075db5"
        other -> expectationFailure $ "Not a short id: " <> show other

    -- No address has 9 characters, so label refuses it as an address
    it "is an address for a word of 9 hexadecimal digits, which is neither a short id nor a txid" $
      readLabelled "a1075db5c" `shouldBe` Right (LabelledAddress "a1075db5c")

    it "is an address, as typed, for any other word" $ do
      readLabelled "1KEn7jEXa7KCLeZy59dka5qRJBLnPMmrLj" `shouldBe` Right (LabelledAddress "1KEn7jEXa7KCLeZy59dka5qRJBLnPMmrLj")
      readLabelled "bitcoincash:qzvzk6v4gdxt705vcmt3nxwwl7j3pfzercknlz3k4u" `shouldBe` Right (LabelledAddress "bitcoincash:qzvzk6v4gdxt705vcmt3nxwwl7j3pfzercknlz3k4u")

    it "is the receiving address i of the account of the type for TYPE/0/i, as the views write a path" $
      readLabelled "bip84/0/3" `shouldBe` Right (LabelledPath (Just Bip84) AddrExternal 3)

    it "is the receiving address i of no type yet for /0/i, which --type or the default type completes" $
      readLabelled "/0/3" `shouldBe` Right (LabelledPath Nothing AddrExternal 3)

    -- label refuses a change address, and says so
    it "is the change address i for /1/i" $
      readLabelled "bip44/1/2" `shouldBe` Right (LabelledPath (Just Bip44) AddrInternal 2)

    it "is refused for a word of the shape of a path that is no path, with the two forms of a path" $
      readLabelled "m/0/3" `shouldBe` Left "The path m/0/3 is not that of an address: /0/i is the receiving address i, and /1/i the change address i"

  describe "The words of label" $ do
    it "are what to label, then its label" $
      parsedLine ["label", "1KEn7jEXa7KCLeZy59dka5qRJBLnPMmrLj", "Rent"]
        `shouldBe` Right (CommandLabel (Label "1KEn7jEXa7KCLeZy59dka5qRJBLnPMmrLj" (Just "Rent") (Target Nothing Nothing)))

    -- label refuses it with what to type, which the parser cannot say
    it "are what to label and no label, for a word alone" $
      parsedLine ["label", "Savings"] `shouldBe` Right (CommandLabel (Label "Savings" Nothing (Target Nothing Nothing)))

    it "are two at most" $
      parsedLine ["label", "1KEn7jEXa7KCLeZy59dka5qRJBLnPMmrLj", "Rent", "October"] `shouldBe` Left "Invalid argument `October'"

    it "are shown in the usage line as two words to give, the first of them what to label" $
      usageOf "label" `shouldBe` "ADDRESS|TXID|PATH|portfolio|wallet LABEL [-n|--network NETWORK] [-t|--type TYPE]"

  describe "The word of label that a description comes after" $ do
    it "is portfolio for the active portfolio, and wallet for its wallet" $ do
      readDescribed "portfolio" `shouldBe` Just DescribedPortfolio
      readDescribed "wallet" `shouldBe` Just DescribedWallet

    it "is read with capitals too" $
      readDescribed "Wallet" `shouldBe` Just DescribedWallet

    it "is no other word, and no name with its index" $ do
      readDescribed "Savings" `shouldBe` Nothing
      readDescribed "portfolio0" `shouldBe` Nothing

    -- label refuses such a name: it describes the active one only
    it "is told from the name of a portfolio or of a wallet with its index" $ do
      indexedName "portfolio12" `shouldBe` Just DescribedPortfolio
      indexedName "wallet0" `shouldBe` Just DescribedWallet
      indexedName "portfolio" `shouldBe` Nothing
      indexedName "wallets" `shouldBe` Nothing

  describe "A word typed for label or tag, of which a refusal says what it reads as" $ do
    it "reads as an address when it is one on some network" $
      labelledShapeOf "1KEn7jEXa7KCLeZy59dka5qRJBLnPMmrLj" `shouldBe` Just "an address"

    it "reads as a path when it is one" $
      labelledShapeOf "bip84/0/3" `shouldBe` Just "a path"

    it "reads as the id of a transaction for 8 hexadecimal digits, the length of a short id" $
      labelledShapeOf "a1075db5" `shouldBe` Just "the id of a transaction"

    it "reads as nothing label labels for an everyday word" $
      labelledShapeOf "Savings" `shouldBe` Nothing

    -- The word has the shape of a path, and readLabelled refuses it as one
    it "reads as nothing label labels for digits and a slash that are no path, such as 50/50" $
      labelledShapeOf "50/50" `shouldBe` Nothing

    -- It reads as an address on no network
    it "reads as nothing label labels for a word that is an address on no network" $
      labelledShapeOf "1KEn7jEXa7KCLeZy59dka5qRJBLnPMmrLj0" `shouldBe` Nothing

  describe "What show shows, by the shape of the word" $ do
    it "is the receiving address i for /0/i, and the change address i for /1/i, of no type yet" $ do
      readShown "/0/3" `shouldBe` Right (ShownPath Nothing AddrExternal 3)
      readShown "/1/0" `shouldBe` Right (ShownPath Nothing AddrInternal 0)

    it "is the address of the account of the type for a path that starts with its type, as the views write it" $ do
      readShown "bip84/0/3" `shouldBe` Right (ShownPath (Just Bip84) AddrExternal 3)
      readShown "bip44/1/0" `shouldBe` Right (ShownPath (Just Bip44) AddrInternal 0)

    -- bip49 has the form of a type, so the word was meant as a path, not as
    -- a file in a directory bip49
    it "is refused as a path for a type hw holds no account of" $
      readShown "bip49/0/3" `shouldBe` Left "The path bip49/0/3 is of no type of account hw holds: a path starts with bip44 or bip84, such as bip84/0/3, or with no type, such as /0/3"

    -- A word of digits and slashes, with perhaps the m, h and ' of a BIP32
    -- path, was meant as a path
    it "is refused as a path for another word of digits and slashes" $ do
      readShown "m/0/3" `shouldBe` Left "The path m/0/3 is not that of an address: /0/i is the receiving address i, and /1/i the change address i"
      readShown "m/44h/0'/0/3" `shouldBe` Left "The path m/44h/0'/0/3 is not that of an address: /0/i is the receiving address i, and /1/i the change address i"

    it "is a coin for a txid by its short id, a colon and an index" $
      case readShown "a1075db5:1" of
        Right (ShownCoin coin) -> coinRefText coin `shouldBe` "a1075db5:1"
        other -> expectationFailure $ "Not a coin: " <> show other

    it "is a transaction for a word of 64 hexadecimal digits" $
      case readShown "ab00000000000000000000000000000000000000000000000000000000000001" of
        Right (ShownTx (FullHash tid)) -> txHashToHex tid `shouldBe` "ab00000000000000000000000000000000000000000000000000000000000001"
        other -> expectationFailure $ "Not a transaction: " <> show other

    it "is a transaction by its short id for a word of 8 hexadecimal digits" $
      case readShown "a1075db5" of
        Right (ShownTx (ShortHash short)) -> shortIdText short `shouldBe` "a1075db5"
        other -> expectationFailure $ "Not a short id: " <> show other

    -- The prefix of a Bitcoin Cash address has a colon, but no hash before
    -- it. The shell offers the address without its prefix.
    it "is an address, as typed, for a word that reads as an address on one of the networks" $ do
      readShown "1KEn7jEXa7KCLeZy59dka5qRJBLnPMmrLj" `shouldBe` Right (ShownAddress "1KEn7jEXa7KCLeZy59dka5qRJBLnPMmrLj")
      readShown "bitcoincash:qzvzk6v4gdxt705vcmt3nxwwl7j3pfzercknlz3k4u" `shouldBe` Right (ShownAddress "bitcoincash:qzvzk6v4gdxt705vcmt3nxwwl7j3pfzercknlz3k4u")
      readShown "qzvzk6v4gdxt705vcmt3nxwwl7j3pfzercknlz3k4u" `shouldBe` Right (ShownAddress "qzvzk6v4gdxt705vcmt3nxwwl7j3pfzercknlz3k4u")

    it "is a file for a name of none of the other shapes" $
      readShown "unsigned.psbt" `shouldBe` Right (ShownFile "unsigned.psbt")

    -- A letter other than m and h is in no path
    it "is a file for a word with a slash and a letter that no path has" $ do
      readShown "./unsigned.psbt" `shouldBe` Right (ShownFile "./unsigned.psbt")
      readShown "/tmp/unsigned.psbt" `shouldBe` Right (ShownFile "/tmp/unsigned.psbt")

    it "is a file named like a short id when its directory comes before it" $
      readShown "./a1075db5" `shouldBe` Right (ShownFile "./a1075db5")

    -- 1KEn7jEXa7KCLeZy59dka5qRJBLnPMmrLj with its last character changed:
    -- the checksum no longer matches
    it "is a file for an address whose checksum is wrong, since it is an address on no network" $
      readShown "1KEn7jEXa7KCLeZy59dka5qRJBLnPMmrLk" `shouldBe` Right (ShownFile "1KEn7jEXa7KCLeZy59dka5qRJBLnPMmrLk")

  describe "A coin as the user types it" $ do
    it "is TXID:INDEX with the txid in full" $
      (coinRefText <$> readCoinRef "a1075db5c0ffee00000000000000000000000000000000000000000000000001:1")
        `shouldBe` Just "a1075db5c0ffee00000000000000000000000000000000000000000000000001:1"

    it "is the short id of its txid, a colon and its index" $
      (coinRefText <$> readCoinRef "a1075db5:1") `shouldBe` Just "a1075db5:1"

    it "is nothing without its index" $
      readCoinRef "a1075db5" `shouldBe` Nothing

  -- The verbose view of portfolios ends each account with its descriptor,
  -- which the user copies whole from the screen
  describe "Whether the pager wraps the lines of a listing" $ do
    it "wraps those of portfolios --verbose" $
      wrapsLines Verbose (CommandPortfolios (OnScreen (Just Verbose))) `shouldBe` True

    it "cuts those of the compact view of portfolios, which shows no descriptor" $
      wrapsLines Compact (CommandPortfolios (OnScreen Nothing)) `shouldBe` False

    it "cuts those of the verbose view of another listing, such as txs" $
      wrapsLines Verbose (CommandTxs Nothing noTxTagFilter (OnScreen (Just Verbose)) (Paging Nothing Nothing Nothing)) `shouldBe` False

  describe "The commands a text of the help names" $ do
    it "are those named in it, in its order" $
      mentionedCommands program "Sign it with signtx, then send it with broadcast." `shouldBe` ["signtx", "broadcast"]

    it "are each named once, however often the text names them" $
      mentionedCommands program "signtx, then broadcast, then signtx again" `shouldBe` ["signtx", "broadcast"]

    it "include a command named by an everyday word only when it is in backticks" $
      mentionedCommands program "The coins of the last sync, as `coins` lists them." `shouldBe` ["coins"]

    -- "sweep" is the transaction, in most of the texts that say it
    it "leave out sweep, an everyday word, unless it is in backticks" $
      mentionedCommands program "A sweep is signed by signsweep, or made by `sweep` alone." `shouldBe` ["signsweep", "sweep"]

    -- "send" is what broadcast does, in most of the texts that say it
    it "leave out send, an everyday word, unless it is in backticks" $
      mentionedCommands program "Then send it with broadcast, or pay with `send` alone." `shouldBe` ["broadcast", "send"]

  describe "What the arguments ask for" $ do
    it "is the help of the command named, with --help anywhere before --" $
      requestOf unitOptions ["--unit", "sat", "sync", "--json", "--help"] `shouldBe` HelpOf (Just "sync")

    it "is the help of the program, with --help and no command" $
      requestOf unitOptions ["-h"] `shouldBe` HelpOf Nothing

    it "takes the word after --unit for a unit, not for a command" $
      requestOf unitOptions ["--unit", "sync", "--help"] `shouldBe` HelpOf Nothing

    it "leaves a --help after -- to the command, as an argument" $
      requestOf unitOptions ["signmsg", "a", "--", "--help"] `shouldBe` Parse

    it "leaves the words of a shell completion to the completion, --help among them" $
      requestOf unitOptions ["--bash-completion-index", "2", "--bash-completion-word", "hw", "--bash-completion-word", "--help"] `shouldBe` Parse

    it "is the help of the program when nothing is typed" $
      requestOf unitOptions [] `shouldBe` NothingTyped

    it "is the version of the program, with --version and no command" $
      requestOf unitOptions ["--version"] `shouldBe` Version

    -- No command takes --version: the parser refuses it
    it "leaves a --version after a command to the parser" $
      requestOf unitOptions ["sync", "--version"] `shouldBe` Parse

    it "is the help, with both --help and --version" $
      requestOf unitOptions ["--version", "--help"] `shouldBe` HelpOf Nothing

  -- A help is shown before the options are parsed
  describe "The pager of a help" $ do
    it "is turned off by --no-pager after the command" $
      pagerOff "no-pager" ["preparetx", "--help", "--no-pager"] `shouldBe` True

    it "is turned off by --no-pager before the command" $
      pagerOff "no-pager" ["--no-pager", "preparetx", "--help"] `shouldBe` True

    it "stays on without --no-pager" $
      pagerOff "no-pager" ["preparetx", "--help"] `shouldBe` False

    -- label takes the word as the label to set
    it "stays on with a --no-pager after --, which is an argument" $
      pagerOff "no-pager" ["label", "--help", "--", "--no-pager"] `shouldBe` False

  describe "A word as bash inserts it" $ do
    -- bash expands a ~/ that starts a word when the command runs
    it "keeps the ~/ of a path from the home, and escapes the rest" $
      escapedForBash "~/my savings.psbt" `shouldBe` "~/my\\ savings.psbt"

    -- bash would read ~notes as the home of a user named notes
    it "escapes a ~ that does not start a path from the home" $
      escapedForBash "~notes" `shouldBe` "\\~notes"

  describe "The words of a shell completion" $ do
    it "are read without the backslashes the shell typed to escape a space or a colon" $
      completionParts ["--bash-completion-index", "2", "--bash-completion-word", "hw", "--bash-completion-word", "rent\\ 2026.csv", "--bash-completion-word", "0a1a\\:"]
        `shouldBe` (Just 2, ["hw", "rent 2026.csv", "0a1a:"], [])

    it "are read the same from zsh and fish, which ask for descriptions" $
      completionParts ["--bash-completion-enriched", "--bash-completion-index", "1", "--bash-completion-word", "hw", "--bash-completion-word", "rent\\ 2026.csv"]
        `shouldBe` (Just 1, ["hw", "rent 2026.csv"], ["--bash-completion-enriched"])

    -- bash breaks --wallet=1 at the =, into three words
    it "are joined where bash broke a word at the = of --option=value, the index moved along" $
      completionParts (wordsTyped 6 ["hw", "preparetx", "--wallet", "=", "1", "--coin", ""])
        `shouldBe` (Just 5, ["hw", "preparetx", "--wallet", "1", "--coin", ""], [])

    it "complete the value after the = bash broke a word at" $
      completionParts (wordsTyped 4 ["hw", "txs", "--network", "=", "bt"])
        `shouldBe` (Just 3, ["hw", "txs", "--network", "bt"], [])

    it "complete an empty value for an = under the cursor, after which bash inserts the value" $
      completionParts (wordsTyped 3 ["hw", "txs", "--network", "="])
        `shouldBe` (Just 3, ["hw", "txs", "--network", ""], [])

    -- bash breaks a1075db5:1 at the colon typed, into three words
    it "are joined where bash broke a word at a colon, the index moved along" $
      completionParts (wordsTyped 4 ["hw", "lockcoin", "a1075db5", ":", "1"])
        `shouldBe` (Just 2, ["hw", "lockcoin", "a1075db5:1"], [])

    -- The cursor is right after the colon, which bash makes the word to
    -- complete
    it "complete the word up to its colon when the colon is under the cursor" $
      completionParts (wordsTyped 3 ["hw", "lockcoin", "a1075db5", ":"])
        `shouldBe` (Just 2, ["hw", "lockcoin", "a1075db5:"], [])

    it "are joined where bash broke a word at a colon before the word to complete, which moves up" $
      completionParts (wordsTyped 5 ["hw", "label", "bitcoincash", ":", "qzvzk", "rent"])
        `shouldBe` (Just 3, ["hw", "label", "bitcoincash:qzvzk", "rent"], [])

    -- bash breaks --coin=a1075db5:1 at the = and at the colon
    it "are joined at the colon and the = of a value typed against its option" $
      completionParts (wordsTyped 6 ["hw", "preparetx", "--coin", "=", "a1075db5", ":", "1"])
        `shouldBe` (Just 3, ["hw", "preparetx", "--coin", "a1075db5:1"], [])

    -- zsh and fish pass a word whole: a colon alone is one the user typed
    it "are not joined at a colon for zsh and fish, which do not break a word there" $
      completionParts ("--bash-completion-enriched" : wordsTyped 3 ["hw", "label", "rent", ":"])
        `shouldBe` (Just 3, ["hw", "label", "rent", ":"], ["--bash-completion-enriched"])

    it "say that bash keeps the word up to the colon it broke the word to complete at" $
      keptByBash (wordsTyped 4 ["hw", "lockcoin", "a1075db5", ":", "1"]) `shouldBe` "a1075db5:"

    it "say that bash keeps the word with its colon when the colon is under the cursor" $
      keptByBash (wordsTyped 3 ["hw", "lockcoin", "a1075db5", ":"]) `shouldBe` "a1075db5:"

    -- An escaped colon breaks nothing: bash replaces the whole word
    it "say that bash keeps nothing of a word it did not break" $
      keptByBash (wordsTyped 2 ["hw", "lockcoin", "a1075db5\\:"]) `shouldBe` ""

    it "ask for the script of bash with the name to run hw by" $
      requestOf unitOptions ["--bash-completion-script", "/usr/local/bin/hw"] `shouldBe` BashScript "/usr/local/bin/hw"

    it "ask for the script of fish with the name to run hw by" $
      requestOf unitOptions ["--fish-completion-script", "/usr/local/bin/hw"] `shouldBe` FishScript "/usr/local/bin/hw"

    -- zsh's script is the one of optparse-applicative
    it "leave the script of zsh to the parser" $
      requestOf unitOptions ["--zsh-completion-script", "hw"] `shouldBe` Parse
  where
    usageOf name = case commandNamed program name of
      Just command -> unwords (map renderPlain (usageParts command.args.usage))
      Nothing -> error $ "hw has no command " <> name
    -- The options of hw that take a value
    unitOptions = ["-u", "--unit"]
    -- The arguments the script of bash passes for the words, with the
    -- index of the one to complete
    wordsTyped :: Int -> [String] -> [String]
    wordsTyped index typed = ["--bash-completion-index", show index] <> concatMap (\w -> ["--bash-completion-word", w]) typed
    allCommands = concatMap (.commands) program.groups
    commandNames = (.name) <$> allCommands
    -- Whether the command takes the option of the long name
    takesOption option name =
      or [Long option `elem` doc.names | command <- allCommands, command.name == name, doc <- command.args.rows]

-- | A command line for each command of the help, in the order of the help:
-- what the command needs, with --wallet and --portfolio where the command
-- keeps them
everyCommandLine :: [(String, [String])]
everyCommandLine =
  [ ("newportfolio", []),
    ("recover", []),
    ("checkmnemonic", []),
    ("mnemonicsheet", ["sheet.pdf"]),
    ("exportwallet", ["--wallet", "1", "-p", "2", "wallet.jsonl"]),
    ("importwallet", ["--wallet", "bc560f95", "-p", "2", "wallet.jsonl"]),
    ("portfolios", []),
    ("switch", ["2", "--wallet", "1"]),
    ("setconfig", ["-n", "bch", "-t", "bip44"]),
    ("showconfig", ["--network", "--portfolio"]),
    ("deleteportfolio", ["2"]),
    ("deletewallet", ["1"]),
    ("sync", []),
    ("receive", ["-t", "bip84"]),
    ("show", ["/0/0"]),
    ("addresses", []),
    ("balances", []),
    ("label", ["1KEn7jEXa7KCLeZy59dka5qRJBLnPMmrLj", "Rent"]),
    ("coins", []),
    ("lockcoin", ["a1075db5:1"]),
    ("unlockcoin", ["a1075db5:1"]),
    ("fees", []),
    ("send", [payment]),
    ("sendall", ["1JCq8Aa9d9rg4T4XV93RV3DMxd5u7GkSSU"]),
    ("preparetx", [payment]),
    ("preparesendall", ["1JCq8Aa9d9rg4T4XV93RV3DMxd5u7GkSSU"]),
    ("signtx", ["a1075db5"]),
    ("broadcast", ["a1075db5"]),
    ("bumpfee", ["a1075db5"]),
    ("txs", []),
    ("pendingtxs", []),
    ("exporttx", ["a1075db5", "tx.psbt"]),
    ("importtx", ["tx.psbt"]),
    ("deletetx", ["a1075db5"]),
    ("tag", ["1KEn7jEXa7KCLeZy59dka5qRJBLnPMmrLj", "kyc"]),
    ("untag", ["1KEn7jEXa7KCLeZy59dka5qRJBLnPMmrLj", "kyc"]),
    ("tags", []),
    ("renametag", ["kyc", "verified"]),
    ("deletetag", ["kyc"]),
    ("newcontact", ["1JCq8Aa9d9rg4T4XV93RV3DMxd5u7GkSSU", "Landlord"]),
    ("contacts", []),
    ("deletecontact", ["1JCq8Aa9d9rg4T4XV93RV3DMxd5u7GkSSU"]),
    ("sweep", ["--sweep-to", "1JCq8Aa9d9rg4T4XV93RV3DMxd5u7GkSSU"]),
    ("preparesweep", ["--sweep-to", "1JCq8Aa9d9rg4T4XV93RV3DMxd5u7GkSSU"]),
    ("signsweep", ["a1075db5"]),
    ("signmsg", ["1KEn7jEXa7KCLeZy59dka5qRJBLnPMmrLj", "Hello"]),
    ("verifymsg", ["1KEn7jEXa7KCLeZy59dka5qRJBLnPMmrLj", "c2lnbmF0dXJl", "Hello"]),
    ("backup", []),
    ("showbackups", []),
    ("restoredatabase", ["copy.sqlite", "--yes"]),
    ("exportsheet", ["wallet.ods"]),
    ("exportjournal", ["hw.ledger"]),
    ("report", ["balances.pdf", "--balances"])
  ]

-- | A payment of 0.001 btc to an address of another wallet, as preparetx
-- takes it
payment :: String
payment = "1JCq8Aa9d9rg4T4XV93RV3DMxd5u7GkSSU:0.001"

-- | The options preparetx reads for a payment, with the options given
preparedWith :: [String] -> Either String PrepareTx
preparedWith options = case parsedLine (["preparetx", payment] <> options) of
  Right (CommandPrepareTx opts) -> Right opts
  Right other -> Left ("Not a preparetx: " <> show other)
  Left err -> Left err

