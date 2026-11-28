{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Tags, as the commands manage them: tag, untag, tags, renametag and
-- deletetag. A tag is a word the user puts on addresses and transactions
-- ("Haskoin.Wallet.Tags"). Tags are of the whole database, so the five
-- commands work whichever portfolio is active.
--
-- The listings keep the rows of a tag ("Haskoin.Wallet.Commands.TagFilter"),
-- a payment spends the coins at the addresses of one, and its new change
-- address takes the tags its coins share when asked
-- ('Haskoin.Wallet.Database.PendingTx.changeTakesTags'). tag and untag go
-- down the chain of change of an address when asked
-- ("Haskoin.Wallet.Database.ChangeChain"). A payment that spends coins of
-- different tags together asks first
-- ('Haskoin.Wallet.Commands.Payment.confirmMixing').
module Haskoin.Wallet.Commands.Tag
  ( cmdTag,
    cmdUntag,
    cmdTags,
    cmdRenameTag,
    cmdDeleteTag,
  )
where

import Control.Applicative ((<|>))
import Control.Monad (forM_, unless, when)
import Control.Monad.Except (ExceptT (..), throwError)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Class (lift)
import Data.Containers.ListUtils (nubOrd)
import Data.Foldable (for_)
import Data.Maybe (isJust, isNothing)
import Data.String.Conversions (cs)
import Haskoin
import Haskoin.Wallet.Cli.Command
import Haskoin.Wallet.Cli.Prompt
import Haskoin.Wallet.Cli.Response
import Haskoin.Wallet.Commands.Answer (retaggedAnswer)
import Haskoin.Wallet.Commands.Contact (NamedAddress (..), contactAnswer, namedAddress, notHeldOrKept)
import Haskoin.Wallet.Commands.Deletion (confirmDeletion, deleteTagNotice)
import Haskoin.Wallet.Commands.Label (namesNothing)
import Haskoin.Wallet.Commands.Run (runDBResponse)
import Haskoin.Wallet.Commands.ShortId (shownViews, walletTxHashOf)
import Haskoin.Wallet.Commands.TagLock (tellingStopped)
import Haskoin.Wallet.Config
import Haskoin.Wallet.Contacts (Contact (..))
import Haskoin.Wallet.Database
import Haskoin.Wallet.Database.Account
import Haskoin.Wallet.Database.Address
import Haskoin.Wallet.Database.ChangeChain (ChainRetagged (..), retagChangeChain)
import Haskoin.Wallet.Database.Contact
import Haskoin.Wallet.Database.PendingTx
import Haskoin.Wallet.Database.Portfolio
import Haskoin.Wallet.Database.Tag
import Haskoin.Wallet.Tags (ShownTag (..), Tag, readTag, tagText)

-- | Put tags on what the first word names, besides the tags it has: an
-- address of the wallet, the address at a path of the active portfolio,
-- or a transaction, as label finds each ('readLabelled'). Then give each
-- tag the colour of --color, if any. With no tag after the first word,
-- that word is a tag, and the command only sets it up: its colour, and
-- whether it locks its coins (@setUpTag@). --lock-coins and
-- --unlock-coins go with a tag alone. Beside something to tag they
-- would read as locking the coins of that address, which they do not:
-- the tag locks the coins of every address that has it.
-- With --with-change, the change down the chain of the payments of an
-- address takes the tags too (@retagNamed@).
--
-- An address and a transaction tell where they are, so they are tagged in
-- their portfolio, whichever is active. A change address takes tags, at
-- its path or by itself, though it takes no label: a tag says which
-- group the coins of an address belong to, and the change of a payment
-- belongs to one as much as what was received
-- ("Haskoin.Wallet.Database.Tag"). A transaction is tagged in every
-- portfolio that holds it ('retagTransaction'). An option that would
-- change nothing is refused rather than ignored, as label refuses it.
--
-- A tag that locks its coins may come to lock a coin that a pending
-- transaction spends: the user is told ('tellingStopped').
cmdTag :: Config -> TagOptions -> IO Response
cmdTag cfg opts =
  runDBResponse cfg . tellingStopped $
    case opts.tags of
      [] -> setUpTag opts
      tag : _ | opts.lockCoins || opts.unlockCoins -> do
        let (option, forOneCoin) =
              if opts.lockCoins
                then ("--lock-coins", "To lock one coin, use lockcoin.")
                else ("--unlock-coins", "To unlock one coin, use unlockcoin.")
            word = cs (tagText tag)
        throwError $
          option
            <> " goes with a tag alone, and is about the coins at every address that has the tag: hw tag "
            <> word
            <> " "
            <> option
            <> ". "
            <> forOneCoin
      tags -> do
        -- The colours first, so that the answer shows the tags in them.
        -- A refusal of what to tag undoes them with the rest.
        for_ opts.color $ \colorM -> lift (forM_ tags (`setTagColor` colorM))
        retagNamed cfg AddTags opts.first tags opts.withChange opts.target

-- | Set up the tag typed alone, and answer with the tag as tags lists
-- it: give it a colour or remove its colour (--color), make it lock its
-- coins (--lock-coins) or stop it (--unlock-coins). The tag is made if
-- the database had none of that name: a tag can be coloured before it
-- is used, and can lock its coins before it has any, so that the first
-- one it receives is locked. --unlock-coins makes no tag: there is
-- nothing to unlock, and the word was most likely mistyped.
--
-- A tag that locks its coins already is locked again without a word,
-- and so for one unlocked twice. The answer shows what the tag does
-- now, and a script can run the command without asking first.
--
-- Without an option there is nothing to do, and the user most likely left
-- out the tags of an address or of a transaction, so the refusal says how
-- to give them. A word that has the shape of something tag tags, such as
-- a short id, and that is no tag of the database yet, is refused too, for
-- the same reason: tag would otherwise make a tag out of the id of a
-- transaction. A tag of that shape that exists is coloured as any other.
-- --network and --type name nothing here, and are refused.
setUpTag :: TagOptions -> ExceptT String (DB IO) Response
setUpTag opts = do
  let typed = opts.first
      forgotten = "To tag it, give the tags after it: hw tag " <> typed <> " TAG"
  when (opts.lockCoins && opts.unlockCoins) $
    throwError "--lock-coins and --unlock-coins say opposite things: give one of them"
  when (isNothing opts.color && not (opts.lockCoins || opts.unlockCoins)) $
    throwError $ case labelledShapeOf typed of
      Just shape -> typed <> " reads as " <> shape <> ". " <> forgotten
      Nothing -> "tag needs what to tag, then the tags: hw tag ADDRESS|TXID|PATH TAG. To colour the tag " <> typed <> ", add --color COLOR"
  when (isJust opts.target.network) $
    throwError "tag takes no --network with a tag alone: a tag is the same in the whole database"
  when (isJust opts.target.kind) $
    throwError "tag takes no --type with a tag alone: a tag is the same in the whole database"
  when opts.withChange $
    throwError "tag takes no --with-change with a tag alone: nothing is tagged"
  tag <- case readTag (cs typed) of
    Right tag -> return tag
    Left refusal -> throwError $ case labelledShapeOf typed of
      Just shape -> typed <> " reads as " <> shape <> ", not as a tag. " <> forgotten
      Nothing -> refusal
  heldM <- lift $ heldTag tag
  for_ (labelledShapeOf typed) $ \shape ->
    when (isNothing heldM) $
      throwError (typed <> " reads as " <> shape <> ", and is no tag yet. " <> forgotten)
  when (opts.unlockCoins && isNothing heldM) $ throwError (noSuchTag tag)
  for_ opts.color $ lift . setTagColor tag
  when (opts.lockCoins || opts.unlockCoins) $ lift (setTagLocksCoins tag opts.lockCoins)
  ResponseTags . filter ((== tag) . shownTag . (.tagUseTag)) <$> lift tagUses

-- | Take tags off what the first word names, which is found as tag finds
-- it. A tag it does not have is refused, and nothing is changed: the
-- command fails, which undoes the tags it took off before. With
-- --with-change, a tag the address does not have still comes off the
-- change addresses down its chain (@retagNamed@).
cmdUntag :: Config -> Untag -> IO Response
cmdUntag cfg opts =
  runDBResponse cfg $ retagNamed cfg RemoveTags opts.tagged opts.tags opts.withChange opts.target

-- | Put tags on what a word names, or take them off, and answer with it
-- as it is now: the address, or the transaction as each portfolio that
-- holds it sees it. The word is read as label reads it ('readLabelled'):
-- a path, a hash, or else an address. --network tells apart an address or
-- a path the wallet has on several networks, and --type completes a path
-- typed without its type. An address that is none of the wallet is looked
-- for in the address book, and its contact takes the tags
-- ('namedAddress').
--
-- With --with-change, the change down the chain of the payments of an
-- address of the wallet follows it
-- ("Haskoin.Wallet.Database.ChangeChain"), and the answer lists the
-- change addresses that did. Only such an address has a chain: the option
-- is refused with a transaction or a contact.
retagNamed :: Config -> Retag -> String -> [Tag] -> Bool -> Target -> ExceptT String (DB IO) Response
retagNamed cfg retag word typedTags withChange target = do
  named <- liftEither (readLabelled word)
  case named of
    LabelledAddress typed -> do
      -- The user most likely put a tag first
      unless (readsAsAddress typed) $
        throwError (namesNothing command "the tags" "ADDRESS|TXID|PATH TAG" typed)
      refuseType "an address"
      found <- namedAddress (notHeldOrKept cfg typed) cfg target.network typed
      case found of
        OwnAddress accId row -> retagAddress accId row
        BookAddress net contact -> do
          refuseWithChange "a contact: its payments are not of the wallet, so hw knows no change of them"
          retagContact net (contactAddress contact)
    LabelledPath kindM addrType index -> do
      when (isJust kindM) $ refuseType "a path that starts with its type"
      portfolio <- requireActivePortfolio
      (accId, row) <- heldAddressAt cfg portfolio target.network (kindM <|> target.kind) addrType index
      retagAddress accId row
    LabelledTx hashNamed -> do
      refuseType "a transaction"
      refuseWithChange "a transaction: the tags that go down the chain of change are those of an address"
      when (isJust target.network) $
        throwError (command <> " takes no --network with a transaction: its tags are the same in every portfolio that holds it")
      hash <- walletTxHashOf hashNamed
      -- One on hidden networks alone is refused before it is tagged.
      -- Otherwise it is tagged wherever it is, and shown where hw shows
      -- it.
      _ <- shownViews cfg hashNamed =<< transactionViews hash
      views <- retagTransaction retag hash tags
      ResponseLabelledTxs edit (onShownNetworks cfg (placeNetwork . fst) views) <$> activePortfolioRef
  where
    -- A tag typed twice is typed once: untag would otherwise look for
    -- it again once it is off, and say that the address does not have it
    tags = nubOrd typedTags
    command = case retag of
      AddTags -> "tag"
      RemoveTags -> "untag"
    edit = case retag of
      AddTags -> TxTagged
      RemoveTags -> TxUntagged
    refuseType what =
      when (isJust target.kind) $
        throwError (command <> " takes no --type with " <> what)
    refuseWithChange what =
      when withChange $
        throwError (command <> " takes no --with-change with " <> what)
    -- untag is refused a tag the address does not have. With
    -- --with-change it still goes down the chain, and is refused only
    -- when no change address had the tags either: a user who forgot
    -- the option the first time runs the command again with it, and
    -- the address has lost the tag by then.
    retagAddress accId row = do
      missing <- case retag of
        AddTags -> [] <$ lift (tagAddress row tags)
        RemoveTags -> lift $ untagAddress row tags
      followedM <-
        if withChange
          then do
            pool <- accountPool <$> getAccountById accId
            Just <$> retagChangeChain retag pool row tags
          else return Nothing
      onContact <- tagsOfContactAt accId row missing
      let lacked = "The address " <> cs (dBAddressAddress row) <> " does not have " <> tagsNamed missing
      case (missing, (.chainChanged) <$> followedM) of
        ([], _) -> return ()
        (_, Nothing) -> throwError (lacked <> onContact)
        (_, Just []) -> throwError (lacked <> ", and no change address down the chain of its payments has " <> oneOrMore missing "it" "them" <> onContact)
        (_, Just _) -> liftIO $ tell terminal (lacked <> ": " <> oneOrMore missing "it" "they" <> " came off the change addresses alone")
      retaggedAnswer retag followedM accId row
    -- What the refusal of untag adds for an address of the wallet that
    -- is also in the address book, when its contact has tags the address
    -- lacks: the user saw them in contacts, and untag denies them. A
    -- wallet came to hold the address of the contact, so the command
    -- works on the address of the wallet ('namedAddress'), and only
    -- deletecontact still reaches the contact.
    tagsOfContactAt accId row missing = do
      net <- accountNetwork <$> getAccountById accId
      contactM <- lift $ contactAt net (dBAddressAddress row)
      return $ case [tag | Just contact <- [contactM], tag <- shownTag <$> contactTags contact, tag `elem` missing] of
        [] -> ""
        had ->
          ". The contact of the address book at this address has "
            <> tagsNamed had
            <> ", but the wallet holds the address now, and untag works on the address of the wallet. To delete the contact, run: hw deletecontact "
            <> cs (dBAddressAddress row)
    -- A word for the tags a sentence named before
    oneOrMore :: [Tag] -> String -> String -> String
    oneOrMore named one several = if length named == 1 then one else several
    -- A contact of the address book takes tags as an address of the
    -- wallet does
    retagContact net address = do
      case retag of
        AddTags -> lift $ tagContact net address tags
        RemoveTags -> do
          missing <- lift $ untagContact net address tags
          unless (null missing) $
            throwError ("The contact " <> cs address <> " does not have " <> tagsNamed missing)
      contactAnswer net address

-- | List every tag of the database, with its colour and with how many
-- addresses and transactions have it, in every portfolio: a tag is of the
-- whole database
cmdTags :: Config -> IO Response
cmdTags cfg = runDBResponse cfg (ResponseTags <$> lift tagUses)

-- | Give a tag a new name in the whole database: on every address and
-- every transaction that has it, in every portfolio, and on every contact
-- of the address book. The tag keeps its colour. The answer shows the tag
-- as it is now, with what has it.
--
-- A new name that another tag has is refused, unless --merge asks for the
-- two to become one ('renameTag'). A merge cannot be taken apart, so it
-- never happens by a slip of the keyboard: the refusal names the option.
-- With --merge, the second word must be a tag. One that is none was most
-- likely mistyped, and a rename to the mistyped word would look done.
--
-- A word that is no tag has nothing to rename and is refused, as
-- deletetag refuses it. So is a new name equal to the old one, which
-- happens when only the capital letters differ: there is one tag for
-- both, and the user is told so rather than shown a rename that changed
-- nothing.
--
-- The command asks no question. Nothing is deleted without --merge, and
-- --merge is the word that agrees to a merge. The copy of the labels hw
-- keeps after each change holds the tags as they were before it
-- (@backUpLabels@). A merge into a tag that locks its coins may lock a
-- coin that a pending transaction spends: the user is told
-- ('tellingStopped').
cmdRenameTag :: Config -> RenameTag -> IO Response
cmdRenameTag cfg opts =
  runDBResponse cfg . tellingStopped $ do
    _ <- requireTags [opts.oldTag]
    when (opts.oldTag == opts.newTag) $
      throwError ("The tag is named " <> old <> " already. Capital letters are taken as small ones.")
    takenM <- lift $ heldTag opts.newTag
    case (takenM, opts.merge) of
      (Just _, False) ->
        throwError ("There is a tag " <> new <> " already. To merge " <> old <> " into it, add --merge: hw renametag " <> old <> " " <> new <> " --merge")
      (Nothing, True) ->
        throwError ("There is no tag " <> new <> " to merge " <> old <> " into. hw tags lists the tags. To rename " <> old <> ", leave out --merge.")
      _ -> return ()
    lift $ renameTag opts.oldTag opts.newTag
    uses <- lift tagUses
    use <- case filter ((== opts.newTag) . shownTag . (.tagUseTag)) uses of
      [use] -> return use
      _ -> throwError (noSuchTag opts.newTag)
    return $ if opts.merge then ResponseTagMerged opts.oldTag use else ResponseTagRenamed opts.oldTag use
  where
    old = cs (tagText opts.oldTag)
    new = cs (tagText opts.newTag)

-- | Delete a tag from the whole database: from every address and every
-- transaction that has it, in every portfolio, and from every contact of
-- the address book, with its colour. The answer says how many had it. A word that is no tag is refused, since
-- there is nothing to delete, and the user most likely mistyped it.
--
-- The user is asked first, unless --yes, and told how many have the tag
-- ('confirmDeletion', 'deleteTagNotice'). The command deletes no funds,
-- no key and no label, but it is easy to take for untag, and it takes
-- the tag off everything at once. The copy of the labels hw keeps after
-- each change holds the tags as they were before it (@backUpLabels@).
cmdDeleteTag :: Config -> Tag -> Bool -> IO Response
cmdDeleteTag cfg tag yes =
  runDBResponse cfg $ do
    _ <- requireTags [tag]
    uses <- lift tagUses
    use <- case filter ((== tag) . shownTag . (.tagUseTag)) uses of
      [use] -> return use
      _ -> throwError (noSuchTag tag)
    confirmDeletion yes (deleteTagNotice use) ("Delete the tag " <> cs (tagText tag) <> "?")
    ResponseTagDeleted tag <$> lift (deleteTag tag)
