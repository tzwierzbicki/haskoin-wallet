# The address book

Contacts: `newcontact`, `contacts`, `deletecontact`, a contact in `label`,
`tag`, `untag` and `show`, and a contact whose address a wallet holds. The
contacts of a transaction are in `claude-notes/transaction-view.md`.

## Which of the two an address is

- `label`, `tag`, `untag` and `show` go through `namedAddress`
  (`Commands/Contact.hs`): the wallets first, on any network, then the
  address book. Only `deletecontact` (`contactNamed`) reaches a contact whose
  address a wallet holds.

## A contact whose address a wallet holds

- Never stored: `contactHolders` (`Database/Contact.hs`) asks the addresses
  each time, on the network of the contact, through `heldRowsOnNetwork`
  (`Database/Address.hs`), which `heldElsewhere` and `newcontact` use too.
- Three places tell the user. `contacts`: its rows are `(Contact, Maybe
  Holder)`. An address answer: `retaggedAnswer` (`Commands/Answer.hs`) reads
  `contactAt`. `sync`: `shownHeldContacts` (`Commands/Sync.hs`), at each run.
- The words are in `Cli/PrettyPrinter/Contact.hs`: `heldWords`,
  `heldContactsDoc`, `alsoContactField`.
- Left as it is, by a ruling: the tag filters of `Database/Tag.hs`
  (`atTaggedContact`, `atContactWithTag`) and `tags` still count the tags of
  such a contact, while the views of a transaction hide it (`txInfoElsewhere`).

## Tests

- A home whose wallet holds the address of a contact: `holdingContact` of
  `Workflow/ContactSpec.hs`. It adds the first address of the test wallet as
  a contact before `receive` gives it out: `newcontact` cannot know it then.
- `ResponseAddress`, `ResponseSync` and `ResponseContacts` are matched by
  position in `Workflow/TestUtils.hs`, `Workflow/TagSpec.hs`, `RegtestUtils.hs`,
  `Arbitrary.hs` and the printer specs: a new field is an edit of each.
