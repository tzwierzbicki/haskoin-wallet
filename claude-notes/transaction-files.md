# Transaction files

Reading, signing and sending a transaction file (`show FILE`, `signtx
--input`, `broadcast --input`, `importtx`): its network and its portfolio.

## Reading a file

- `readTxFileIn` and `txFileOfBytes` in `Commands.hs`, then `psbtTxFile` in
  `Tx/PsbtFile.hs`, whose header says what a file holds. A transaction alone,
  in hexadecimal, is read only when the wallet holds it as pending.
- A file `hw` wrote names its network, in a field of its own. A file of
  another wallet names none. `fileNetwork` in `Commands.hs` puts the file and
  `--network` together, and refuses when they differ.
- The network the signatures of a PSBT are finalized on (`signedOn` in
  `psbtTxFile`): that of the file, else that of the pool its derivation
  records name, else the network of the command.

## The portfolio of a file

- `transactionPool` in `Database/PendingTx.hs` finds it, for show, signtx
  and broadcast (`sendSigned`). Its Haddock has the rules: the portfolio that
  pays first, the active one among several, the test networks.
- Its refusals: a hidden network, several portfolios, "The transaction
  involves an account of another network" (a portfolio on another network
  than the one named), an output path too far (`checkOutputReach`).
- `importtx` generates the addresses its file pays, with a limit of its own:
  see "The change address" in `claude-notes/payments.md`.
- No portfolio: the transaction is from outside the wallet
  (`parseOutsideTxSignData` in `Tx/Signing.hs`, type `TxExternal`), on the
  network named, or else on the default network.

## Importing and signing a file

- `importedTx` in `Database/PendingTx.hs` says what an import did (`Imported`).
  The version held, or the unsigned file of a transaction held signed, is
  `HeldAlready`, and no error. `sendSigned` counts on it: a file that
  `signsweep --output` wrote has the fingerprint of the pool, the data held none.
- `transactionToSign` in `Commands.hs` reads what signtx and signsweep sign.
  `refuseForMnemonic` and `refuseForPrivateKeys` then refuse the wrong signer,
  and only then `signedFileOf` asks about the `--input` file. A new refusal
  goes before that question and before the secret.
- signsweep signs a transaction only with a key for every input. One that
  spends a coin of the wallet and a coin from outside is signed outside `hw`
  first, then by signtx (`outsideUnsigned` in `Tx/Signing.hs`).

## Sending a file

- `sendTxRead` in `Commands.hs`, then `sendSigned`. A transaction held as
  pending goes on the network of its pool and is marked sent. One not held
  goes through `transactionPool`, and nothing in the wallet is updated.
- "Invalid signature for input N (ADDRESS)" comes from `verifyInputs` in
  `Tx/Signing.hs`. A file signed for Bitcoin and sent on bch gets it when no
  portfolio is involved, since the two chains sign differently.

## Tests

- `Workflow/TestUtils.hs`: `writeTxFile` writes signing data as `hw` does,
  `writeOtherWalletTxFile` without the network. Both carry the keys of the
  accounts of portfolio0 on btc, so the file involves the test portfolio
  once the home has it.
- `Workflow/SigningSpec.hs`: `unsignedPayment` and `signedPayment`, a payment
  of the test wallet from the coin at `/0/0`, and `coreFile` for the files
  Bitcoin Core wrote.
- `transactionPool` alone: the tests around the local `poolFound` in
  `Database/PendingTxSpec.hs`.
- `importtx` of a payment the wallet sent: a payment shuffles its outputs, so
  its id changes from run to run. Take the id `broadcast` answers (`txidOf`),
  and compare its `shortOf`. The unsigned file of a legacy payment has another
  id than the payment sent, so it gets the "spent already" refusal.
