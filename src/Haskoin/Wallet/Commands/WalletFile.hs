{-# LANGUAGE OverloadedStrings #-}

-- | The files of a wallet: exportwallet writes the wallets, their portfolios
-- and their labels to a BIP 329 file, and importwallet reads one into the
-- database. "Haskoin.Wallet.Labels" reads and writes the records. show
-- reads such a file without changing the database
-- ("Haskoin.Wallet.Commands.Show").
module Haskoin.Wallet.Commands.WalletFile
  ( cmdImportWallet,
    cmdExportWallet,
  )
where

import Control.Monad (unless, when)
import Control.Monad.Except (ExceptT (..), throwError, withExceptT)
import Control.Monad.Trans.Class (lift)
import Data.Maybe (isNothing)
import Haskoin.Wallet.Bip329 (recordsText)
import Haskoin.Wallet.Cli.Response
import Haskoin.Wallet.Commands.Run (runDBResponse)
import Haskoin.Wallet.Commands.TagLock (tellingStopped)
import Haskoin.Wallet.Config
import Haskoin.Wallet.Database
import Haskoin.Wallet.Database.Portfolio
import Haskoin.Wallet.FileIO
import Haskoin.Wallet.Labels (ImportScope (..), databaseRecords, holdsAnything, importFile, readWalletFile, walletRecords)
import Haskoin.Wallet.Util
import Numeric.Natural (Natural)

-- | Add the portfolios of a BIP 329 file to the database and set the
-- labels it carries, as "Haskoin.Wallet.Labels" reads it. The file names
-- each of its wallets by the fingerprint of its seed. --wallet reads one
-- wallet of the file, by that fingerprint, and --portfolio one portfolio
-- ('scopeWallet', 'scopePortfolio'). The answer says what became of each
-- wallet and each portfolio read, what the file labels that the wallet
-- does not have yet, and which portfolio is active: the first one
-- created, in a database that had none.
cmdImportWallet :: Config -> ImportScope -> FilePath -> IO Response
cmdImportWallet cfg scope file =
  runDBResponse cfg . tellingStopped $ do
    content <- liftEitherIO $ readTextFile file
    -- A key or a descriptor alone, or a file of another kind, holds no
    -- record: it is refused, not read as an empty file
    unless (holdsAnything (readWalletFile content)) $ throwError (couldNotRead file notAWalletFile)
    -- The import is undone whole when it is refused, which the refusal
    -- says. show shares its refusals, and changes nothing either way.
    (report, active) <- withActiveAfter $ withExceptT (<> ". Nothing was changed") (importFile scope content)
    return (ResponseImport report active)

-- | Why a file without any record is refused by importwallet
notAWalletFile :: String
notAWalletFile =
  "It holds no record of a wallet. importwallet reads the file exportwallet writes. \
  \A key or a descriptor alone is one account, and does not say which portfolio it belongs to"

-- | Write portfolios to a BIP 329 file, with their labels and the coins
-- the user locked. Without options, the file holds every portfolio of
-- every wallet, then the address book ('databaseRecords'). --wallet and
-- --portfolio write less (@portfoliosToExport@), and no address book.
cmdExportWallet :: Config -> Maybe Natural -> Maybe Natural -> FilePath -> IO Response
cmdExportWallet cfg walletM portfolioM file =
  runDBResponse cfg $ do
    portfolios <- portfoliosToExport walletM portfolioM
    checkFileToWrite file
    -- The address book goes in the file of the whole database alone: a
    -- file narrowed to a wallet or a portfolio is made to show less
    records <- if isNothing walletM && isNothing portfolioM then databaseRecords portfolios else walletRecords portfolios
    liftEitherIO $ writeTextFile file (recordsText records)
    return $ ResponseFile file

-- | The portfolios exportwallet writes, in the order of the wallets and
-- of the portfolios of each.
--
-- Without an option, they are every portfolio of every wallet. The file
-- is then the whole database. One file moves it to another computer, and
-- one file keeps every label before a wallet is deleted. The active
-- portfolio plays no part. So the same command writes the same file
-- whichever portfolio the user last switched to.
--
-- The wallet of the active portfolio was the default before. A user of
-- several mnemonics then had to switch and export once for each. A
-- wallet they forgot was left out without a word.
--
-- The options narrow the file down. --wallet writes every portfolio of
-- one wallet. It is for a file that must not show the other wallets,
-- since a file of several wallets tells its reader that one person holds
-- them all. --portfolio writes one portfolio. Its wallet is the one
-- --wallet names, or the wallet of the active portfolio without --wallet,
-- as for switch ('walletNamedOrActive').
--
-- A file of several wallets has no field that a file of one wallet
-- lacks. Each record names its wallet in its origin, by the fingerprint
-- of the mnemonic. BIP 329 gives the origin to tell apart the wallets of
-- one export, and importwallet finds each wallet by it
-- ("Haskoin.Wallet.Labels"). The index of a wallet is not in the file.
-- So wallet0 here may be wallet1 on the computer that reads the file.
--
-- A wallet or a portfolio the database does not have is refused. So is
-- the export of a database without a wallet ('noWallets'), since its
-- file would be empty.
portfoliosToExport :: Maybe Natural -> Maybe Natural -> ExceptT String (DB IO) [Portfolio]
portfoliosToExport walletM portfolioM = case (walletM, portfolioM) of
  (Nothing, Nothing) -> do
    portfolios <- lift getAllPortfolios
    when (null portfolios) $ throwError noWallets
    return portfolios
  (Just wallet, Nothing) -> lift . walletPortfolios =<< walletNamed wallet
  (_, Just index) -> do
    wallet <- walletNamedOrActive walletM
    pure <$> portfolioNamed wallet index
