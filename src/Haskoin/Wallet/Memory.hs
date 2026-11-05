{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE CPP #-}

-- | The memory of the hw process. While a command runs, it holds what the
-- user typed: a mnemonic, a passphrase, private keys.
--
-- hw cannot wipe these secrets after use. They are immutable values, and
-- the garbage collector copies them where it likes. Wiping one copy would
-- leave the others.
--
-- So hw protects the memory as a whole instead. It keeps it out of the
-- places where it would outlive the command or be read by another program.
-- Those are a core file, a crash report and a debugger.
module Haskoin.Wallet.Memory
  ( keepMemoryPrivate,
  )
where

import Control.Exception (IOException, try)
import Control.Monad (void)
import System.Posix.Resource (Resource (ResourceCoreFileSize), ResourceLimit (ResourceLimit), ResourceLimits (ResourceLimits), setResourceLimit)

#if defined(linux_HOST_OS)
import Foreign.C.Error (throwErrnoIfMinus1_)
import Foreign.C.Types (CInt (..), CULong (..))
#endif

-- | Keep the memory of this process to itself, from now until it exits.
-- hw calls this before anything else. No secret is then ever in a memory
-- that can leak.
--
-- Both settings are best effort. A system that refuses one still runs the
-- command, as it did before hw had them.
keepMemoryPrivate :: IO ()
keepMemoryPrivate = do
  ignoringFailure writeNoCoreFile
  ignoringFailure refuseDumps

-- | Limit the size of a core file to zero. The system then writes no core
-- file when hw crashes. Every Unix has this limit.
--
-- The hard limit goes to zero with the soft one, so nothing can raise it
-- again. A program hw starts, such as the pager, inherits the limit.
writeNoCoreFile :: IO ()
writeNoCoreFile = setResourceLimit ResourceCoreFileSize (ResourceLimits (ResourceLimit 0) (ResourceLimit 0))

-- | Mark the process as not dumpable. Only Linux has this mark.
--
-- The kernel then makes no core dump at all. The limit on the core file
-- does not stop a dump that is piped to a crash handler, and most
-- desktops have one. The mark does.
--
-- The kernel also refuses that another program of the same user attaches
-- to hw as a debugger, or opens its memory under @/proc@.
--
-- A program hw starts is dumpable again. The kernel resets the mark when
-- a process runs another program.
refuseDumps :: IO ()
#if defined(linux_HOST_OS)
refuseDumps = throwErrnoIfMinus1_ "prctl" $ c_prctl prSetDumpable 0 0 0 0

-- prctl takes a variable number of arguments. The capi convention calls
-- it through a C wrapper, which is the only supported way to call such a
-- function.
foreign import capi unsafe "sys/prctl.h prctl"
  c_prctl :: CInt -> CULong -> CULong -> CULong -> CULong -> IO CInt

foreign import capi "sys/prctl.h value PR_SET_DUMPABLE"
  prSetDumpable :: CInt
#else
refuseDumps = return ()
#endif

-- | Run a setting, and carry on if the system refuses it
ignoringFailure :: IO () -> IO ()
ignoringFailure setting = void (try setting :: IO (Either IOException ()))
