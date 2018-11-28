module Interfaces.SystemIO

import Control.Monad.StateE
import Control.IOExceptJ
import Core.Context

import IdrisJvm.IO
import IdrisJvm.System

public export
interface SystemIO (m : Type -> Type) where
  getArgs : SE s err m (List String)

export
SystemIO IO where
  getArgs = lift getArgs

export
SystemIO JVM_IO where
  getArgs = lift getArgs

export
SystemIO (IOExcept (Error annot)) where
  getArgs = lift (ioe_lift getArgs)
