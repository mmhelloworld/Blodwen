module Interfaces.FileIO

import Control.Monad.StateE
import Control.IOExceptJ
import Core.Context
import IdrisJvm.IO
import IdrisJvm.File

public export
interface FileIO (m : Type -> Type) where
  readFile : (fname : String) -> 
             SE s err m (Either FileError String)
  writeFile : (fname : String) -> (content : String) ->
              SE s err m (Either FileError ())

export
FileIO JVM_IO where
  readFile f = lift (IdrisJvm.File.readFile f)
  writeFile f c = lift (IdrisJvm.File.writeFile f c)

export
FileIO (IOExcept (Error annot)) where
  -- not handling errors for now
  readFile f = lift (ioe_lift (IdrisJvm.File.readFile f))
  writeFile f c = lift (ioe_lift (IdrisJvm.File.writeFile f c))
