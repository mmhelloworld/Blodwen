module Compiler.Common

import Compiler.CompileExpr
import Compiler.Inline

import Core.Context
import Core.TT

import Data.CSet

import IdrisJvm.IO
import IdrisJvm.File
    
public export
record Codegen annot where
  constructor MkCG
  compileExpr : Ref Ctxt Defs -> 
                ClosedTerm -> (outfile : String) -> Core annot (Maybe String)
  executeExpr : Ref Ctxt Defs -> ClosedTerm -> Core annot ()

export
compile : {auto c : Ref Ctxt Defs} ->
          Codegen annot ->
          ClosedTerm -> (outfile : String) -> Core annot (Maybe String)
compile {c} cg = compileExpr cg c

export
execute : {auto c : Ref Ctxt Defs} ->
          Codegen annot ->
          ClosedTerm -> Core annot ()
execute {c} cg = executeExpr cg c

getAllDesc : List Name -> SortedSet -> Gamma -> SortedSet
getAllDesc [] ns g = ns
getAllDesc (n :: rest) ns g
  = if contains n ns
       then getAllDesc rest ns g
       else case lookupGlobalExact n g of
                 Nothing => getAllDesc rest ns g
                 Just def => -- assert_total $
                   let refs = refersTo def in
                       getAllDesc (rest ++ refs) (insert n ns) g

-- This is a duplicate of the function in Core.Context, but here we need to guarantee
-- we get *all* descendents, not just ones we may need for the occurs check.
-- TODO: Tidy this up.
getDesc : Name -> Gamma -> List Name
getDesc n g
    = CSet.toList $ getAllDesc [n] empty g

-- Find all the names which need compiling, from a given expression, and compile
-- them to CExp form (and update that in the Defs)
export
findUsedNames : {auto c : Ref Ctxt Defs} -> Term vars -> Core annot (List Name)
findUsedNames tm
    = do defs <- get Ctxt
         let ns = toList (getRefs tm)
         let allNs = ns ++ concatMap (\n => getDesc n (gamma defs)) ns
         let cns = toList (fromList allNs)
         traverse compileDef cns
         traverse inlineDef cns
         pure cns

-- Some things missing from Prelude.File

export
exists : String -> JVM_IO Bool
exists f 
    = do Right ok <- openFile f Read
             | Left err => pure False
         closeFile ok
         pure True

export
tmpName : JVM_IO String
tmpName = getTemporaryFileName

