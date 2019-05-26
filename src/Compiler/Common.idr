module Compiler.Common

import Compiler.CompileExpr
import Compiler.Inline

import Core.Context
import Core.TT

import Data.CMap
import Data.CSet

import IdrisJvm.IO
import IdrisJvm.File

||| Generic interface to some code generator
||| @annot Type of error/annotations in Core
public export
record Codegen annot where
  constructor MkCG
  ||| Compile a Blodwen expression, saving it to a file.
  compileExpr : Ref Ctxt Defs ->
                ClosedTerm -> (outfile : String) -> Core annot (Maybe String)
  ||| Execute a Blodwen expression directly.
  executeExpr : Ref Ctxt Defs -> ClosedTerm -> Core annot ()

||| compile
||| Given a value of type Codegen, produce a standalone function
||| that executes the `compileExpr` method of the Codegen
export
compile : {auto c : Ref Ctxt Defs} ->
          Codegen annot ->
          ClosedTerm -> (outfile : String) -> Core annot (Maybe String)
compile {c} cg = compileExpr cg c

||| execute
||| As with `compile`, produce a functon that executes
||| the `executeExpr` method of the given Codegen
export
execute : {auto c : Ref Ctxt Defs} ->
          Codegen annot ->
          ClosedTerm -> Core annot ()
execute {c} cg = executeExpr cg c

||| Get all desc's from a set + gamma?
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

-- Calculate a unique tag for each type constructor name we're compiling
-- This is so that type constructor names get globally unique tags
mkNameTags : Defs -> NameTags -> Int -> List Name -> NameTags
mkNameTags defs tags t [] = tags
mkNameTags defs tags t (n :: ns)
    = case lookupDefExact n (gamma defs) of
           Just (TCon _ _ _ _ _ _)
              => mkNameTags defs (insert n t tags) (t + 1) ns
           _ => mkNameTags defs tags t ns

-- Find all the names which need compiling, from a given expression, and compile
-- them to CExp form (and update that in the Defs)
export
findUsedNames : {auto c : Ref Ctxt Defs} -> Term vars -> Core annot (List Name, NameTags)
findUsedNames tm
    = do defs <- get Ctxt
         let ns = toList (getRefs tm)
         let allNs = ns ++ concatMap (\n => getDesc n (gamma defs)) ns
         let cns = toList (fromList allNs)
         -- Initialise the type constructor list with explicit names for
         -- the primitives (this is how we look up the tags)
         -- Use '1' for '->' constructor (although we can't match it yet!)
         let tyconInit = insert (UN "->") 1 $
                         insert (UN "Type") 2 $
                            primTags 3 empty 
                                     [IntType, IntegerType, StringType,
                                      CharType, DoubleType, WorldType]
         let tycontags = mkNameTags defs tyconInit 100 cns
         traverse (compileDef tycontags) cns
         traverse inlineDef cns
         pure (cns, tycontags)
  where
    primTags : Int -> NameTags -> List Constant -> NameTags
    primTags t tags [] = tags
    primTags t tags (c :: cs)
        = primTags (t + 1) (insert (UN (show c)) t tags) cs

-- Some things missing from Prelude.File

||| check to see if a given file exists
export
exists : String -> JVM_IO Bool
exists f 
    = do Right ok <- openFile f Read
             | Left err => pure False
         closeFile ok
         pure True

||| generate a temporary file/name
export
tmpName : JVM_IO String
tmpName = getTemporaryFileName

{-
||| change the access rights for a file
export
chmod : String -> Int -> JVM_IO ()
chmod f m = File.chmod f m
-}