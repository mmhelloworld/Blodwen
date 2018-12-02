module Core.Normalise

import Core.CaseTree
import Core.Context
import Core.Primitives
import Core.TT

-- import Control.Monad.State
import Data.List
import Data.Vect

import IdrisJvm.IO

%default covering -- total is hard here, because the things we're evaluating
                  -- might not themselves terminate, but covering is important.

export
toClosure : EvalOpts -> Env Term outer -> Term outer -> Closure outer
toClosure h env tm = MkClosure h [] env tm

-- Needs 'eval', defined later
export
evalClosure : Defs -> Closure free -> NF free

%name LocalEnv loc, loc1
%name Closure thunk, thunk1

Stack : List Name -> Type
Stack vars = List (Closure vars)

parameters (defs : Defs, opts : EvalOpts)
  mutual
    eval : Env Term free -> LocalEnv free vars -> Stack free ->
           Term (vars ++ free) -> NF free
    eval env loc stk (Local r p) = evalLocal env loc stk r p
    eval env loc stk (Ref nt fn)
         = evalRef env loc stk nt fn
    eval env loc (closure :: stk) (Bind x (Lam _ _ ty) sc) 
         = eval env (closure :: loc) stk sc
    eval env loc stk (Bind x b@(Let n val ty) sc) 
         = if holesOnly opts
              then NBind x (map (eval env loc stk) b)
                      (\arg => eval env (arg :: loc) stk sc)
              else eval env (MkClosure opts loc env val :: loc) stk sc
    eval env loc stk (Bind x b sc) 
         = NBind x (map (eval env loc stk) b)
               (\arg => eval env (arg :: loc) stk sc)
    eval env loc stk (App fn arg) 
         = eval env loc (MkClosure opts loc env arg :: stk) fn
    eval env loc stk (PrimVal x) = NPrimVal x
    eval env loc stk Erased = NErased
    eval env loc stk TType = NType

    evalLocal : Env Term free -> LocalEnv free vars -> Stack free -> 
                Maybe RigCount -> Elem x (vars ++ free) -> NF free
    evalLocal {vars = []} env loc stk r p 
        = if isLet p env
             -- getBinder does a lot of work to weaken the types as
             -- necessary, so only do it if we really need to
             then case getBinder p env of
                       Let _ val ty => eval env [] stk val
                       b => NApp (NLocal r p) stk
             else NApp (NLocal r p) stk
      where
        isLet : Elem x vars -> Env tm vars -> Bool
        isLet Here (Let _ _ _ :: env) = True
        isLet Here _ = False
        isLet (There p) (b :: env) = isLet p env
    evalLocal {vars = (x :: xs)} 
              env ((MkClosure _ loc' env' tm') :: locs) stk r Here 
        = eval env' loc' stk tm'
    evalLocal {vars = (x :: xs)} env (_ :: loc) stk r (There later) 
        = evalLocal env loc stk r later

    evalOp : (Vect arity (NF free) -> Maybe (NF free)) ->
             NameType -> Name -> Stack free -> NF free
    evalOp {arity} fn nt n stk
        = case takeFromStack arity stk of
               -- Stack must be exactly the right height
               Just (args, []) => 
                  let argsnf = map (evalClosure defs) args in
                      case fn argsnf of
                           Nothing => NApp (NRef nt n) stk
                           Just res => res
               _ => NApp (NRef nt n) stk
                   
    evalDef : Env Term free -> LocalEnv free vars -> Stack free ->
              NameType -> Name -> Def -> NF free
    evalDef env loc stk nt fn (PMDef h args tree _ _)
        = if h || not (holesOnly opts) then
             case extendFromStack args loc stk of
                  Nothing => NApp (NRef nt fn) stk
                  Just (loc', stk') => 
                       case evalTree env loc' stk' tree of
                            Nothing => NApp (NRef nt fn) stk
                            Just val => val
             else NApp (NRef nt fn) stk
    -- Don't check 'holesOnly' here - effectively, this gives us constant
    -- folding f the stack happens to be appropriate
    evalDef env loc stk nt fn (Builtin op) = evalOp (getOp op) nt fn stk
    evalDef env loc stk nt fn (DCon tag arity _) = NDCon fn tag arity stk
    evalDef env loc stk nt fn (TCon tag arity _ _ _) = NTCon fn tag arity stk
    evalDef env loc stk nt fn _ = NApp (NRef nt fn) stk

    -- Only evaluate the name if its definition is visible in the current 
    -- namespace
    evalRef : Env Term free -> LocalEnv free vars -> Stack free ->
              NameType -> Name -> NF free
    evalRef env loc stk nt fn
        = case lookupGlobalExact fn (gamma defs) of
               Just def => 
                    if evalAll opts ||
                         reducibleIn (currentNS defs) fn (visibility def)
                       then evalDef env loc stk nt fn (definition def)
                       else toRef (definition def) stk
               _ => NApp (NRef nt fn) stk
      where
        toRef : Def -> Stack free -> NF free
        toRef (DCon t a _) stk = NDCon fn t a stk
        toRef (TCon t a _ _ _) stk = NTCon fn t a stk
        toRef _ stk = NApp (NRef nt fn) stk

    -- Take arguments from the stack, as long as there's enough.
    -- Returns the arguments, and the rest of the stack
    takeFromStack : (arity : Nat) -> Stack free ->
                    Maybe (Vect arity (Closure free), Stack free)
    takeFromStack arity stk = takeStk arity stk []
      where
        takeStk : (remain : Nat) -> Stack free -> 
                  Vect got (Closure free) -> 
                  Maybe (Vect (got + remain) (Closure free), Stack free)
        takeStk {got} Z stk acc = Just (rewrite plusZeroRightNeutral got in
                                    reverse acc, stk)
        takeStk (S k) [] acc = Nothing
        takeStk {got} (S k) (arg :: stk) acc 
           = rewrite sym (plusSuccRightSucc got k) in
                     takeStk k stk (arg :: acc)

    extendFromStack : (args : List Name) -> 
                      LocalEnv free vars -> Stack free ->
                      Maybe (LocalEnv free (args ++ vars), Stack free)
    extendFromStack [] loc stk = Just (loc, stk)
    extendFromStack (n :: ns) loc [] = Nothing
    extendFromStack (n :: ns) loc (arg :: args) 
         = do (loc', stk') <- extendFromStack ns loc args
              pure (arg :: loc', stk')

    getCaseBound : List (Closure free) ->
                   (args : List Name) ->
                   LocalEnv free vars ->
                   Maybe (LocalEnv free (args ++ vars))
    getCaseBound [] [] loc = Just loc
    getCaseBound [] (x :: xs) loc = Nothing -- mismatched arg length
    getCaseBound (arg :: args) [] loc = Nothing -- mismatched arg length
    getCaseBound (arg :: args) (n :: ns) loc 
         = do loc' <- getCaseBound args ns loc
              pure (arg :: loc')

    tryAlt : Env Term free ->
             LocalEnv free (more ++ vars) ->
             Stack free -> NF free -> CaseAlt more ->
             Maybe (NF free)
    tryAlt {more} {vars} env loc stk (NDCon nm tag' arity args') (ConCase x tag args sc) 
         = if tag == tag'
              then do bound <- getCaseBound args' args loc
                      let loc' : LocalEnv _ ((args ++ more) ++ vars) 
                          = rewrite sym (appendAssociative args more vars) in
                                    bound
                      evalTree env loc' stk sc
              else Nothing
    tryAlt env loc stk (NPrimVal c') (ConstCase c sc) 
         = if c == c' then evalTree env loc stk sc
                      else Nothing
    tryAlt env loc stk val (DefaultCase sc) 
         = if concrete val 
              then evalTree env loc stk sc
              else Nothing
      where
        concrete : NF free -> Bool
        concrete (NDCon _ _ _ _) = True
        concrete (NPrimVal _) = True
        concrete _ = False
    tryAlt _ _ _ _ _ = Nothing


    findAlt : Env Term free ->
              LocalEnv free (args ++ vars) ->
              Stack free -> NF free -> List (CaseAlt args) ->
              Maybe (NF free)
    findAlt env loc stk val [] = Nothing
    findAlt env loc stk val (x :: xs) 
         = case tryAlt env loc stk val x of
                Nothing => findAlt env loc stk val xs
                Just x => Just x

    evalTree : Env Term free ->
               LocalEnv free (args ++ vars) -> Stack free -> 
               CaseTree args ->
               Maybe (NF free)
    evalTree {args} {vars} {free} env loc stk (Case x _ alts) 
      = let x' : List.Elem _ ((args ++ vars) ++ free) 
               = rewrite sym (appendAssociative args vars free) in
                         elemExtend x
            xval = evalLocal env loc [] Nothing x' in
                   findAlt env loc stk xval alts
    evalTree {args} {vars} {free} env loc stk (STerm tm) 
          = let tm' : Term ((args ++ vars) ++ free) 
                    = rewrite sym (appendAssociative args vars free) in
                              embed tm in
            Just (eval env loc stk tm')
    evalTree env loc stk (Unmatched msg) = Nothing
    evalTree env loc stk Impossible = Nothing
    
evalClosure defs (MkClosure h loc env tm)
    = eval defs h env loc [] tm
    

export
nf : Defs -> Env Term free -> Term free -> NF free
nf defs env tm = eval defs defaultOpts env [] [] tm

-- Only evaluate names which stand for solved holes
export
nfHoles : Defs -> Env Term free -> Term free -> NF free
nfHoles defs env tm = eval defs withHoles env [] [] tm

-- Evaluate everything, even if not visible or not total (but work as
-- normal under binders and delay)
-- ('normalise' mode at the REPL)
export
nfAll : Defs -> Env Term free -> Term free -> NF free
nfAll defs env tm = eval defs withAll env [] [] tm

genName : IORef Int -> String -> JVM_IO Name
genName num root 
    = do n <- readIORef num
         writeIORef num (n + 1)
         pure (MN root n)

public export
interface Quote (tm : List Name -> Type) where
  quote : Defs -> Env Term vars -> tm vars -> Term vars
  quoteGen : IORef Int ->
             Defs -> Env Term vars -> tm vars -> JVM_IO (Term vars)

  -- Ugh. An STRef would be better (even if it would be implemented exactly
  -- like this, at least it would have an interface that prevented any chance
  -- of problems...)
  quote defs env tm 
      = unsafePerformIO (do num <- newIORef 0
                            quoteGen num defs env tm)
   
mutual
  quoteArgs : IORef Int -> Defs -> Env Term free -> List (Closure free) -> 
              JVM_IO (List (Term free))
  quoteArgs num defs env [] = pure []
  quoteArgs num defs env (thunk :: args) 
        = pure $ !(quoteGen num defs env (evalClosure defs thunk)) :: 
                 !(quoteArgs num defs env args)

  quoteHead :  NHead free -> JVM_IO (Term free)
  quoteHead (NLocal r y) = pure $ Local r y
  quoteHead (NRef nt n) = pure $ Ref nt n

  quoteBinder : IORef Int -> Defs -> Env Term free -> Binder (NF free) -> 
                JVM_IO (Binder (Term free))
  quoteBinder num defs env (Lam c x ty) 
      = do ty' <- quoteGen num defs env ty
           pure (Lam c x ty')
  quoteBinder num defs env (Let c val ty) 
      = do val' <- quoteGen num defs env val
           ty' <- quoteGen num defs env ty
           pure (Let c val' ty')
  quoteBinder num defs env (Pi c x ty) 
      = do ty' <- quoteGen num defs env ty
           pure (Pi c x ty')
  quoteBinder num defs env (PVar c ty) 
      = do ty' <- quoteGen num defs env ty
           pure (PVar c ty')
  quoteBinder num defs env (PLet c val ty) 
      = do val' <- quoteGen num defs env val
           ty' <- quoteGen num defs env ty
           pure (PLet c val' ty')
  quoteBinder num defs env (PVTy c ty) 
      = do ty' <- quoteGen num defs env ty
           pure (PVTy c ty')

  export
  Quote NF where
    quoteGen num defs env (NBind n b sc) 
        = do var <- genName num "qv"
             sc' <- quoteGen num defs env (sc (toClosure defaultOpts env (Ref Bound var)))
             b' <- quoteBinder num defs env b
             pure (Bind n b' (refToLocal Nothing var n sc'))
    quoteGen num defs env (NApp f args) 
        = do f' <- quoteHead f
             args' <- quoteArgs num defs env args
             pure $ apply f' args'
    quoteGen num defs env (NDCon nm tag arity xs) 
        = if isDelay nm defs
             then do xs' <- quoteArgs num defs env (map toHolesOnly xs)
                     pure $ apply (Ref (DataCon tag arity) nm) xs'
             else do xs' <- quoteArgs num defs env xs
                     pure $ apply (Ref (DataCon tag arity) nm) xs'
      where
        toHolesOnly : Closure vs -> Closure vs
        toHolesOnly (MkClosure _ locs env tm) = MkClosure withHoles locs env tm
    quoteGen num defs env (NTCon nm tag arity xs) 
        = do xs' <- quoteArgs num defs env xs
             pure $ apply (Ref (TyCon tag arity) nm) xs'
    quoteGen num defs env (NPrimVal x) = pure $ PrimVal x
    quoteGen num defs env NErased = pure $ Erased
    quoteGen num defs env NType = pure $ TType

  export
  Quote Term where
    quoteGen num defs env tm = pure tm

export
Quote Closure where
  quoteGen num defs env thunk = quoteGen num defs env (evalClosure defs thunk)

export
normalise : Defs -> Env Term free -> Term free -> Term free
normalise defs env tm = quote defs env (nf defs env tm)

export
normaliseHoles : Defs -> Env Term free -> Term free -> Term free
normaliseHoles defs env tm = quote defs env (nfHoles defs env tm)

export
normaliseAll : Defs -> Env Term free -> Term free -> Term free
normaliseAll defs env tm = quote defs env (nfAll defs env tm)

export
getValArity : Defs -> Env Term vars -> NF vars -> Nat
getValArity defs env (NBind x (Pi _ _ _) sc) 
    = S (getValArity defs env (sc (MkClosure defaultOpts [] env Erased)))
getValArity defs env val = 0

export
getArity : Defs -> Env Term vars -> Term vars -> Nat
getArity defs env tm = getValArity defs env (nf defs env tm)

public export
interface Convert (tm : List Name -> Type) where
  convert : Defs -> Env Term vars -> tm vars -> tm vars -> Bool
  convGen : IORef Int ->
            Defs -> Env Term vars -> tm vars -> tm vars -> JVM_IO Bool

  -- Ugh. An STRef would be better (even if it would be implemented exactly
  -- like this, at least it would have an interface that prevented any chance
  -- of problems...)
  convert defs env tm tm' 
      = unsafePerformIO (do num <- newIORef 0
                            convGen num defs env tm tm')

mutual
  allConv : IORef Int -> Defs -> Env Term vars ->
            List (Closure vars) -> List (Closure vars) -> JVM_IO Bool
  allConv num defs env [] [] = pure True
  allConv num defs env (x :: xs) (y :: ys) 
      = pure $ !(convGen num defs env x y) && !(allConv num defs env xs ys)
  allConv num defs env _ _ = pure False
  
  chkConvHead : Defs -> Env Term vars ->
                NHead vars -> NHead vars -> JVM_IO Bool
  chkConvHead defs env (NLocal _ x) (NLocal _ y) = pure $ sameVar x y
  chkConvHead defs env (NRef x y) (NRef x' y') = pure $ y == y'
  chkConvHead defs env x y = pure False
  
  -- Comparing multiplicities when converting pi binders
  subRig : RigCount -> RigCount -> Bool
  subRig Rig1 RigW = True -- we can pass a linear function if a general one is expected
  subRig x y = x == y -- otherwise, the multiplicities need to match up

  convBinders : IORef Int -> Defs -> Env Term vars ->
                Binder (NF vars) -> Binder (NF vars) -> JVM_IO Bool
  convBinders num defs env (Pi cx ix tx) (Pi cy iy ty)
      = if ix /= iy || not (subRig cx cy)
           then pure False
           else convGen num defs env tx ty
  convBinders num defs env (Lam cx ix tx) (Lam cy iy ty)
      = if ix /= iy || cx /= cy
           then pure False
           else convGen num defs env tx ty
  convBinders num defs env bx by
      = if multiplicity bx /= multiplicity by
           then pure False
           else convGen num defs env (binderType bx) (binderType by)

  export
  Convert NF where
    convGen num defs env (NBind x b scope) (NBind x' b' scope') 
        = do var <- genName num "convVar"
             let c = MkClosure defaultOpts [] env (Ref Bound var)
             bok <- convBinders num defs env b b'
             if bok
                then convGen num defs env (scope c) (scope' c)
                else pure False
    convGen num defs env tmx@(NBind x (Lam c ix tx) scx) tmy
        = let etay = nf defs env (Bind x (Lam c ix (quote (noGam defs) env tx))
                                   (App (weaken (quote (noGam defs) env tmy))
                                        (Local Nothing Here))) in
              convGen num defs env tmx etay
    convGen num defs env tmx tmy@(NBind y (Lam c iy ty) scy)
        = let etax = nf defs env (Bind y (Lam c iy (quote (noGam defs) env ty))
                                   (App (weaken (quote (noGam defs) env tmx))
                                        (Local Nothing Here))) in
              convGen num defs env etax tmy
    convGen num defs env (NApp val args) (NApp val' args') 
        = do hs <- chkConvHead defs env val val'
             as <- allConv num defs env args args'
             pure $ hs && as
    convGen num defs env (NDCon _ tag _ xs) (NDCon _ tag' _ xs') 
        = do as <- allConv num defs env xs xs'
             pure (tag == tag' && as)
    convGen num defs env (NTCon name tag _ xs) (NTCon name' tag' _ xs')
        = do as <- allConv num defs env xs xs'
             -- Need to compare names rather than tags since tags may be
             -- reused in different namespaces!
             pure (name == name' && as)
    convGen num defs env (NPrimVal x) (NPrimVal y) = pure (x == y)
    convGen num defs env NErased _ = pure True
    convGen num defs env _ NErased = pure True
    convGen num defs env NType NType = pure True
    convGen num defs env x y = pure False

  export
  Convert Term where
    convGen num defs env x y 
        = if x == y 
             then pure True
             else convGen num defs env (nf defs env x) (nf defs env y)

  export
  Convert Closure where
    convGen num defs env thunkx thunky
        = convGen num defs env (evalClosure defs thunkx)
                               (evalClosure defs thunky)

