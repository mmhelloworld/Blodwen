module Core.Typecheck

import Core.TT
import Core.Context
import Core.Normalise
import Core.CaseTree

import Data.List

%default covering

export
doConvert : (Quote tm, Convert tm) => 
            annot -> Gamma -> Env Term outer -> 
            tm outer -> tm outer -> Either (Error annot) ()
doConvert loc gam env x y 
    = if convert gam env x y 
         then pure ()
         else error (CantConvert loc env (quote gam env x) (quote gam env y))

parameters (loc : annot, gam : Gamma)
  mutual
    chk : Env Term vars -> Raw -> Either (Error annot) (Term vars, Term vars)
    chk env (RVar x) 
        = case defined x env of
               Nothing => 
                  case lookupDefTyExact x gam of
                       Just (PMDef _ _ _, ty) => 
                            pure $ (Ref Func x, embed ty)
                       Just (DCon tag arity _, ty) => 
                            pure $ (Ref (DataCon tag arity) x, embed ty)
                       Just (TCon tag arity _ _, ty) => 
                            pure $ (Ref (TyCon tag arity) x, embed ty)
                       Just (_, ty) => 
                            pure $ (Ref Func x, embed ty)
                       Nothing => error (UndefinedName loc x)
               Just lv => pure $ (Local lv, binderType (getBinder lv env))
    chk env (RBind nm b sc) 
        = do (b', bt) <- chkBinder env b
             (sc', sct) <- chk {vars = nm :: _} (b' :: env) sc
             pure $ discharge nm b' bt sc' sct
    chk env (RApp f a) 
        = do (f', fty) <- chk env f
             case nf gam env fty of
                  NBind _ (Pi _ ty) scdone => 
                        do (a', aty) <- chk env a
                           doConvert loc gam env (quote gam env ty) aty
                           let sc' = scdone (toClosure False env a')
                           pure (App f' a', quote gam env sc')
                  _ => error (NotFunctionType loc fty)
    chk env (RPrimVal x) = pure $ chkConstant x
    chk env RType = pure (TType, TType)

    chkBinder : Env Term vars -> Binder Raw -> 
                Either (Error annot) (Binder (Term vars), Term vars)
    chkBinder env (Lam x ty) 
        = do (tyv, tyt) <- chk env ty
             doConvert loc gam env tyt TType
             pure (Lam x tyv, tyt)
    chkBinder env (Let val ty) 
        = do (tyv, tyt) <- chk env ty
             (valv, valt) <- chk env val
             doConvert loc gam env tyv valt
             doConvert loc gam env tyt TType
             pure (Let valv tyv, tyt)
    chkBinder env (Pi x ty) 
        = do (tyv, tyt) <- chk env ty
             doConvert loc gam env tyt TType
             pure (Pi x tyv, tyt)
    chkBinder env (PVar ty) 
        = do (tyv, tyt) <- chk env ty
             doConvert loc gam env tyt TType
             pure (PVar tyv, tyt)
    chkBinder env (PLet val ty) 
        = do (tyv, tyt) <- chk env ty
             (valv, valt) <- chk env val
             doConvert loc gam env tyv valt
             doConvert loc gam env tyt TType
             pure (PLet valv tyv, tyt)
    chkBinder env (PVTy ty) 
        = do (tyv, tyt) <- chk env ty
             doConvert loc gam env tyt TType
             pure (PVTy tyv, tyt)

    discharge : (nm : Name) -> Binder (Term vars) -> Term vars ->
                Term (nm :: vars) -> Term (nm :: vars) -> 
                (Term vars, Term vars)
    discharge nm (Lam x ty) bindty scope scopety 
         = (Bind nm (Lam x ty) scope, Bind nm (Pi x ty) scopety)
    discharge nm (Let val ty) bindty scope scopety 
         = (Bind nm (Let val ty) scope, Bind nm (Let val ty) scopety)
    discharge nm (Pi x ty) bindty scope scopety 
         = (Bind nm (Pi x ty) scope, bindty)
    discharge nm (PVar ty) bindty scope scopety 
         = (Bind nm (PVar ty) scope, Bind nm (PVTy ty) scopety)
    discharge nm (PLet val ty) bindty scope scopety 
         = (Bind nm (PLet val ty) scope, Bind nm (PLet val ty) scopety)
    discharge nm (PVTy ty) bindty scope scopety 
         = (Bind nm (PVTy ty) scope, bindty)

    chkConstant : Constant -> (Term vars, Term vars)
    chkConstant (I x) = (PrimVal (I x), PrimVal IntType)
    chkConstant (BI x) = (PrimVal (BI x), PrimVal IntegerType)
    chkConstant (Str x) = (PrimVal (Str x), PrimVal StringType)
    chkConstant (Ch x) = (PrimVal (Ch x), PrimVal CharType)
    chkConstant (Db x) = (PrimVal (Db x), PrimVal DoubleType)
    chkConstant IntType = (PrimVal IntType, TType)
    chkConstant IntegerType = (PrimVal IntegerType, TType)
    chkConstant StringType = (PrimVal StringType, TType)
    chkConstant CharType = (PrimVal CharType, TType)
    chkConstant DoubleType = (PrimVal DoubleType, TType)

export
checkHas : annot -> (gam : Gamma) -> Env Term vars ->
           (term : Raw) -> (expected : Term vars) -> 
           Either (Error annot) (Term vars)
checkHas loc gam env tm exp
    = do (val, ty) <- chk loc gam env tm
         doConvert loc gam env ty exp
         pure val

export
check : {auto c : Ref Ctxt Defs} ->
        annot -> Env Term vars ->
        (term : Raw) -> (expected : Term vars) -> 
        Core annot (Term vars)
check loc env term expected 
    = case checkHas loc !getCtxt env term expected of
           Left err => throw err
           Right ok => pure ok

export
infer : {auto c : Ref Ctxt Defs} ->
        annot -> Env Term vars ->
        (term : Raw) -> Core annot (Term vars, Term vars)
infer loc env term
    = case chk loc !getCtxt env term of
           Left err => throw err
           Right ok => pure ok

export
checkConvert : {auto c : Ref Ctxt Defs} ->
               annot -> Env Term vars ->
               (x : Term vars) -> (y : Term vars) ->
               Core annot ()
checkConvert loc env x y 
    = case doConvert loc !getCtxt env x y of
           Left err => throw err
           Right ok => pure ()

