{-# LANGUAGE DeriveFunctor #-}

module Hibiscus.Syntax.Ast where

import Data.Fix (Fix (..))
import Data.Text (Text)

data Name loc = Name loc Text

data Prim
  = PInt Int
  | PFloat Float
  | PString Text
  | PBool Bool

type Decl loc v a t = Either (TyDecl loc v t) (FnDecl loc v a t)

data TyDecl loc v t = TyDecl loc v [v] [TyCtor loc v t]

data TyCtor loc v t = TyCtor loc v [t]

data FnDecl loc v a t
  = FnSig loc v t
  | FnDef loc v a

data TypeF loc v r
  = TVar loc v
  | TData loc v [r]
  | TArrow loc r r
  | TTuple loc [r]
  | TArray loc Int r
  deriving (Functor)

newtype Type loc v = Type {unType :: Fix (TypeF loc v)}

tVar :: loc -> v -> Type loc v
tVar loc var = Type $ Fix $ TVar loc var

tData :: loc -> v -> [Type loc v] -> Type loc v
tData loc name args = Type $ Fix $ TData loc name (fmap unType args)

tArrow :: loc -> Type loc v -> Type loc v -> Type loc v
tArrow loc t1 t2 = Type $ Fix $ TArrow loc (unType t1) (unType t2)

tTuple :: loc -> [Type loc v] -> Type loc v
tTuple loc ts = Type $ Fix $ TTuple loc (fmap unType ts)

tArray :: loc -> Int -> Type loc v -> Type loc v
tArray loc len ty = Type $ Fix $ TArray loc len (unType ty)

data ExprF loc v t r
  = EPrim loc Prim
  | EVar loc v
  | ELam loc v r
  | EApp loc r r
  | EIf loc r r r
  | ELet loc [FnDecl loc v r t] r
  | ECase loc r [Case loc v r]
  | ETuple loc [r]
  | EArray loc [r]

newtype Expr loc v = Expr {unExpr :: Fix (ExprF loc v (Type loc v))}

data Case loc v a = Case loc (Pat loc v) a

data PatF loc v r
  = PatWild loc
  | PatPrim loc Prim
  | PatBind loc v
  | PatCtor loc v [r]
  | PatTuple loc [r]

newtype Pat loc v = Pat {unPat :: Fix (PatF loc v)}
