module Hibiscus.Syntax.Pretty where

import Data.Fix (foldFix)
import Hibiscus.Syntax.Ast
import Prettyprinter
import Data.Text

instance (Pretty v) => Pretty (Type loc v) where
  pretty (Type m) = foldFix prettyAlg m
   where
    prettyAlg :: (Pretty v) => TypeF loc v (Doc ann) -> Doc ann
    prettyAlg (TVar _ v) = pretty v
    prettyAlg (TData _ f args) = hsep (pretty f : args)
    prettyAlg (TArrow _ t1 t2) = parens $ t1 <+> "->" <+> t2 -- TODO: deal with optional parens later
    prettyAlg (TTuple _ ts) = hsep (punctuate comma ts)
    prettyAlg (TArray _ n t) = t <> brackets (pretty n)
