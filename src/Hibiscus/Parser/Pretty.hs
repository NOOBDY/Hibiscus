module Hibiscus.Parser.Pretty where

import Hibiscus.Parser.Lexer.Interface (Range (..), RangedToken (..), Token, Pos (..))
import Prettyprinter

instance Pretty Pos where
  pretty (Pos line col) = pretty line <> ":" <> pretty col

instance Pretty Range where
  pretty (Range start stop) = pretty start <> "-" <> pretty stop
