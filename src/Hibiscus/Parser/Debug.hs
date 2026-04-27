module Hibiscus.Parser.Debug where

import qualified Data.ByteString.Lazy.Char8 as BS
import Debug.Trace
import Hibiscus.Parser.Lexer
import Hibiscus.Parser.Lexer.Interface
import Hibiscus.Parser.Pretty
import Prettyprinter

lexAll :: Lexer ()
lexAll = do
  tok <- scan
  case rtToken tok of
    TkEOF -> pure ()
    x -> do
      traceM $ show x <> " " <> show (pretty (rtRange tok))
      lexAll

m file = do
  source <- BS.readFile file
  case runLexer lexAll source of
    Left err -> print err
    Right _ -> return ()
  return ()
