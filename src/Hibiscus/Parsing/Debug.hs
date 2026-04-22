module Hibiscus.Parsing.Debug where

import Debug.Trace
import Hibiscus.Parsing.Lexer.Interface
import Hibiscus.Parsing.Lexer2
import qualified Data.ByteString.Lazy.Char8 as BS

lexAll :: Lexer ()
lexAll = do
  tok <- scan
  case tok of
    TkEOF -> pure ()
    x -> do
      traceM (show x)
      lexAll

m file = do
  source <- BS.readFile file
  case runLexer lexAll source of
    Left _ -> return ()
    Right err -> print err
  return ()
