module Hibiscus.Parsing.Debug where

import Debug.Trace
import Hibiscus.Parser.Lexer.Interface
import Hibiscus.Parser.Lexer
import qualified Data.ByteString.Lazy.Char8 as BS

lexAll :: Lexer ()
lexAll = do
  tok <- scan
  case rtToken tok of
    TkEOF -> pure ()
    x -> do
      traceM (show tok)
      lexAll

m file = do
  source <- BS.readFile file
  case runLexer lexAll source of
    Left _ -> return ()
    Right err -> print err
  return ()
