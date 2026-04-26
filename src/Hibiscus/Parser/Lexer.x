-- vim: set ft=haskell

-- following tutorial from https://amelia.how/posts/parsing-layout.html
-- TODO: hopefully able to use `%wrapper monadUserState-bytestring`
-- and add ranges to tokens

{
module Hibiscus.Parser.Lexer where

import Control.Monad.Except (throwError)
import Control.Monad.State (gets, modify')
import qualified Data.ByteString.Lazy.Char8 as BS
import qualified Data.Text.Encoding as T
import Hibiscus.Parser.Lexer.Interface
}

%encoding "latin1"

$digit = [0-9]
$lower = [a-z]
$upper = [A-Z]

@ident = [$lower \_] [$lower $upper $digit \_ \']*
@ctor = $upper [$lower $upper $digit \_ \']*
@string = \"[^\"]*\" -- `^` is negation

:-

[\ \t]+ ;

<0> "in"    { token TkIn }
<0> "let"   { layoutKw TkLet }
<0> "where" { layoutKw TkWhere }

<0> @ident  { emit (TkIdent . T.decodeUtf8 . BS.toStrict) }
<0> @ctor   { emit (TkCtor . T.decodeUtf8 . BS.toStrict) }
<0> \\      { token TkBackslash }
<0> "->"    { token TkArrow }
<0> \=      { token TkEqual }
<0> \(      { token TkLParen }
<0> \)      { token TkRParen }
<0> \{      { token TkLCurly }
<0> \}      { token TkRCurly }

<layout> {
  "--" .* \n  ;
  \n          ;

  \{          { openBrace }
  ()          { startLayout }
}

<empty_layout> () { emptyLayout }

<newline> {
  \n          ;
  "--" .* \n  ;

  ()          { offsideRule }
}

<eof> () { doEOF }

{
handleEOF = do
  pushStartCode eof
  scan

doEOF _ = do
  t <- getLayout
  case t of
    Just _ -> do
      popLayout
      pure TkVRCurly
    Nothing -> do
      popStartCode
      pure TkEOF

scan :: Lexer Token
scan = do
  input@(Input _ _ str) <- gets lexerInput
  code <- getStartCode
  case alexScan input code of
    AlexEOF -> handleEOF
    AlexError (Input _ _ inp) ->
      throwError $ "Lexical error: " <> show (BS.head inp)
    AlexSkip input' _ -> do
      modify' $ \s -> s{lexerInput = input'}
      scan
    AlexToken input' tokl action -> do
      modify' $ \s -> s{lexerInput = input'}
      action (BS.take (fromIntegral tokl) str)

layoutKw t _ = do
  pushStartCode layout
  pure t

openBrace _ = do
  popStartCode
  pushLayout ExplicitLayout
  pure TkLCurly

startLayout _ = do
  popStartCode

  reference <- getLayout
  col <- gets (posCol . inpPos . lexerInput)

  if Just (LayoutColumn col) <= reference
    then pushStartCode empty_layout
    else pushLayout (LayoutColumn col)

  pure TkVLCurly

emptyLayout _ = do
  popStartCode
  pushStartCode newline
  pure TkVRCurly

offsideRule _ = do
  context <- getLayout
  col <- gets (posCol . inpPos . lexerInput)

  let continue = do
        popStartCode
        scan

  case context of
    Just (LayoutColumn col') -> do
      case col `compare` col' of
        EQ -> do
          popStartCode
          pure TkVSemi
        GT -> continue
        LT -> do
          popLayout
          pure TkVRCurly
    Nothing -> continue
}
