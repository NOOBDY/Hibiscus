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
import Prettyprinter
import Hibiscus.Parser.Pretty
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

<0> @ident  { emit TkIdent }
<0> @ctor   { emit TkCtor }
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

doEOF inp len = do
  t <- getLayout
  case t of
    Just _ -> do
      popLayout
      pure RangedToken
        { rtToken = TkVRCurly
        , rtRange = mkRange inp len
        }
    Nothing -> do
      popStartCode
      pure RangedToken
        { rtToken = TkEOF
        , rtRange = mkRange inp len
        }

scan :: Lexer RangedToken
scan = do
  input@(Input _ _ str bPos) <- gets lexerInput
  -- traceM $ BS.unpack str
  code <- getStartCode
  case alexScan input code of
    AlexEOF -> handleEOF
    AlexError (Input pos _ inp bPos) ->
      throwError $ "Lexical error on '" <> [BS.head inp] <> "' at " <> show (pretty pos)
    AlexSkip input' _ -> do
      modify' $ \s -> s{lexerInput = input'}
      scan
    AlexToken input'@(Input p _ _ bPos') tokl action -> do
      modify' $ \s -> s{lexerInput = input'}
      action input (bPos' - bPos)

-- layoutKw :: Token -> AlexInput -> Int64 -> Lexer Token
layoutKw tk inp len = do
  pushStartCode layout
  pure RangedToken
    { rtToken = tk
    , rtRange = mkRange inp len
    }

openBrace inp len = do
  popStartCode
  pushLayout ExplicitLayout
  pure RangedToken
    { rtToken = TkLCurly
    , rtRange = mkRange inp len
    }

startLayout inp len = do
  popStartCode

  reference <- getLayout
  col <- gets (posCol . inpPos . lexerInput)

  if Just (LayoutColumn col) <= reference
    then pushStartCode empty_layout
    else pushLayout (LayoutColumn col)

  pure RangedToken
    { rtToken = TkLCurly
    , rtRange = mkRange inp len
    }

emptyLayout inp len = do
  popStartCode
  pushStartCode newline
  pure RangedToken
    { rtToken = TkVRCurly
    , rtRange = mkRange inp len
    }

offsideRule inp len = do
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
          pure RangedToken
            { rtToken = TkVSemi
            , rtRange = mkRange inp len
            }
        GT -> continue
        LT -> do
          popLayout
          pure RangedToken
            { rtToken = TkVRCurly
            , rtRange = mkRange inp len
            }
    Nothing -> continue
}
