module Hibiscus.Parsing.Lexer.Interface where

import Control.Monad.Except (MonadError)
import Control.Monad.State (MonadState, StateT (runStateT), gets, modify')
import Data.ByteString.Lazy.Char8 (ByteString)
import qualified Data.ByteString.Lazy.Char8 as BS
import Data.Char (ord)
import Data.List (uncons)
import Data.List.NonEmpty (NonEmpty ((:|)))
import qualified Data.List.NonEmpty as NE
import Data.Text (Text)
import Data.Word (Word8)

data Token
  = TkIdent Text
  | TkCtor Text
  | TkString Text
  | TkInt Int
  | TkFloat Float
  | TkBool Bool
  | -- keywords
    TkLet
  | TkIn
  | TkWhere
  | TkIf
  | TkThen
  | TkElse
  | -- punctuation
    TkEqual -- '='
  | TkSemi -- ';'
  | TkLCurly -- '{'
  | TkRCurly -- '}'
  | TkLParen -- '('
  | TkRParen -- ')'
  | TkLBrack -- '['
  | TkRBrack -- ']'
  | TkBackslash -- '\'
  | TkArrow -- '->'
  | TkDColon -- '::'
  -- layout virtual punctuation
  | TkVLCurly
  | TkVSemi
  | TkVRCurly
  | TkEOF
  deriving (Eq, Show)

data Pos = Pos
  { posLine :: {-# UNPACK #-} !Int
  , posCol :: {-# UNPACK #-} !Int
  }
  deriving (Eq, Show)

data AlexInput = Input
  { inpPos :: Pos
  , inpLast :: {-# UNPACK #-} !Char
  , inpStream :: !ByteString
  }
  deriving (Eq, Show)

alexGetByte :: AlexInput -> Maybe (Word8, AlexInput)
alexGetByte inp@Input{inpPos = pos, inpStream = str} = advance <$> BS.uncons str
 where
  advance ('\n', rest) =
    ( fromIntegral (ord '\n')
    , Input
        { inpPos = Pos{posLine = posLine pos + 1, posCol = 1}
        , inpLast = '\n'
        , inpStream = rest
        }
    )
  advance (c, rest) =
    ( fromIntegral (ord c)
    , Input
        { inpPos = Pos{posLine = posLine pos, posCol = posCol pos + 1}
        , inpLast = c
        , inpStream = rest
        }
    )

alexInputPrevChar :: AlexInput -> Char
alexInputPrevChar = inpLast

type LexerError = String

type Lexer a = StateT LexerState (Either LexerError) a

data Layout = ExplicitLayout | LayoutColumn Int
  deriving (Eq, Show, Ord)

data LexerState = LS
  { lexerInput :: {-# UNPACK #-} !AlexInput
  , lexerStartCodes :: {-# UNPACK #-} !(NonEmpty Int)
  , lexerLayout :: [Layout]
  }
  deriving (Eq, Show)

initState :: ByteString -> LexerState
initState str =
  LS
    { lexerInput = Input (Pos 0 1) '\n' str
    , lexerStartCodes = 0 :| []
    , lexerLayout = []
    }

runLexer :: Lexer a -> ByteString -> Either LexerError a
runLexer act s = fst <$> runStateT act (initState s)

getStartCode :: Lexer Int
getStartCode = gets (NE.head . lexerStartCodes)

pushStartCode :: Int -> Lexer ()
pushStartCode i = modify' $ \st ->
  st{lexerStartCodes = NE.cons i (lexerStartCodes st)}

popStartCode :: Lexer ()
popStartCode = modify' $ \st ->
  st
    { lexerStartCodes =
        case lexerStartCodes st of
          _ :| [] -> 0 :| []
          _ :| (x : xs) -> x :| xs
    }

getLayout :: Lexer (Maybe Layout)
getLayout = gets (fmap fst . uncons . lexerLayout)

pushLayout :: Layout -> Lexer ()
pushLayout i = modify' $ \st ->
  st{lexerLayout = i : lexerLayout st}

popLayout :: Lexer ()
popLayout = modify' $ \st ->
  st
    { lexerLayout =
        case lexerLayout st of
          _ : xs -> xs
          [] -> []
    }

emit :: (ByteString -> Token) -> ByteString -> Lexer Token
emit f str = pure (f str)

token :: Token -> ByteString -> Lexer Token
token tk str = pure tk
