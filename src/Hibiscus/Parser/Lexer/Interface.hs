module Hibiscus.Parser.Lexer.Interface where

import Control.Monad.Except (MonadError)
import Control.Monad.State (MonadState, StateT (runStateT), gets, modify')
import Data.ByteString.Lazy.Char8 (ByteString)
import qualified Data.ByteString.Lazy.Char8 as BS
import Data.Char (ord)
import Data.Int (Int64)
import Data.List (uncons)
import Data.List.NonEmpty (NonEmpty ((:|)))
import qualified Data.List.NonEmpty as NE
import Data.Text (Text)
import qualified Data.Text.Encoding as T
import Data.Word (Word8)
import Hibiscus.Parsing.Lexer (runAlex)

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

data Range = Range
  { rangeStart :: Pos
  , rangeStop :: Pos
  }

data RangedToken = RangedToken
  { rtToken :: Token
  , rtRange :: Range
  }

data AlexInput = Input
  { inpPos :: Pos
  , inpLast :: {-# UNPACK #-} !Char
  , inpStream :: !ByteString
  , inpBytePos :: !Int64
  }
  deriving (Eq, Show)

alexGetByte :: AlexInput -> Maybe (Word8, AlexInput)
alexGetByte inp@Input{inpPos = pos, inpStream = str, inpBytePos = bPos} = advance <$> BS.uncons str
 where
  advance ('\n', rest) =
    ( fromIntegral (ord '\n')
    , Input
        { inpPos = Pos{posLine = posLine pos + 1, posCol = 1}
        , inpLast = '\n'
        , inpStream = rest
        , inpBytePos = bPos + 1
        }
    )
  advance (c, rest) =
    ( fromIntegral (ord c)
    , Input
        { inpPos = Pos{posLine = posLine pos, posCol = posCol pos + 1}
        , inpLast = c
        , inpStream = rest
        , inpBytePos = bPos + 1
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
    { lexerInput = Input (Pos 0 1) '\n' str 0
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

type Action a = AlexInput -> Int64 -> Lexer a

emit :: (Text -> Token) -> Action Token
emit tk inp@(Input _ _ str _) len = (pure . tk . T.decodeUtf8 . BS.toStrict) (BS.take len str)

token :: Token -> Action Token
token tk inp@(Input _ _ str _) len = pure tk

mkRange :: AlexInput -> Int64 -> Range
mkRange (Input start _ str _) len = Range{rangeStart = start, rangeStop = stop}
 where
  -- TODO: optionally make it configurable
  tabSize :: Int
  tabSize = 8

  movePos :: Pos -> Char -> Pos
  movePos (Pos line col) '\t' = Pos line (col + tabSize - ((col - 1) `mod` tabSize))
  movePos (Pos line _) '\n' = Pos (line + 1) 1
  movePos (Pos line col) _ = Pos line (col + 1)

  stop = BS.foldl' movePos start (BS.take len str)
