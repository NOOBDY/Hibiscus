-- vim: set ft=haskell

{
{-# OPTIONS -w #-} -- suppress millions of Happy warnings
module Parser(parseType, parseExpr, parseCommand) where

import qualified Data.ByteString.Lazy.Char8 as ByteString
import Data.List(foldl')

import Representation
import Lexer
}

--------------------------------------------------------------------------------
-- Directives
--------------------------------------------------------------------------------

%tokentype { Token }

%token
  REAL { TOK_REAL }
  BOOL { TOK_BOOL }
  TEX { TOK_TEX }
  TEXKIND1D { TOK_TEXKIND1D }
  TEXKIND2D { TOK_TEXKIND2D }
  TEXKIND3D { TOK_TEXKIND3D }
  TEXKINDCUBE { TOK_TEXKINDCUBE }
  LITERAL_BOOL { TOK_LITERAL_BOOL $$ }
  LITERAL_INT { TOK_LITERAL_INT $$ }
  LITERAL_FLOAT { TOK_LITERAL_FLOAT $$ }
  IDENTIFIER { TOK_IDENTIFIER $$ }
  COMMA { TOK_COMMA }
  RANGE_DOTS { TOK_RANGE_DOTS }
  LBRACKET { TOK_LBRACKET }
  RBRACKET { TOK_RBRACKET }
  LPAREN { TOK_LPAREN }
  RPAREN { TOK_RPAREN }
  BACKTICK { TOK_BACKTICK }
  WILDCARD { TOK_WILDCARD }
  OP_SUBSCRIPT { TOK_OP_SUBSCRIPT }
  OP_SWIZZLE { TOK_OP_SWIZZLE }
  OP_SCALAR_ADD { TOK_OP_SCALAR_ADD }
  OP_SCALAR_NEG_OP_SCALAR_SUB { TOK_OP_SCALAR_NEG_OP_SCALAR_SUB }
  OP_SCALAR_MUL { TOK_OP_SCALAR_MUL }
  OP_SCALAR_DIV { TOK_OP_SCALAR_DIV }
  OP_VECTOR_ADD { TOK_OP_VECTOR_ADD }
  OP_VECTOR_NEG_OP_VECTOR_SUB { TOK_OP_VECTOR_NEG_OP_VECTOR_SUB }
  OP_VECTOR_MUL { TOK_OP_VECTOR_MUL }
  OP_VECTOR_DIV { TOK_OP_VECTOR_DIV }
  OP_VECTOR_SCALAR_MUL { TOK_OP_VECTOR_SCALAR_MUL }
  OP_VECTOR_SCALAR_DIV { TOK_OP_VECTOR_SCALAR_DIV }
  OP_MATRIX_MATRIX_LINEAR_MUL { TOK_OP_MATRIX_MATRIX_LINEAR_MUL }
  OP_MATRIX_VECTOR_LINEAR_MUL { TOK_OP_MATRIX_VECTOR_LINEAR_MUL }
  OP_VECTOR_MATRIX_LINEAR_MUL { TOK_OP_VECTOR_MATRIX_LINEAR_MUL }
  OP_LT { TOK_OP_LT }
  OP_GT { TOK_OP_GT }
  OP_LTE { TOK_OP_LTE }
  OP_GTE { TOK_OP_GTE }
  OP_EQ { TOK_OP_EQ }
  OP_NEQ { TOK_OP_NEQ }
  OP_AND { TOK_OP_AND }
  OP_OR { TOK_OP_OR }
  OP_APPLY { TOK_OP_APPLY }
  OP_COMPOSE { TOK_OP_COMPOSE }
  IF { TOK_IF }
  THEN { TOK_THEN }
  ELSE { TOK_ELSE }
  LET { TOK_LET }
  EQUALS { TOK_EQUALS }
  IN { TOK_IN }
  TYPESPECIFIER { TOK_TYPESPECIFIER }
  RARROW { TOK_RARROW }
  LAMBDA { TOK_LAMBDA }

%right OP_APPLY
%left OP_OR
%left OP_AND
%nonassoc OP_EQ OP_NEQ
%nonassoc OP_LT OP_LTE OP_GT OP_GTE
%left OP_SCALAR_ADD OP_SCALAR_NEG_OP_SCALAR_SUB OP_VECTOR_ADD OP_VECTOR_NEG_OP_VECTOR_SUB
%right OP_MATRIX_VECTOR_LINEAR_MUL
%left OP_SCALAR_MUL OP_SCALAR_DIV OP_VECTOR_MUL OP_VECTOR_DIV OP_VECTOR_SCALAR_MUL OP_VECTOR_SCALAR_DIV OP_MATRIX_MATRIX_LINEAR_MUL OP_VECTOR_MATRIX_LINEAR_MUL
%left OP_SUBSCRIPT OP_SWIZZLE
%left OP_COMPOSE

%monad { P }
%lexer { lexer } { TOK_EOF } -- lexer :: (Token -> P a) -> P a

%name parseExprInner expr -- :: P Expr
%name parseTypeInner type -- :: P Type
%name parseCommandInner command -- :: P Command

%error { parseError } -- parseError :: Token -> P a

%expect 2
-- Both conflicts are in the ex_type production.
-- 1) "\x::a y" as shift "\x::(a y)" or reduce "\(x::a) y"?
-- 2) "\x::a->b" as shift "\x::(a->b)" or reduce "\(x::a)->b"?
-- Both should be shifted; thankfully, that's the default behaviour.

%%

--------------------------------------------------------------------------------
-- Grammar
--------------------------------------------------------------------------------
-- Note:
-- Right recursion is avoided where possible.
-- This means that in some places, lists are constructed backwards and reversed.
--------------------------------------------------------------------------------

--
-- Types
--

tuple_ex_type_inner :: { [ExType] }
  : ex_type COMMA ex_type { $3:$1:[] }
  | tuple_ex_type_inner COMMA ex_type { $3:$1 }
  ;

primary_ex_type :: { ExType }
  : LPAREN RPAREN { ExTypeUnit }
  | REAL { ExTypeReal }
  | BOOL { ExTypeBool }
  | TEX TEXKIND1D { ExTypeTex TexKind1D }
  | TEX TEXKIND2D { ExTypeTex TexKind2D }
  | TEX TEXKIND3D { ExTypeTex TexKind3D }
  | TEX TEXKINDCUBE { ExTypeTex TexKindCube }
  | primary_ex_type LITERAL_INT {% if $2 > 0 then return $ ExTypeArray $1 (ExDimFix $2) else do pos <- getLineColP; failP $ ParserError pos $ ParserErrorBadFixedDim $2 }
  | LPAREN tuple_ex_type_inner RPAREN { ExTypeTuple (reverse $2) }
  | IDENTIFIER { ExTypeVar $1 }
  | primary_ex_type IDENTIFIER { ExTypeArray $1 (ExDimVar $2) }
  | LPAREN ex_type RPAREN { $2 }
  ;

ex_type :: { ExType } -- right recursion for right associativity
  : primary_ex_type RARROW ex_type { ExTypeFun $1 $3 }
  | primary_ex_type { $1 }
  ;

type :: { Type }
  : ex_type {% do { vrefs <- getFreshVarRefsP; let {(t, vrefs') = typeFromExType vrefs $1}; putFreshVarRefsP vrefs'; return t; } }
  ;

opt_type :: { Maybe Type }
  : {- empty -} { Nothing }
  | TYPESPECIFIER type { Just $2 }
  ;


--
-- Expressions
--

unamb_infix_operator :: { Operator }
  : OP_SUBSCRIPT { OpSubscript }
  | OP_SWIZZLE { OpSwizzle }
  | OP_SCALAR_ADD { OpScalarAdd }
  | OP_SCALAR_MUL { OpScalarMul }
  | OP_SCALAR_DIV { OpScalarDiv }
  | OP_VECTOR_ADD { OpVectorAdd }
  | OP_VECTOR_MUL { OpVectorMul }
  | OP_VECTOR_DIV { OpVectorDiv }
  | OP_VECTOR_SCALAR_MUL { OpVectorScalarMul }
  | OP_VECTOR_SCALAR_DIV { OpVectorScalarDiv }
  | OP_MATRIX_MATRIX_LINEAR_MUL { OpMatrixMatrixLinearMul }
  | OP_MATRIX_VECTOR_LINEAR_MUL { OpMatrixVectorLinearMul }
  | OP_VECTOR_MATRIX_LINEAR_MUL { OpVectorMatrixLinearMul }
  | OP_LT { OpLessThan }
  | OP_GT { OpGreaterThan }
  | OP_LTE { OpLessThanEqual }
  | OP_GTE { OpGreaterThanEqual }
  | OP_EQ { OpEqual }
  | OP_NEQ { OpNotEqual }
  | OP_AND { OpAnd }
  | OP_OR { OpOr }
  | OP_APPLY { OpApply }
  | OP_COMPOSE { OpCompose }
  ;

amb_infix_operator :: { Operator }
  : unamb_infix_operator { $1 }
  | OP_SCALAR_NEG_OP_SCALAR_SUB { OpScalarSub }
  | OP_VECTOR_NEG_OP_VECTOR_SUB { OpVectorSub }
  ;

section_expr :: { Expr }
  : LPAREN amb_infix_operator RPAREN { ExprVar (show $2) }
  | LPAREN unamb_infix_operator operand_expr RPAREN { ExprLambda (PattVar "_x" Nothing) (ExprApp (ExprApp (ExprVar (show $2)) (ExprVar "_x")) $3) }
  | LPAREN operand_expr amb_infix_operator RPAREN { ExprApp (ExprVar (show $3)) $2 }
  ;

tuple_expr_inner :: { [Expr] }
  : expr COMMA expr { $3:$1:[] }
  | tuple_expr_inner COMMA expr { $3:$1 }
  ;

tuple_expr :: { Expr }
  : LPAREN tuple_expr_inner RPAREN { ExprTuple (reverse $2) }
  ;

array_expr_inner :: { [Expr] }
  : expr { $1:[] }
  | array_expr_inner COMMA expr { $3:$1 }
  ;

array_expr :: { Expr }
  : LBRACKET array_expr_inner RBRACKET { ExprArray (reverse $2) }
  ;

array_range_expr :: { Expr }
  : LBRACKET LITERAL_INT RANGE_DOTS LITERAL_INT RBRACKET { ExprArray (map (ExprRealLiteral . fromInteger) (if $2<=$4 then [$2..$4] else reverse [$4..$2])) }
  ;

primary_expr :: { Expr }
  : LPAREN RPAREN { ExprUnitLiteral }
  | LITERAL_INT { ExprRealLiteral (fromInteger $1) }
  | LITERAL_FLOAT { ExprRealLiteral $1 }
  | LITERAL_BOOL { ExprBoolLiteral $1 }
  | IDENTIFIER { ExprVar $1 }
  | tuple_expr { $1 }
  | array_expr { $1 }
  | array_range_expr { $1 }
  | section_expr { $1 }
  | LPAREN expr RPAREN { $2 }
  ;

app_expr :: { Expr }
  : app_expr primary_expr { ExprApp $1 $2 }
  | primary_expr { $1 }
  ;

infix_expr :: { Expr }
  : infix_expr BACKTICK IDENTIFIER BACKTICK app_expr { infixExpr $3 $1 $5 }
  | app_expr { $1 }
  ;

operand_expr :: { Expr }
  : OP_SCALAR_NEG_OP_SCALAR_SUB operand_expr { prefixExpr (show OpScalarNeg) $2 }
  | OP_VECTOR_NEG_OP_VECTOR_SUB operand_expr { prefixExpr (show OpVectorNeg) $2 }
  --
  | operand_expr OP_SUBSCRIPT operand_expr { infixExpr (show OpSubscript) $1 $3 }
  | operand_expr OP_SWIZZLE operand_expr { infixExpr (show OpSwizzle) $1 $3 }
  | operand_expr OP_SCALAR_ADD operand_expr { infixExpr (show OpScalarAdd) $1 $3 }
  | operand_expr OP_SCALAR_NEG_OP_SCALAR_SUB operand_expr { infixExpr (show OpScalarSub) $1 $3 }
  | operand_expr OP_SCALAR_MUL operand_expr { infixExpr (show OpScalarMul) $1 $3 }
  | operand_expr OP_SCALAR_DIV operand_expr { infixExpr (show OpScalarDiv) $1 $3 }
  | operand_expr OP_VECTOR_ADD operand_expr { infixExpr (show OpVectorAdd) $1 $3 }
  | operand_expr OP_VECTOR_NEG_OP_VECTOR_SUB operand_expr { infixExpr (show OpVectorSub) $1 $3 }
  | operand_expr OP_VECTOR_MUL operand_expr { infixExpr (show OpVectorMul) $1 $3 }
  | operand_expr OP_VECTOR_DIV operand_expr { infixExpr (show OpVectorDiv) $1 $3 }
  | operand_expr OP_VECTOR_SCALAR_MUL operand_expr { infixExpr (show OpVectorScalarMul) $1 $3 }
  | operand_expr OP_VECTOR_SCALAR_DIV operand_expr { infixExpr (show OpVectorScalarDiv) $1 $3 }
  | operand_expr OP_MATRIX_MATRIX_LINEAR_MUL operand_expr { infixExpr (show OpMatrixMatrixLinearMul) $1 $3 }
  | operand_expr OP_MATRIX_VECTOR_LINEAR_MUL operand_expr { infixExpr (show OpMatrixVectorLinearMul) $1 $3 }
  | operand_expr OP_VECTOR_MATRIX_LINEAR_MUL operand_expr { infixExpr (show OpVectorMatrixLinearMul) $1 $3 }
  | operand_expr OP_LT operand_expr { infixExpr (show OpLessThan) $1 $3 }
  | operand_expr OP_GT operand_expr { infixExpr (show OpGreaterThan) $1 $3 }
  | operand_expr OP_LTE operand_expr { infixExpr (show OpLessThanEqual) $1 $3 }
  | operand_expr OP_GTE operand_expr { infixExpr (show OpGreaterThanEqual) $1 $3 }
  | operand_expr OP_EQ operand_expr { infixExpr (show OpEqual) $1 $3 }
  | operand_expr OP_NEQ operand_expr { infixExpr (show OpNotEqual) $1 $3 }
  | operand_expr OP_AND operand_expr { infixExpr (show OpAnd) $1 $3 }
  | operand_expr OP_OR operand_expr { infixExpr (show OpOr) $1 $3 }
  | operand_expr OP_APPLY operand_expr { infixExpr (show OpApply) $1 $3 }
  | operand_expr OP_COMPOSE operand_expr { infixExpr (show OpCompose) $1 $3 }
  --
  | infix_expr { $1 }
  ;

expr :: { Expr }
  : LAMBDA patts RARROW expr { foldl' (flip ExprLambda) $4 $2 }
  | IF expr THEN expr ELSE expr { ExprIf $2 $4 $6 }
  | LET patt EQUALS expr IN expr { ExprLet $2 $4 $6 }
  | LET IDENTIFIER opt_type patts EQUALS expr IN expr { ExprLet (PattVar $2 $3) (foldl' (flip ExprLambda) $6 $4) $8 }
  | operand_expr { $1 }
  ;


--
-- Patterns
--

tuple_patt_inner :: { [Patt] }
  : patt COMMA patt { $3:$1:[] }
  | tuple_patt_inner COMMA patt { $3:$1 }
  ;

tuple_patt :: { Patt }
  : LPAREN tuple_patt_inner RPAREN opt_type { PattTuple (reverse $2) $4 }
  ;

array_patt_inner :: { [Patt] }
  : patt { $1:[] }
  | array_patt_inner COMMA patt { $3:$1 }
  ;

array_patt :: { Patt }
  : LBRACKET array_patt_inner RBRACKET opt_type { PattArray (reverse $2) $4 }
  ;

patt :: { Patt }
  : WILDCARD opt_type { PattWild $2 }
  | LPAREN RPAREN opt_type { PattUnit $3 }
  | IDENTIFIER opt_type { PattVar $1 $2 }
  | tuple_patt { $1 }
  | array_patt { $1 }
  | LPAREN patt RPAREN { $2 }
  ;

patts :: { [Patt] }
  : patt { $1:[] }
  | patts patt { $2:$1 }
  ;


--
-- Commands, for the interactive debugger.
--

command :: { Command }
  : expr { CommandExpr $1 }
  | LET patt EQUALS expr { CommandLet $2 $4 }
  | LET IDENTIFIER opt_type patts EQUALS expr { CommandLet (PattVar $2 $3) (foldl' (flip ExprLambda) $6 $4) }
  ;


--------------------------------------------------------------------------------
-- Trailer
--------------------------------------------------------------------------------

{
-- Helper functions to simplify the grammar actions.

prefixExpr :: String -> Expr -> Expr
prefixExpr op a = ExprApp (ExprVar op) a

infixExpr :: String -> Expr -> Expr -> Expr
infixExpr op a b = ExprApp (ExprApp (ExprVar op) a) b


-- Exported entry points.
-- Either return an error string and source position, or the result and final state.

parseType :: ([TypeVarRef], [DimVarRef]) -> ByteString.ByteString -> Either CompileError (Type, ([TypeVarRef], [DimVarRef]))
parseType = genParser parseTypeInner

parseExpr :: ([TypeVarRef], [DimVarRef]) -> ByteString.ByteString -> Either CompileError (Expr, ([TypeVarRef], [DimVarRef]))
parseExpr = genParser parseExprInner

parseCommand :: ([TypeVarRef], [DimVarRef]) -> ByteString.ByteString -> Either CompileError (Command, ([TypeVarRef], [DimVarRef]))
parseCommand = genParser parseCommandInner

genParser :: P a -> ([TypeVarRef], [DimVarRef]) -> ByteString.ByteString -> Either CompileError (a, ([TypeVarRef], [DimVarRef]))
genParser entry_point vrefs src =
  case unP entry_point PState{ alex_inp = (alexStartPos, alexStartChr, src), fresh_vrefs = vrefs } of
    POk PState{ fresh_vrefs = vrefs' } result -> Right (result, vrefs')
    PFailed PState{ alex_inp = (AlexPos _ l c, _, _) } err -> Left err


-- Parser error function.
parseError :: Token -> P a
parseError t = do
  pos <- getLineColP
  failP $ ParserError pos $ ParserErrorNoParse t
}
