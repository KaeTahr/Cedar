{
module Main where
}

-- Adheres to C Standard ISO/IEC 9899:1999 a.k.a. C99
-- See Chapter 6
-- Cedar additions are indicated by {CEDAR}

-- resources:
-- (useful)
-- https://haskell-happy.readthedocs.io/en/latest/

%name calc
%tokentype { Token }
%error { parseError }

%token
  -- Keywords
  'auto'         { Keyword "auto" }
  'break'        { Keyword "break" }
  -- ... Include all other keywords
  'layout'       { Keyword "layout" }  -- Dargent keyword
  -- ... Include all Dargent keywords

  -- Identifiers
  Identifier     { Identifier $$ }

  -- Constants
  IntConst       { TokenIntegerConstant $$ }
  FloatConst     { TokenFloatingConstant $$ }

  -- Punctuators
  '+'            { Punctuator "+" }
  '-'            { Punctuator "-" }
  '*'            { Punctuator "*" }
  '/'            { Punctuator "/" }
  -- ... Include all other punctuators

  -- Memory Sizes
  MemorySize     { MemorySize $$ }

%%

Program :: [ExternalDeclaration]
  = ExternalDeclarationList  { $1 }

ExternalDeclarationList :: [ExternalDeclaration]
  = ExternalDeclaration               { [$1] }
  | ExternalDeclarationList ExternalDeclaration { $1 ++ [$2] }

ExternalDeclaration :: ExternalDeclaration
  = FunctionDefinition  { FunctionDef $1 }
  | Declaration         { $1 }

FunctionDefinition :: FunctionDef
  = DeclarationSpecifiers Declarator DeclarationList? CompoundStatement
    { FunctionDefNode $1 $2 $3 $4 }

Declaration :: Declaration
  = DeclarationSpecifiers InitDeclaratorList? ';'
    { DeclarationNode $1 $2 }

-- ... Add more grammar rules for statements, expressions, etc.

%%

parseError :: [Token] -> a
parseError tokens = error $ "Parse error: unexpected tokens " ++ show tokens