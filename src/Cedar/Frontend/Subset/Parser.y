{
module Cedar.Frontend.Subset.Parser (parseString) where

import Cedar.Frontend.Subset.AST
import Cedar.Frontend.Subset.Lexer
}

%name parseString
%tokentype { Token }
%error { parseError }

%token
  KwType   { KwType }
  KwRecord { KwRecord }
  KwEndian { KwEndian }
  KwLE     { KwLE }
  KwBE     { KwBE }
  KwME     { KwME }
  TyU8     { TyU8 }   TyU16 { TyU16 }   TyU32 { TyU32 }   TyU64 { TyU64 }
  TyI8     { TyI8 }   TyI16 { TyI16 }   TyI32 { TyI32 }   TyI64 { TyI64 }
  LBrace   { LBrace } RBrace { RBrace }
  LBrack   { LBrack } RBrack { RBrack }
  Colon    { Colon }  Comma  { Comma }
  At       { At }     BByte  { BByte }
  Eq       { Eq }
  Ident    { Ident $$ } Quoted { Quoted $$ }
  IntLit   { IntLit $$ }

%%

Start :: { [TopDecl] }
  : DeclList                      { $1 }

DeclList :: { [TopDecl] }
  :                                 { [] }
  | DeclList Decl                   { $1 ++ [$2] }

Decl :: { TopDecl }
  : KwType Ident Eq Type            { TypeDecl $2 $4 }

Type :: { PType }
  : PrimTy                          { TPrim $1 }
  | Type LBrack IntLit RBrack       { TArray $1 $3 }
  | KwRecord LBrace Fields RBrace   { TRecord $3 }

PrimTy :: { Prim }
  : TyU8  { U8 }  | TyU16 { U16 } | TyU32 { U32 } | TyU64 { U64 }
  | TyI8  { I8 }  | TyI16 { I16 } | TyI32 { I32 } | TyI64 { I64 }

Fields :: { [PField] }
  :                                   { [] }
  | Fields Field                      { $1 ++ [$2] }
  | Fields Field Comma                { $1 ++ [$2] }

Field :: { PField }
  : Name Colon Type At IntLit BByte                 { PField $1 $3 $5 Nothing }
  | Name Colon Type At IntLit BByte KwEndian End    { PField $1 $3 $5 (Just $8) }

Name :: { String }
  : Ident   { $1 }
  | Quoted  { $1 }

End :: { Endian }
  : KwLE { LE } | KwBE { BE } | KwME { ME }

{
parseError :: [Token] -> a
parseError _ = error "Parse error"
}