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
  KwLayout { KwLayout }
  KwRecord { KwRecord }
  KwEndian { KwEndian }
  KwLE     { KwLE }
  KwBE     { KwBE }
  KwME     { KwME }
  KwAfter  { KwAfter }
  KwBefore { KwBefore }
  TyU8     { TyU8 }   TyU16 { TyU16 }   TyU32 { TyU32 }   TyU64 { TyU64 }
  TyI8     { TyI8 }   TyI16 { TyI16 }   TyI32 { TyI32 }   TyI64 { TyI64 }
  LBrace   { LBrace } RBrace { RBrace }
  LBrack   { LBrack } RBrack { RBrack }
  Colon    { Colon }  Comma  { Comma }
  At       { At }     BByte  { BByte }
  Eq       { Eq }     Semi   { Semi }
  Ident    { Ident $$ } Quoted { Quoted $$ }
  IntLit   { IntLit $$ }

%%

Start :: { [TopDecl] }
  : DeclList                                { $1 }

DeclList :: { [TopDecl] }
  :                                          { [] }
  | DeclList Decl                            { $1 ++ [$2] }

Decl :: { TopDecl }
  : KwType   Ident Eq Type                   { TypeDecl   $2 $4 }
  | KwLayout Ident Eq Type Semi              { LayoutDecl $2 $4 }

-- Types ---------------------------------------------------

Type :: { PType }                   -- used at top-level layouts; may be a record-with-offset
  : RecordTy                        { $1 }
  | NonRecType                      { $1 }   -- (top-level prim/array is allowed if you want)

-- Non-record types (do not carry their own offset)
NonRecType :: { PType }
  : PrimTy                          { TPrim $1 }
  | NonRecType LBrack IntLit RBrack { TArray $1 $3 }

-- Record type *does* carry its own offset
RecordTy :: { PType }
  : KwRecord LBrace Fields RBrace Offset
      { TRecord $5 $3 }

PrimTy :: { Prim }
  : TyU8  { U8 }  | TyU16 { U16 } | TyU32 { U32 } | TyU64 { U64 }
  | TyI8  { I8 }  | TyI16 { I16 } | TyI32 { I32 } | TyI64 { I64 }
  | IntLit BByte  { case $1 of
                      1 -> U8; 2 -> U16; 4 -> U32; 8 -> U64
                      n -> error ("unsupported byte size: " ++ show n)
                  }

-- Offsets -------------------------------------------------

Offset :: { POffset }
  : At IntLit BByte                 { AbsB $2 }
  | At Name KwAfter  OptSize        { RelAfter  $2 $4 }
  | At Name KwBefore OptSize        { RelBefore $2 $4 }
  | KwAfter  Name OptSize           { RelAfter  $2 $3 }
  | KwBefore Name OptSize           { RelBefore $2 $3 }

AbsOffset :: { POffset }
  : At IntLit BByte                          { AbsB $2 }

RelOffset :: { POffset }
  -- “@ base after/before [nB]”
  : At Name KwAfter  OptSize                 { RelAfter  $2 $4 }
  | At Name KwBefore OptSize                 { RelBefore $2 $4 }
  -- postfix “after/before base [nB]”
  | KwAfter  Name OptSize                    { RelAfter  $2 $3 }
  | KwBefore Name OptSize                    { RelBefore $2 $3 }


OptSize :: { Int }
  : IntLit BByte                    { $1 }
  |                                 { 0 }

-- Fields --------------------------------------------------

Fields :: { [PField] }
  : Field                           { [$1] }
  | Fields Comma Field              { $1 ++ [$3] }

Field :: { PField }
  -- case 1: record type already carries @...
  : Name Colon RecordTy                    { PField $1 $3 (AbsB 0) Nothing }
    -- ^ we ignore the (AbsB 0) in PField; your Lower step should pull the offset from the TRecord
  -- case 2: non-record type requires an explicit offset
  | Name Colon NonRecType Offset          { PField $1 $3 $4 Nothing }
  | Name Colon NonRecType Offset KwEndian End
                                          { PField $1 $3 $4 (Just $6) }
Name :: { String }
  : Ident                                    { $1 }
  | Quoted                                   { $1 }

End :: { Endian }
  : KwLE { LE } | KwBE { BE } | KwME { ME }

{
parseError :: [Token] -> a
parseError ts = error ("Parse error. Next tokens: " ++ take 200 (show ts))
}
