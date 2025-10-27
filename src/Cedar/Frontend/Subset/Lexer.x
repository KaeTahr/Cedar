{
module Cedar.Frontend.Subset.Lexer (Token(..), alexScanTokens) where
}

%wrapper "basic"

$digit   = 0-9
$alpha   = [A-Za-z_]
$alnum   = [A-Za-z0-9_]
$space   = [\ \t\r\n]
$stringc = [^"\\\n]

tokens :-

$space+                           ;
"//".*                            ;

"type"                            { \_ -> KwType }
"record"                          { \_ -> KwRecord }
"endian"                          { \_ -> KwEndian }
"LE"                              { \_ -> KwLE }
"BE"                              { \_ -> KwBE }
"ME"                              { \_ -> KwME }

"U8"                              { \_ -> TyU8 }
"U16"                             { \_ -> TyU16 }
"U32"                             { \_ -> TyU32 }
"U64"                             { \_ -> TyU64 }
"I8"                              { \_ -> TyI8 }
"I16"                             { \_ -> TyI16 }
"I32"                             { \_ -> TyI32 }
"I64"                             { \_ -> TyI64 }

"{"                               { \_ -> LBrace }
"}"                               { \_ -> RBrace }
"["                               { \_ -> LBrack }
"]"                               { \_ -> RBrack }
":"                               { \_ -> Colon }
","                               { \_ -> Comma }
"@"                               { \_ -> At }
"B"                               { \_ -> BByte }
"="                               { \_ -> Eq }
"|"                               { \_ -> Pipe }

$alpha $alnum*                    { \s -> Ident s }
\" $stringc* \"                   { \s -> Quoted (read s) }

$digit+                           { \s -> IntLit (read s) }

{
data Token
  = KwType | KwRecord
  | KwEndian | KwLE | KwBE | KwME
  | TyU8 | TyU16 | TyU32 | TyU64 | TyI8 | TyI16 | TyI32 | TyI64
  | LBrace | RBrace | LBrack | RBrack | Colon | Comma | At | BByte | Eq | Pipe
  | Ident String | Quoted String | IntLit Int
  deriving (Show, Eq)
}
