{
module Main (main) where
import qualified Data.Text
}

-- Adheres to C Standard ISO/IEC 9899:1999 a.k.a. C99
-- See Chapter 6.4 
-- Dargent additions are indicated by {DARGENT}

-- other standards:
-- Unicode 16.0

-- resources:
-- (useful)
-- https://haskell-alex.readthedocs.io/en/latest/index.html
-- https://www.csd.uwo.ca/~mmorenom/CS447/Lectures/Lexical.html/node11.html
-- https://gcc.gnu.org/onlinedocs/cpp/Character-sets.html

-- (others)
-- https://serokell.io/blog/lexing-with-alex
-- https://www.maxkopinsky.com/Write-You-a-Haskell-2/9/9.1_lexing 
-- https://gdevanla.github.io/posts/wya-lexer.html 

%wrapper "basic" -- keeps track of line number and column number
                -- could use posn-strict-text to be more memory efficient

-- MACROS

-- general
$digit         = [0-9]
$alpha         = [a-zA-Z]
$octalDigit    = [0-7]
$hexDigit      = [0-9a-fA-F]
$nondigit      = [_$alpha] 

-- Universal Chars
@hexQuad            = $hexDigit{4}
@universalCharName  = (\\ u @hexQuad) | (\\ U @hexQuad{2})

-- Identifier 6.4.2
@identifierNondigit = $nondigit | @universalCharName
@identifier =  @identifierNondigit (@identifierNondigit | $digit)*
-- unsure what implementation defined characters are

-- Constants
-- Integer 
$nonzeroDigit    = [1-9]

@unsignedSuffix  = "u" | "U"
@longSuffix      = "l" | "L"
@longLongSuffix  = "ll" | "LL"
@hexPrefix       = "0x" | "0X"

@octalConstant   = 0 $octalDigit*
@decimalConstant = $nonzeroDigit $digit* 
@hexConstant     = @hexPrefix $hexDigit+

@integerSuffix   = (@unsignedSuffix? @longSuffix?) 
  | (@longSuffix? @unsignedSuffix?) | (@unsignedSuffix? @longLongSuffix)  
  | (@longLongSuffix @unsignedSuffix?)

@integerConstant = (@octalConstant @integerSuffix?) 
  | (@decimalConstant @integerSuffix?) | (@hexConstant @integerSuffix?)

-- Floating
$floatingSuffix = [fFlL]

@fractionalConstant = ($digit* "." $digit+) | ($digit+ ".")
@sign               = "+" | "-" -- could write as [\+\-]
@exponentPart       = [eE] @sign? $digit+
@decimalFloating    = (@fractionalConstant @exponentPart? $floatingSuffix?) 
  | ($digit+ @exponentPart $floatingSuffix?)

@hexadecimalFractional   = ($hexDigit* "." $hexDigit+) | ($hexDigit+ ".")
@binaryExponentPart      = [pP] @sign? $digit+
@hexadecimalFloating     = 
  (@hexPrefix @hexadecimalFractional @binaryExponentPart $floatingSuffix?) 
  | (@hexPrefix $hexDigit+ @binaryExponentPart $floatingSuffix?)

@floatingConstant = @decimalFloating | @hexadecimalFloating

-- Enum
-- makes me think that we need to classify things multiple ways?
-- current pattern matching won't work here
@enumConstant = @identifier

-- Character

-- escapes appear as \\ in output for some reason
-- examine simple char
$simpleChar                = [\' \" \? \\ \a \b \f \n \r \t \v]
@hexadecimalEscapeSequence = [\\] "x" $hexDigit+
@octalEscapeSequence       = [\\] $octalDigit{1,3}
@simpleEscapeSequence      = [\\] $simpleChar

@escapeSequnce             = @simpleEscapeSequence | @hexadecimalEscapeSequence
  | @octalEscapeSequence | @universalCharName 



-- MISSING: any member of the source character set except the single-quote ', backslash \, or new-line character
--@cChar          =    


-- Character Sets 5.2.1 (SOURCE AND EXECUTION SETS ARE PLACEHOLDERS)

-- all blocks of unicode

-- basic source and execution character set
$basicCharSet = [a-zA-Z0-9 \! \" \# \% \& \' \( \) \* \+ \, \- \. \/ \: \; \< 
\= \> \? \[ \\ \] \ˆ \_ \{ \| \} \˜] -- this tilde looks wrong

-- extended characters
-- eg. latin alphabet with accents, currency symbols, symbols in languages

-- source character set (UTF-8)
@srcCharSet = $basicCharSet 

-- execution character set (UTF-8 by default)
@srcCharSet = $basicCharSet 

-- any member of the source character set except the single-quote ', backslash \, or new-line character

-- String Literals
-- string literal
-- s-char-sequence
-- s-char

-- Keywords 6.4.1
-- C keywords
@cKeyword = "auto" | "break" | "case" | "char" | "const" | "continue" 
  | "default" | "do" | "double" | "else" | "enum" | "extern" | "float" | "for" 
  | "goto" | "if" | "inline" | "int" | "long" | "register" | "restrict" 
  | "return" | "short" | "signed" | "sizeof" | "static" | "struct" | "switch" 
  | "typedef" | "union" | "unsigned" | "void" | "volatile" | "while" | "_Bool" 
  | "_Complex" | "_Imaginary"

-- PUNCTUATORS 6.4.6
@punctuator = "[" | "]" | "(" | ")" | "{" | "}" | "." | "&" | "*" | "+" | "-" 
  | "~" | "!" | "/" | "%" | "<" | ">" | "^" | "|" | "?" | ":" | ";" | "=" 
  | "," | "#" | "->" | "++" | "--" | "&&" | "||" | "<=" | ">=" | "==" | "!=" 
  | "<<" | ">>" | "+=" | "-=" | "*=" | "/=" | "%=" | "<<=" | ">>=" | "&=" 
  | "^=" | "|=" | "..." | "##" | "<:" | ":>" | "<%" | "%>" | "%:" | "%:%:"


-- {DARGENT} 
-- Dargent keywords
@dargentKeyword = "layout" | "before" | "after" | "gap" | "at" | "BE" | "LE"
-- not including machine endianess

-- Dargent Memory Sizes
@dargentMemorySize = $digit+ B \  $octalDigit b

-- overview

-- token:
  -- keyword 
  -- identifier
  -- constant
  -- string-literal
  -- punctuator


tokens :-
  -- Whitespace
  $white+                      ;

  -- DARGENT memory size
  @dargentMemorySize       {\s -> MemorySize s}

  -- COMMENTS 6.4.9
  -- not immediately clear how to implement
  -- no need to tokenize comments

  -- KEYWORDS 6.4.1
  @cKeyword | @dargentKeyword {\s -> Keyword s}

  -- CONSTANTS 6.4.4
  -- Int Const 6.4.4.1
  @integerConstant   { \s -> TokenIntegerConstant s } 

  -- Float Const 6.4.4.2
  @floatingConstant  { \s -> TokenFloatingConstant s }
  
  -- Enum Const 6.4.4.3
  -- exactly mirrors identifier pattern
  -- handled semantically
  -- cannot see a reason to handle at a lexer level yet

  -- Char Const 6.4.4.4
  -- @octalEscapeSequence  { \s -> TokenCharConstant s }
  -- @hexadecimalEscapeSequence  { \s -> TokenCharConstant s }
  -- --[L]? $c-char-sequence  { \s -> TokenCharConstant s } 

  -- STRING-LITERALS 6.4.5

  -- PUNCTUATORS 6.4.6
  @punctuator         { \s -> Punctuator s }

  -- IDENTIFIERS 6.4.2 
  -- should go last since it is the most general
  @identifier { \s -> Identifier s }

  -- Debugging
  . { \s -> error ("Lexical error: unexpected input " ++ show s) }


{
-- Each action has type :: String -> Token (template hangover)

-- token constructors!
data Token
  = Keyword String
  | Identifier String
  | Punctuator String
  | TokenFloatingConstant String
  | TokenIntegerConstant String 
  | TokenCharConstant String
  | MemorySize String
  deriving (Eq, Show)

main :: IO ()
main = do
  s <- getContents
  print (alexScanTokens s)
}

-- instructions for running
-- cd .\code\Cedar\Lexer\
-- alex Lexer.x
-- ghc Lexer.hs -o lexer
-- Get-Content input.txt | ./lexer