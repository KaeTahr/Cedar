module Cedar.Main where

-- contains transpilation pipeline

-- import Lexer
-- import Parser
-- import CodeGen
-- import System.Environment (getArgs)
-- import System.IO (hGetContents, stdin)
-- import Tokens
-- import Control.Exception (catch, IOException)

-- main :: IO ()
-- main = do
--   args <- getArgs
--   input <- case args of
--     []         -> hGetContents stdin
--     [filename] -> readFile filename `catch` handleReadError
--     _          -> error "Usage: transpiler [filename]"

--   let tokens = alexScanTokens input
--   let ast = parse tokens
--   let outputCode = generateProgram (Program ast)
--   putStrLn outputCode

-- handleReadError :: IOException -> IO String
-- handleReadError _ = error "Error: Unable to read file."



-- Test for CodeGen

import System.IO
import qualified Data.Char as Char
import Cedar.CodeGen.EmitC

student :: CBoundStruct
student = CBoundStruct
  { sName     = "Student"
  , sSizeBits = 96  -- 12 bytes
  , sFields   =
      [ CField "name"   (CPrim Ptr)            (Range 0   64) (Just LE)
      , CField "age"    (CPrim I8)             (Range 64   8) Nothing
      , CField "maths"  (CPrim I8)             (Range 72   8) Nothing
      , CField "physics"(CPrim I16)            (Range 80  16) (Just LE)
      ]
  }

main :: IO ()
main = do
  writeFile "student_cedar.h" (emitHeader student)
  writeFile "student_cedar.c" (emitImpl   student)
  putStrLn "Wrote student_cedar.h / student_cedar.c"