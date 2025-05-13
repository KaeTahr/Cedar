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