module Main where

import qualified Data.ByteString.Char8 as B
import System.Environment (getArgs)
import System.Directory (createDirectoryIfMissing)
import System.FilePath ((</>))

import Cedar.Pipeline (compileToStrings)
import Cedar.Frontend.Subset.AST
import Cedar.Frontend.Subset.Lexer (alexScanTokens)
import Cedar.Frontend.Subset.Parser (parseString)
import Cedar.Frontend.Subset.Lower (lowerToCL)

main :: IO ()
main = do
  args <- getArgs
  case args of
    [inFile, typeName, outDir] -> run inFile typeName outDir
    _ -> putStrLn "Usage: cedarc <file.cedar> <TypeName> <outDir>"

run :: FilePath -> String -> FilePath -> IO ()
run inFile typeName outDir = do
  src <- readFile inFile
  let toks = alexScanTokens src
      decls = parseString toks
      pick = [ d | d@(TypeDecl nm _) <- decls, nm == typeName ]
  case pick of
    (d:_) -> do
      let (cTy, lLay) = lowerToCL d
          (hdr, impl) = compileToStrings typeName cTy lLay
      createDirectoryIfMissing True outDir
      let base = map toLower' typeName
      B.writeFile (outDir </> base ++ ".h") (B.pack hdr)
      B.writeFile (outDir </> base ++ ".c") (B.pack impl)
      putStrLn "OK"
    _ -> fail ("Type not found: " ++ typeName)
  where
    toLower' c | 'A' <= c && c <= 'Z' = toEnum (fromEnum c + 32)
               | otherwise            = c
