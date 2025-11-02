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
  let decls = parseString toks

  -- pick the decl by name, accepting either TypeDecl or LayoutDecl
  let pick = [ d | d@(TypeDecl nm _)   <- decls, nm == typeName ]
          ++ [ d | d@(LayoutDecl nm _) <- decls, nm == typeName ]

  case pick of
    (d:_) -> do
      let (cType, lLayout) = lowerToCL d
      let (hdr, impl) = compileToStrings typeName cType lLayout
      createDirectoryIfMissing True outDir
      let base = map toLower' typeName
      B.writeFile (outDir </> base ++ ".h") (B.pack hdr)
      B.writeFile (outDir </> base ++ ".c") (B.pack impl)
      putStrLn "OK"
    _ -> fail ("Type not found: " ++ typeName)
    [] ->
      error ("No declaration named " ++ show typeName ++ " found")
  where
    toLower' c | 'A' <= c && c <= 'Z' = toEnum (fromEnum c + 32)
               | otherwise            = c