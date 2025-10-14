module Main where

import qualified Data.ByteString.Char8 as B
import System.Directory (createDirectoryIfMissing)
import System.FilePath ((</>))
import Data.Char (toLower)
import Cedar.CodeGen.CodeGen (emitHeader, emitImpl)
import Cedar.CodeGen.Allocation (AlignedBitRange(..))
import Cedar.Layout.Surface (Endianness(..))

main :: IO ()
main = do
  let outDir = "out"
  createDirectoryIfMissing True outDir

  let typeName  = "t1"
      wordCount = 3

      -- Fields:
      -- f1: 32 bits at word 0, bit 0
      -- f2: 64 bits split as 32@word1 + 32@word2 (both at bit 0)
      parts :: [(String, [AlignedBitRange], Endianness)]
      parts =
        [ ("f1", [AlignedBitRange 32 0 0], ME)
        , ("f2", [AlignedBitRange 32 0 1, AlignedBitRange 32 0 2], ME)
        ]

  let base = map toLower typeName
  B.writeFile (outDir </> base ++ ".h") (B.pack (emitHeader typeName wordCount))
  B.writeFile (outDir </> base ++ ".c") (B.pack (emitImpl   typeName parts))
