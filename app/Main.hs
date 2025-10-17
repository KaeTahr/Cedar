module Main where

import qualified Data.ByteString.Char8 as B
import System.FilePath ((</>))
import System.Directory (createDirectoryIfMissing)

import Cedar.CodeGen.CodeGen (emitHeader, emitImpl)
import Cedar.Layout.ToCodeGen (layoutToCodeGenParts, wordCountFromParts)

-- bring in (or build) your aligned layout here
import Cedar.Layout.Core (DataLayout(..), DataLayout'(..))
import Cedar.CodeGen.Allocation (rangeToAlignedRanges) -- to align manually
import Cedar.Layout.Core (alignLayout')

-- Suppose you have an already-aligned record layout value:
studentLR :: DataLayout [AlignedBitRange]
-- You can mock one for now if needed.

main :: IO ()
main = do
  let outDir   = "out"
      typeName = "t1"

  createDirectoryIfMissing True outDir

  -- get parts + size from the layout
  let parts     = layoutToCodeGenParts studentLR
      wordCount = wordCountFromParts parts

  -- generate header and impl
  B.writeFile (outDir </> (map toLower typeName ++ ".h"))
    (B.pack (emitHeader typeName wordCount))

  B.writeFile (outDir </> (map toLower typeName ++ ".c"))
    (B.pack (emitImpl typeName parts))

  putStrLn "Generated C in ./out/"
