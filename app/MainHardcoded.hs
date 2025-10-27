module Main where

import qualified Data.ByteString.Char8 as B
import qualified Data.Map as M
import System.FilePath ((</>))
import System.Directory (createDirectoryIfMissing)

import Cedar.CodeGen.CodeGen (emitHeader, emitImpl)
import Cedar.Layout.ToCodeGen (layoutRecordToCGStruct)
import Cedar.Layout.Core (DataLayout(..), DataLayout'(..))
import Cedar.CodeGen.Allocation (AlignedBitRange(..))
import Cedar.Layout.Surface (Endianness(..))

studentLR :: DataLayout [AlignedBitRange]
studentLR =
  Layout $ RecordLayout $ M.fromList
    [ ( "f1", PrimLayout [AlignedBitRange 32 0 0] ME )
    , ( "f2", PrimLayout [AlignedBitRange 32 0 1, AlignedBitRange 32 0 2] BE )
    ]

main :: IO ()
main = do
  let outDir   = "out"
      typeName = "t1"
      cg       = layoutRecordToCGStruct typeName studentLR

  createDirectoryIfMissing True outDir
  B.writeFile (outDir </> (map toLower' typeName ++ ".h")) (B.pack (emitHeader cg))
  B.writeFile (outDir </> (map toLower' typeName ++ ".c")) (B.pack (emitImpl cg))
  putStrLn "Generated C in ./out/"
  where
    toLower' c | 'A' <= c && c <= 'Z' = toEnum (fromEnum c + 32)
               | otherwise            = c
