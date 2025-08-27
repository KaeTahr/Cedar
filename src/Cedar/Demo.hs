module Main where

import System.Directory (createDirectoryIfMissing)
import System.FilePath  ((</>))
import qualified Data.ByteString.Char8 as B

import Cedar.CodeGen.Boundary
import Cedar.CodeGen.EmitC (emitHeader, emitImpl)

-- Demo: a simple “Student” record packed in a byte blob.
-- Fields are placed at absolute bit offsets; you can make them overlap,
-- leave gaps, or change endianness per field.

student :: CBoundStruct
student =
  let fields =
        [ CField "student_number" (CPrim I32) (mkRange 0    32) (Just LE)
        , CField "dob"            (CPrim I32) (mkRange 32   32) (Just LE)
        , CField "maths"          (CPrim I32) (mkRange 64   32) (Just BE)
        , CField "physics"        (CPrim I32) (mkRange 96   32) (Just BE)
        ]
      totalBits = totalSizeBitsFromFields fields
  in CBoundStruct
       { sName = "Student"
       , sSizeBits = totalBits
       , sFields = fields
       }

main :: IO ()
main = do
  let outDir = "gen"
  createDirectoryIfMissing True outDir
  let hPath = outDir </> "student_cedar.h"
      cPath = outDir </> "student_cedar.c"
  writeFile hPath (emitHeader student)
  writeFile cPath (emitImpl student)
  putStrLn $ "wrote: " ++ hPath
  putStrLn $ "wrote: " ++ cPath