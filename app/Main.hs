module Main where

import System.Directory (createDirectoryIfMissing)
import System.FilePath  ((</>))
import qualified Data.ByteString.Char8 as B

import Cedar.CodeGen.Boundary
import Cedar.CodeGen.CodeGen   -- <— your generator (emitHeader/emitImpl or similar)

student :: CBoundStruct
student = CBoundStruct
  { sName     = "Student"
  , sSizeBits = totalSizeBitsFromFields fields
  , sFields   = fields
  }
  where
    fields =
      [ CField "id"    (CPrim I32) (Range 0   32) (Just LE)
      , CField "age"   (CPrim I16) (Range 32  16) (Just LE)
      , CField "grade" (CPrim I8 ) (Range 48   8) (Just LE)
      ]

main :: IO ()
main = do
  let outDir = "out"
  createDirectoryIfMissing True outDir
  B.writeFile (outDir </> "student_cedar.h") (B.pack (emitHeader student))
  B.writeFile (outDir </> "student_cedar.c") (B.pack (emitImpl   student))
  putStrLn "Wrote out/student_cedar.h and out/student_cedar.c"