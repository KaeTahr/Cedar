module Main where

import qualified Data.ByteString.Char8 as B
import System.FilePath ((</>))
import System.Directory (createDirectoryIfMissing)

import Cedar.Pipeline (compileToStrings)

-- semantic ASTs
import qualified Cedar.Semantic.C as C
import qualified Cedar.Semantic.L as L


-- C
studentC :: C.CType
studentC = C.Struct [
    ("Student Number", C.Int C.I32 C.Signed C.noattr), 
    ("DOB", C.Int C.I32 C.Signed C.noattr), 
    ("Grades", C.Struct [
        ("Maths", C.Int C.I32 C.Signed C.noattr),
        ("Physics", C.Int C.I32 C.Signed C.noattr)
        ])
    ]

-- L
studentL :: L.Layout
studentL = L.Struct (L.AbsOffset (L.Bit 0))
    [
    ("Student Number", L.Primitive (L.Byte 4) (L.AbsOffset (L.Byte 0)) L.LE),
    ("DOB", L.Primitive (L.Byte 4) (L.RelOffset "Grades" (L.After (L.Byte 0))) L.LE),
    ("Grades", L.Struct (L.RelOffset "Student Number" (L.After (L.Byte 2))) [
        ("Maths", L.Primitive (L.Byte 4) (L.AbsOffset (L.Byte 0)) L.BE),
        ("Physics", L.Primitive (L.Byte 4) (L.RelOffset "Maths" (L.After (L.Byte 0))) L.BE)
        ] L.ME)
    ] L.LE


main :: IO ()
main = do
  let outDir   = "out"
      typeName = "Student"

      (hdr, impl) = compileToStrings typeName studentC studentL

  createDirectoryIfMissing True outDir
  let base = fmap toLower' typeName
      hPath = outDir </> base ++ ".h"
      cPath = outDir </> base ++ ".c"

  B.writeFile hPath (B.pack hdr)
  B.writeFile cPath (B.pack impl)
  putStrLn "Generated C in ./out/:"
  putStrLn ("  " ++ hPath)
  putStrLn ("  " ++ cPath)

  where
    toLower' c | 'A' <= c && c <= 'Z' = toEnum (fromEnum c + 32)
               | otherwise            = c
