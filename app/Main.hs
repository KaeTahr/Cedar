module Main where

import Cedar.CodeGen.Boundary
  ( Prim(..), CType(..), Endian(..)
  , Range(..), CField(..), CBoundStruct(..)
  , totalSizeBitsFromFields
  )
import Cedar.CodeGen.EmitC (emitHeader, emitImpl)

student :: CBoundStruct
student =
  let fields =
        [ CField "id"   (CPrim I32) (Range 0   32) (Just LE)
        , CField "age"  (CPrim I16) (Range 32  16) (Just BE)
        , CField "gpa"  (CPrim F32) (Range 48  32) Nothing
        ]
      sizeBits = totalSizeBitsFromFields fields  -- safer than hardcoding
  in CBoundStruct { sName = "Student", sSizeBits = sizeBits, sFields = fields }

main :: IO ()
main = do
  writeFile "student_cedar.h" (emitHeader student)
  writeFile "student_cedar.c" (emitImpl student)
  putStrLn "Wrote student_cedar.h and student_cedar.c"
