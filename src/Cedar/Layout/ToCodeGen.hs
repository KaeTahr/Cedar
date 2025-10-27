module Cedar.Layout.ToCodeGen
  ( CGStruct(..)
  , CGField(..)
  , layoutRecordToCGStruct
  ) where

import qualified Data.Map as M

import Cedar.CodeGen.Allocation (AlignedBitRange(..))
import Cedar.Layout.Surface (Endianness(..))
import Cedar.Layout.Core (DataLayout(..), DataLayout'(..))
import Cedar.Compat.Basic (Size(..))  -- <-- add this

-- Codegen-facing struct description
data CGStruct = CGStruct
  { cgTypeName  :: String
  , cgWordCount :: Int
  , cgFields    :: [CGField]
  }

data CGField = CGField
  { cgName   :: String
  , cgRanges :: [AlignedBitRange]
  , cgEndian :: Endianness
  , cgSigned :: Bool
  }

-- convert Size (Integer-backed) to Int
toInt :: Size -> Int
toInt = fromIntegral

layoutRecordToCGStruct :: String -> DataLayout [AlignedBitRange] -> CGStruct
layoutRecordToCGStruct typeName (Layout (RecordLayout mp)) =
  let fields =
        [ CGField fname ranges omega False
        | (fname, PrimLayout ranges omega) <- M.toList mp
        ]

      -- FIX: convert wordOffsetABR (Size) -> Int before maximum
      maxLane :: Int
      maxLane =
        maximum (0 : [ toInt woff
                     | CGField _ rs _ _ <- fields
                     , AlignedBitRange _ _ woff <- rs
                     ])

      wordCount :: Int
      wordCount = maxLane + 1

  in CGStruct typeName wordCount fields

layoutRecordToCGStruct typeName _ =
  CGStruct typeName 0 []
