-- Depracated?
-- This file was used for the demo pipeline of hardcoded LRs
-- LR/LRToCodeGen implements new functionality
-- File is left for completion of previous examples

module Cedar.Layout.ToCodeGen
  ( CGStruct(..)
  , CGField(..)
  , layoutRecordToCGStruct
  ) where

import qualified Data.Map as M
import           Data.List (intercalate)

import Cedar.CodeGen.Allocation (AlignedBitRange(..))
import Cedar.Layout.Surface (Endianness(..))
import Cedar.Layout.Core (DataLayout(..), DataLayout'(..))  -- Layout, RecordLayout, PrimLayout
import Cedar.Compat.Basic (Size(..))

-- Codegen-facing struct description
data CGStruct = CGStruct
  { cgTypeName  :: String
  , cgWordCount :: Int                -- number of 32-bit lanes
  , cgFields    :: [CGField]
  }

-- One field = name + its ABR slices + endianness + signedness
data CGField = CGField
  { cgName   :: String
  , cgRanges :: [AlignedBitRange]
  , cgEndian :: Endianness
  , cgSigned :: Bool
  }

-- convert Size (Integer-backed) to Int
toInt :: Size -> Int
toInt = fromIntegral

-- Build a CGStruct from a (possibly nested) record layout of primitives.
-- We flatten nested record paths into "parent__child__leaf".
layoutRecordToCGStruct :: String -> DataLayout [AlignedBitRange] -> CGStruct
layoutRecordToCGStruct typeName (Layout root) =
  let fields = flatten [] root
      maxLane =
        maximum (0 : [ toInt woff
                     | CGField _ rs _ _ <- fields
                     , AlignedBitRange _ _ woff <- rs
                     ])
      wordCount = maxLane + 1
  in CGStruct typeName wordCount fields

-- If given a non-record top-level, just return empty
layoutRecordToCGStruct typeName _ = CGStruct typeName 0 []

-- Recursively flatten a DataLayout' tree into CGFields.
-- Accumulate a path of names; join with "__" at leaves.
flatten :: [String] -> DataLayout' [AlignedBitRange] -> [CGField]
flatten path (RecordLayout mp) =
  concat
    [ case sub of
        PrimLayout ranges omega ->
          [ CGField (joinName (path ++ [fname])) ranges omega False ]
        RecordLayout _ ->
          flatten (path ++ [fname]) sub
    | (fname, sub) <- M.toList mp
    ]
flatten path (PrimLayout ranges omega) =
  [ CGField (joinName path) ranges omega False ]

-- join path segments with "__"
joinName :: [String] -> String
joinName = intercalate "__"
