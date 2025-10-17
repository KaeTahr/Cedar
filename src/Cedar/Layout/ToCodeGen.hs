module Cedar.Layout.ToCodeGen
  ( CGField(..), CGStruct(..)
  , layoutRecordToCGStruct
  , wordCountFromStruct
  ) where

import qualified Data.Map as M
import Cedar.CodeGen.Allocation (AlignedBitRange(..))
import Cedar.Layout.Core
import Cedar.Layout.Surface (Endianness(..))

data CGField = CGField
  { cgName   :: String
  , cgRanges :: [AlignedBitRange]
  , cgEndian :: Endianness
  } deriving (Show, Eq)

data CGStruct = CGStruct
  { cgTypeName :: String
  , cgWordCount :: Int
  , cgFields   :: [CGField]
  } deriving (Show, Eq)

-- Compute words needed from AlignedBitRanges (32-bit lanes assumed here)
wordsNeeded :: [AlignedBitRange] -> Int
wordsNeeded rs =
  let maxWord = maximum (0 : [ fromIntegral w | AlignedBitRange _ _ w <- rs ])
  in maxWord + (if null rs then 0 else 1)

wordCountFromStruct :: CGStruct -> Int
wordCountFromStruct (CGStruct _ _ fs) =
  maximum (1 : [ wordsNeeded (cgRanges f) | f <- fs ])

-- Bridge: only handles RecordLayout of PrimLayout for now
layoutRecordToCGStruct :: String -> DataLayout [AlignedBitRange] -> CGStruct
layoutRecordToCGStruct typeName (Layout (RecordLayout mp)) =
  let fields =
        [ CGField { cgName = fname
                  , cgRanges = rs
                  , cgEndian = ω
                  }
        | (fname, PrimLayout rs ω) <- M.toList mp
        ]
      wc = maximum (1 : [ wordsNeeded (cgRanges f) | f <- fields ])
  in CGStruct { cgTypeName = typeName, cgWordCount = wc, cgFields = fields }
layoutRecordToCGStruct _ _ =
  error "layoutRecordToCGStruct: expected Layout (RecordLayout ...)"
