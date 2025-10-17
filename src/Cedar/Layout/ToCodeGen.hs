{-# LANGUAGE NamedFieldPuns #-}
module Cedar.Layout.ToCodeGen
  ( layoutToCodeGenParts
  , wordCountFromParts
  ) where

import qualified Data.Map as M
import Data.List (foldl')
import Cedar.CodeGen.Allocation (AlignedBitRange(..))
import Cedar.Layout.Surface (Endianness(..))
import Cedar.Layout.Core
  ( DataLayout(..)
  , DataLayout'(..)
  )

-- | Extract (fieldName, [AlignedBitRange], Endianness) triples from a record layout.
--   Expects a *record* layout whose fields are PrimLayout leaves (already aligned to ABR).
--   Anything else is ignored for now.
--   TODO:  extend?
layoutToCodeGenParts
  :: DataLayout [AlignedBitRange]
  -> [(String, [AlignedBitRange], Endianness)]
layoutToCodeGenParts (Layout (RecordLayout fields)) =
  [ (fname, brs, ω)
  | (fname, PrimLayout { bitsDL = brs, endianness = ω }) <- M.toList fields
  ]
layoutToCodeGenParts _ = []
  -- Future: handle nested records/sums/arrays here if you want

-- | Compute how many 32-bit words we need for the backing storage,
--   based on the maximum wordOffset touched by any field’s aligned ranges.
wordCountFromParts
  :: [(String, [AlignedBitRange], Endianness)]
  -> Int
wordCountFromParts parts =
  let maxWord = foldl' step 0 parts
      step acc (_, brs, _) =
        let localMax = maximum (0 : [ fromInteger (wordOffsetABR r) + 1 | r <- brs ])
        in max acc localMax
  in max 1 maxWord
