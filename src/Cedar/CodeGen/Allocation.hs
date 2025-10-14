-- ported from dargent
-- cogent/src/Cogent/Dargent/Allocation.hs

{-# LANGUAGE ScopedTypeVariables #-}

module Cedar.CodeGen.Allocation
  ( -- bit ranges
   BitRange(..)
  , newBitRangeFromTo
  , newBitRangeBaseSize
  , emptyBitRange
  , primBitRangeBits
  , pointerBitRange
  , isZeroSizedBR
    -- allocations (sets of bit ranges with payloads/paths)
  , AllocationBlock
  , OverlappingAllocationBlocks(..)
  , Allocation'(..)
  , emptyAllocation
  , singletonAllocation
  , undeterminedAllocation
  , (\/)
  , (/\)
  , overlaps
  , beginningOfAllocation
  , endOfAllocation
  , containsAllocVars
    -- aligned chunks
  , AlignedBitRange(..)
  , alignSize
  , alignOffsettable
  , rangeToAlignedRanges
  ) where

import Cedar.Compat.Basic
  ( Size, VarName
  , byteSizeBits, wordSizeBits, pointerSizeBits
  , Offsettable(..)
  )

-- ===== Bit ranges =====

-- contiguous bit slice: [bitOffset .. bitOffset + bitSize - 1]
data BitRange = BitRange
  { bitSizeBR   :: Size
  , bitOffsetBR :: Size
  } deriving (Eq, Ord, Show)

instance Offsettable BitRange where
  offset n br
    | n >= 0    = br { bitOffsetBR = bitOffsetBR br + n }
    | otherwise = error "offset: negative offset not allowed"

newBitRangeFromTo :: Size -> Size -> Maybe BitRange
newBitRangeFromTo from to
  | 0 <= from && from <= to = Just BitRange { bitSizeBR = to - from, bitOffsetBR = from }
  | otherwise               = Nothing

newBitRangeBaseSize :: Size -> Size -> Maybe BitRange
newBitRangeBaseSize bitOffsetBR bitSizeBR
  | bitOffsetBR >= 0 && bitSizeBR >= 0 = Just BitRange {..}
  | otherwise                          = Nothing

emptyBitRange :: BitRange
emptyBitRange = BitRange { bitSizeBR = 0, bitOffsetBR = 0 }

primBitRangeBits :: Size -> BitRange
primBitRangeBits n = BitRange { bitSizeBR = n, bitOffsetBR = 0 }

pointerBitRange :: BitRange
pointerBitRange = BitRange { bitSizeBR = pointerSizeBits, bitOffsetBR = 0 }

isZeroSizedBR :: BitRange -> Bool
isZeroSizedBR BitRange{..} = bitSizeBR == 0

-- ===== Allocations =====

-- the smallest piece of an allocation
type AllocationBlock p = (BitRange, p)

newtype OverlappingAllocationBlocks p =
  OverlappingAllocationBlocks { unOverlappingAllocationBlocks :: (AllocationBlock p, AllocationBlock p) }
  deriving (Eq, Show, Ord)

-- a set of bit ranges (union) + “allocation variables” (for unknown offsets)
data Allocation' p = Allocation
  { unAllocation :: [AllocationBlock p]
  , allocVars    :: [(VarName, Size)]
  } deriving (Eq, Show, Ord)

instance Offsettable (Allocation' p) where
  offset n (Allocation bs vs) =
    Allocation (map (\(br,p) -> (offset n br, p)) bs) (map (\(v,off) -> (v, off + n)) vs)

emptyAllocation :: Allocation' p
emptyAllocation = Allocation [] []

singletonAllocation :: AllocationBlock p -> Allocation' p
singletonAllocation b = Allocation [b] []

undeterminedAllocation :: [VarName] -> Allocation' p
undeterminedAllocation vs = Allocation [] (map (\v -> (v,0)) vs)

-- union (disjunction)
(\/) :: forall p. Ord p => Allocation' p -> Allocation' p -> Allocation' p
(Allocation a1 vs1) \/ (Allocation a2 vs2) = Allocation (a1 ++ a2) (vs1 ++ vs2)

-- do two bit ranges overlap (strictly positive intersection)?
overlaps :: BitRange -> BitRange -> Bool
overlaps (BitRange s1 o1) (BitRange s2 o2) =
  o1 < o2 + s2 && o2 < o1 + s1 && s1 > 0 && s2 > 0

-- conjunction: ensures two allocations can be used simultaneously (no overlaps)
(/\) :: forall p. Ord p
     => Allocation' p -> Allocation' p
     -> Either [OverlappingAllocationBlocks p] (Allocation' p)
(Allocation a1 vs1) /\ (Allocation a2 vs2) =
  case allOverlappingBlocks a1 a2 of
    xs@(_ : _) -> Left xs
    []         -> Right $ Allocation (a1 ++ a2) (vs1 ++ vs2)
  where
    allOverlappingBlocks :: [AllocationBlock p] -> [AllocationBlock p] -> [OverlappingAllocationBlocks p]
    allOverlappingBlocks xbs ybs =
      [ OverlappingAllocationBlocks (xb, yb)
      | xb@(brx,_) <- xbs
      , yb@(bry,_) <- ybs
      , overlaps brx bry
      ]

beginningOfAllocation :: Allocation' p -> Size
beginningOfAllocation (Allocation []     _ ) = 0
beginningOfAllocation (Allocation blocks _ ) = minimum [ o | (BitRange _ o, _) <- blocks ]

endOfAllocation :: Allocation' p -> Size
endOfAllocation (Allocation []     _ ) = 0
endOfAllocation (Allocation blocks _ ) = maximum [ o + s | (BitRange s o, _) <- blocks ]

containsAllocVars :: Allocation' p -> Bool
containsAllocVars = not . null . allocVars

-- ===== Aligned chunking =====

-- a chunk that fits within a fixed-sized “word” lane (e.g., 32-bit)
data AlignedBitRange = AlignedBitRange
  { bitSizeABR    :: Size   -- <= align lane size
  , bitOffsetABR  :: Size   -- <  align lane size
  , wordOffsetABR :: Size   -- which lane (0-based)
  } deriving (Eq, Ord, Show)

-- round up to a multiple
alignSize :: Size -> Size -> Size
alignSize k n =
  let (q,r) = n `quotRem` k
  in if r == 0 then n else (q+1)*k

-- align an Offsettable thing (assumed to start at offset 0) to >= minBitOffset,
-- snapping to multiples of alignBitSize
alignOffsettable :: Offsettable a => Size -> Size -> a -> a
alignOffsettable alignBitSize minBitOffset =
  offset (alignSize alignBitSize minBitOffset)

-- split a BitRange into lane-aligned chunks
-- if bitSizeBR == 0, returns []
rangeToAlignedRanges :: Size -> BitRange -> [AlignedBitRange]
rangeToAlignedRanges lane (BitRange size offset)
  | size <= 0  = []
  | otherwise  = go (offset `div` lane) (offset `mod` lane) size
  where
    go :: Size -> Size -> Size -> [AlignedBitRange]
    go _    _    0 = []
    go wOff bOff remSz =
      let takeBits = min remSz (lane - bOff)
          this     = AlignedBitRange
                       { bitSizeABR    = takeBits
                       , bitOffsetABR  = bOff
                       , wordOffsetABR = wOff
                       }
      in this : go (wOff + 1) 0 (remSz - takeBits)