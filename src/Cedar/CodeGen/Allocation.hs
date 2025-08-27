{-# LANGUAGE RecordWildCards #-}
module Cedar.CodeGen.Allocation
  ( -- sizes
    byteSizeBits
  , wordSizeBits
    -- bit ranges
  , BitRange(..)
  , newBitRangeBaseSize
  , newBitRangeFromTo
  , AlignedBitRange(..)
  , rangeToAlignedRanges
  , alignSize
  ) where

-- TODO: should be in a config file
byteSizeBits :: Integer
byteSizeBits = 8

-- TODO: we're just assuming 64-bit words
wordSizeBits :: Integer
wordSizeBits = 64

-- A contiguous bit slice: [bitOffset .. bitOffset + bitSize - 1]
data BitRange = BitRange
  { bitSizeBR   :: Integer
  , bitOffsetBR :: Integer
  } deriving (Eq, Ord, Show)

newBitRangeBaseSize :: Integer -> Integer -> Maybe BitRange
newBitRangeBaseSize bitOffsetBR bitSizeBR
  | bitOffsetBR >= 0 && bitSizeBR >= 0 = Just BitRange{..}
  | otherwise                          = Nothing

newBitRangeFromTo :: Integer -> Integer -> Maybe BitRange
newBitRangeFromTo from to
  | 0 <= from && from <= to = Just BitRange { bitSizeBR = to - from, bitOffsetBR = from }
  | otherwise               = Nothing

-- True iff two non-empty bit ranges overlap
overlaps :: BitRange -> BitRange -> Bool
overlaps (BitRange s1 o1) (BitRange s2 o2) =
  o1 < o2 + s2 && o2 < o1 + s1 && s1 > 0 && s2 > 0

-- A chunk that is aligned within a fixed-size “word” lane.
-- Example field that spans multiple words gets split into these.
data AlignedBitRange = AlignedBitRange
  { bitSizeABR    :: Integer   -- <= alignSize
  , bitOffsetABR  :: Integer   -- < alignSize
  , wordOffsetABR :: Integer   -- which word (0-based) within the backing blob
  } deriving (Eq, Ord, Show)

-- Round size up to a multiple of k.
alignSize :: Integer -> Integer -> Integer
alignSize k n =
  let (q,r) = n `quotRem` k
  in if r == 0 then n else (q+1)*k

-- Split a BitRange into aligned chunks for a given lane size (word or byte).
-- If bitSizeBR == 0, returns [].
rangeToAlignedRanges :: Integer -> BitRange -> [AlignedBitRange]
rangeToAlignedRanges align (BitRange size offset)
  | size <= 0 = []
  | otherwise =
      go (offset `div` align) (offset `mod` align) size
  where
    go :: Integer -> Integer -> Integer -> [AlignedBitRange]
    go _    _    0    = []
    go wOff bOff remSz =
      let takeBits = min remSz (align - bOff)
          this     = AlignedBitRange { bitSizeABR    = takeBits
                                     , bitOffsetABR  = bOff
                                     , wordOffsetABR = wOff
                                     }
      in this : go (wOff + 1) 0 (remSz - takeBits)

-- | A single allocated slice, annotated with some payload 'p'.
type AllocationBlock p = (BitRange, p)

-- | An allocation = a set of blocks.
newtype Allocation' p = Allocation
  { unAllocation :: [AllocationBlock p]
  } deriving (Eq, Show, Ord)

type Allocation = Allocation' String   -- you can pick a nicer payload later

-- | Empty allocation
emptyAllocation :: Allocation' p
emptyAllocation = Allocation []

-- | Singleton allocation
singletonAllocation :: AllocationBlock p -> Allocation' p
singletonAllocation b = Allocation [b]

-- | Union: just concatenates the block lists.
(\/) :: Allocation' p -> Allocation' p -> Allocation' p
(Allocation a1) \/ (Allocation a2) = Allocation (a1 ++ a2)

-- | Conjunction: succeeds only if there’s no overlap.
(/\) :: Allocation' p -> Allocation' p -> Either [ (AllocationBlock p, AllocationBlock p) ] (Allocation' p)
(Allocation a1) /\ (Allocation a2) =
  case allOverlaps a1 a2 of
    [] -> Right (Allocation (a1 ++ a2))
    os -> Left os
  where
    allOverlaps :: [AllocationBlock p] -> [AllocationBlock p] -> [(AllocationBlock p, AllocationBlock p)]
    allOverlaps xbs ybs =
      [ (xb,yb) | xb@(r1,_) <- xbs, yb@(r2,_) <- ybs, overlaps r1 r2 ]