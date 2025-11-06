-- Original Author: Goerge Anderson

module Cedar.Semantic.LR where

import Data.List

-- Memory (In Bit representation)

type MemorySize = Int -- Size in bits
type Offset = MemorySize
type Size = MemorySize

data Range = Range Offset Size
    deriving (Show)

--------------------------------------------------------------------------------

type FieldName = String
type Length = Int

data Endianess = BE | LE | ME -- ME, endianess of the target machine
    deriving (Show, Eq)

-- layouts need to be non-empty
data Layout = Primitive Range Endianess
    | Array Offset Layout Length Endianess
    | Struct Offset [(FieldName, Layout)] Endianess
    | Union Offset [(FieldName, Layout)] Endianess
    deriving (Show)

-- removed (no memory size for composites now): CompositeLayoutTooSmall

data RawError =
    NegativeOffset
    | NegativeSizeAllocation -- Could have a zero size union field (Just Nothing)
    | StructOverlappingFields
    deriving (Show)

-- Result can accumulate multiple errors
data Result = NoError | Errors [RawError]
    deriving (Show)

-- Define Semigroup and Monoid instances for Result to allow error accumulation
instance Semigroup Result where
    NoError <> NoError = NoError
    NoError <> Errors es = Errors es
    Errors es <> NoError = Errors es
    Errors es1 <> Errors es2 = Errors (es1 ++ es2)

instance Monoid Result where
    mempty = NoError

--------------------------------------------------------------------------------
-- Basic operations:

-- Function for getting the fieldname from a list of fields
findFieldName :: String -> [(String, Layout)] -> Maybe Layout
findFieldName = lookup

-- Get the minimum start offset of a layout
start :: Layout -> MemorySize
start (Primitive (Range primOffset _) _) = primOffset
start (Array arrayOffset _ _ _) = arrayOffset
start (Struct structOffset fields _) =
    if null fields then structOffset else minimum [start fld + structOffset | (_, fld) <- fields]
start (Union unionOffset fields _) =
    if null fields then unionOffset else minimum [start fld + unionOffset | (_, fld) <- fields]

-- Get the end offset of a layout
end :: Layout -> MemorySize
end (Primitive (Range primOffset size) _) = primOffset + size
end (Array arrayOffset elemLayout len _) =
    arrayOffset + (sizeOf elemLayout * fromIntegral len)
end (Struct structOffset fields _) =
    if null fields then structOffset else maximum [end fld + structOffset | (_, fld) <- fields]
end (Union unionOffset fields _) =
    unionOffset + maximum (unionOffset : [sizeOf fld | (_, fld) <- fields])

-- Calculate the length of a layout
len :: Layout -> MemorySize
len layout = end layout - start layout

-- Apply an offset to a layout, recursively adjusting fields
offset' :: Offset -> Layout -> Layout
offset' baseOffset (Primitive (Range primOffset size) endianess) =
    Primitive (Range (baseOffset + primOffset) size) endianess
offset' baseOffset (Array arrayOffset elemLayout len endianess) =
    Array (baseOffset + arrayOffset) elemLayout len endianess
offset' baseOffset (Struct structOffset fields endianess) =
    Struct (baseOffset + structOffset) adjustedFields endianess
  where
    adjustedFields = [(name, offset' (baseOffset + structOffset) fldLayout) | (name, fldLayout) <- fields]
offset' baseOffset (Union unionOffset fields endianess) =
    Union (baseOffset + unionOffset) adjustedFields endianess
  where
    adjustedFields = [(name, offset' (baseOffset + unionOffset) fldLayout) | (name, fldLayout) <- fields]

-- Helper function to calculate size based on layout type
sizeOf :: Layout -> Size
sizeOf (Primitive (Range _ size) _) = size
sizeOf (Array _ layout len _) = sizeOf layout * fromIntegral len
sizeOf (Struct _ fields _) = maximum $ 0 : [end fld | (_, fld) <- fields]
sizeOf (Union _ fields _) = maximum $ 0 : [sizeOf fld | (_, fld) <- fields]

--------------------------------------------------------------------------------

-- Well-formedness check with error reporting:

wf :: Layout -> Result
-- Primitive Layout
wf (Primitive (Range offset size) _) =
    (if offset < 0 then Errors [NegativeOffset] else NoError) <>
    (if size < 0 then Errors [NegativeSizeAllocation] else NoError)

-- Array Layout
wf (Array offset innerLayout len endianess) =
    (if offset < 0 then Errors [NegativeOffset] else NoError) <>
    (if len < 0 then Errors [NegativeSizeAllocation] else NoError) <>
    wf innerLayout

-- Struct Layout
wf layout@(Struct offset fields _) =
    (if offset < 0 then Errors [NegativeOffset] else NoError) <>
    (if noOverlaps layout then NoError else Errors [StructOverlappingFields]) <>
    mconcat [wf fldLayout | (_, fldLayout) <- fields]

-- Union Layout
wf layout@(Union offset fields _) =
    (if offset < 0 then Errors [NegativeOffset] else NoError) <>
    mconcat [wf fldLayout | (_, fldLayout) <- fields]


--------------------------------------------------------------------------------
-- Helper operations:

-- Extract all ranges from a layout
getRanges :: Layout -> [Range]
getRanges layout = getRanges' layout 0

getRanges' :: Layout -> Offset -> [Range]
getRanges' (Primitive (Range offset size) _) baseOffset =
    [Range (baseOffset + offset) size]
getRanges' (Array offset layout len _) baseOffset =
    let elemSize = sizeOf layout
    in concat [getRanges' layout (baseOffset + offset + n * elemSize) | n <- [0..(len - 1)]]
getRanges' (Struct offset fields _) baseOffset =
    concatMap (\(_, fldLayout) -> getRanges' fldLayout (baseOffset + offset)) fields
getRanges' (Union offset fields _) baseOffset =
    concatMap (\(_, fldLayout) -> getRanges' fldLayout (baseOffset + offset)) fields

-- Check that no two ranges have overlapping offsets
noOverlaps :: Layout -> Bool
noOverlaps layout = case layout of
    Union _ _ _ -> True  -- Overlaps are acceptable in unions
    _ -> checkNoOverlap sortedRanges
  where
    ranges = getRanges layout
    sortedRanges = sortOn (\(Range o _) -> o) ranges

checkNoOverlap :: [Range] -> Bool
checkNoOverlap [] = True
checkNoOverlap [_] = True
checkNoOverlap (Range o1 s1 : Range o2 s2 : rest) =
    (o1 + s1 <= o2) && checkNoOverlap (Range o2 s2 : rest)
--------------------------------------------------------------------------------
-- | Example Layouts for Testing
-- Test Case 1: Valid Primitive Layout
testValidPrimitive :: IO ()
testValidPrimitive = do
    let layout = Primitive (Range 0 8) LE
    putStrLn "Test Case 1: Valid Primitive Layout"
    print $ wf layout
    -- Expected Output: NoError

-- Test Case 2: Valid Struct Layout with No Overlaps
testValidStruct :: IO ()
testValidStruct = do
    let layout = Struct 0
                    [ ("field1", Primitive (Range 0 8) LE)
                    , ("field2", Primitive (Range 8 8) LE)
                    ] LE
    putStrLn "\nTest Case 2: Valid Struct Layout with No Overlaps"
    print $ wf layout
    -- Expected Output: NoError

-- Test Case 3: Valid Union Layout with Overlaps
testValidUnion :: IO ()
testValidUnion = do
    let layout = Union 0
                    [ ("field1", Primitive (Range 0 8) LE)
                    , ("field2", Primitive (Range 0 16) LE)
                    ] LE
    putStrLn "\nTest Case 3: Valid Union Layout with Overlaps"
    print $ wf layout
    -- Expected Output: NoError

-- Test Case 4: Negative Offset in Primitive
testNegativeOffsetPrimitive :: IO ()
testNegativeOffsetPrimitive = do
    let layout = Primitive (Range (-1) 8) LE
    putStrLn "\nTest Case 4: Negative Offset in Primitive"
    print $ wf layout
    -- Expected Output: Errors [NegativeOffset]

-- Test Case 5: Negative Size Allocation in Primitive
testNegativeSizePrimitive :: IO ()
testNegativeSizePrimitive = do
    let layout = Primitive (Range 0 (-8)) LE
    putStrLn "\nTest Case 5: Negative Size Allocation in Primitive"
    print $ wf layout
    -- Expected Output: Errors [NegativeSizeAllocation]

-- Test Case 6: Negative Offset in Struct
testNegativeOffsetStruct :: IO ()
testNegativeOffsetStruct = do
    let layout = Struct (-2)
                    [ ("field1", Primitive (Range 0 8) LE)
                    ] LE
    putStrLn "\nTest Case 6: Negative Offset in Struct"
    print $ wf layout
    -- Expected Output: Errors [NegativeOffset]

-- Test Case 7: Negative Size Allocation in Array Element
testNegativeSizeInArrayElement :: IO ()
testNegativeSizeInArrayElement = do
    let elementLayout = Primitive (Range 0 (-8)) LE
        layout = Array 0 elementLayout 5 LE
    putStrLn "\nTest Case 7: Negative Size Allocation in Array Element"
    print $ wf layout
    -- Expected Output: Errors [NegativeSizeAllocation]

-- Test Case 8: Struct with Overlapping Fields
testStructOverlappingFields :: IO ()
testStructOverlappingFields = do
    let layout = Struct 0
                    [ ("field1", Primitive (Range 0 8) LE)
                    , ("field2", Primitive (Range 4 8) LE)
                    ] LE
    putStrLn "\nTest Case 8: Struct with Overlapping Fields"
    print $ wf layout
    -- Expected Output: Errors [StructOverlappingFields]

-- Test Case 9: Multiple Errors in Struct
testMultipleErrorsInStruct :: IO ()
testMultipleErrorsInStruct = do
    let layout = Struct (-1)
                    [ ("field1", Primitive (Range 0 (-8)) LE)
                    , ("field2", Primitive (Range 4 8) LE)
                    , ("field3", Primitive (Range 2 4) LE)
                    ] LE
    putStrLn "\nTest Case 9: Multiple Errors in Struct"
    print $ wf layout
    -- Expected Output: Errors [NegativeOffset, NegativeSizeAllocation, StructOverlappingFields]

-- Test Case 10: Valid Array Layout
testValidArray :: IO ()
testValidArray = do
    let elementLayout = Primitive (Range 0 8) LE
        layout = Array 0 elementLayout 4 LE
    putStrLn "\nTest Case 10: Valid Array Layout"
    print $ wf layout
    -- Expected Output: NoError

-- Test Case 11: Array with Negative Offset
testNegativeOffsetArray :: IO ()
testNegativeOffsetArray = do
    let elementLayout = Primitive (Range 0 8) LE
        layout = Array (-5) elementLayout 4 LE
    putStrLn "\nTest Case 11: Array with Negative Offset"
    print $ wf layout
    -- Expected Output: Errors [NegativeOffset]

-- Test Case 12: Array with Negative Length
testNegativeLengthArray :: IO ()
testNegativeLengthArray = do
    let elementLayout = Primitive (Range 0 8) LE
        layout = Array 0 elementLayout (-3) LE
    putStrLn "\nTest Case 12: Array with Negative Size Allocation"
    print $ wf layout
    -- Expected Output: Errors [NegativeSizeAllocation]

-- cd .\code\
-- ghci Cedar\Semantic\LR.hs
-- main

-- Main function to run all test cases
main :: IO ()
main = do
    testValidPrimitive
    testValidStruct
    testValidUnion
    testNegativeOffsetPrimitive
    testNegativeSizePrimitive
    testNegativeOffsetStruct
    testNegativeSizeInArrayElement
    testStructOverlappingFields
    testMultipleErrorsInStruct
    testValidArray
    testNegativeOffsetArray
    testNegativeLengthArray


