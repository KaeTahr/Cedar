module Cedar.Semantic.L where

-- In ByteBit/Bit/Byte Representation

import qualified Cedar.Semantic.BasicType as BMT
import qualified Data.Set as Set

-- MEMORY:

data MemorySize = Byte Int | Bit Int | ByteBit Int Int
    deriving (Show, Eq)

bitsToAByte :: Int
bitsToAByte = 8

-- credit Dargent for the idea
-- do addition in bits not bits and bytes
extractBits :: MemorySize -> Int
extractBits (Byte a) = bitsToAByte * a
extractBits (Bit b) = b
extractBits (ByteBit a b) = bitsToAByte * a + b

convert :: Int -> MemorySize
convert bits
    | bits < 8 = Bit bits
    | otherwise = ByteBit (bits `div` bitsToAByte) (bits `mod` bitsToAByte)

instance Ord MemorySize where
    compare x y = compare (extractBits x) (extractBits y)

-- may need there defined or may not
instance Num MemorySize where
    (+) :: MemorySize -> MemorySize -> MemorySize
    m1 + m2 = convert (extractBits m1 + extractBits m2)

    (*) :: MemorySize -> MemorySize -> MemorySize
    m1 * m2 = convert (extractBits m1 * extractBits m2)

    negate :: MemorySize -> MemorySize
    negate (Byte a) = Byte (negate a)
    negate (Bit a) = Bit (negate a)
    negate (ByteBit a b) = ByteBit (negate a) (negate b)

    fromInteger :: Integer -> MemorySize
    fromInteger n = Bit (fromInteger n)
    
    abs :: MemorySize -> MemorySize
    abs (Byte a) = Byte (abs a)
    abs (Bit a) = Bit (abs a)
    abs (ByteBit a b) = ByteBit (abs a) (abs b)

    signum :: MemorySize -> MemorySize
    signum (Byte a) = Byte (signum a)
    signum (Bit a) = Bit (signum a)
    signum (ByteBit a b) = ByteBit (signum a) (signum b)

--------------------------------------------------------------------------------

data Endianess = BE | LE | ME 
    deriving (Show)

data Order = After MemorySize | Before MemorySize
    deriving (Show)

data Offset = RelOffset FieldName Order | AbsOffset MemorySize
    deriving (Show)

type FieldName = String
type Length = Int

data Layout = Primitive MemorySize Offset Endianess
    | Array Offset Layout Length Endianess
    | Struct Offset [(FieldName, Layout)] Endianess
    | Union Offset [(FieldName, Layout)] Endianess
    deriving (Show)

-- Errors
data RawError = 
    ArrayNonPositiveLength
    | RelativeOffsetInfiniteLoop
    | SelfReferentialRelativeOffset
    | FieldReferenceNotInScope
    | NonUniqueFieldNames
    | NonStructRelativeOffset
    | EmptyCompositeLayout
    deriving (Show)

-- Result can accumulate multiple errors
data Result = NoError | Errors [RawError]
    deriving (Show)

-- Main well-formedness function
wf :: Layout -> Result
wf layout = wf' layout [] Nothing True [] Set.empty

-- Internal well-formedness function with context
wf' :: Layout -> [[FieldName]] -> Maybe FieldName -> Bool -> [FieldName] -> Set.Set (FieldName, FieldName) -> Result
wf' (Primitive _ offset _) inScope currentFieldName relOffsetsAllowed chain visited = 
    checkOffset offset inScope currentFieldName relOffsetsAllowed chain visited

wf' (Array offset layout len _) inScope currentFieldName relOffsetsAllowed chain visited = combineAll [
    checkArrayLength len,
    checkOffset offset inScope currentFieldName relOffsetsAllowed chain visited,
    wf' layout inScope currentFieldName relOffsetsAllowed chain visited
    ]

wf' (Struct offset fields _) inScope currentFieldName relOffsetsAllowed chain visited = combineAll [
    checkOffset offset inScope currentFieldName relOffsetsAllowed chain visited,
    checkUniqueFieldNames fields,
    if null fields then Errors [EmptyCompositeLayout] else NoError,
    let currentScope = map fst fields
        newInScope = currentScope : inScope
    in wfFields fields newInScope relOffsetsAllowed chain visited
    ]

wf' (Union offset fields _) inScope currentFieldName _ chain visited = combineAll [
    checkOffset offset inScope currentFieldName False chain visited, -- Relative Offsets not allowed
    checkUniqueFieldNames fields,
    if null fields then Errors [EmptyCompositeLayout] else NoError,
    let currentScope = map fst fields
        newInScope = currentScope : inScope
    in wfFields fields newInScope False chain visited
    ]

-- Process fields in a composite layout
wfFields :: [(FieldName, Layout)] -> [[FieldName]] -> Bool -> [FieldName] -> Set.Set (FieldName, FieldName) -> Result
wfFields fields inScope relOffsetsAllowed chain visited =
    combineAll [wf' layout inScope (Just fieldName) relOffsetsAllowed (chain ++ [fieldName]) visited | (fieldName, layout) <- fields]

-- Check offsets, including infinite loop detection
checkOffset :: Offset -> [[FieldName]] -> Maybe FieldName -> Bool -> [FieldName] -> Set.Set (FieldName, FieldName) -> Result
checkOffset (AbsOffset _) _ _ _ _ _ = NoError
checkOffset (RelOffset refFieldName _) inScope currentFieldName relOffsetsAllowed chain visited
    | not relOffsetsAllowed = Errors [NonStructRelativeOffset]
    | Just refFieldName == currentFieldName = Errors [SelfReferentialRelativeOffset]
    | fieldInScope refFieldName inScope = NoError
    | otherwise = Errors [FieldReferenceNotInScope]

-- Helper function to check if a field is in scope
fieldInScope :: FieldName -> [[FieldName]] -> Bool
fieldInScope refFieldName inScope = any (refFieldName `elem`) inScope

-- Helper Functions:
-- Combines two Results, accumulating errors
combine :: Result -> Result -> Result
combine NoError r = r
combine r NoError = r
combine (Errors es1) (Errors es2) = Errors (es1 ++ es2)

-- Combines a list of Results, accumulating all errors
combineAll :: [Result] -> Result
combineAll results = 
    let errors = concat [es | Errors es <- results]
    in if null errors then NoError else Errors errors

-- Check that array length is positive
checkArrayLength :: Int -> Result
checkArrayLength len
    | len < 1   = Errors [ArrayNonPositiveLength]
    | otherwise = NoError

-- Check that all field names are unique
checkUniqueFieldNames :: [(FieldName, Layout)] -> Result
checkUniqueFieldNames fields =
    let names = map fst fields
        duplicates = findDuplicates names
    in if null duplicates
       then NoError
       else Errors [NonUniqueFieldNames]

-- Utility to find duplicates in a list
findDuplicates :: Ord a => [a] -> [a]
findDuplicates xs = go xs Set.empty Set.empty
  where
    go [] _ duplicates = Set.toList duplicates
    go (y:ys) seen duplicates
        | y `Set.member` seen = go ys seen (Set.insert y duplicates)
        | otherwise           = go ys (Set.insert y seen) duplicates

-- Test Cases:

-- Test Case 1: ArrayNonPositiveLength
testArrayNonPositiveLength :: IO ()
testArrayNonPositiveLength = do
    let layout = Array (AbsOffset 0) (Primitive 4 (AbsOffset 0) LE) 0 LE
    print $ wf layout
    -- Expected Output: Errors [ArrayNonPositiveLength]

-- Test Case 2: RelativeOffsetInfiniteLoop
testRelativeOffsetInfiniteLoop :: IO ()
testRelativeOffsetInfiniteLoop = do
    let layout = Struct (AbsOffset 0) 
                    [ ("field1", Primitive 4 (RelOffset "field2" (After 0)) LE)
                    , ("field2", Primitive 4 (RelOffset "field1" (After 0)) LE)
                    ] LE
    print $ wf layout
    -- Expected Output: NoError (Since infinite loop detection is not implemented)

-- Test Case 3: SelfReferentialRelativeOffset
testSelfReferentialRelativeOffset :: IO ()
testSelfReferentialRelativeOffset = do
    let layout = Struct (AbsOffset 0) 
                    [ ("field1", Primitive 4 (RelOffset "field1" (After 0)) LE)
                    ] LE
    print $ wf layout
    -- Expected Output: Errors [SelfReferentialRelativeOffset]

-- Test Case 4: FieldReferenceNotInScope
testFieldReferenceNotInScope :: IO ()
testFieldReferenceNotInScope = do
    let layout = Struct (AbsOffset 0) 
                    [ ("field1", Primitive 4 (RelOffset "unknownField" (After 0)) LE)
                    ] LE
    print $ wf layout
    -- Expected Output: Errors [FieldReferenceNotInScope]

-- Test Case 5: NonUniqueFieldNames
testNonUniqueFieldNames :: IO ()
testNonUniqueFieldNames = do
    let layout = Struct (AbsOffset 0) 
                    [ ("field1", Primitive 4 (AbsOffset 0) LE)
                    , ("field1", Primitive 4 (AbsOffset 4) LE)
                    ] LE
    print $ wf layout
    -- Expected Output: Errors [NonUniqueFieldNames]

-- Test Case 6: RelativeOffsetInUnion
testRelativeOffsetInUnion :: IO ()
testRelativeOffsetInUnion = do
    let layout = Union (AbsOffset 0) 
                    [ ("field1", Primitive 4 (RelOffset "field2" (After 0)) LE)
                    , ("field2", Primitive 4 (AbsOffset 0) LE)
                    ] LE
    print $ wf layout
    -- Expected Output: Errors [RelativeOffsetInUnion]

-- Test Case 7: Multiple Errors in Struct
-- Errors Expected:
-- - NonUniqueFieldNames
-- - FieldReferenceNotInScope
testMultipleErrorsInStruct :: IO ()
testMultipleErrorsInStruct = do
    let layout = Struct (AbsOffset 0) 
                    [ ("field1", Primitive 4 (RelOffset "unknownField" (After 0)) LE)
                    , ("field1", Primitive 4 (AbsOffset 4) LE)
                    ] LE
    print $ wf layout
    -- Expected Output: Errors [NonUniqueFieldNames, FieldReferenceNotInScope]

-- Test Case 8: Multiple Errors in Array
-- Errors Expected:
-- - ArrayNonPositiveLength
-- - SelfReferentialRelativeOffset
testMultipleErrorsInArray :: IO ()
testMultipleErrorsInArray = do
    let elementLayout = Primitive 4 (RelOffset "element" (After 0)) LE
        layout = Array (AbsOffset 0) elementLayout 0 LE
    print $ wf layout
    -- Expected Output: Errors [ArrayNonPositiveLength, SelfReferentialRelativeOffset]

-- Test Case 9: Multiple Errors in Union
-- Errors Expected:
-- - NonUniqueFieldNames
-- - RelativeOffsetInUnion
testMultipleErrorsInUnion :: IO ()
testMultipleErrorsInUnion = do
    let layout = Union (AbsOffset 0) 
                    [ ("field1", Primitive 4 (RelOffset "field2" (After 0)) LE)
                    , ("field1", Primitive 4 (AbsOffset 0) LE)
                    ] LE
    print $ wf layout
    -- Expected Output: Errors [NonUniqueFieldNames, RelativeOffsetInUnion]

-- Test Case 10: Multiple Nested Errors
-- Errors Expected:
-- - NonUniqueFieldNames
-- - SelfReferentialRelativeOffset
-- - FieldReferenceNotInScope
testMultipleNestedErrors :: IO ()
testMultipleNestedErrors = do
    let innerStruct = Struct (AbsOffset 0) 
                        [ ("innerField1", Primitive 4 (RelOffset "innerField1" (After 0)) LE)
                        , ("innerField1", Primitive 4 (RelOffset "nonExistentField" (After 0)) LE)
                        ] LE
        outerStruct = Struct (AbsOffset 0) 
                        [ ("outerField1", innerStruct)
                        , ("outerField2", Primitive 4 (RelOffset "outerField1" (After 0)) LE)
                        , ("outerField1", Primitive 4 (AbsOffset 0) LE)  -- Duplicate field name
                        ] LE
    print $ wf outerStruct
    -- Expected Output: Errors [NonUniqueFieldNames, SelfReferentialRelativeOffset, FieldReferenceNotInScope, RelativeOffsetInfiniteLoop]

-- Test Case 11: Empty Struct
testEmptyStruct :: IO ()
testEmptyStruct = do
    let layout = Struct (AbsOffset (Byte 0)) [] LE
    print $ wf layout
    -- Expected Output: Errors [EmptyCompositeLayout]

-- Test Case 12: Empty Union
testEmptyUnion :: IO ()
testEmptyUnion = do
    let layout = Union (AbsOffset (Byte 0)) [] LE
    print $ wf layout
    -- Expected Output: Errors [EmptyCompositeLayout]

-- cd .\code\
-- ghci Cedar\Semantic\L.hs
-- main

-- Main function to run all test cases
main :: IO ()
main = do
    putStrLn "Test Case 1: ArrayNonPositiveLength"
    testArrayNonPositiveLength
    putStrLn "\nTest Case 2: RelativeOffsetInfiniteLoop"
    testRelativeOffsetInfiniteLoop
    putStrLn "\nTest Case 3: SelfReferentialRelativeOffset"
    testSelfReferentialRelativeOffset
    putStrLn "\nTest Case 4: FieldReferenceNotInScope"
    testFieldReferenceNotInScope
    putStrLn "\nTest Case 5: NonUniqueFieldNames"
    testNonUniqueFieldNames
    putStrLn "\nTest Case 6: RelativeOffsetInUnion"
    testRelativeOffsetInUnion
    putStrLn "\nTest Case 7: Multiple Errors in Struct"
    testMultipleErrorsInStruct
    putStrLn "\nTest Case 8: Multiple Errors in Array"
    testMultipleErrorsInArray
    putStrLn "\nTest Case 9: Multiple Errors in Union"
    testMultipleErrorsInUnion
    putStrLn "\nTest Case 10: Multiple Nested Errors"
    testMultipleNestedErrors
    putStrLn "\nTest Case 11: Empty Struct"
    testEmptyStruct
    putStrLn "\nTest Case 12: Empty Union"
    testEmptyUnion