module Cedar.Semantic.CR_LR_matching where

import qualified Cedar.Semantic.CR as CR
import qualified Cedar.Semantic.LR as LR
import qualified Cedar.Semantic.BasicType as BMT

import Control.Exception (assert)

-- | Matching relation: Determines if a CRType matches an LR Layout
matching :: BMT.Config -> CR.CRType -> LR.Layout -> Bool
-- Match a basic memory type to a primitive layout
matching config (CR.BasicMemType bmt) (LR.Primitive (LR.Range _ size) _) =
    BMT.size config bmt <= size  -- Ensure the LR layout can fit the CR basic memory type

-- Match a struct type to an LR struct layout
matching config (CR.Struct cfields) (LR.Struct _ lfields _) =
    length cfields == length lfields &&
    all (\(fname, cfield) -> matchingSingleField config fname cfield lfields) cfields

-- Match a union type to an LR union layout
matching config (CR.Union cfields) (LR.Union _ lfields _) =
    length cfields == length lfields &&
    all (\(fname, cfield) -> matchingSingleField config fname cfield lfields) cfields

-- Match an array type to an LR array layout
matching config (CR.Array celement clen) (LR.Array _ llayout llen _) =
    clen == llen && matching config celement llayout

-- Wildcard case: No match
matching _ _ _ = False

-- | Helper function to match a single field by name
matchingSingleField :: BMT.Config -> CR.FieldName -> CR.CRType -> [(CR.FieldName, LR.Layout)] -> Bool
matchingSingleField config fname cfield lfields =
    case lookup fname lfields of
        Just llayout -> matching config cfield llayout
        Nothing -> False


--- TESTING
-- Define the default configuration
config :: BMT.Config
config = BMT.defaultConfig

-- Test Case 1: Matching BasicMemType to Primitive
testCase1 :: IO ()
testCase1 = do
    let crType = CR.BasicMemType BMT.I32
        lrLayout = LR.Primitive (LR.Range 0 32) LR.LE
        expected = True
        actual = matching config crType lrLayout
    putStrLn "Test Case 1: Matching BasicMemType to Primitive"
    putStrLn $ "Expected: " ++ show expected ++ ", Actual: " ++ show actual
    assert (actual == expected) (return ())

-- Test Case 2: Matching Structs
testCase2 :: IO ()
testCase2 = do
    let crType = CR.Struct
            [ ("field1", CR.BasicMemType BMT.I32)
            , ("field2", CR.BasicMemType BMT.I8)
            ]
        lrLayout = LR.Struct 0
            [ ("field1", LR.Primitive (LR.Range 0 32) LR.LE)
            , ("field2", LR.Primitive (LR.Range 32 8) LR.LE)
            ] LR.LE
        expected = True
        actual = matching config crType lrLayout
    putStrLn "\nTest Case 2: Matching Structs"
    putStrLn $ "Expected: " ++ show expected ++ ", Actual: " ++ show actual
    assert (actual == expected) (return ())

-- Test Case 3: Matching Arrays
testCase3 :: IO ()
testCase3 = do
    let crType = CR.Array (CR.BasicMemType BMT.I8) 10
        lrLayout = LR.Array 0 (LR.Primitive (LR.Range 0 8) LR.LE) 10 LR.LE
        expected = True
        actual = matching config crType lrLayout
    putStrLn "\nTest Case 3: Matching Arrays"
    putStrLn $ "Expected: " ++ show expected ++ ", Actual: " ++ show actual
    assert (actual == expected) (return ())

-- Test Case 4: Non-matching Struct (field mismatch)
testCase4 :: IO ()
testCase4 = do
    let crType = CR.Struct
            [ ("field1", CR.BasicMemType BMT.I32)
            , ("field3", CR.BasicMemType BMT.I8)
            ]
        lrLayout = LR.Struct 0
            [ ("field1", LR.Primitive (LR.Range 0 32) LR.LE)
            , ("field2", LR.Primitive (LR.Range 32 8) LR.LE)
            ] LR.LE
        expected = False
        actual = matching config crType lrLayout
    putStrLn "\nTest Case 4: Non-matching Struct (field mismatch)"
    putStrLn $ "Expected: " ++ show expected ++ ", Actual: " ++ show actual
    assert (actual == expected) (return ())

-- Additional Test Cases:

-- Test Case 5: Matching Pointer Type
testCase5 :: IO ()
testCase5 = do
    let crType = CR.BasicMemType BMT.Pointer
        pointerSize = BMT.size config BMT.Pointer
        lrLayout = LR.Primitive (LR.Range 0 pointerSize) LR.LE
        expected = True
        actual = matching config crType lrLayout
    putStrLn "\nTest Case 5: Matching Pointer Type"
    putStrLn $ "Expected: " ++ show expected ++ ", Actual: " ++ show actual
    assert (actual == expected) (return ())

-- Test Case 6: Non-matching due to size mismatch
testCase6 :: IO ()
testCase6 = do
    let crType = CR.BasicMemType BMT.I64
        lrLayout = LR.Primitive (LR.Range 0 32) LR.LE  -- Only 32 bits in LR
        expected = False
        actual = matching config crType lrLayout
    putStrLn "\nTest Case 6: Non-matching due to size mismatch"
    putStrLn $ "Expected: " ++ show expected ++ ", Actual: " ++ show actual
    assert (actual == expected) (return ())

-- Test Case 7: Matching Union Types
testCase7 :: IO ()
testCase7 = do
    let crType = CR.Union
            [ ("field1", CR.BasicMemType BMT.I32)
            , ("field2", CR.BasicMemType BMT.I64)
            ]
        lrLayout = LR.Union 0
            [ ("field1", LR.Primitive (LR.Range 0 32) LR.LE)
            , ("field2", LR.Primitive (LR.Range 0 64) LR.LE)
            ] LR.LE
        expected = True
        actual = matching config crType lrLayout
    putStrLn "\nTest Case 7: Matching Union Types"
    putStrLn $ "Expected: " ++ show expected ++ ", Actual: " ++ show actual
    assert (actual == expected) (return ())

-- Test Case 8: Non-matching Array Length
testCase8 :: IO ()
testCase8 = do
    let crType = CR.Array (CR.BasicMemType BMT.I8) 5
        lrLayout = LR.Array 0 (LR.Primitive (LR.Range 0 8) LR.LE) 10 LR.LE  -- Length mismatch
        expected = False
        actual = matching config crType lrLayout
    putStrLn "\nTest Case 8: Non-matching Array Length"
    putStrLn $ "Expected: " ++ show expected ++ ", Actual: " ++ show actual
    assert (actual == expected) (return ())

-- cd .\code\
-- ghci Cedar\Semantic\CR_LR_matching.hs
-- main

-- Main function to run all test cases
main :: IO ()
main = do
    testCase1
    testCase2
    testCase3
    testCase4
    testCase5
    testCase6
    testCase7
    testCase8
    putStrLn "\nAll tests completed successfully."

