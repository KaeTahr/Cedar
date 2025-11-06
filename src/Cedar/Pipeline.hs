module Cedar.Pipeline
  ( compileToStrings
  ) where

import qualified Cedar.Semantic.C as C
import qualified Cedar.Semantic.L as L
import qualified Cedar.Semantic.CR as CR
import qualified Cedar.Semantic.LR as LR
import qualified Cedar.Semantic.C_to_CR as C_to_CR
import qualified Cedar.Semantic.L_to_LR as L_to_LR

import Cedar.Bridge.LRToCG   (layoutLRToCGStruct)
import Cedar.CodeGen.CodeGen (emitHeader, emitImpl)

-- Given a type tag (C struct name), C type, and surface Layout,
-- run reductions, bridge LR → CodeGen, and return (header, impl).
compileToStrings :: String -> C.CType -> L.Layout -> (String, String)
compileToStrings typeName cType lLayout =
  let _cr = C_to_CR.reduction cType
      lr  = L_to_LR.reduction lLayout
      cg  = layoutLRToCGStruct typeName lr
  in  (emitHeader cg, emitImpl cg)
