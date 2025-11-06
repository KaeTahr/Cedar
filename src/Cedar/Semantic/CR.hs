-- original author: George Anderson

module Cedar.Semantic.CR where

import qualified Cedar.Semantic.BasicType as BMT

type FieldName = String
type Length = Int

data CRType = 
    BasicMemType BMT.BasicMemType
    | Struct [(FieldName, CRType)]
    | Union [(FieldName, CRType)]  
    | Array CRType Length
    deriving (Show)