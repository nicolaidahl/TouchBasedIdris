{-# LANGUAGE DeriveGeneric #-}
module AbsSyntaxTree where

import GHC.Generics (Generic)
import Data.Aeson (FromJSON, ToJSON)

data TIAbsSyntaxTree =
  TIAbsSyntaxTree {
    name :: String,
    topLevelDec :: [TITopLevelDec]
  }
  deriving (Show, Generic)

instance FromJSON TIAbsSyntaxTree
instance ToJSON   TIAbsSyntaxTree

data TITopLevelDec = 
  TIFunctionDec {
    ident :: String,
    titype :: TIExpr,
    clauses :: [TIClause]
  } 
  | TIDataDec {
    ident :: String,
    titype :: TIExpr,
    constructors :: [TIConstructor]
  }
  deriving (Show, Generic)

instance FromJSON TITopLevelDec
instance ToJSON   TITopLevelDec

data TIClause = 
  TIClause {
    lhs :: [TIExpr],
    rhs :: TIExpr
  }
  deriving (Show, Generic)

instance FromJSON TIClause
instance ToJSON   TIClause

data TIConstructor = 
  TIConstructor {
    constructor :: String,
    constructorType :: TIExpr
  }
  deriving (Show, Generic)

instance FromJSON TIConstructor
instance ToJSON   TIConstructor

data TIExpr = 
    TIConst TIConst
  | TIApp TIExpr [TIExpr]
  | TILam String TIExpr
  | TIPi (Maybe String) TIExpr TIExpr
  | TIVar String
  | TIRef String
  deriving (Show, Generic)

instance FromJSON TIExpr
instance ToJSON   TIExpr

data TIConst = 
    TIString String
  | TIInt Integer
  | TIFloat Float
  | TIStringTy
  | TIIntTy
  | TIFloatTy
  | TITypeTy
  deriving (Show, Generic)

instance FromJSON TIConst
instance ToJSON   TIConst
