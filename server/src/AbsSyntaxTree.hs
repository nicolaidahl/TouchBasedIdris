{-# LANGUAGE DeriveGeneric #-}
module AbsSyntaxTree where

import GHC.Generics (Generic)
import Data.Aeson (FromJSON, ToJSON)
import Data.Text

data TIAbsSyntaxTree =
  TIAbsSyntaxTree {
    name :: Text,
    topLevelDec :: [TITopLevelDec]
  }
  deriving (Show, Generic)

instance FromJSON TIAbsSyntaxTree
instance ToJSON   TIAbsSyntaxTree

data TITopLevelDec = 
  TIFunctionDec {
    ident :: Text,
    titype :: TIExpr,
    clauses :: [TIClause]
  } 
  | TIDataDec {
    ident :: Text,
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
    constructor :: Text,
    constructorType :: TIExpr
  }
  deriving (Show, Generic)

instance FromJSON TIConstructor
instance ToJSON   TIConstructor

data TIExpr = 
    TIConst TIConst
  | TIApp TIExpr [TIExpr]
  | TILam Text TIExpr
  | TIPi (Maybe Text) TIExpr TIExpr
  | TIVar Text
  | TIRef Text
  deriving (Show, Generic)

instance FromJSON TIExpr
instance ToJSON   TIExpr

data TIConst = 
    TIString Text
  | TIInt Int
  | TIFloat Double
  | TIStringTy
  | TIIntTy
  | TIFloatTy
  | TITypeTy
  deriving (Show, Generic)

instance FromJSON TIConst
instance ToJSON   TIConst
