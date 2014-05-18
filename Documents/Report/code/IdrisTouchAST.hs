data TIAbsSyntaxTree =
  TIAbsSyntaxTree {
    name :: Text,
    topLevelDec :: [TITopLevelDec]
  }
  deriving (Show, Generic)

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

data TIConstructor = 
  TIConstructor {
    constructor :: Text,
    constructorType :: TIExpr
  }
  deriving (Show, Generic)

data TIExpr = 
    TIConst TIConst
  | TIApp TIExpr [TIExpr]
  | TILam Text TIExpr
  | TIPi (Maybe Text) TIExpr TIExpr
  | TIVar Text
  | TIRef Text
  deriving (Show, Generic)

data TIConst = 
    TIString Text
  | TIInt Int
  | TIFloat Double
  | TIStringTy
  | TIIntTy
  | TIFloatTy
  | TITypeTy
  deriving (Show, Generic)