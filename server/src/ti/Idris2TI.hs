module TI.Idris2TI where

import TI.AbsSyntaxTree
import Idris.AbsSyntaxTree
import Idris.Core.TT as TT
import Data.Text
import Control.Monad (join)

--pDecls2TITopLevelDec :: [PDecls] -> [TITopLevelDec]
--pDecls2TI (PData d ds si fc o decs) :: t = 
--pDecls2TI _ = Nothing 

--pData2TIData :: PData -> Maybe TIData
--pData2TIData (PData i t cons) = TIData (show i)

pTerm2TIExpr :: PTerm -> Maybe TIExpr
pTerm2TIExpr (PConstant c) = fmap TIConst (pConstant2TIConst c)
pTerm2TIExpr (PApp _ lam args) = let pterms = (sequence $ fmap pArg2PTerm args) in
                                 case join $ fmap (sequence . fmap pTerm2TIExpr) pterms of
                                   Just a -> fmap (\l -> TIApp l a) (pTerm2TIExpr lam)
                                   Nothing -> Nothing
pTerm2TIExpr (PLam (UN n) b t) = fmap (TILam n) (pTerm2TIExpr b)
pTerm2TIExpr (PPi _ n a b)     = case ((pTerm2TIExpr a), (pTerm2TIExpr b)) of
                                   (Just ta, Just tb) -> Just $ TIPi (Just $ pack $ show n) ta tb
                                   _                  -> Nothing
pTerm2TIExpr _ = Nothing

pArg2PTerm :: PArg -> Maybe PTerm
pArg2PTerm (PExp _ _ a) = Just a
pArg2PTerm _            = Nothing

pConstant2TIConst :: TT.Const -> Maybe TIConst
pConstant2TIConst (TT.Str s)        = Just $ TIString $ pack s
pConstant2TIConst (I i)             = Just $ TIInt i
pConstant2TIConst (Fl f)            = Just $ TIFloat f
pConstant2TIConst (StrType)         = Just $ TIStringTy
pConstant2TIConst (AType (ATInt _)) = Just $ TIIntTy 
pConstant2TIConst (AType ATFloat)   = Just $ TIFloatTy
pConstant2TIConst _                 = Nothing
