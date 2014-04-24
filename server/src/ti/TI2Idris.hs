module TI.TI2Idris where

import TI.AbsSyntaxTree
import Idris.AbsSyntaxTree
import Idris.Core.TT as TT
import Idris.Docstrings
import Cheapskate
import Data.Text

defaultDoc :: Doc
defaultDoc = markdown (Options False False False False) (pack "")

defaultFC :: FC
defaultFC = FC "test.idr" (0, 0) (0, 0)

defaultPlicity :: Plicity
defaultPlicity = Exp [] Static True

ti2PDecls :: TIAbsSyntaxTree -> [PDecl]
ti2PDecls (TIAbsSyntaxTree _ decs) = fmap tiTopLevelDec2PDecl decs

tiTopLevelDec2PDecl :: TITopLevelDec -> PDecl
tiTopLevelDec2PDecl (TIFunctionDec n _ cs) = PClauses defaultFC [] (UN n) (fmap (tiClause2PClause n) cs)
tiTopLevelDec2PDecl (TIDataDec n t cs) = PData defaultDoc [] defaultSyntax defaultFC [] (tiData2PData n t cs)

tiClause2PClause :: Text -> TIClause -> PClause
tiClause2PClause n (TIClause l r) = PClause defaultFC (UN n) (tiExpr2PTerm r) [] (tiExpr2PTerm r) [] -- FIXME: No lhs

tiData2PData :: Text -> TIExpr -> [TIConstructor] -> PData
tiData2PData n ty constrs = PDatadecl (UN n) (tiExpr2PTerm ty) (fmap tiConstructor2PConstructor constrs)

type PConstructor t = (Docstring, [(Name, Docstring)], Name, t, FC, [Name])

tiConstructor2PConstructor :: TIConstructor -> PConstructor PTerm
tiConstructor2PConstructor (TIConstructor n ty) = (defaultDoc, [], (UN n), (tiExpr2PTerm ty), defaultFC, [])

tiExpr2PTerm :: TIExpr -> PTerm
tiExpr2PTerm (TIConst c)      = PConstant $ tiConst2PConstant c
tiExpr2PTerm (TIApp lam args) = PApp defaultFC (tiExpr2PTerm lam) (fmap (pTerm2PArg . tiExpr2PTerm) args)
tiExpr2PTerm (TILam n body)   = PLam (UN n) (tiExpr2PTerm body) (tiExpr2PTerm body)
tiExpr2PTerm (TIPi bind a b)  = let n = case bind of 
                                             Just t -> UN t
                                             Nothing -> UN $ pack ""
                                in PPi defaultPlicity n (tiExpr2PTerm a) (tiExpr2PTerm b) -- FIXME
tiExpr2PTerm (TIVar n)        = PPatvar defaultFC (UN n)
tiExpr2PTerm (TIRef n)        = PRef defaultFC (UN n)

pTerm2PArg :: PTerm -> PArg
pTerm2PArg t = PExp 0 [] (UN (pack "")) t

tiConst2PConstant :: TIConst -> Const
tiConst2PConstant (TIString s) = TT.Str $ unpack s
tiConst2PConstant (TIInt i)    = I i
tiConst2PConstant (TIFloat f)  = Fl f
tiConst2PConstant TIStringTy   = StrType
tiConst2PConstant TIIntTy      = AType $ ATInt ITNative
tiConst2PConstant TIFloatTy    = AType ATFloat
--FIXME: tiConst2PConstant TITypeTy     = 
