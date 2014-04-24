{-# LANGUAGE OverloadedStrings #-}
module TI.Evaluator where
import TI.AbsSyntaxTree

import System.IO
import Data.Text as Text
import Data.List as List

import Idris.AbsSyntaxTree
import Idris.Parser


gimmePdecl :: [PDecl]
gimmePdecl = []

findFunc :: TIAbsSyntaxTree -> Text.Text -> Maybe TITopLevelDec
findFunc (TIAbsSyntaxTree name decs) ident = 
    List.find (\dec -> case dec of
                          (TIFunctionDec funcName _ _) -> funcName == ident
                          (TIDataDec dataName _ _)     -> dataName == ident) decs



initClause :: TITopLevelDec -> IO (Maybe TITopLevelDec)
initClause (TIFunctionDec ident titype _) = 
  do let pdecl = gimmePdecl
     handle <- getAndOpenFile "evaluator_log" WriteMode
     --state  <- loadDecls handle "somefile" [] pdecl
     return Nothing
initClause _                              = return Nothing

caseSplit :: TITopLevelDec -> Maybe String
caseSplit (TIFunctionDec ident titype clauses) = Just ""
caseSplit _ = Nothing

metaVarFill :: TITopLevelDec -> Text -> Maybe TITopLevelDec
metaVarFill (TIFunctionDec ident titype clauses) _ = Nothing
metaVarFill _ _ = Nothing



getAndOpenFile :: String -> IOMode -> IO Handle
getAndOpenFile name mode =
    do file <- openFile name mode
       return file















