{-# LANGUAGE OverloadedStrings #-}
module Evaluator where

import AbsSyntaxTree

import Prelude hiding (pi)

import Text.Trifecta.Delta
import Text.Trifecta hiding (span, stringLiteral, charLiteral, natural, symbol, char, string, whiteSpace)
import Text.Parser.LookAhead
import Text.Parser.Expression
import qualified Text.Parser.Token as Tok
import qualified Text.Parser.Char as Chr
import qualified Text.Parser.Token.Highlight as Hi

import Text.PrettyPrint.ANSI.Leijen (Doc, plain)
import qualified Text.PrettyPrint.ANSI.Leijen as ANSI

import Idris.AbsSyntax hiding (namespace, params)
import Idris.DSL
import Idris.Imports
import Idris.Delaborate
import Idris.Error
import Idris.ElabDecls
import Idris.ElabTerm
import Idris.Coverage
import Idris.IBC
import Idris.Unlit
import Idris.Providers
import Idris.Output
import Idris.Parser

import Idris.ParseHelpers
import Idris.ParseOps
import Idris.ParseExpr
import Idris.ParseData

import Idris.Docstrings

import Util.DynamicLinker

import Idris.Core.TT
import Idris.Core.Evaluate

import Control.Applicative
import Control.Monad

import Data.Function
import Data.Maybe
import qualified Data.List.Split as Spl
import Data.List
import Data.Monoid
import Data.Char
import Data.Ord
import qualified Data.Map as M
import qualified Data.HashSet as HS
import qualified Data.Text as T
import qualified Data.ByteString.UTF8 as UTF8
import qualified Data.Set as S

import Debug.Trace

import System.FilePath
import System.IO

initClause :: TITopLevelDec -> Maybe String
initClause (TIFunctionDec ident titype _) = Just ""
initClause _                              = Nothing

caseSplit :: TITopLevelDec -> Maybe String
caseSplit (TITopLevelDec ident titype clauses) = Just ""
caseSplit _ = Nothing




loadDecls :: Handle -> FilePath -> [String] -> [PDecl] -> Idris ()
loadDecls h f mname ds'
             = do let ds = namespaces mname ds'
                  i <- getIState
                  let def_total = default_total i

                  logLvl 3 (show $ showDecls True ds)
                  i <- getIState
                  logLvl 10 (show (toAlist (idris_implicits i)))
                  logLvl 3 (show (idris_infixes i))
                  -- Now add all the declarations to the context
                  v <- verbose
                  when v $ ihputStrLn h $ "Type checking " ++ f
                  -- we totality check after every Mutual block, so if
                  -- anything is a single definition, wrap it in a
                  -- mutual block on its own
                  elabDecls toplevel (map toMutual ds)
                  i <- getIState
                  -- simplify every definition do give the totality checker
                  -- a better chance
                  mapM_ (\n -> do logLvl 5 $ "Simplifying " ++ show n
                                  updateContext (simplifyCasedef n))
                           (map snd (idris_totcheck i))
                  -- build size change graph from simplified definitions
                  iLOG "Totality checking"
                  i <- getIState
                  mapM_ buildSCG (idris_totcheck i)
                  mapM_ checkDeclTotality (idris_totcheck i)

                  -- Redo totality check for deferred names
                  let deftots = idris_defertotcheck i
                  iLOG $ "Totality checking " ++ show deftots
                  mapM_ (\x -> do tot <- getTotality x
                                  case tot of
                                       Total _ -> setTotality x Unchecked
                                       _ -> return ()) (map snd deftots)
                  mapM_ buildSCG deftots
                  mapM_ checkDeclTotality deftots

                  iLOG ("Finished " ++ f)
                  ibcsd <- valIBCSubDir i
                  iLOG "Universe checking"
                  iucheck
                  let ibc = ibcPathNoFallback ibcsd f
                  i <- getIState
                  addHides (hide_list i)

                  -- Finally, write an ibc if checking was successful

                  ok <- noErrors
                  when ok $
                    idrisCatch (do writeIBC f ibc; clearIBC)
                               (\c -> return ()) -- failure is harmless
                  i <- getIState
                  putIState (i { default_total = def_total,
                                 hide_list = [] })
                  return ()
  where
    namespaces :: [String] -> [PDecl] -> [PDecl]
    namespaces []     ds = ds
    namespaces (x:xs) ds = [PNamespace x (namespaces xs ds)]

    toMutual :: PDecl -> PDecl
    toMutual m@(PMutual _ d) = m
    toMutual (PNamespace x ds) = PNamespace x (map toMutual ds)
    toMutual x = let r = PMutual (fileFC "single mutual") [x] in
                 case x of
                   PClauses _ _ _ _ -> r
                   PClass _ _ _ _ _ _ _ -> r
                   PInstance _ _ _ _ _ _ _ _ -> r
                   _ -> x
