{-# LANGUAGE OverloadedStrings #-}
module Main where

import AbsSyntaxTree

import Control.Applicative
import Snap.Core
import Snap.Util.FileServe
import Snap.Http.Server
import Data.Aeson (decode, encode)
import Data.ByteString.Internal (ByteString)

contentType :: ByteString 
contentType = "application/json; charset=utf-8"

main :: IO ()
main = quickHttpServe site

site :: Snap ()
site =
    ifTop (writeBS "hello world") <|>
    route [ ("type", method POST typeHandler),
            ("test", test) ] <|>
    dir "static" (serveDirectory ".")

test :: Snap ()
test = do
  modifyResponse $ setHeader "Content-Type" contentType
  let nat = TIDataDec { ident = "Nat", 
                        titype = TIConst TITypeTy, 
                        constructors = [ 
                          TIConstructor "Z" $ TIRef "Nat",
                          TIConstructor "S" $ TIPi Nothing (TIRef "Nat") (TIRef "Nat")
                        ] 
                      }
  let tiVect = TIDataDec { ident = "Vect", 
                           titype = TIPi Nothing (TIRef "Nat") (TIPi Nothing (TIConst TITypeTy) (TIConst TITypeTy)), 
                           constructors = [
                             TIConstructor "Nil" (TIApp (TIRef "Vect") [TIRef "Z", TIVar "a"]), 
                             TIConstructor "(::)" (TIPi Nothing (TIVar "a") (TIPi Nothing (TIApp (TIRef "Vect") [TIVar "k", TIVar "a"]) (TIApp (TIRef "Vect") [TIApp (TIRef "S") [TIVar "k"], TIVar "a"])))
                           ]
                         }
  -- let tiZip  = TIFunctionDec { ident = "Zip", titype = "Vect n a -> Vect n b -> Vect n (a, b)", clauses = ["Nil Nil = Nil", "(x :: xs) (y :: ys) = (x, y) :: zip xs ys"]}
  let json = TIAbsSyntaxTree { name = "test", topLevelDec = [ nat, tiVect ] }
  writeLBS $ encode json

typeHandler :: Snap ()
typeHandler = do
  modifyResponse $ setHeader "Content-Type" contentType
  astJSON <- readRequestBody 128
  let ast = decode astJSON :: Maybe TIAbsSyntaxTree
  case ast of Nothing -> writeBS "You Suck"
              Just a  -> writeLBS $ encode a
