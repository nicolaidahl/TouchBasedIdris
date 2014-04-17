{-# LANGUAGE OverloadedStrings #-}
module Main where

import AbsSyntaxTree

import Control.Applicative
import Snap.Core
import Snap.Util.FileServe
import Snap.Http.Server
import Data.Aeson (decode, encode)
import Data.ByteString as BS
import Data.ByteString.Lazy as LBS

lbs2BS :: LBS.ByteString -> BS.ByteString
lbs2BS = BS.concat . LBS.toChunks

contentType :: BS.ByteString 
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
  astJSON <- readRequestBody 32768
  let decodedAst = decode astJSON :: Maybe TIAbsSyntaxTree
  case (decodedAst) of 
    Nothing -> do
      modifyResponse $ setResponseStatus 500 "Internal Server Error"
      logError $ BS.append "Error, couldn't deserialize: " $ lbs2BS astJSON
      writeBS $ "Error, couldn't deserialize"
    Just ast -> do
      let encodedAst = encode ast
      logError $ BS.append "Deserialized: " $ lbs2BS encodedAst -- This probably shouldn't go in the error log
      writeLBS encodedAst
