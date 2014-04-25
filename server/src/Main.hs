{-# LANGUAGE OverloadedStrings #-}
module Main where

import TI.AbsSyntaxTree
import TI.TI2Idris
import TI.Idris2TI

import Control.Applicative
import Snap.Core
import Control.Monad.IO.Class
import Snap.Util.FileServe
import Snap.Http.Server
import Data.Aeson (decode, encode)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as C8
import qualified Data.ByteString.Lazy as LBS
import qualified GHC.Int

lbs2BS :: LBS.ByteString -> BS.ByteString
lbs2BS = BS.concat . LBS.toChunks

contentType :: BS.ByteString 
contentType = "application/json; charset=utf-8"

maxSize :: GHC.Int.Int64
maxSize = 32768

main :: IO ()
main = quickHttpServe site

site :: Snap ()
site =
    ifTop (writeBS "hello world") <|>
    route [ ("type", method POST typeHandler),
            ("translate", method POST convert) ] <|>
    dir "static" (serveDirectory ".")

deserializationError :: LBS.ByteString -> Snap ()
deserializationError astJSON = do
  modifyResponse $ setResponseStatus 500 "Internal Server Error"
  logError $ BS.append "Error, couldn't deserialize: " $ lbs2BS astJSON
  writeBS $ "Error, couldn't deserialize"

convert :: Snap ()
convert = do
  modifyResponse $ setHeader "Content-Type" contentType
  astJSON <- readRequestBody maxSize
  let decodedAst = decode astJSON :: Maybe TIAbsSyntaxTree
  case decodedAst of
    Nothing -> deserializationError $ astJSON
    Just ast -> do
      let idrisAST = ti2PDecls ast
      writeBS $ C8.pack $ show idrisAST

typeHandler :: Snap ()
typeHandler = do
  modifyResponse $ setHeader "Content-Type" contentType
  astJSON <- readRequestBody maxSize
  let decodedAst = decode astJSON :: Maybe TIAbsSyntaxTree
  case (decodedAst) of 
    Nothing -> deserializationError $ astJSON
    Just ast -> do
      let encodedAst = encode ast
      liftIO $ putStrLn $ "Deserialized: " ++ (show encodedAst)
      writeLBS encodedAst
