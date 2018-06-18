--Library for flash cards on Google Sheets
--Copyright Laurence Emms 2018

module Lib (runFlashCards) where

import System.IO
import System.Environment

import qualified Data.Text as T

import qualified Data.ByteString.Char8 as C

--import Network.HTTP.Simple
import Network.HTTP.Client
import Network.HTTP.Client.TLS
import Network.HTTP.Types.Status

import Data.Aeson
import Data.Aeson.Types
import qualified Data.Map as M

import Web.Browser

data AuthResponse = AuthResponse {accessToken :: T.Text,
                                  tokenType :: T.Text,
                                  expiresIn :: Int,
                                  refreshToken :: T.Text}

instance FromJSON AuthResponse where
    parseJSON (Object v) = AuthResponse
        <$> v .: T.pack "access_token"
        <*> v .: T.pack "token_type"
        <*> v .: T.pack "expires_in"
        <*> v .: T.pack "refresh_token"
    parseJSON invalid = typeMismatch "AuthResponse" invalid

data RowsResponse = RowsResponse {range :: T.Text,
                                  majorDimension :: T.Text,
                                  values :: [[T.Text]]}

instance FromJSON RowsResponse where
    parseJSON (Object v) = RowsResponse
        <$> v .: T.pack "range"
        <*> v .: T.pack "majorDimension"
        <*> v .: T.pack "values"
    parseJSON invalid = typeMismatch "RowsResponse" invalid

getValues :: RowsResponse -> [[T.Text]]
getValues (RowsResponse {values = thisValues}) = thisValues

doFlashCards :: Manager -> String -> AuthResponse -> IO ()
doFlashCards manager spreadSheetID (AuthResponse {accessToken = thisAccessToken,
                                                  tokenType = thisTokenType,
                                                  expiresIn = thisExpiresIn,
                                                  refreshToken = thisRefreshToken})
    = do putStrLn $ "Access token: " ++ (show thisAccessToken)
         rowsRequest <- parseRequest $ "GET https://sheets.googleapis.com/v4/spreadsheets/" ++ spreadSheetID ++ "/values/Sheet1!A1:C2?access_token=" ++ (T.unpack thisAccessToken)
         putStrLn $ "Request: " ++ (show rowsRequest)
         rowsResponse <- httpLbs rowsRequest manager
         let maybeRowsData = decode (responseBody rowsResponse) :: Maybe RowsResponse
         case maybeRowsData of
             Nothing -> putStrLn "No rows found"
             Just rowsData -> putStrLn $ "Rows: " ++ (show (getValues rowsData))

runFlashCards :: IO ()
runFlashCards = do putStrLn "Running flash cards"
                   args <- getArgs
                   manager <- newManager tlsManagerSettings
                   openBrowser ("https://accounts.google.com/o/oauth2/v2/auth?scope=https://www.googleapis.com/auth/spreadsheets&response_type=code&state=security_token%3D138r5719ru3e1%26url%3Doauth2.example.com/token&redirect_uri=urn:ietf:wg:oauth:2.0:oob&client_id=" ++ (args !! 0))
                   putStrLn "Please enter authorization code:"
                   hFlush stdout
                   authCode <- getLine
                   putStrLn ("Token: " ++ authCode)
                   initialRequest <- parseRequest "https://www.googleapis.com/oauth2/v4/token"
                   let pairs = fmap (\(x, y) -> (C.pack x, C.pack y))
                                   [("code", authCode),
                                    ("client_id", (args !! 0)),
                                    ("client_secret", (args !! 1)),
                                    ("redirect_uri", "urn:ietf:wg:oauth:2.0:oob"),
                                    ("grant_type", "authorization_code")]
                       request = urlEncodedBody pairs initialRequest
                   putStrLn $ "Pairs: " ++ (show pairs)
                   putStrLn $ "Request: " ++ (show request)
                   hFlush stdout
                   response <- httpLbs request manager
                   putStrLn $ show response
                   let status = responseStatus response
                   if status == status200
                   then do let body = responseBody response
                           putStrLn $ show body
                           let maybeBodyData = decode body :: Maybe AuthResponse
                           case maybeBodyData of
                               Nothing -> putStrLn "No body found"
                               Just bodyData -> doFlashCards manager (args !! 2) bodyData
                   else putStrLn "Error occurred"
