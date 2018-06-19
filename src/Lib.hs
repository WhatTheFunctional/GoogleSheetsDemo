--Library for flash cards on Google Sheets
--Copyright Laurence Emms 2018

module Lib (runFlashCards) where

import Control.Monad.Trans.Class
import Control.Monad.Trans.Maybe

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

data Connection = Connection Manager AuthResponse

getValues :: RowsResponse -> [[T.Text]]
getValues (RowsResponse {values = thisValues}) = thisValues

doFlashCards :: [[T.Text]] -> MaybeT IO ()
doFlashCards [] = lift $ return ()
doFlashCards (row : rows)
    = do lift $ putStrLn $ T.unpack (row !! 0)
         lift $ hFlush stdout
         lift getLine
         lift $ putStrLn $ T.unpack (row !! 1)
         lift $ hFlush stdout
         lift getLine
         doFlashCards rows

getFlashCards :: String -> String -> Connection -> MaybeT IO [[T.Text]]
getFlashCards spreadSheetID rowsToRead (Connection manager (AuthResponse {accessToken = thisAccessToken,
                                                            tokenType = thisTokenType,
                                                            expiresIn = thisExpiresIn,
                                                            refreshToken = thisRefreshToken}))
    = do rowsRequest <- parseRequest ("GET https://sheets.googleapis.com/v4/spreadsheets/" ++
                                      spreadSheetID ++
                                      "/values/Sheet1!A1:B" ++ rowsToRead ++ "?access_token=" ++
                                      (T.unpack thisAccessToken))
         rowsResponse <- lift $ httpLbs rowsRequest manager
         maybeRowsResponse <- return (decode (responseBody rowsResponse) :: Maybe RowsResponse)
         MaybeT $ return $ fmap getValues maybeRowsResponse

createConnection :: Manager -> AuthResponse -> Maybe Connection
createConnection manager authResponse = Just $ Connection manager authResponse

setupConnection :: String -> String -> MaybeT IO Connection
setupConnection clientID clientSecret
    = do manager <- lift $ newManager tlsManagerSettings
         lift $ openBrowser ("https://accounts.google.com/o/oauth2/v2/auth?" ++
                             "scope=https://www.googleapis.com/auth/spreadsheets&" ++
                             "response_type=code&" ++
                             "state=security_token%3D138r5719ru3e1%26url%3Doauth2.example.com/token&" ++
                             "redirect_uri=urn:ietf:wg:oauth:2.0:oob&" ++
                             "client_id=" ++ clientID)
         lift $ putStrLn "Please enter authorization code:"
         lift $ hFlush stdout
         authCode <- lift $ getLine
         initialRequest <- lift $ parseRequest "https://www.googleapis.com/oauth2/v4/token"
         let pairs = fmap (\(x, y) -> (C.pack x, C.pack y))
                          [("code", authCode),
                           ("client_id", clientID),
                           ("client_secret", clientSecret),
                           ("redirect_uri", "urn:ietf:wg:oauth:2.0:oob"),
                           ("grant_type", "authorization_code")]
             request = urlEncodedBody pairs initialRequest
         response <- lift $ httpLbs request manager
         if responseStatus response == status200
         then do let body = responseBody response
                 do bodyData <- MaybeT $ return $ (decode body :: Maybe AuthResponse)
                    MaybeT $ return $ createConnection manager bodyData
         else MaybeT $ return $ Nothing

runFlashCardsMaybe :: MaybeT IO ()
runFlashCardsMaybe = do lift $ putStrLn "Running flash cards"
                        args <- lift $ getArgs
                        if length args < 4
                        then lift $ putStrLn "Usage: GoogleSheetsDemo-exe <client_id> <client_secret> <spreadsheet_id> <rows_to_read>"
                        else let clientID = args !! 0
                                 clientSecret = args !! 1
                                 spreadSheetID = args !! 2
                                 rowsToRead = args !! 3
                             in do connection <- setupConnection clientID clientSecret
                                   flashCards <- getFlashCards spreadSheetID rowsToRead connection
                                   doFlashCards flashCards

runFlashCards :: IO ()
runFlashCards = do runMaybeT runFlashCardsMaybe
                   putStrLn "Done"
