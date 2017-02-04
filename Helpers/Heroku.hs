module Helpers.Heroku (herokuConf) where

import Prelude
import Data.ByteString (ByteString)
import Data.Text (Text)
import Data.Text.Encoding (encodeUtf8)
import Web.Heroku (dbConnParams)

import qualified Data.Text as T

herokuConf :: IO ByteString
herokuConf = do
    params <- dbConnParams

    let connStr = formatParams params
    return connStr

    where
        formatParams :: [(Text, Text)] -> ByteString
        formatParams = encodeUtf8 . T.unwords . map toKeyValue

toKeyValue :: (Text, Text) -> Text
toKeyValue (k, v) = k `T.append` "=" `T.append` v