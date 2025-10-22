module Http (
  lookups,
  lookupW,
) where

import Control.Monad.IO.Class
import Control.Monad.Reader (ask)
import Data.ByteString as B
import Data.ByteString.Lazy as BL
import Data.Generics.Labels ()
import Env

import Data.Text as Text
import Data.Text.Encoding as Text

import Network.Wreq

import Control.Lens

import Codec.Compression.Brotli qualified as Brotli

dictionaryUrl :: String
dictionaryUrl = "https://www.google.com/search"

decode :: Response BL.ByteString -> Maybe BL.ByteString
decode res =
  case encoding of
    "br" -> Brotli.decompress <$> body
    _ -> body
 where
  encoding = res ^. responseHeader "Content-Encoding"
  body = res ^? responseBody

toText :: BL.ByteString -> Text
toText = Text.decodeUtf8 . B.concat . BL.toChunks

getBody :: Response BL.ByteString -> Maybe Text
getBody res = toText <$> decode res

getOpts :: Env -> Text.Text -> Options
getOpts env word =
  defaults
    & param "q" .~ ["define+" <> word]
    & header "Cookie" .~ [env ^. #cookies]
    & header "User-Agent" .~ ["Mozilla/5.0 (X11; Linux x86_64; rv:109.0) Gecko/20100101 Firefox/118.0"]

lookupW :: Env -> Text.Text -> IO (Maybe Text)
lookupW env word = do
  let opts = getOpts env word
  liftIO $ getBody <$> getWith opts dictionaryUrl

lookups :: [Text.Text] -> App [Maybe Text]
lookups texts = do
  env <- ask
  liftIO $ traverse (lookupW env) texts
