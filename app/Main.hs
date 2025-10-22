import Control.Lens ((^.))
import Control.Monad.Reader (MonadIO (liftIO), ReaderT, runReaderT)
import Data.Text qualified as Text
import Document (scrapers)
import Http (lookups)
import System.Environment
import System.Process

import Data.Maybe
import Data.Text
import Text.Pretty.Simple (pPrint)

import Control.Monad.Logger.Aeson

import Env
import Html.Internal

collectInfo :: IO Env
collectInfo = do
  cookies <- getEnv "COOKIES"
  let env = setCookie cookies
  return env

communicate :: Maybe [Structure] -> App [()]
communicate = \case
  Nothing -> pure [()]
  Just res -> liftIO $ traverse (\(Structure y) -> callProcess "/usr/bin/copyq" ["copy", "text/html", unpack y]) res

main :: IO ()
main = do
  env <- collectInfo
  input <- (Text.pack <$>) <$> getArgs
  runStderrLoggingT do
    logDebug $ "The args are:" :# ["input" .= input]
    documents <- sequence <$> runReaderT (lookups input ^. #unApp) env
    logDebug $ "The documents are" :# ["documents" .= documents]
    let scraped = documents >>= scrapers
    logDebug $ "The results are:" :# ["scraped" .= scraped]

    let httt = htmls <$> scraped
    _ <- runReaderT (communicate httt ^. #unApp) env
    logDebug $ "The htmls are:" :# ["input" .= httt]
