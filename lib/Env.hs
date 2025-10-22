module Env (Env, setCookie, App, Logger) where

import Control.Lens ((&), (.~))
import Control.Monad.IO.Class
import Control.Monad.Logger.Aeson (LoggingT, MonadLogger)
import Control.Monad.Reader
import Data.ByteString as B
import Data.Generics.Labels ()
import Data.Text as T
import Data.Text.Encoding qualified as T
import GHC.Generics (Generic)

newtype Logger a = LoggingT (IO a)
  deriving newtype
    ( Functor
    , Applicative
    , Monad
    , MonadIO
    )

newtype App a = App {unApp :: ReaderT Env (LoggingT IO) a}
  deriving newtype
    ( Functor
    , Applicative
    , Monad
    , MonadIO
    , MonadReader Env
    , MonadLogger
    )
  deriving (Generic)

data Env = Env
  { cookies :: B.ByteString
  , userAgent :: B.ByteString
  , dictionaryUrl :: String
  }
  deriving (Generic, Show)

defaultEnv :: Env
defaultEnv =
  Env
    { userAgent =
        "Mozilla/5.0 (X11; Linux x86_64; rv:109.0) Gecko/20100101 Firefox/118.0"
    , dictionaryUrl = "https://www.google.com/search"
    }

setCookie :: String -> Env
setCookie x = defaultEnv & #cookies .~ (T.encodeUtf8 . T.pack) x
