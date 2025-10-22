module Document (
  scraper,
  scrapers,
  Document (..),
  Document.Word (..),
  Document.Part (..),
) where

import Data.Data

-- import           Control.Monad
import Data.Char (toLower)

-- import           Data.Maybe
import Data.Text as Text hiding (toLower)
import Text.Regex (Regex)
import Text.Regex.Base.RegexLike (RegexMaker (makeRegex))

-- import Control.Monad.Logger.Aeson (LoggingT, MonadLogger)
import Data.Aeson (ToJSON)

-- import Env

-- import Env (Logger)
import GHC.Generics (Generic)
import Text.HTML.Scalpel.Core

newtype Document = Document Text deriving (Show)

re :: String -> Regex
re = makeRegex

type Title = Text.Text

type Transcription = Text.Text

data Kind
  = Noun
  | Pronoun
  | Verb
  | Adjective
  | Adverb
  | Preposition
  | Conjunction
  | Interjection
  deriving (Typeable, Data, Generic)
instance ToJSON Kind

tol :: String -> String
tol (x : xs) = toLower x : xs
tol [] = []

instance Show Kind where
  show = tol . showConstr . toConstr

type Variant = Text.Text

type Description = Text.Text

data Part = Part Kind Variant [Description] deriving (Show, Generic)
instance ToJSON Part

data Word = Word Title Transcription [Part] deriving (Show, Generic)
instance ToJSON Document.Word

toWordKind :: Text -> Kind
toWordKind = \case
  "noun" -> Noun
  "pronoun" -> Pronoun
  "verb" -> Verb
  "phrasal verb of" -> Verb
  "adjective" -> Adjective
  "adverb" -> Adverb
  "preposition" -> Preposition
  "Conjunction" -> Conjunction
  "Interjection" -> Interjection
  _ -> Noun -- Undefined type here

ttype :: Scraper Text Kind
ttype = toWordKind <$> text ("i" // "span" @: ["class" @= "YrbPuc"])

variant :: Scraper Text Variant
variant = text ("div" @: ["jsname" @= "jUIvqc"])

description :: Scraper Text [Description]
description = texts $ "div" @: ["data-dobid" @= "dfn"] -- // "span"

part :: Scraper Text Part
part = Part <$> ttype <*> variant <*> description

parts :: Scraper Text [Part]
parts = chroots ("div" @: ["jsname" @= "r5Nvmf"]) part

title :: Scraper Title Title
title = text $ "span" @: ["data-dobid" @= "hdw"]

transcription :: Scraper Text Text
transcription = text $ "span" @: ["class" @= "wHYlTd"]

textData :: Scraper Text Document.Word
textData = Word <$> title <*> transcription <*> parts

widget :: Scraper Text Document.Word
-- TODO merge them into one word
-- widget = chroots ("div" @: ["id" @=~ re "tsuid_[0-9]+"]) textData
widget = chroot ("div" @: ["id" @=~ re "tsuid_.*", "class" @= "xpdbox"]) textData

scraper :: Text -> Maybe Document.Word
scraper body = scrapeStringLike body widget

scrapers :: [Text] -> Maybe [Document.Word]
scrapers = traverse scraper
