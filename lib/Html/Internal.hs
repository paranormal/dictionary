module Html.Internal where

import Prelude hiding (div)

import Data.Text

import Data.Aeson
import Document (Part (..), Word (..))
import GHC.Generics (Generic)

newtype Structure = Structure Text
  deriving (Show, Semigroup, Monoid, Generic)

instance ToJSON Structure where
  toJSON = genericToJSON defaultOptions

newtype Content = Content Text

-- Structure
section :: Content -> Structure
section (Content text) = Structure $ el "div" text

ol :: [Structure] -> Structure
ol xs =
  let
    li = Data.Text.concat $ el "li" . structure <$> xs
   in
    Structure $ el "ol" li

structure :: Structure -> Text
structure (Structure x) = x

-- Content
italic :: Content -> Content
italic (Content text) = Content $ el "i" text

content :: Text -> Content
content = Content . escape

-- Utilities

el :: Text -> Text -> Text
el tag text = "<" <> tag <> ">" <> text <> "</" <> tag <> ">"

escape :: Text -> Text
escape = Data.Text.foldr ((<>) . escapeChar) ""
 where
  escapeChar :: Char -> Text
  escapeChar = \case
    '<' -> "&lt;"
    '>' -> "&gt;"
    '&' -> "&amp;"
    '"' -> "&quot;"
    '\'' -> "&#39;"
    x -> pack [x]

htmls :: [Document.Word] -> [Structure]
htmls = fmap front

front :: Document.Word -> Structure
front (Document.Word title transcription parts) =
  (section . content) title
    <> (section . content) transcription
    <> foldMap part parts

part :: Part -> Structure
part (Part kind variant description) =
  (section . italic . content . pack . show) kind
    <> (section . content) variant
    <> ol (section . content <$> description)
