module Network.XMPP.Language
  ( XMLLang
  , LocalizedText
  , localTexts
  , localizedText
  , getXMLLang
  , xmlLangAttr
  , getLocalized
  ) where

import Data.Text (Text)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Text.XML

import Network.XMPP.XML

type XMLLang = Maybe Text
newtype LocalizedText = LocalizedText { localTexts :: Map XMLLang Text }
                      deriving (Show, Eq)

localizedText :: Map XMLLang Text -> Maybe LocalizedText
localizedText m
  | M.null m = Nothing
  | otherwise = Just $ LocalizedText m

getXMLLang :: Element -> XMLLang
getXMLLang = getAttr $ xmlName "lang"

xmlLangAttr :: XMLLang -> [(Name, Text)]
xmlLangAttr Nothing = []
xmlLangAttr (Just lang) = [(xmlName "lang", lang)]

getLocalized :: LocalizedText -> XMLLang -> Text
getLocalized lt Nothing = case M.lookup Nothing $ localTexts lt of
  Just t -> t
  Nothing -> snd $ head $ M.toAscList $ localTexts lt
getLocalized lt lang@(Just _) = case M.lookup lang $ localTexts lt of
  Just t -> t
  Nothing -> getLocalized lt Nothing
