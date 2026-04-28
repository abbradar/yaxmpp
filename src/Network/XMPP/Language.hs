{-# LANGUAGE Strict #-}

module Network.XMPP.Language (
  XMLLang,
  LocalizedText,
  localTexts,
  localizedFromTexts,
  localizedFromText,
  localizedFromElement,
  xmlLangGet,
  xmlLangAttr,
  localizedGet,
  localizedElements,
) where

import Data.Aeson (FromJSON (..), ToJSON (..))
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Maybe
import Data.String.Interpolate (i)
import Data.Text (Text)
import Text.XML
import Text.XML.Cursor hiding (element)
import qualified Text.XML.Cursor as XC

import Network.XMPP.Stanza
import Network.XMPP.Utils
import Network.XMPP.XML

type XMLLang = Maybe Text
newtype LocalizedText = LocalizedText {localTexts :: Map XMLLang Text}
  deriving (Show, Eq)

-- | JSON encoding: list of @[lang, text]@ pairs (lang is @null@ when absent).
instance ToJSON LocalizedText where
  toJSON (LocalizedText m) = toJSON $ M.toList m

instance FromJSON LocalizedText where
  parseJSON v = LocalizedText . M.fromList <$> parseJSON v

localizedFromTexts :: Map XMLLang Text -> Maybe LocalizedText
localizedFromTexts m
  | M.null m = Nothing
  | otherwise = Just $ LocalizedText m

localizedFromText :: Text -> LocalizedText
localizedFromText = LocalizedText . M.singleton Nothing

localizedFromElement :: Name -> [Element] -> Either StanzaError (Maybe LocalizedText)
localizedFromElement name elems = case fromChildren elems $/ XC.element name &| curElement of
  [] -> Right Nothing
  bodies -> do
    texts <- mapM getOneBody bodies
    textsMap <- case mapDisjointFromList texts of
      Nothing -> Left $ badRequest [i|conflicting textual data for language|]
      Just r -> Right r
    Right $ Just $ LocalizedText textsMap
 where
  getOneBody e = do
    cont <- fmap mconcat $ mapM getBodyContent $ elementNodes e
    Right (xmlLangGet e, cont)
  getBodyContent (NodeContent t) = Right t
  getBodyContent _ = Left $ badRequest [i|#{name} element should contain only textual data|]

xmlLangGet :: Element -> XMLLang
xmlLangGet = getAttr $ xmlName "lang"

xmlLangAttr :: XMLLang -> [(Name, Text)]
xmlLangAttr Nothing = []
xmlLangAttr (Just lang) = [(xmlName "lang", lang)]

localizedGet :: XMLLang -> LocalizedText -> Text
localizedGet Nothing lt = case M.lookup Nothing $ localTexts lt of
  Just t -> t
  Nothing -> case M.toAscList $ localTexts lt of
    (_, t) : _ -> t
    [] -> error "localizedGet: empty LocalizedText"
localizedGet lang@(Just _) lt = case M.lookup lang $ localTexts lt of
  Just t -> t
  Nothing -> localizedGet Nothing lt

localizedElements :: Name -> LocalizedText -> [Element]
localizedElements name t = map toElement $ M.toList $ localTexts t
 where
  toElement (lang, text) = element name (maybeToList $ fmap (xmlName "lang",) lang) [NodeContent text]
