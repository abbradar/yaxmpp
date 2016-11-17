module Network.XMPP.Language
  ( XMLLang
  , LocalizedText
  , localTexts
  , localizedFromTexts
  , localizedFromText
  , localizedFromElement
  , xmlLangGet
  , xmlLangAttr
  , localizedGet
  , localizedElements
  ) where

import Data.Maybe
import Data.Text (Text)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Text.XML
import Text.InterpolatedString.Perl6 (qq)
import Text.XML.Cursor hiding (element)
import qualified Text.XML.Cursor as XC

import Network.XMPP.XML
import Network.XMPP.Utils
import Network.XMPP.Stanza

type XMLLang = Maybe Text
newtype LocalizedText = LocalizedText { localTexts :: Map XMLLang Text }
                      deriving (Show, Eq)

localizedFromTexts :: Map XMLLang Text -> Maybe LocalizedText
localizedFromTexts m
  | M.null m = Nothing
  | otherwise = Just $ LocalizedText m

localizedFromText :: Text -> LocalizedText
localizedFromText = LocalizedText . M.singleton Nothing

localizedFromElement :: Name -> [Element] -> Maybe (Either StanzaError LocalizedText)
localizedFromElement name elems = case fromChildren elems $/ XC.element name &| curElement of
  [] -> Nothing
  bodies -> Just $ do
    texts <- mapM getOneBody bodies
    textsMap <- case mapDisjointFromList texts of
      Nothing -> Left $ badRequest [qq|localizedElement: conflicting textual data for language|]
      Just r -> return r
    return $ LocalizedText textsMap
  
  where getOneBody e = do
          cont <- fmap mconcat $ mapM getBodyContent $ elementNodes e
          return (xmlLangGet e, cont)
        getBodyContent (NodeContent t) = return t
        getBodyContent _ = Left $ badRequest [qq|localizedElement: $name element should contain only textual data|]

xmlLangGet :: Element -> XMLLang
xmlLangGet = getAttr $ xmlName "lang"

xmlLangAttr :: XMLLang -> [(Name, Text)]
xmlLangAttr Nothing = []
xmlLangAttr (Just lang) = [(xmlName "lang", lang)]

localizedGet :: XMLLang -> LocalizedText -> Text
localizedGet Nothing lt = case M.lookup Nothing $ localTexts lt of
  Just t -> t
  Nothing -> snd $ head $ M.toAscList $ localTexts lt
localizedGet lang@(Just _) lt = case M.lookup lang $ localTexts lt of
  Just t -> t
  Nothing -> localizedGet Nothing lt

localizedElements :: Name -> LocalizedText -> [Element]
localizedElements name t =  map toElement $ M.toList $ localTexts t
  where toElement (lang, text) = element name (maybeToList $ fmap (xmlName "lang", ) lang) [NodeContent text]
