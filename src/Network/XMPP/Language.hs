module Network.XMPP.Language
  ( XMLLang
  , LocalizedText
  , localTexts
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
import Network.XMPP.Stanza

type XMLLang = Maybe Text
newtype LocalizedText = LocalizedText { localTexts :: Map XMLLang Text }
                      deriving (Show, Eq)

localizedFromText :: Map XMLLang Text -> Maybe LocalizedText
localizedFromText m
  | M.null m = Nothing
  | otherwise = Just $ LocalizedText m

localizedFromElement :: Name -> [Element] -> Maybe (Either StanzaError LocalizedText)
localizedFromElement name elems = case fromChildren elems $/ XC.element name &| curElement of
  [] -> Nothing
  bodies -> Just $ fmap (LocalizedText . M.fromList) $ mapM getOneBody bodies
  
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

localizedGet :: LocalizedText -> XMLLang -> Text
localizedGet lt Nothing = case M.lookup Nothing $ localTexts lt of
  Just t -> t
  Nothing -> snd $ head $ M.toAscList $ localTexts lt
localizedGet lt lang@(Just _) = case M.lookup lang $ localTexts lt of
  Just t -> t
  Nothing -> localizedGet lt Nothing

localizedElements :: Name -> LocalizedText -> [Element]
localizedElements name t =  map toElement $ M.toList $ localTexts t
  where toElement (lang, text) = element name (maybeToList $ fmap (xmlName "lang", ) lang) [NodeContent text]
