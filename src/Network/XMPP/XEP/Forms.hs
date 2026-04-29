{-# LANGUAGE Strict #-}

-- | XEP-0004: Data Forms (@jabber:x:data@) and XEP-0128 form-type scoping.
module Network.XMPP.XEP.Forms (
  formsNS,
  formsName,
  formField,
  formTypeField,
  submitForm,
  FormType,
  Form (..),
  parseForm,
  emitForm,
) where

import Data.Aeson (FromJSON, ToJSON)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Maybe
import Data.Text (Text)
import GHC.Generics (Generic)
import Text.XML
import Text.XML.Cursor hiding (element)
import qualified Text.XML.Cursor as XC

import Network.XMPP.XML

formsNS :: Text
formsName :: Text -> Name
(formsNS, formsName) = namePair "jabber:x:data"

{- | Single-value form field. The second argument is the optional @type@
attribute (e.g. @Just "hidden"@ for @FORM_TYPE@, @Just "boolean"@, etc.).
-}
formField :: Text -> Maybe Text -> Text -> Element
formField var ft val =
  element
    (formsName "field")
    (("var", var) : maybeToList (fmap ("type",) ft))
    [NodeElement $ element (formsName "value") [] [NodeContent val]]

{- | XEP-0068 @FORM_TYPE@ hidden field, used to scope standardized field sets
(MAM, MUC config, pubsub node config, …). Not required by XEP-0004 itself.
-}
formTypeField :: Text -> Element
formTypeField formType = formField "FORM_TYPE" (Just "hidden") formType

-- | Wrap the given fields in an @\<x type='submit'\>@ element.
submitForm :: [Element] -> Element
submitForm fields =
  element (formsName "x") [("type", "submit")] $ map NodeElement fields

-- | Wire name of a XEP-0128 @FORM_TYPE@ value.
type FormType = Text

{- | A generic data form (XEP-0004): the form's fields keyed by @var@. The
@FORM_TYPE@ scoping field (XEP-0128) is intentionally not carried here —
parsers and consumers handle it separately as a 'Maybe' 'FormType'
alongside the 'Form'.
-}
newtype Form = Form
  { formFields :: Map Text [Text]
  -- ^ Non-@FORM_TYPE@ fields: @var@ → list of @\<value\>@ contents.
  }
  deriving (Show, Eq, Generic)

instance ToJSON Form
instance FromJSON Form

{- | Parse an @\<x xmlns="jabber:x:data"\>@ element as a 'Form' plus the
optional 'FormType'. Layered result:

* @Right Nothing@ — element is not a data form (wrong name); skip it.
* @Right (Just (mFormType, form))@ — successfully parsed.
* @Left err@ — element is a data form but malformed.
-}
parseForm :: Element -> Either Text (Maybe (Maybe FormType, Form))
parseForm e
  | elementName e /= formsName "x" = Right Nothing
  | otherwise = do
      pairs <- traverse fieldPair $ fromElement e $/ XC.element (formsName "field") &| curElement
      let formTypeValues = [v | ("FORM_TYPE", vs) <- pairs, v <- vs, v /= ""]
      mFormType <- case formTypeValues of
        [] -> Right Nothing
        [v] -> Right (Just v)
        _ -> Left "multiple FORM_TYPE values"
      let formFields = M.fromListWith (++) [(var, vs) | (var, vs) <- pairs, var /= "FORM_TYPE"]
      Right $ Just (mFormType, Form {formFields})
 where
  fieldPair fld = case getAttr "var" fld of
    Nothing -> Left "<field> without var attribute"
    Just var -> Right (var, [mconcat (fromElement fld $/ XC.element (formsName "value") &/ content)])

{- | Render a 'Form' as @\<x type="result"\>@. When a @FORM_TYPE@ value is
supplied it is emitted first; remaining fields follow in sorted @var@
order.
-}
emitForm :: Maybe Text -> Form -> Element
emitForm mFormType Form {formFields} =
  element (formsName "x") [("type", "result")] $
    typeNode <> [NodeElement (multiField var vs) | (var, vs) <- M.toAscList formFields]
 where
  typeNode = case mFormType of
    Just ft -> [NodeElement (formField "FORM_TYPE" (Just "hidden") ft)]
    Nothing -> []
  multiField var vs =
    element (formsName "field") [("var", var)] $
      [NodeElement $ element (formsName "value") [] [NodeContent v] | v <- vs]
