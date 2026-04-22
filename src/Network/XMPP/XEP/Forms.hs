{-# LANGUAGE Strict #-}

-- | XEP-0004: Data Forms (@jabber:x:data@).
module Network.XMPP.XEP.Forms (
  formsNS,
  formsName,
  formField,
  formTypeField,
  submitForm,
) where

import Data.Maybe
import Data.Text (Text)
import Text.XML

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
