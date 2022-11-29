module Network.XMPP.XEP.Forwarding
  ( SetPage(..)
  , SetQuery(..)
  , ResultSetRange(..)
  , ResultSet(..)
  , rsmElement
  , parseRSM
  ) where

import Control.Monad
import Data.Maybe
import Data.Text (Text)
import qualified Data.Text as T
import Text.XML
import Text.XML.Cursor hiding (element)
import qualified Text.XML.Cursor as XC
import TextShow (showt)
import Text.Read

import Network.XMPP.Utils
import Network.XMPP.XML

rsmNS :: Text
rsmName :: Text -> Name
(rsmNS, rsmName) = namePair "http://jabber.org/protocol/rsm"

data SetPage = Before (Maybe Text)
             | After Text
             | Index Integer
             deriving (Show, Eq)

data SetQuery = SetQuery { rsmMax :: Integer
                         , rsmPage :: Maybe SetPage
                         }
              deriving (Show, Eq)

data ResultSetRange = ResultSetRange { rsmFirst :: Text
                                     , rsmFirstIndex :: Integer
                                     , rsmLast :: Text
                                     }
                    deriving (Show, Eq)

data ResultSet = ResultSet { rsmCount :: Integer
                           , rsmRange :: Maybe ResultSetRange
                           }
               deriving (Show, Eq)

rsmElement :: SetQuery -> Element
rsmElement (SetQuery {..}) = element (rsmName "set") [] (maxElement : filterElements)
  where maxElement = NodeElement (element (rsmName "max") [] [NodeContent $ showt rsmMax])
        filterElements = maybeToList $ fmap (NodeElement . resultPageElement) rsmPage

        resultPageElement (Before ment) = element (rsmName "before") [] $ maybeToList $ fmap NodeContent ment
        resultPageElement (After ent) = element (rsmName "after") [] [NodeContent ent]
        resultPageElement (Index idx) = element (rsmName "index") [] [NodeContent $ showt idx]

parseRSM :: Element -> Either String (Maybe ResultSet)
parseRSM el =
  case fromElement el $/ XC.element (rsmName "set") &| curElement of
    [] -> Right Nothing
    setE:_ -> do
      let cursor = fromElement setE
      countText <- maybeToEither "No count" $ listToMaybe $ cursor $/ XC.element (rsmName "count") &/ content
      rsmCount <- maybeToEither "Invalid count" $ readMaybe $ T.unpack countText
      when (rsmCount < 0) $ Left "Invalid count"
      let firstEs = cursor $/ XC.element (rsmName "first") &| curElement
      let lastEs = cursor $/ XC.element (rsmName "last") &| curElement
      case (firstEs, lastEs) of
        ([], []) -> return $ Just ResultSet { rsmCount, rsmRange = Nothing }
        (firstE:_, lastE:_) -> do
          rsmFirst <- maybeToEither "No first item" $ listToMaybe $ fromElement firstE $/ content
          rsmFirstIndex <- maybeToEither "Invalid index" $ readAttr (rsmName "index") firstE
          when (rsmFirstIndex < 0) $ Left "Invalid count"
          rsmLast <- maybeToEither "No last item" $ listToMaybe $ fromElement lastE $/ content
          return $ Just ResultSet { rsmCount, rsmRange = Just $ ResultSetRange {..} }
        _ -> Left "Invalid first/last items"
