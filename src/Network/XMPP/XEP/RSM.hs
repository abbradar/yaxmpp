{-# LANGUAGE Strict #-}

module Network.XMPP.XEP.RSM (
  SetPage (..),
  SetQuery (..),
  ResultSetRange (..),
  ResultSet (..),
  rsmElement,
  parseRSM,
) where

import Control.Monad
import Data.Maybe
import Data.Text (Text)
import qualified Data.Text as T
import Text.Read
import Text.XML
import Text.XML.Cursor hiding (element)
import qualified Text.XML.Cursor as XC
import TextShow (showt)

import Network.XMPP.Utils
import Network.XMPP.XML

rsmName :: Text -> Name
rsmName = nsName "http://jabber.org/protocol/rsm"

data SetPage
  = Before (Maybe Text)
  | After Text
  | Index Integer
  deriving (Show, Eq)

data SetQuery = SetQuery
  { rsmMax :: Integer
  , rsmPage :: Maybe SetPage
  }
  deriving (Show, Eq)

data ResultSetRange = ResultSetRange
  { rsmFirst :: Text
  , rsmFirstIndex :: Maybe Integer
  -- ^ XEP-0059: SHOULD be included when feasible, but MAY be omitted.
  , rsmLast :: Text
  }
  deriving (Show, Eq)

data ResultSet = ResultSet
  { rsmCount :: Maybe Integer
  -- ^ XEP-0059: OPTIONAL — servers may omit if count is expensive.
  , rsmRange :: Maybe ResultSetRange
  }
  deriving (Show, Eq)

rsmElement :: SetQuery -> Element
rsmElement (SetQuery {..}) = element (rsmName "set") [] (maxElement : filterElements)
 where
  maxElement = NodeElement (element (rsmName "max") [] [NodeContent $ showt rsmMax])
  filterElements = maybeToList $ fmap (NodeElement . resultPageElement) rsmPage

  resultPageElement (Before ment) = element (rsmName "before") [] $ maybeToList $ fmap NodeContent ment
  resultPageElement (After ent) = element (rsmName "after") [] [NodeContent ent]
  resultPageElement (Index idx) = element (rsmName "index") [] [NodeContent $ showt idx]

parseRSM :: Element -> Either String (Maybe ResultSet)
parseRSM el =
  case fromElement el $/ XC.element (rsmName "set") &| curElement of
    [] -> Right Nothing
    setE : _ -> do
      let cursor = fromElement setE
      rsmCount <- case listToMaybe $ cursor $/ XC.element (rsmName "count") &/ content of
        Nothing -> Right Nothing
        Just countText -> do
          c <- maybeToEither "Invalid count" $ readMaybe $ T.unpack countText
          when (c < 0) $ Left "Invalid count"
          Right (Just c)
      let firstEs = cursor $/ XC.element (rsmName "first") &| curElement
      let lastEs = cursor $/ XC.element (rsmName "last") &| curElement
      case (firstEs, lastEs) of
        ([], []) -> return $ Just ResultSet {rsmCount, rsmRange = Nothing}
        (firstE : _, lastE : _) -> do
          rsmFirst <- maybeToEither "No first item" $ listToMaybe $ fromElement firstE $/ content
          rsmFirstIndex <- case readAttr (rsmName "index") firstE of
            Nothing -> Right Nothing
            Just idx
              | idx < 0 -> Left "Invalid index"
              | otherwise -> Right (Just idx)
          rsmLast <- maybeToEither "No last item" $ listToMaybe $ fromElement lastE $/ content
          return $ Just ResultSet {rsmCount, rsmRange = Just $ ResultSetRange {..}}
        _ -> Left "Invalid first/last items"
