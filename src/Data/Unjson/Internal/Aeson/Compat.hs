{-# LANGUAGE CPP #-}
module Data.Unjson.Internal.Aeson.Compat
  ( module Map
#if !MIN_VERSION_aeson(2,0,1)
  , KeyMap
#endif
  , fromText
  , toText
  , convertPair
  , LazyKeyMap
  , testErrorMessage
  , fromString
  , toString
  , lazyKeyMapFromList
  ) where

import Data.Text
import qualified Data.Aeson.Types as A

#if MIN_VERSION_aeson(2,0,1)

import Data.Aeson.KeyMap as Map
import qualified Data.Aeson.Key as K

fromText :: Text -> K.Key
fromText = K.fromText

toText :: K.Key -> Text
toText = K.toText

fromString :: String -> K.Key
fromString = K.fromString

toString :: K.Key -> String
toString = K.toString

convertPair :: (Text, A.Value) -> A.Pair
convertPair (t, v) = (K.fromText t, v)

type LazyKeyMap = KeyMap

testErrorMessage :: Text
testErrorMessage = "Error in $: parsing KeyMap failed, expected Object, but encountered String"

lazyKeyMapFromList :: [(K.Key, v)] -> Map.KeyMap v
lazyKeyMapFromList = Map.fromList

#else

import Data.HashMap.Strict as Map
import Data.Hashable (Hashable)
import qualified Data.HashMap.Lazy as LazyHashMap

type KeyMap a = Map.HashMap Text a
type LazyKeyMap a = LazyHashMap.HashMap Text a

fromText :: Text -> Text
fromText = id

toText :: Text -> Text
toText = id

fromString :: String -> String
fromString = id

toString :: String -> String
toString = id

convertPair :: (Text, A.Value) -> (Text, A.Value)
convertPair = id

testErrorMessage :: Text
testErrorMessage = "Error in $: parsing HashMap ~Text failed, expected Object, but encountered String"

lazyKeyMapFromList :: (Eq k, Hashable k) => [(k, v)] -> HashMap k v
lazyKeyMapFromList = LazyHashMap.fromList
#endif
