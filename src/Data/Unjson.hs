module Data.Unjson
where

import qualified Data.Aeson as Aeson
import qualified Data.Text as Text
import qualified Data.Vector as Vector
import Data.Typeable
import Data.Monoid
import Control.Applicative
import Control.Applicative.Free
import Data.Scientific
import Data.Attoparsec.Number
import qualified Data.HashMap.Strict as HashMap
import Control.Exception
import Data.Traversable

data PathElem = PathElemKey Text.Text
              | PathElemIndex Int
  deriving (Typeable, Eq, Ord, Show)

type Path = [PathElem]

showPath :: Bool -> Path -> Text.Text
showPath _ [] = ""
showPath True (PathElemKey key : rest) = key <> showPath False rest
showPath False (PathElemKey key : rest) = "." <> key <> showPath False rest
showPath _ (PathElemIndex key : rest) = "[" <> Text.pack (show key) <> "]" <> showPath False rest

data Anchored a = Anchored Path a
  deriving (Typeable, Functor, Eq, Ord)

instance (Show a) => Show (Anchored a) where
  show (Anchored path value) = Text.unpack (showPath True path) ++ ": " ++ show value

instance (Typeable a, Show a) => Exception (Anchored a)

------------------------------------------------------------

data DocumentValue
  = DocumentValue { documentValueText :: Text.Text                   -- ^ description of this particular item
                  , documentValueKeys :: [(Text.Text, DocumentKey)]  -- ^ description of its parts, key-value
                  }
  deriving (Eq, Ord, Show, Typeable)

data DocumentKey
  = DocumentKey { documentKeyText  :: Text.Text          -- ^ description of this particular item
                , documentKeyValue :: DocumentValue      -- ^ description of its parts, key-value
                }
  deriving (Eq, Ord, Show, Typeable)


{-
documentF :: UnjsonX' a -> Documentation
documentF (Field key docstring p) = Documentation "" [(key, docstring, document p)]
documentF (Leaf _) = Documentation "" [] -- we could have documentation here...

document :: UnjsonX a -> Documentation
document (Pure x) = Documentation "" []
document (Ap a b) = Documentation (a1 <> b1) (a2 <> b2)
  where
    Documentation a1 a2 = documentF a
    Documentation b1 b2 = document b
-}

----------------------------------

type Problem = Anchored Text.Text
type Problems = [Problem]

data Result a = Result a Problems
  deriving (Functor, Show, Ord, Eq)

instance Applicative Result where
  pure a = Result a []
  Result a pa <*> Result b pb = Result (a b) (pa ++ pb)

resultWithThrow :: Anchored Text.Text -> Result a
resultWithThrow msg = Result (throw msg) [msg]


----------------------------------------

data ValueDef a where
  SimpleValueDef :: (Anchored Aeson.Value -> Result k) -> ValueDef k
  ArrayValueDef  :: ValueDef k -> ValueDef [k]
  ObjectValueDef :: Ap FieldDef k -> ValueDef k

data FieldDef a where
  FieldReqDef :: Text.Text -> ValueDef a -> FieldDef a
  FieldOptDef :: Text.Text -> ValueDef a -> FieldDef (Maybe a)
  FieldDefDef :: Text.Text -> a -> ValueDef a -> FieldDef a


parse :: ValueDef a -> Anchored Aeson.Value -> Result a
parse (SimpleValueDef f) v = f v
parse (ArrayValueDef f) (Anchored path (Aeson.Array v))
  = sequenceA (zipWith (\v i -> parse f (Anchored (path ++ [PathElemIndex i]) v)) (Vector.toList v) [0..])
parse (ObjectValueDef f) (Anchored path (Aeson.Object v))
  = runAp (lookupByFieldDef (Anchored path v)) f

lookupByFieldDef :: Anchored Aeson.Object -> FieldDef a -> Result a
lookupByFieldDef (Anchored path v) (FieldReqDef name valuedef)
  = case HashMap.lookup name v of
      Just x  -> parse valuedef (Anchored (path ++ [PathElemKey name]) x)
      Nothing -> resultWithThrow (Anchored (path ++ [PathElemKey name]) "missing key")
lookupByFieldDef (Anchored path v) (FieldDefDef name def valuedef)
  = case HashMap.lookup name v of
      Just x  -> parse valuedef (Anchored (path ++ [PathElemKey name]) x)
      Nothing -> Result def []
lookupByFieldDef (Anchored path v) (FieldOptDef name valuedef)
  = case HashMap.lookup name v of
      Just x  -> fmap Just (parse valuedef (Anchored (path ++ [PathElemKey name]) x))
      Nothing -> Result Nothing []

field :: Text.Text -> ValueDef a -> Ap FieldDef a
field key valuedef = liftAp (FieldReqDef key valuedef)

fieldOpt :: Text.Text -> ValueDef a -> Ap FieldDef (Maybe a)
fieldOpt key valuedef = liftAp (FieldOptDef key valuedef)

fieldDef :: Text.Text -> a -> ValueDef a -> Ap FieldDef a
fieldDef key a valuedef = liftAp (FieldDefDef key a valuedef)

arrayOf :: ValueDef a -> ValueDef [a]
arrayOf valuedef = ArrayValueDef valuedef

liftAesonFromJSON :: (Aeson.FromJSON a) => ValueDef a
liftAesonFromJSON = SimpleValueDef (\(Anchored path value) ->
                                        case Aeson.fromJSON value of
                                          Aeson.Success result -> Result result []
                                          Aeson.Error message -> resultWithThrow (Anchored path (Text.pack message)))
