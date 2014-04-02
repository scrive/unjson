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

class Unjson a where
  valueDef :: ValueDef a

instance (Unjson a) => Unjson [a] where
  valueDef = arrayOf valueDef

instance Unjson Text.Text where
  valueDef = liftAesonFromJSON

instance (Unjson a,Unjson b) => Unjson (a,b) where
  valueDef = TupleValueDef $ renumber $
               pure (,)
               <*> field ""
               <*> field ""

instance (Unjson a,Unjson b,Unjson c) => Unjson (a,b,c) where
  valueDef = TupleValueDef $ renumber $
               pure (,,)
               <*> field ""
               <*> field ""
               <*> field ""

instance (Unjson a,Unjson b,Unjson c,Unjson d) => Unjson (a,b,c,d) where
  valueDef = TupleValueDef $ renumber $
               pure (,,,)
               <*> field ""
               <*> field ""
               <*> field ""
               <*> field ""

data ValueDef a where
  SimpleValueDef :: (Anchored Aeson.Value -> Result k) -> ValueDef k
  ArrayValueDef  :: ValueDef k -> ValueDef [k]
  ObjectValueDef :: Ap FieldDef k -> ValueDef k
  TupleValueDef  :: Ap TupleFieldDef k -> ValueDef k

class IsValueDef a where
  type IsValueDefResult a
  toValueDef :: a -> ValueDef (IsValueDefResult a)

instance IsValueDef (Anchored Aeson.Value -> Result k) where
  type IsValueDefResult (Anchored Aeson.Value -> Result k) = k
  toValueDef = SimpleValueDef

instance IsValueDef (Ap FieldDef k) where
  type IsValueDefResult (Ap FieldDef k) = k
  toValueDef = ObjectValueDef

instance IsValueDef (ValueDef k) where
  type IsValueDefResult (ValueDef k) = k
  toValueDef = id


data FieldDef a where
  FieldReqDef :: Text.Text -> ValueDef a -> FieldDef a
  FieldOptDef :: Text.Text -> ValueDef a -> FieldDef (Maybe a)
  FieldDefDef :: Text.Text -> a -> ValueDef a -> FieldDef a

data TupleFieldDef a where
  TupleFieldReqDef :: Int -> ValueDef a -> TupleFieldDef a
  TupleFieldOptDef :: Int -> ValueDef a -> TupleFieldDef (Maybe a)
  TupleFieldDefDef :: Int -> a -> ValueDef a -> TupleFieldDef a

renumber :: Ap FieldDef a -> Ap TupleFieldDef a
renumber x = renumber' 0 x

renumber' :: Int -> Ap FieldDef a -> Ap TupleFieldDef a
renumber' n (Pure z) = Pure z
renumber' n (Ap c r) = Ap (ren n c) (renumber' (n+1) r)

countAp :: Ap x a -> Int
countAp (Pure _) = 0
countAp (Ap _ r) = 1 + countAp r

ren n (FieldReqDef _ b) = TupleFieldReqDef n b
ren n (FieldOptDef _ b) = TupleFieldOptDef n b
ren n (FieldDefDef _ d b) = TupleFieldDefDef n d b

parse :: (IsValueDef d) => d -> Anchored Aeson.Value -> Result (IsValueDefResult d)
parse d = parse1 (toValueDef d)

parse1 :: ValueDef a -> Anchored Aeson.Value -> Result a
parse1 (SimpleValueDef f) v = f v
parse1 (ArrayValueDef f) (Anchored path (Aeson.Array v))
  = sequenceA (zipWith (\v i -> parse1 f (Anchored (path ++ [PathElemIndex i]) v)) (Vector.toList v) [0..])
parse1 (ObjectValueDef f) (Anchored path (Aeson.Object v))
  = runAp (lookupByFieldDef (Anchored path v)) f
parse1 (TupleValueDef f) (Anchored path (Aeson.Array v))
  = runAp (lookupByTupleFieldDef (Anchored path v)) f

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

lookupByTupleFieldDef :: Anchored Aeson.Array -> TupleFieldDef a -> Result a
lookupByTupleFieldDef (Anchored path v) (TupleFieldReqDef idx valuedef)
  = case v Vector.!? idx of
      Just x  -> parse valuedef (Anchored (path ++ [PathElemIndex idx]) x)
      Nothing -> resultWithThrow (Anchored (path ++ [PathElemIndex idx]) "missing key")
lookupByTupleFieldDef (Anchored path v) (TupleFieldDefDef idx def valuedef)
  = case v Vector.!? idx of
      Just x  -> parse valuedef (Anchored (path ++ [PathElemIndex idx]) x)
      Nothing -> Result def []
lookupByTupleFieldDef (Anchored path v) (TupleFieldOptDef idx valuedef)
  = case v Vector.!? idx of
      Just x  -> fmap Just (parse valuedef (Anchored (path ++ [PathElemIndex idx]) x))
      Nothing -> Result Nothing []

fieldBy :: (IsValueDef d) => Text.Text -> d -> Ap FieldDef (IsValueDefResult d)
fieldBy key valuedef = liftAp (FieldReqDef key (toValueDef valuedef))

field :: (Unjson a) => Text.Text -> Ap FieldDef a
field key = fieldBy key valueDef

field' :: (Aeson.FromJSON a) => Text.Text -> Text.Text -> Ap FieldDef a
field' key docstring = fieldBy key liftAesonFromJSON

fieldOptBy :: (IsValueDef d) => Text.Text -> d -> Ap FieldDef (Maybe (IsValueDefResult d))
fieldOptBy key valuedef = liftAp (FieldOptDef key (toValueDef valuedef))

fieldOpt :: (Unjson a) => Text.Text -> Ap FieldDef (Maybe a)
fieldOpt key = fieldOptBy key valueDef

fieldOpt' :: (Aeson.FromJSON a) => Text.Text -> Text.Text -> Ap FieldDef (Maybe a)
fieldOpt' key docstring = fieldOptBy key liftAesonFromJSON

fieldDefBy :: (IsValueDef d) => Text.Text -> (IsValueDefResult d) -> d -> Ap FieldDef (IsValueDefResult d)
fieldDefBy key a valuedef = liftAp (FieldDefDef key a (toValueDef valuedef))

fieldDef :: (Unjson a) => Text.Text -> a -> Ap FieldDef a
fieldDef key a = fieldDefBy key a valueDef

fieldDef' :: (Aeson.FromJSON a) => Text.Text -> a -> Text.Text -> Ap FieldDef a
fieldDef' key def docstring = fieldDefBy key def liftAesonFromJSON

arrayOf :: (IsValueDef d) => d -> ValueDef [(IsValueDefResult d)]
arrayOf valuedef = ArrayValueDef (toValueDef valuedef)

arrayOf' :: (Aeson.FromJSON a) => ValueDef [a]
arrayOf' = arrayOf liftAesonFromJSON

liftAesonFromJSON :: (Aeson.FromJSON a) => ValueDef a
liftAesonFromJSON = SimpleValueDef (\(Anchored path value) ->
                                        case Aeson.fromJSON value of
                                          Aeson.Success result -> Result result []
                                          Aeson.Error message -> resultWithThrow (Anchored path (Text.pack message)))
