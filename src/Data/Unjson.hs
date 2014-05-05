


-- | Very nice module

module Data.Unjson
( Unjson(..)
, ValueDef(..)
, Problem
, Problems
, Path
, PathElem(..)
, ArrayValueMode(..)
, serialize1
, parseUpdating
, field
, field'
, fieldBy
, fieldOpt
, fieldOpt'
, fieldOptBy
, fieldDef
, fieldDef'
, fieldDefBy
, arrayOf
, arrayOf'
, render
, liftAesonFromJSON
, Result(..)
, Anchored(..)
, parse
, PrimaryKeyExtraction(..)
)
where


{-

* write more documentation

-}

import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Types as Aeson
import qualified Data.Text as Text
import qualified Data.Vector as Vector
import qualified Data.Map as Map
import Data.Typeable
import Data.Monoid
import Control.Applicative
import Control.Applicative.Free
import Data.Scientific
import Data.Attoparsec.Number
import qualified Data.HashMap.Strict as HashMap
import Control.Exception
import Data.Traversable

import qualified Text.PrettyPrint.HughesPJ as P

-- | Describe a path from root JSON element to a specific
-- position. JSON has only two types of containers: objects and
-- arrays, so there are only two types of keys needed to index into
-- those containers: 'Int' and 'Text.Text'.
--
-- 'PathElem's are rendered in a nice way. For example: @key.key2[34]@
-- indexes into \"key\", then into \"key2\" then into index 34 of an
-- array.
data PathElem = PathElemKey Text.Text
              | PathElemIndex Int
  deriving (Typeable, Eq, Ord, Show)

type Path = [PathElem]

showPath :: Bool -> Path -> Text.Text
showPath _ [] = ""
showPath True (PathElemKey key : rest) = key <> showPath False rest
showPath False (PathElemKey key : rest) = "." <> key <> showPath False rest
showPath _ (PathElemIndex key : rest) = "[" <> Text.pack (show key) <> "]" <> showPath False rest

-- | A value at a specific position in JSON object.
data Anchored a = Anchored Path a
  deriving (Typeable, Functor, Eq, Ord)

instance (Show a) => Show (Anchored a) where
  show (Anchored path value) = Text.unpack (showPath True path) ++ ": " ++ show value

instance (Typeable a, Show a) => Exception (Anchored a)

-- | Problem information is represented as a 'Text.Text' attached to a
-- specific point in the JSON represenation tree.
type Problem = Anchored Text.Text

-- | In general JSON deserialization may result in many
-- problems. Unjson reports all the problems at once.
type Problems = [Problem]

-- | Parsing result. The value 'a' is only reliable when 'Problems' is
-- an empty list.
--
-- 'Problems' is list of issues encountered while parsing. 'Unjson'
-- parsers continue forward and are able to find many problems at
-- once.
--
-- Note that problems are anchored to specific elements of JSON so it
-- should be easy to find and spot an error.
--
-- Even if list of problems is not empty, the returned value may be
-- partially usable.
data Result a = Result a Problems
  deriving (Functor, Show, Ord, Eq)

instance Applicative Result where
  pure a = Result a []
  Result a pa <*> Result b pb = Result (a b) (pa ++ pb)

instance Monad Result where
  return = pure
  Result a pa >>= m = Result ma (pa ++ pma)
    where Result ma pma = m a

resultWithThrow :: Anchored Text.Text -> Result a
resultWithThrow msg = Result (throw msg) [msg]

-- | 'Unjson' typeclass describes all types that can be parsed from
-- JSON and JSON generated from their values.
--
-- Class 'Unjson' has primitive types lifted from 'Aeson.FromJSON' and
-- 'Aeson.ToJSON'.
class Unjson a where
  -- | Definition of bidirectional parser for type 'a'.
  valueDef :: ValueDef a

instance (Unjson a) => Unjson [a] where
  valueDef = arrayOf valueDef

instance Unjson Text.Text where
  valueDef = liftAesonFromJSON

instance Unjson Int where
  valueDef = liftAesonFromJSON

instance Unjson String where
  valueDef = liftAesonFromJSON

instance (Unjson a,Unjson b) => Unjson (a,b) where
  valueDef = TupleValueDef
                 $ pure (,)
               <*> liftAp (TupleFieldDef 0 (\(p,_) -> p) valueDef)
               <*> liftAp (TupleFieldDef 1 (\(_,p) -> p) valueDef)

instance (Unjson a,Unjson b,Unjson c) => Unjson (a,b,c) where
  valueDef = TupleValueDef
               $ pure (,,)
               <*> liftAp (TupleFieldDef 0 (\(p,_,_) -> p) valueDef)
               <*> liftAp (TupleFieldDef 1 (\(_,p,_) -> p) valueDef)
               <*> liftAp (TupleFieldDef 2 (\(_,_,p) -> p) valueDef)

instance (Unjson a,Unjson b,Unjson c,Unjson d) => Unjson (a,b,c,d) where
  valueDef = TupleValueDef
               $ pure (,,,)
               <*> liftAp (TupleFieldDef 0 (\(p,_,_,_) -> p) valueDef)
               <*> liftAp (TupleFieldDef 1 (\(_,p,_,_) -> p) valueDef)
               <*> liftAp (TupleFieldDef 2 (\(_,_,p,_) -> p) valueDef)
               <*> liftAp (TupleFieldDef 3 (\(_,_,_,p) -> p) valueDef)

instance (Unjson a,Unjson b,Unjson c,Unjson d
         ,Unjson e) => Unjson (a,b,c,d
                              ,e) where
  valueDef = TupleValueDef
               $ pure (,,,,)
               <*> liftAp (TupleFieldDef 0 (\(p,_,_,_,_) -> p) valueDef)
               <*> liftAp (TupleFieldDef 1 (\(_,p,_,_,_) -> p) valueDef)
               <*> liftAp (TupleFieldDef 2 (\(_,_,p,_,_) -> p) valueDef)
               <*> liftAp (TupleFieldDef 3 (\(_,_,_,p,_) -> p) valueDef)
               <*> liftAp (TupleFieldDef 4 (\(_,_,_,_,p) -> p) valueDef)

instance (Unjson a,Unjson b,Unjson c,Unjson d
         ,Unjson e,Unjson f)
       => Unjson (a,b,c,d
                 ,e,f) where
  valueDef = TupleValueDef
               $ pure (,,,,,)
               <*> liftAp (TupleFieldDef 0 (\(p,_,_,_,_,_) -> p) valueDef)
               <*> liftAp (TupleFieldDef 1 (\(_,p,_,_,_,_) -> p) valueDef)
               <*> liftAp (TupleFieldDef 2 (\(_,_,p,_,_,_) -> p) valueDef)
               <*> liftAp (TupleFieldDef 3 (\(_,_,_,p,_,_) -> p) valueDef)
               <*> liftAp (TupleFieldDef 4 (\(_,_,_,_,p,_) -> p) valueDef)
               <*> liftAp (TupleFieldDef 5 (\(_,_,_,_,_,p) -> p) valueDef)

instance (Unjson a,Unjson b,Unjson c,Unjson d
         ,Unjson e,Unjson f,Unjson g)
       => Unjson (a,b,c,d
                 ,e,f,g) where
  valueDef = TupleValueDef
               $ pure (,,,,,,)
               <*> liftAp (TupleFieldDef 0 (\(p,_,_,_,_,_,_) -> p) valueDef)
               <*> liftAp (TupleFieldDef 1 (\(_,p,_,_,_,_,_) -> p) valueDef)
               <*> liftAp (TupleFieldDef 2 (\(_,_,p,_,_,_,_) -> p) valueDef)
               <*> liftAp (TupleFieldDef 3 (\(_,_,_,p,_,_,_) -> p) valueDef)
               <*> liftAp (TupleFieldDef 4 (\(_,_,_,_,p,_,_) -> p) valueDef)
               <*> liftAp (TupleFieldDef 5 (\(_,_,_,_,_,p,_) -> p) valueDef)
               <*> liftAp (TupleFieldDef 6 (\(_,_,_,_,_,_,p) -> p) valueDef)

instance (Unjson a,Unjson b,Unjson c,Unjson d
         ,Unjson e,Unjson f,Unjson g,Unjson h)
       => Unjson (a,b,c,d
                 ,e,f,g,h) where
  valueDef = TupleValueDef
               $ pure (,,,,,,,)
               <*> liftAp (TupleFieldDef 0 (\(p,_,_,_,_,_,_,_) -> p) valueDef)
               <*> liftAp (TupleFieldDef 1 (\(_,p,_,_,_,_,_,_) -> p) valueDef)
               <*> liftAp (TupleFieldDef 2 (\(_,_,p,_,_,_,_,_) -> p) valueDef)
               <*> liftAp (TupleFieldDef 3 (\(_,_,_,p,_,_,_,_) -> p) valueDef)
               <*> liftAp (TupleFieldDef 4 (\(_,_,_,_,p,_,_,_) -> p) valueDef)
               <*> liftAp (TupleFieldDef 5 (\(_,_,_,_,_,p,_,_) -> p) valueDef)
               <*> liftAp (TupleFieldDef 6 (\(_,_,_,_,_,_,p,_) -> p) valueDef)
               <*> liftAp (TupleFieldDef 7 (\(_,_,_,_,_,_,_,p) -> p) valueDef)

instance (Unjson a,Unjson b,Unjson c,Unjson d
         ,Unjson e,Unjson f,Unjson g,Unjson h
         ,Unjson i)
       => Unjson (a,b,c,d
                 ,e,f,g,h
                 ,i) where
  valueDef = TupleValueDef
               $ pure (,,,,,,,,)
               <*> liftAp (TupleFieldDef 0 (\(p,_,_,_,_,_,_,_,_) -> p) valueDef)
               <*> liftAp (TupleFieldDef 1 (\(_,p,_,_,_,_,_,_,_) -> p) valueDef)
               <*> liftAp (TupleFieldDef 2 (\(_,_,p,_,_,_,_,_,_) -> p) valueDef)
               <*> liftAp (TupleFieldDef 3 (\(_,_,_,p,_,_,_,_,_) -> p) valueDef)
               <*> liftAp (TupleFieldDef 4 (\(_,_,_,_,p,_,_,_,_) -> p) valueDef)
               <*> liftAp (TupleFieldDef 5 (\(_,_,_,_,_,p,_,_,_) -> p) valueDef)
               <*> liftAp (TupleFieldDef 6 (\(_,_,_,_,_,_,p,_,_) -> p) valueDef)
               <*> liftAp (TupleFieldDef 7 (\(_,_,_,_,_,_,_,p,_) -> p) valueDef)
               <*> liftAp (TupleFieldDef 8 (\(_,_,_,_,_,_,_,_,p) -> p) valueDef)

instance (Unjson a,Unjson b,Unjson c,Unjson d
         ,Unjson e,Unjson f,Unjson g,Unjson h
         ,Unjson i,Unjson j)
       => Unjson (a,b,c,d
                 ,e,f,g,h
                 ,i,j) where
  valueDef = TupleValueDef
               $ pure (,,,,,,,,,)
               <*> liftAp (TupleFieldDef 0 (\(p,_,_,_,_,_,_,_,_,_) -> p) valueDef)
               <*> liftAp (TupleFieldDef 1 (\(_,p,_,_,_,_,_,_,_,_) -> p) valueDef)
               <*> liftAp (TupleFieldDef 2 (\(_,_,p,_,_,_,_,_,_,_) -> p) valueDef)
               <*> liftAp (TupleFieldDef 3 (\(_,_,_,p,_,_,_,_,_,_) -> p) valueDef)
               <*> liftAp (TupleFieldDef 4 (\(_,_,_,_,p,_,_,_,_,_) -> p) valueDef)
               <*> liftAp (TupleFieldDef 5 (\(_,_,_,_,_,p,_,_,_,_) -> p) valueDef)
               <*> liftAp (TupleFieldDef 6 (\(_,_,_,_,_,_,p,_,_,_) -> p) valueDef)
               <*> liftAp (TupleFieldDef 7 (\(_,_,_,_,_,_,_,p,_,_) -> p) valueDef)
               <*> liftAp (TupleFieldDef 8 (\(_,_,_,_,_,_,_,_,p,_) -> p) valueDef)
               <*> liftAp (TupleFieldDef 9 (\(_,_,_,_,_,_,_,_,_,p) -> p) valueDef)

instance (Unjson a,Unjson b,Unjson c,Unjson d
         ,Unjson e,Unjson f,Unjson g,Unjson h
         ,Unjson i,Unjson j,Unjson k)
       => Unjson (a,b,c,d
                 ,e,f,g,h
                 ,i,j,k) where
  valueDef = TupleValueDef
               $ pure (,,,,,,,,,,)
               <*> liftAp (TupleFieldDef 0 (\(p,_,_,_,_,_,_,_,_,_,_) -> p) valueDef)
               <*> liftAp (TupleFieldDef 1 (\(_,p,_,_,_,_,_,_,_,_,_) -> p) valueDef)
               <*> liftAp (TupleFieldDef 2 (\(_,_,p,_,_,_,_,_,_,_,_) -> p) valueDef)
               <*> liftAp (TupleFieldDef 3 (\(_,_,_,p,_,_,_,_,_,_,_) -> p) valueDef)
               <*> liftAp (TupleFieldDef 4 (\(_,_,_,_,p,_,_,_,_,_,_) -> p) valueDef)
               <*> liftAp (TupleFieldDef 5 (\(_,_,_,_,_,p,_,_,_,_,_) -> p) valueDef)
               <*> liftAp (TupleFieldDef 6 (\(_,_,_,_,_,_,p,_,_,_,_) -> p) valueDef)
               <*> liftAp (TupleFieldDef 7 (\(_,_,_,_,_,_,_,p,_,_,_) -> p) valueDef)
               <*> liftAp (TupleFieldDef 8 (\(_,_,_,_,_,_,_,_,p,_,_) -> p) valueDef)
               <*> liftAp (TupleFieldDef 9 (\(_,_,_,_,_,_,_,_,_,p,_) -> p) valueDef)
               <*> liftAp (TupleFieldDef 10 (\(_,_,_,_,_,_,_,_,_,_,p) -> p) valueDef)

instance (Unjson a,Unjson b,Unjson c,Unjson d
         ,Unjson e,Unjson f,Unjson g,Unjson h
         ,Unjson i,Unjson j,Unjson k,Unjson l)
       => Unjson (a,b,c,d
                 ,e,f,g,h
                 ,i,j,k,l) where
  valueDef = TupleValueDef
               $ pure (,,,,,,,,,,,)
               <*> liftAp (TupleFieldDef 0 (\(p,_,_,_,_,_,_,_,_,_,_,_) -> p) valueDef)
               <*> liftAp (TupleFieldDef 1 (\(_,p,_,_,_,_,_,_,_,_,_,_) -> p) valueDef)
               <*> liftAp (TupleFieldDef 2 (\(_,_,p,_,_,_,_,_,_,_,_,_) -> p) valueDef)
               <*> liftAp (TupleFieldDef 3 (\(_,_,_,p,_,_,_,_,_,_,_,_) -> p) valueDef)
               <*> liftAp (TupleFieldDef 4 (\(_,_,_,_,p,_,_,_,_,_,_,_) -> p) valueDef)
               <*> liftAp (TupleFieldDef 5 (\(_,_,_,_,_,p,_,_,_,_,_,_) -> p) valueDef)
               <*> liftAp (TupleFieldDef 6 (\(_,_,_,_,_,_,p,_,_,_,_,_) -> p) valueDef)
               <*> liftAp (TupleFieldDef 7 (\(_,_,_,_,_,_,_,p,_,_,_,_) -> p) valueDef)
               <*> liftAp (TupleFieldDef 8 (\(_,_,_,_,_,_,_,_,p,_,_,_) -> p) valueDef)
               <*> liftAp (TupleFieldDef 9 (\(_,_,_,_,_,_,_,_,_,p,_,_) -> p) valueDef)
               <*> liftAp (TupleFieldDef 10 (\(_,_,_,_,_,_,_,_,_,_,p,_) -> p) valueDef)
               <*> liftAp (TupleFieldDef 11 (\(_,_,_,_,_,_,_,_,_,_,_,p) -> p) valueDef)

-- | Specify how arrays should be handled. Default is
-- 'ArrayValueModeStrict' that does not do anything special with
-- arrays.
data ArrayValueMode
  -- | Require JSON array. On output always output array.
  = ArrayValueModeStrict

  -- | Allow non-array element, in that case it will be treated as a
  -- single element array. On output always output array.
  | ArrayValueModeParseSingle

  -- | Allow non-array element, in that case it will be treated as a
  -- single element array. On output output single element if array
  -- has one element.
  | ArrayValueModeParseAndOutputSingle
  deriving (Eq, Ord, Show, Typeable)

-- | Pair of functions. First one should extract a value uniquelly
-- indentifying an element of an array, the second one should extract
-- equivalent value from a JSON value. Used in array parsing, see
-- 'ArrayValueDef'.
data PrimaryKeyExtraction k = forall pk . (Ord pk) => PrimaryKeyExtraction (k -> pk) (ValueDef pk)

data ValueDef a where
  SimpleValueDef :: (Anchored Aeson.Value -> Result k) -> (k -> Aeson.Value) -> ValueDef k
  ArrayValueDef  :: Maybe (PrimaryKeyExtraction k) -> ArrayValueMode -> ValueDef k -> ValueDef [k]
  ObjectValueDef :: Ap (FieldDef k) k -> ValueDef k
  TupleValueDef  :: Ap (TupleFieldDef k) k -> ValueDef k
  -- DisjointValueDef :: Ap (FieldDef k) k -> ValueDef k

-- | Define a relation between a field of an object in JSON and a
-- field in a Haskell record structure.  'FieldDef' holds information
-- about a documentation string, key name, Haskell data accessor and
-- parsing definition.  'FieldDef' has three cases for fields that are
-- required, optional (via 'Maybe') or jave default value.
data FieldDef s a where
  FieldReqDef :: Text.Text -> Text.Text -> (s -> a) -> ValueDef a -> FieldDef s a
  FieldOptDef :: Text.Text -> Text.Text -> (s -> Maybe a) -> ValueDef a -> FieldDef s (Maybe a)
  FieldDefDef :: Text.Text -> Text.Text -> a -> (s -> a) -> ValueDef a -> FieldDef s a

-- | Define a tuple element. 'TupleFieldDef' holds information about
-- index, accessor function and a parser definition.
data TupleFieldDef s a where
  TupleFieldDef :: Int -> (s -> a) -> ValueDef a -> TupleFieldDef s a

tupleDefToArray :: s -> Ap (TupleFieldDef s) a -> [Aeson.Value]
tupleDefToArray _ (Pure _) = []
tupleDefToArray s (Ap (TupleFieldDef _ f d) r) =  (serialize1 d (f s)) : tupleDefToArray s r


objectDefToArray :: s -> Ap (FieldDef s) a -> [(Text.Text,Aeson.Value)]
objectDefToArray _ (Pure _) = []
objectDefToArray s (Ap (FieldReqDef key _ f d) r) = (key,serialize1 d (f s)) : objectDefToArray s r
objectDefToArray s (Ap (FieldOptDef key _ f d) r) =
  case f s of
    Nothing -> objectDefToArray s r
    Just g ->  (key,serialize1 d g) : objectDefToArray s r
objectDefToArray s (Ap (FieldDefDef key _ _ f d) r) = (key,serialize1 d (f s)) : objectDefToArray s r

-- | Given a definition of a value and a value produce a JSON.
serialize1 :: ValueDef a -> a -> Aeson.Value
serialize1 (SimpleValueDef _ g) a = g a
serialize1 (ArrayValueDef _ ArrayValueModeParseAndOutputSingle f) [a] =
  serialize1 f a
serialize1 (ArrayValueDef _ _m f) a =              -- here compiler should know that 'a' is a list
  Aeson.toJSON (map (serialize1 f) a)
serialize1 (ObjectValueDef f) a =
  Aeson.object (objectDefToArray a f)
serialize1 (TupleValueDef f) a =
  Aeson.toJSON (tupleDefToArray a f)

-- | Count how many applications there are. Useful for error
-- reporting.
countAp :: Int -> Ap x a -> Int
countAp !n (Pure _) = n
countAp n (Ap _ r) = countAp (succ n) r

parseUpdating :: ValueDef a -> a -> Anchored Aeson.Value -> Result a
parseUpdating v a = parseUpdating1 v (Just a)

parseUpdating1 :: ValueDef a -> Maybe a -> Anchored Aeson.Value -> Result a
parseUpdating1 (SimpleValueDef f _) _ov v = f v
parseUpdating1 (ArrayValueDef (Just (PrimaryKeyExtraction pk_from_object pk_from_json)) m f) (Just ov) (Anchored path v)
  = case Aeson.parseEither Aeson.parseJSON v of
      Right v ->
        sequenceA (zipWith (\v i -> (lookupObjectByJson (Anchored (path ++ [PathElemIndex i]) v)) >>= \ov ->
                                        parseUpdating1 f ov
                                        (Anchored (path ++ [PathElemIndex i]) v))
                                    (Vector.toList v) [0..])
      Left e -> case m of
          ArrayValueModeStrict ->
            resultWithThrow (Anchored path (Text.pack e))
          _ ->
            sequenceA [(lookupObjectByJson (Anchored (path ++ [PathElemIndex 0]) v)) >>= \ov ->
                                        parseUpdating1 f ov
                                        (Anchored (path ++ [PathElemIndex 0]) v)]
  where
    objectMap = Map.fromList (map (\o -> (pk_from_object o, o)) ov)
    lookupObjectByJson js = parseUpdating1 pk_from_json Nothing js >>= \val -> return (Map.lookup val objectMap)

parseUpdating1 (ArrayValueDef _ m f) _ov (Anchored path v)
  = case Aeson.parseEither Aeson.parseJSON v of
      Right v ->
        sequenceA (zipWith (\v i -> parseUpdating1 f Nothing (Anchored (path ++ [PathElemIndex i]) v)) (Vector.toList v) [0..])
      Left e -> case m of
          ArrayValueModeStrict ->
            resultWithThrow (Anchored path (Text.pack e))
          _ ->
            sequenceA [parseUpdating1 f Nothing (Anchored (path ++ [PathElemIndex 0]) v)]

parseUpdating1 (ObjectValueDef f) ov (Anchored path v)
  = case Aeson.parseEither Aeson.parseJSON v of
      Right v ->
        runAp (lookupByFieldDef (Anchored path v) ov) f
      Left e ->
        resultWithThrow (Anchored path (Text.pack e))
parseUpdating1 (TupleValueDef f) ov (Anchored path v)
  = case Aeson.parseEither Aeson.parseJSON v of
      Right v ->
        let r@(Result g h) = runAp (lookupByTupleFieldDef (Anchored path v) ov) f
            tupleSize = countAp 0 f
            arrayLength = Vector.length v
        in if tupleSize == arrayLength
             then r
             else Result g (h ++ [Anchored path ("cannot parse array of length " <> Text.pack (show arrayLength) <>
                                                " into tuple of size " <> Text.pack (show tupleSize))])
      Left e ->
        resultWithThrow (Anchored path (Text.pack e))

parse :: ValueDef a -> Anchored Aeson.Value -> Result a
parse vd = parseUpdating1 vd Nothing


lookupByFieldDef :: Anchored Aeson.Object -> Maybe s -> FieldDef s a -> Result a
lookupByFieldDef (Anchored path v) ov (FieldReqDef name docstring f valuedef)
  = case HashMap.lookup name v of
      Just x  -> parseUpdating1 valuedef (fmap f ov) (Anchored (path ++ [PathElemKey name]) x)
      Nothing -> case ov of
                   Just xov -> Result (f xov) []
                   Nothing -> resultWithThrow (Anchored (path ++ [PathElemKey name]) "missing key")
lookupByFieldDef (Anchored path v) ov (FieldDefDef name docstring def f valuedef)
  = case HashMap.lookup name v of
      Just Aeson.Null -> Result def []
      Just x  -> parseUpdating1 valuedef (fmap f ov) (Anchored (path ++ [PathElemKey name]) x)
      Nothing -> case ov of
                   Just xov -> Result (f xov) []
                   Nothing -> Result def []
lookupByFieldDef (Anchored path v) ov (FieldOptDef name docstring f valuedef)
  = case HashMap.lookup name v of
      Just Aeson.Null -> Result Nothing []
      Just x  -> case ov of
                   Just xov -> fmap Just (parseUpdating1 valuedef (f xov) (Anchored (path ++ [PathElemKey name]) x))
                   Nothing -> fmap Just (parseUpdating1 valuedef Nothing (Anchored (path ++ [PathElemKey name]) x))
      Nothing -> case ov of
                   Just xov -> Result (f xov) []
                   Nothing -> Result Nothing []


lookupByTupleFieldDef :: Anchored Aeson.Array -> Maybe s -> TupleFieldDef s a -> Result a
lookupByTupleFieldDef (Anchored path v) ov (TupleFieldDef idx f valuedef)
  = case v Vector.!? idx of
      Just x  -> parseUpdating1 valuedef (fmap f ov) (Anchored (path ++ [PathElemIndex idx]) x)
      Nothing -> resultWithThrow (Anchored (path ++ [PathElemIndex idx]) "missing key")

-- | Declare a required field with definition given inline by valuedef.
fieldBy :: Text.Text -> (s -> a) -> Text.Text -> ValueDef a -> Ap (FieldDef s) a
fieldBy key f docstring valuedef = liftAp (FieldReqDef key docstring f valuedef)

-- | Declare a required field with definition from 'Unjson' typeclass.
field :: (Unjson a) => Text.Text -> (s -> a) -> Text.Text -> Ap (FieldDef s) a
field key f docstring = fieldBy key f docstring valueDef

-- | Declare a required field of a primitive type.
field' :: (Aeson.FromJSON a,Aeson.ToJSON a) => Text.Text -> (s -> a) -> Text.Text -> Ap (FieldDef s) a
field' key f docstring = fieldBy key f docstring liftAesonFromJSON

-- | Declare an optional field and definition by valuedef.
fieldOptBy :: Text.Text -> (s -> Maybe a) -> Text.Text -> ValueDef a -> Ap (FieldDef s) (Maybe a)
fieldOptBy key f docstring valuedef = liftAp (FieldOptDef key docstring f valuedef)

-- | Declare an optional field and definition by 'Unjson' typeclass.
fieldOpt :: (Unjson a) => Text.Text -> (s -> Maybe a) -> Text.Text -> Ap (FieldDef s) (Maybe a)
fieldOpt key f docstring = fieldOptBy key f docstring valueDef

-- | Declare an optional field of primitive type.
fieldOpt' :: (Aeson.FromJSON a,Aeson.ToJSON a) => Text.Text -> (s -> Maybe a) -> Text.Text -> Ap (FieldDef s) (Maybe a)
fieldOpt' key f docstring = fieldOptBy key f docstring liftAesonFromJSON

-- | Declare a field with default value and definition by valuedef.
fieldDefBy :: Text.Text -> a -> (s -> a) -> Text.Text -> ValueDef a -> Ap (FieldDef s) a
fieldDefBy key def f docstring valuedef = liftAp (FieldDefDef key docstring def f valuedef)

-- | Declare a field with default value and definition by 'Unjson' typeclass.
fieldDef :: (Unjson a) => Text.Text -> a -> (s -> a) -> Text.Text -> Ap (FieldDef s) a
fieldDef key def f docstring = fieldDefBy key def f docstring valueDef

-- | Declate a field with primitive type lifted from Aeson and a default value.
fieldDef' :: (Aeson.FromJSON a,Aeson.ToJSON a) => Text.Text -> a -> (s -> a) -> Text.Text -> Ap (FieldDef s) a
fieldDef' key def f docstring = fieldDefBy key def f docstring liftAesonFromJSON

-- | Declare array of values where each of them is described by valuedef.
arrayOf :: ValueDef a -> ValueDef [a]
arrayOf valuedef = ArrayValueDef Nothing ArrayValueModeStrict valuedef

-- | Declare array of primitive values lifed from 'Aeson'.
arrayOf' :: (Aeson.FromJSON a,Aeson.ToJSON a) => ValueDef [a]
arrayOf' = arrayOf liftAesonFromJSON

-- | Use 'Aeson.fromJSON' and 'Aeson.toJSON' to create a
-- 'ValueDef'. This function is useful when lifted type is one of the
-- primitives. Although it can be used to lift user defined instances,
-- it is not advisable as there is too much information lost in the
-- process and proper error infomation is not possible.
liftAesonFromJSON :: (Aeson.FromJSON a,Aeson.ToJSON a) => ValueDef a
liftAesonFromJSON = SimpleValueDef (\(Anchored path value) ->
                                        case Aeson.fromJSON value of
                                          Aeson.Success result -> Result result []
                                          Aeson.Error message -> resultWithThrow (Anchored path (Text.pack message)))
                                   Aeson.toJSON


-- | Renders documentation for a parser into a multiline string. It is
-- expected that this string is a human readable representation that
-- can go directly to console.
render :: ValueDef a -> String
render = P.render . renderDoc

-- | Renders documentation for a parser into a 'P.Doc'.
renderDoc :: ValueDef a -> P.Doc
renderDoc (SimpleValueDef _ _) = P.empty
renderDoc (ArrayValueDef _ _m f) = P.text "array" P.$+$
             P.nest 4 (renderDoc f)
renderDoc (ObjectValueDef f) = -- P.text "object" P.$+$
             P.nest 4 (P.vcat (renderFields f))
renderDoc (TupleValueDef f) = P.text "tuple of size " P.<> P.int (countAp 0 f) P.$+$
             P.nest 4 (P.vcat (renderTupleFields f))

renderFields :: Ap (FieldDef s) a -> [P.Doc]
renderFields (Pure _) = []
renderFields (Ap (FieldReqDef key docstring _f d) r) =
  (P.text (Text.unpack key) P.<> P.text " (req): " P.$+$ P.nest 4 (P.text (Text.unpack docstring) P.$+$ renderDoc d))
    : renderFields r
renderFields (Ap (FieldOptDef key docstring _f d) r) =
  (P.text (Text.unpack key) P.<> P.text " (opt): " P.$+$ P.nest 4 (P.text (Text.unpack docstring) P.$+$ renderDoc d))
    : renderFields r
renderFields (Ap (FieldDefDef key docstring _f _ d) r) =
  (P.text (Text.unpack key) P.<> P.text " (def): " P.$+$ P.nest 4 (P.text (Text.unpack docstring) P.$+$ renderDoc d))
   : renderFields r

renderTupleFields :: Ap (TupleFieldDef s) a -> [P.Doc]
renderTupleFields (Pure _) = []
renderTupleFields (Ap (TupleFieldDef index _f d) r) =
  (if P.isEmpty s
     then P.empty
     else (P.int index P.<> P.text ": " P.$+$ s))
    : renderTupleFields r
  where
    s = renderDoc d
