-- | @Unjson@: bidirectional JSON (de)serialization with strong error
-- reporting capabilities and automatica documentation generation.
--
-- @Data.Unjson@ offers:
--
-- * single definition for serialization and deserialization
--
-- * parse and update mode
--
-- * exact error reporting
--
-- * required, optional and fields with default values
--
-- * first class object, array and tuple support
--
-- * lifting of Aeson instances
--
-- * automatic documentation generation
--
-- For examples have a look at 'Unjson', 'parse', 'update' and
-- 'render'.
module Data.Unjson
( Unjson(..)
, ValueDef
, Problem
, Problems
, Path
, PathElem(..)
, ArrayValueMode(..)
, serialize
, objectOf
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
, arrayWithModeOf
, arrayOf'
, arrayWithModeOf'
, arrayWithPrimaryKeyOf
, arrayWithModeAndPrimaryKeyOf
, render
, renderDoc
, liftAesonFromJSON
, Result(..)
, Anchored(..)
, parse
, update
)
where

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
-- those containers: 'Int' and 'Text.Text'. See 'Path'.
data PathElem = PathElemKey Text.Text
              | PathElemIndex Int
  deriving (Typeable, Eq, Ord, Show)

-- | 'Path's are rendered in a nice way. For example: @key.key2[34]@
-- indexes into \"key\", then into \"key2\" then into index 34 of an
-- array.
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
-- Example declaration:
--
-- > instance Unjson Thing where
-- >     valueDef = objectOf $ pure Thing
-- >         <*> field "key1"
-- >               thingField1
-- >               "Required field of type with Unjson instance"
-- >         <*> fieldBy "key2"
-- >               thingField2
-- >               "Required field with parser given below"
-- >               unjsonForKey2
-- >         <*> field' "key3"
-- >               thingField3
-- >               "Required field of type with (ToJSON,FromjSON) instances"
-- >         <*> fieldOpt "key4"
-- >               thingField4
-- >               "Optional field of type with Unjson instance"
-- >         <*> fieldOptBy "key5"
-- >               thingField5
-- >               "Optional field with parser given below"
-- >               unjsonForKey5
-- >         <*> fieldOpt' "key6"
-- >               thingField6
-- >               "Optional field of type with (ToJSON,FromjSON) instances"
-- >         <*> fieldDef "key7"
-- >               thingField7
-- >               "Optional field with default of type with Unjson instance"
-- >         <*> fieldDefBy "key8"
-- >               thingField8
-- >               "Optional field with default with parser given below"
-- >               unjsonForKey8
-- >         <*> fieldDef' "key9"
-- >               thingField9
-- >               "Optional field with default of type with (ToJSON,FromjSON) instances"
class Unjson a where
  -- | Definition of a bidirectional parser for a type 'a'. See
  -- 'parse, 'update', 'render' to see how to use it.
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

-- | 'PrimaryKeyExtraction' is needed to keep 'Ord pk' constraint
-- attached. Elements of array may be matched based on a primary
-- key. A primary key has to be extracted both from existing array
-- structure and from JSON array elements. Then a 'Set' is constructed
-- so that lookups are efficient. Then for each element in JSON a
-- corresponding element in old object is looked for. If found the
-- element is updated, if not found it is parsed fresh.
data PrimaryKeyExtraction k = forall pk . (Ord pk) => PrimaryKeyExtraction (k -> pk) (ValueDef pk)

-- | Opaque 'ValueDef' defines a bidirectional JSON parser.
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
tupleDefToArray s (Ap (TupleFieldDef _ f d) r) =  (serialize d (f s)) : tupleDefToArray s r


objectDefToArray :: s -> Ap (FieldDef s) a -> [(Text.Text,Aeson.Value)]
objectDefToArray _ (Pure _) = []
objectDefToArray s (Ap (FieldReqDef key _ f d) r) = (key,serialize d (f s)) : objectDefToArray s r
objectDefToArray s (Ap (FieldOptDef key _ f d) r) =
  case f s of
    Nothing -> objectDefToArray s r
    Just g ->  (key,serialize d g) : objectDefToArray s r
objectDefToArray s (Ap (FieldDefDef key _ _ f d) r) = (key,serialize d (f s)) : objectDefToArray s r

-- | Given a definition of a value and a value produce a JSON.
serialize :: ValueDef a -> a -> Aeson.Value
serialize (SimpleValueDef _ g) a = g a
serialize (ArrayValueDef _ ArrayValueModeParseAndOutputSingle f) [a] =
  serialize f a
serialize (ArrayValueDef _ _m f) a =              -- here compiler should know that 'a' is a list
  Aeson.toJSON (map (serialize f) a)
serialize (ObjectValueDef f) a =
  Aeson.object (objectDefToArray a f)
serialize (TupleValueDef f) a =
  Aeson.toJSON (tupleDefToArray a f)

-- | Count how many applications there are. Useful for error
-- reporting.
countAp :: Int -> Ap x a -> Int
countAp !n (Pure _) = n
countAp n (Ap _ r) = countAp (succ n) r

parseUpdating :: ValueDef a -> Maybe a -> Anchored Aeson.Value -> Result a
parseUpdating (SimpleValueDef f _) _ov v = f v
parseUpdating (ArrayValueDef (Just (PrimaryKeyExtraction pk_from_object pk_from_json)) m f) (Just ov) (Anchored path v)
  = case Aeson.parseEither Aeson.parseJSON v of
      Right v ->
        sequenceA (zipWith (\v i -> (lookupObjectByJson (Anchored (path ++ [PathElemIndex i]) v)) >>= \ov ->
                                        parseUpdating f ov
                                        (Anchored (path ++ [PathElemIndex i]) v))
                                    (Vector.toList v) [0..])
      Left e -> case m of
          ArrayValueModeStrict ->
            resultWithThrow (Anchored path (Text.pack e))
          _ ->
            sequenceA [(lookupObjectByJson (Anchored (path ++ [PathElemIndex 0]) v)) >>= \ov ->
                                        parseUpdating f ov
                                        (Anchored (path ++ [PathElemIndex 0]) v)]
  where
    objectMap = Map.fromList (map (\o -> (pk_from_object o, o)) ov)
    lookupObjectByJson js = parseUpdating pk_from_json Nothing js >>= \val -> return (Map.lookup val objectMap)

parseUpdating (ArrayValueDef _ m f) _ov (Anchored path v)
  = case Aeson.parseEither Aeson.parseJSON v of
      Right v ->
        sequenceA (zipWith (\v i -> parseUpdating f Nothing (Anchored (path ++ [PathElemIndex i]) v)) (Vector.toList v) [0..])
      Left e -> case m of
          ArrayValueModeStrict ->
            resultWithThrow (Anchored path (Text.pack e))
          _ ->
            sequenceA [parseUpdating f Nothing (Anchored (path ++ [PathElemIndex 0]) v)]

parseUpdating (ObjectValueDef f) ov (Anchored path v)
  = case Aeson.parseEither Aeson.parseJSON v of
      Right v ->
        runAp (lookupByFieldDef (Anchored path v) ov) f
      Left e ->
        resultWithThrow (Anchored path (Text.pack e))
parseUpdating (TupleValueDef f) ov (Anchored path v)
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

-- | Parse JSON according to unjson definition.
--
-- Example:
--
-- > let json = Aeson.object [ ... ]
-- > let Result val iss = parse unjsonThing (Anchored [] json)
-- > if null iss
-- >   then putStrLn ("Parsed: " ++ show val)
-- >   else putStrLn ("Not parsed, issues: " ++ show iss)
--
-- For parsing of fields the following rules apply:
--
-- - required fields generate an error if json key is missing
--
-- - for optional fields Nothing is returned if json key is missing,
-- Just value otherwise
--
-- - for fields with default value, the default value is returned if
-- key is missing, otherwise the parsed value is returned
--
-- Note that Unjson makes strong difference between missing keys and
-- values that result in parse errors.
--
-- For discussion of update mode see 'update'.
parse :: ValueDef a -> Anchored Aeson.Value -> Result a
parse vd = parseUpdating vd Nothing

-- | Update object with JSON according to unjson definition.
--
-- Example:
--
-- > let original = Thing { ... }
-- > let json = Aeson.object [ ... ]
-- > let Result val iss = update original unjsonThing (Anchored [] json)
-- > if null iss
-- >   then putStrLn ("Updated: " ++ show val)
-- >   else putStrLn ("Not updated, issues: " ++ show iss)
--
-- For updating of fields the following rules apply:
--
-- - required fields take the original value if json key is missing
--
-- - optional fields take the original value if json key is missing
-- unless the value is @null@, then Nothing is returned (reset to
-- Nothing)
--
-- - fields with default value take the original value if json key is
-- missing unless the value is @null@, then the default value is
-- returned (reset to default)
--
-- Note that Unjson makes strong difference between missing keys and
-- values that result in parse errors.
--
-- For discussion of parse mode see 'parse'.
update :: a -> ValueDef a -> Anchored Aeson.Value -> Result a
update a vd = parseUpdating vd (Just a)

lookupByFieldDef :: Anchored Aeson.Object -> Maybe s -> FieldDef s a -> Result a
lookupByFieldDef (Anchored path v) ov (FieldReqDef name docstring f valuedef)
  = case HashMap.lookup name v of
      Just x  -> parseUpdating valuedef (fmap f ov) (Anchored (path ++ [PathElemKey name]) x)
      Nothing -> case ov of
                   Just xov -> Result (f xov) []
                   Nothing -> resultWithThrow (Anchored (path ++ [PathElemKey name]) "missing key")
lookupByFieldDef (Anchored path v) ov (FieldDefDef name docstring def f valuedef)
  = case HashMap.lookup name v of
      Just Aeson.Null -> Result def []
      Just x  -> parseUpdating valuedef (fmap f ov) (Anchored (path ++ [PathElemKey name]) x)
      Nothing -> case ov of
                   Just xov -> Result (f xov) []
                   Nothing -> Result def []
lookupByFieldDef (Anchored path v) ov (FieldOptDef name docstring f valuedef)
  = case HashMap.lookup name v of
      Just Aeson.Null -> Result Nothing []
      Just x  -> case ov of
                   Just xov -> fmap Just (parseUpdating valuedef (f xov) (Anchored (path ++ [PathElemKey name]) x))
                   Nothing -> fmap Just (parseUpdating valuedef Nothing (Anchored (path ++ [PathElemKey name]) x))
      Nothing -> case ov of
                   Just xov -> Result (f xov) []
                   Nothing -> Result Nothing []


lookupByTupleFieldDef :: Anchored Aeson.Array -> Maybe s -> TupleFieldDef s a -> Result a
lookupByTupleFieldDef (Anchored path v) ov (TupleFieldDef idx f valuedef)
  = case v Vector.!? idx of
      Just x  -> parseUpdating valuedef (fmap f ov) (Anchored (path ++ [PathElemIndex idx]) x)
      Nothing -> resultWithThrow (Anchored (path ++ [PathElemIndex idx]) "missing key")

-- | Declare a required field with definition given inline by valuedef.
--
-- Example:
--
-- > unjsonThing :: ValueDef Thing
-- > unjsonThing = objectOf $ pure Thing
-- >    <*> fieldBy "credentials"
-- >          thingCredentials
-- >          "Credentials to use"
-- >          unjsonCredentials
-- >
-- > data Thing = Thing { thingCredentials :: Credentials, ... }
-- > unjsonCredentials :: ValueDef Credentials
fieldBy :: Text.Text -> (s -> a) -> Text.Text -> ValueDef a -> Ap (FieldDef s) a
fieldBy key f docstring valuedef = liftAp (FieldReqDef key docstring f valuedef)

-- | Declare a required field with definition from 'Unjson' typeclass.
--
-- Example:
--
-- > unjsonThing :: ValueDef Thing
-- > unjsonThing = objectOf $ pure Thing
-- >    <*> field "credentials"
-- >          thingCredentials
-- >          "Credentials to use"
-- >
-- > data Thing = Thing { thingCredentials :: Credentials, ... }
-- > instance Unjson Credentials where ...
field :: (Unjson a) => Text.Text -> (s -> a) -> Text.Text -> Ap (FieldDef s) a
field key f docstring = fieldBy key f docstring valueDef

-- | Declare a required field of a primitive type.
--
-- Example:
--
-- > unjsonThing :: ValueDef Thing
-- > unjsonThing = objectOf $ pure Thing
-- >    <*> field' "port"
-- >          thingPort
-- >          "Port to listen on"
-- >
-- > data Thing = Thing { thingPort :: Int, ... }
field' :: (Aeson.FromJSON a,Aeson.ToJSON a) => Text.Text -> (s -> a) -> Text.Text -> Ap (FieldDef s) a
field' key f docstring = fieldBy key f docstring liftAesonFromJSON

-- | Declare an optional field and definition by valuedef.
--
-- Example:
--
-- > unjsonThing :: ValueDef Thing
-- > unjsonThing = objectOf $ pure Thing
-- >    <*> fieldOptBy "credentials"
-- >          thingCredentials
-- >          "Optional credentials to use"
-- >          unjsonCredentials
-- >
-- > data Thing = Thing { thingCredentials :: Credentials, ... }
-- > unjsonCredentials :: ValueDef Credentials
fieldOptBy :: Text.Text -> (s -> Maybe a) -> Text.Text -> ValueDef a -> Ap (FieldDef s) (Maybe a)
fieldOptBy key f docstring valuedef = liftAp (FieldOptDef key docstring f valuedef)

-- | Declare an optional field and definition by 'Unjson' typeclass.
--
-- Example:
--
-- > unjsonThing :: ValueDef Thing
-- > unjsonThing = objectOf $ pure Thing
-- >    <*> fieldOpt "credentials"
-- >          thingCredentials
-- >          "Optional credentials to use"
-- >
-- > data Thing = Thing { thingCredentials :: Credentials, ... }
-- > instance Unjson Credentials where ...
fieldOpt :: (Unjson a) => Text.Text -> (s -> Maybe a) -> Text.Text -> Ap (FieldDef s) (Maybe a)
fieldOpt key f docstring = fieldOptBy key f docstring valueDef

-- | Declare an optional field of primitive type.
--
-- Example:
--
-- > unjsonThing :: ValueDef Thing
-- > unjsonThing = objectOf $ pure Thing
-- >    <*> fieldDef' "port"
-- >          thingPort
-- >          "Optional port to listen on"
-- >
-- > data Thing = Thing { thingPort :: Int, ... }
fieldOpt' :: (Aeson.FromJSON a,Aeson.ToJSON a) => Text.Text -> (s -> Maybe a) -> Text.Text -> Ap (FieldDef s) (Maybe a)
fieldOpt' key f docstring = fieldOptBy key f docstring liftAesonFromJSON

-- | Declare a field with default value and definition by valuedef.
--
-- Example:
--
-- > unjsonThing :: ValueDef Thing
-- > unjsonThing = objectOf $ pure Thing
-- >    <*> fieldDefBy "credentials" defaultCredentials
-- >          thingCredentials
-- >          "Credentials to use, defaults to defaultCredentials"
-- >          unjsonCredentials
-- >
-- > data Thing = Thing { thingCredentials :: Credentials, ... }
-- > unjsonCredentials :: ValueDef Credentials
fieldDefBy :: Text.Text -> a -> (s -> a) -> Text.Text -> ValueDef a -> Ap (FieldDef s) a
fieldDefBy key def f docstring valuedef = liftAp (FieldDefDef key docstring def f valuedef)

-- | Declare a field with default value and definition by 'Unjson' typeclass.
--
-- Example:
--
-- > unjsonThing :: ValueDef Thing
-- > unjsonThing = objectOf $ pure Thing
-- >    <*> fieldDef "credentials" defaultCredentials
-- >          thingCredentials
-- >          "Credentials to use, defaults to defaultCredentials"
-- >
-- > data Thing = Thing { thingCredentials :: Credentials, ... }
-- > instance Unjson Credentials where ...
fieldDef :: (Unjson a) => Text.Text -> a -> (s -> a) -> Text.Text -> Ap (FieldDef s) a
fieldDef key def f docstring = fieldDefBy key def f docstring valueDef

-- | Declate a field with primitive type lifted from Aeson and a default value.
--
-- Example:
--
-- > unjsonThing :: ValueDef Thing
-- > unjsonThing = objectOf $ pure Thing
-- >    <*> fieldDef' "port" 80
-- >          thingPort
-- >          "Port to listen on, defaults to 80"
-- >
-- > data Thing = Thing { thingPort :: Int, ... }
fieldDef' :: (Aeson.FromJSON a,Aeson.ToJSON a) => Text.Text -> a -> (s -> a) -> Text.Text -> Ap (FieldDef s) a
fieldDef' key def f docstring = fieldDefBy key def f docstring liftAesonFromJSON

-- | Declare an object as bidirectional mapping from JSON object to Haskell record and back.
--
-- Example:
--
-- > unjsonThing :: ValueDef Thing
-- > unjsonThing = objectOf $ pure Thing
-- >    ...field definitions go here
--
-- Use field functions to specify fields of an object: 'field',
-- 'field'', 'fieldBy', 'fieldOpt', 'fieldOpt'', 'fieldOptBy',
-- 'fieldDef', 'fieldDef'' or 'fieldDefBy'.
objectOf :: Ap (FieldDef a) a -> ValueDef a
objectOf fields = ObjectValueDef fields

-- | Declare array of values where each of them is described by valuedef.
--
-- Example:
--
-- > unjsonArrayOfThings :: ValueDef [Thing]
-- > unjsonArrayOfThings = arrayOf unjsonThing
-- >
-- > unjsonThing :: ValueDef Thing
-- > unjsonThing = ...
arrayOf :: ValueDef a -> ValueDef [a]
arrayOf = arrayWithModeOf ArrayValueModeStrict

-- | Declare array of values where each of them is described by
-- valuedef. Accepts mode specifier.
--
-- Example:
--
-- > unjsonArrayOfThings :: ValueDef [Thing]
-- > unjsonArrayOfThings = arrayOf unjsonThing
-- >
-- > unjsonThing :: ValueDef Thing
-- > unjsonThing = ...
arrayWithModeOf :: ArrayValueMode -> ValueDef a -> ValueDef [a]
arrayWithModeOf mode valuedef = ArrayValueDef Nothing mode valuedef

-- | Declare array of primitive values lifed from 'Aeson'.
--
-- Example:
--
-- > unjsonArrayOfInt :: ValueDef [Int]
-- > unjsonArrayOfInt = arrayOf'
arrayOf' :: (Aeson.FromJSON a,Aeson.ToJSON a) => ValueDef [a]
arrayOf' = arrayOf liftAesonFromJSON

-- | Declare array of primitive values lifed from 'Aeson'. Accepts
-- mode specifier.
--
-- Example:
--
-- > unjsonArrayOfIntOrSimpleInt :: ValueDef [Int]
-- > unjsonArrayOfIntOrSimpleInt = arrayWithModeOf'
arrayWithModeOf' :: (Aeson.FromJSON a,Aeson.ToJSON a)
                 => ArrayValueMode
                 -> ValueDef [a]
arrayWithModeOf' mode = arrayWithModeOf mode liftAesonFromJSON


-- | Declare array pf objects with given parsers that should be
-- matched by a primary key. Accepts mode specifier.
--
-- Example:
--
-- > unjsonArrayOfIntOrSimpleInt :: ValueDef [Int]
-- > unjsonArrayOfIntOrSimpleInt = arrayWithModeOf'
arrayWithModeAndPrimaryKeyOf :: (Ord pk)
                             => ArrayValueMode
                             -> (a -> pk)
                             -> ValueDef pk
                             -> ValueDef a
                             -> ValueDef [a]
arrayWithModeAndPrimaryKeyOf mode pk1 pk2 valuedef =
  ArrayValueDef (Just (PrimaryKeyExtraction pk1 pk2)) mode valuedef

-- | Declare array pf objects with given parsers that should be
-- matched by a primary key. Accepts mode specifier.
--
-- Example:
--
-- > unjsonArrayOfIntOrSimpleInt :: ValueDef [Int]
-- > unjsonArrayOfIntOrSimpleInt = arrayWithModeOf'
arrayWithPrimaryKeyOf :: (Ord pk)
                      => (a -> pk)
                      -> ValueDef pk
                      -> ValueDef a
                      -> ValueDef [a]
arrayWithPrimaryKeyOf pk1 pk2 valuedef =
  arrayWithModeAndPrimaryKeyOf ArrayValueModeStrict pk1 pk2 valuedef

-- | Use 'Aeson.fromJSON' and 'Aeson.toJSON' to create a
-- 'ValueDef'. This function is useful when lifted type is one of the
-- primitives. Although it can be used to lift user defined instances,
-- it is not advisable as there is too much information lost in the
-- process and proper error infomation is not possible.
--
-- Example:
--
-- > instance Unjson MyType where
-- >     valueDef = liftAesonFromJSON
liftAesonFromJSON :: (Aeson.FromJSON a,Aeson.ToJSON a) => ValueDef a
liftAesonFromJSON = SimpleValueDef (\(Anchored path value) ->
                                        case Aeson.fromJSON value of
                                          Aeson.Success result -> Result result []
                                          Aeson.Error message -> resultWithThrow (Anchored path (Text.pack message)))
                                   Aeson.toJSON


-- | Renders documentation for a parser into a multiline string. It is
-- expected that this string is a human readable representation that
-- can go directly to console.
--
-- Example rendering:
--
-- >    hostname (req):
-- >        The hostname this service is visible as
-- >    port (def):
-- >        Port to listen on
-- >    credentials (req):
-- >        User admin credentials
-- >            username (req):
-- >                Name of the user
-- >            password (req):
-- >                Password for the user
-- >            domain (opt):
-- >                Domain for user credentials
-- >    comment (opt):
-- >        Optional comment, free text
-- >    alternates (opt):
-- >        Alternate names for this server
-- >        tuple of size 2
-- >            1:
-- >                username (req):
-- >                    Name of the user
-- >                password (req):
-- >                    Password for the user
-- >                domain (opt):
-- >                    Domain for user credentials
-- >
render :: ValueDef a -> String
render = P.render . renderDoc

-- | Renders documentation for a parser into a 'P.Doc'. See 'render'
-- for example.
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
