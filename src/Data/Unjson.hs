module Data.Unjson
where


{-

* need to support the 'update' mode with combinig function
* need to support the 'update' mode with null as reset to default functionality
* write more documentation

* need to support bidirectional, it is very annoying to try to write it both ways
* looks like bidirectional could provide for update...
* create useful export list
* can have item-or-array mode: make an array out of singular items, this could be automatic provided that nested value def is not array def... might be confusing though


-}

import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Types as Aeson
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

{-
instance (Unjson a,Unjson b,Unjson c,Unjson d
         ,Unjson e) => Unjson (a,b,c,d
                              ,e) where
  valueDef = TupleValueDef
               $ pure (,,,,)
               <*> liftAp (TupleFieldDef 0 valueDef)
               <*> liftAp (TupleFieldDef 1 valueDef)
               <*> liftAp (TupleFieldDef 2 valueDef)
               <*> liftAp (TupleFieldDef 3 valueDef)
               <*> liftAp (TupleFieldDef 4 valueDef)

instance (Unjson a,Unjson b,Unjson c,Unjson d
         ,Unjson e,Unjson f)
       => Unjson (a,b,c,d
                 ,e,f) where
  valueDef = TupleValueDef
               $ pure (,,,,,)
               <*> liftAp (TupleFieldDef 0 valueDef)
               <*> liftAp (TupleFieldDef 1 valueDef)
               <*> liftAp (TupleFieldDef 2 valueDef)
               <*> liftAp (TupleFieldDef 3 valueDef)
               <*> liftAp (TupleFieldDef 4 valueDef)
               <*> liftAp (TupleFieldDef 5 valueDef)

instance (Unjson a,Unjson b,Unjson c,Unjson d
         ,Unjson e,Unjson f,Unjson g)
       => Unjson (a,b,c,d
                 ,e,f,g) where
  valueDef = TupleValueDef
               $ pure (,,,,,,)
               <*> liftAp (TupleFieldDef 0 valueDef)
               <*> liftAp (TupleFieldDef 1 valueDef)
               <*> liftAp (TupleFieldDef 2 valueDef)
               <*> liftAp (TupleFieldDef 3 valueDef)
               <*> liftAp (TupleFieldDef 4 valueDef)
               <*> liftAp (TupleFieldDef 5 valueDef)
               <*> liftAp (TupleFieldDef 6 valueDef)

instance (Unjson a,Unjson b,Unjson c,Unjson d
         ,Unjson e,Unjson f,Unjson g,Unjson h)
       => Unjson (a,b,c,d
                 ,e,f,g,h) where
  valueDef = TupleValueDef
               $ pure (,,,,,,,)
               <*> liftAp (TupleFieldDef 0 valueDef)
               <*> liftAp (TupleFieldDef 1 valueDef)
               <*> liftAp (TupleFieldDef 2 valueDef)
               <*> liftAp (TupleFieldDef 3 valueDef)
               <*> liftAp (TupleFieldDef 4 valueDef)
               <*> liftAp (TupleFieldDef 5 valueDef)
               <*> liftAp (TupleFieldDef 6 valueDef)
               <*> liftAp (TupleFieldDef 7 valueDef)

instance (Unjson a,Unjson b,Unjson c,Unjson d
         ,Unjson e,Unjson f,Unjson g,Unjson h
         ,Unjson i)
       => Unjson (a,b,c,d
                 ,e,f,g,h
                 ,i) where
  valueDef = TupleValueDef
               $ pure (,,,,,,,,)
               <*> liftAp (TupleFieldDef 0 valueDef)
               <*> liftAp (TupleFieldDef 1 valueDef)
               <*> liftAp (TupleFieldDef 2 valueDef)
               <*> liftAp (TupleFieldDef 3 valueDef)
               <*> liftAp (TupleFieldDef 4 valueDef)
               <*> liftAp (TupleFieldDef 5 valueDef)
               <*> liftAp (TupleFieldDef 6 valueDef)
               <*> liftAp (TupleFieldDef 7 valueDef)
               <*> liftAp (TupleFieldDef 8 valueDef)

instance (Unjson a,Unjson b,Unjson c,Unjson d
         ,Unjson e,Unjson f,Unjson g,Unjson h
         ,Unjson i,Unjson j)
       => Unjson (a,b,c,d
                 ,e,f,g,h
                 ,i,j) where
  valueDef = TupleValueDef
               $ pure (,,,,,,,,,)
               <*> liftAp (TupleFieldDef 0 valueDef)
               <*> liftAp (TupleFieldDef 1 valueDef)
               <*> liftAp (TupleFieldDef 2 valueDef)
               <*> liftAp (TupleFieldDef 3 valueDef)
               <*> liftAp (TupleFieldDef 4 valueDef)
               <*> liftAp (TupleFieldDef 5 valueDef)
               <*> liftAp (TupleFieldDef 6 valueDef)
               <*> liftAp (TupleFieldDef 7 valueDef)
               <*> liftAp (TupleFieldDef 8 valueDef)
               <*> liftAp (TupleFieldDef 9 valueDef)

instance (Unjson a,Unjson b,Unjson c,Unjson d
         ,Unjson e,Unjson f,Unjson g,Unjson h
         ,Unjson i,Unjson j,Unjson k)
       => Unjson (a,b,c,d
                 ,e,f,g,h
                 ,i,j,k) where
  valueDef = TupleValueDef
               $ pure (,,,,,,,,,,)
               <*> liftAp (TupleFieldDef 0 valueDef)
               <*> liftAp (TupleFieldDef 1 valueDef)
               <*> liftAp (TupleFieldDef 2 valueDef)
               <*> liftAp (TupleFieldDef 3 valueDef)
               <*> liftAp (TupleFieldDef 4 valueDef)
               <*> liftAp (TupleFieldDef 5 valueDef)
               <*> liftAp (TupleFieldDef 6 valueDef)
               <*> liftAp (TupleFieldDef 7 valueDef)
               <*> liftAp (TupleFieldDef 8 valueDef)
               <*> liftAp (TupleFieldDef 9 valueDef)
               <*> liftAp (TupleFieldDef 10 valueDef)

instance (Unjson a,Unjson b,Unjson c,Unjson d
         ,Unjson e,Unjson f,Unjson g,Unjson h
         ,Unjson i,Unjson j,Unjson k,Unjson l)
       => Unjson (a,b,c,d
                 ,e,f,g,h
                 ,i,j,k,l) where
  valueDef = TupleValueDef
               $ pure (,,,,,,,,,,,)
               <*> liftAp (TupleFieldDef 0 valueDef)
               <*> liftAp (TupleFieldDef 1 valueDef)
               <*> liftAp (TupleFieldDef 2 valueDef)
               <*> liftAp (TupleFieldDef 3 valueDef)
               <*> liftAp (TupleFieldDef 4 valueDef)
               <*> liftAp (TupleFieldDef 5 valueDef)
               <*> liftAp (TupleFieldDef 6 valueDef)
               <*> liftAp (TupleFieldDef 7 valueDef)
               <*> liftAp (TupleFieldDef 8 valueDef)
               <*> liftAp (TupleFieldDef 9 valueDef)
               <*> liftAp (TupleFieldDef 10 valueDef)
               <*> liftAp (TupleFieldDef 11 valueDef)
-}

data ValueDef a where
  SimpleValueDef :: (Anchored Aeson.Value -> Result k) -> (k -> Aeson.Value) -> ValueDef k
  ArrayValueDef  :: ValueDef k -> ValueDef [k]
  ObjectValueDef :: Ap (FieldDef k) k -> ValueDef k
  TupleValueDef  :: Ap (TupleFieldDef k) k -> ValueDef k

data FieldDef s a where
  FieldReqDef :: Text.Text -> Text.Text -> (s -> a) -> ValueDef a -> FieldDef s a
  FieldOptDef :: Text.Text -> Text.Text -> (s -> Maybe a) -> ValueDef a -> FieldDef s (Maybe a)
  FieldDefDef :: Text.Text -> Text.Text -> a -> (s -> a) -> ValueDef a -> FieldDef s a

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

serialize1 :: ValueDef a -> a -> Aeson.Value
serialize1 (SimpleValueDef _ g) a = g a
serialize1 (ArrayValueDef f) a =              -- here compiler should know that 'a' is a list
  Aeson.toJSON (map (serialize1 f) a)
serialize1 (ObjectValueDef f) a =
  Aeson.object (objectDefToArray a f)
serialize1 (TupleValueDef f) a =
  Aeson.toJSON (tupleDefToArray a f)

countAp :: Int -> Ap x a -> Int
countAp !n (Pure _) = n
countAp n (Ap _ r) = countAp (succ n) r

parse :: ValueDef a -> Anchored Aeson.Value -> Result a
parse = parse1

parse1 :: ValueDef a -> Anchored Aeson.Value -> Result a
parse1 (SimpleValueDef f _) v = f v
parse1 (ArrayValueDef f) (Anchored path v)
  = case Aeson.parseEither Aeson.parseJSON v of
      Right v ->
        sequenceA (zipWith (\v i -> parse1 f (Anchored (path ++ [PathElemIndex i]) v)) (Vector.toList v) [0..])
      Left e ->
        resultWithThrow (Anchored path (Text.pack e))
parse1 (ObjectValueDef f) (Anchored path v)
  = case Aeson.parseEither Aeson.parseJSON v of
      Right v ->
        runAp (lookupByFieldDef (Anchored path v)) f
      Left e ->
        resultWithThrow (Anchored path (Text.pack e))
parse1 (TupleValueDef f) (Anchored path v)
  = case Aeson.parseEither Aeson.parseJSON v of
      Right v ->
        let r@(Result g h) = runAp (lookupByTupleFieldDef (Anchored path v)) f
            tupleSize = countAp 0 f
            arrayLength = Vector.length v
        in if tupleSize == arrayLength
             then r
             else Result g (h ++ [Anchored path ("cannot parse array of length " <> Text.pack (show arrayLength) <>
                                                " into tuple of size " <> Text.pack (show tupleSize))])
      Left e ->
        resultWithThrow (Anchored path (Text.pack e))

lookupByFieldDef :: Anchored Aeson.Object -> FieldDef s a -> Result a
lookupByFieldDef (Anchored path v) (FieldReqDef name docstring _ valuedef)
  = case HashMap.lookup name v of
      Just x  -> parse valuedef (Anchored (path ++ [PathElemKey name]) x)
      Nothing -> resultWithThrow (Anchored (path ++ [PathElemKey name]) "missing key")
lookupByFieldDef (Anchored path v) (FieldDefDef name docstring def _ valuedef)
  = case HashMap.lookup name v of
      Just x  -> parse valuedef (Anchored (path ++ [PathElemKey name]) x)
      Nothing -> Result def []
lookupByFieldDef (Anchored path v) (FieldOptDef name docstring _ valuedef)
  = case HashMap.lookup name v of
      Just x  -> fmap Just (parse valuedef (Anchored (path ++ [PathElemKey name]) x))
      Nothing -> Result Nothing []

lookupByTupleFieldDef :: Anchored Aeson.Array -> TupleFieldDef s a -> Result a
lookupByTupleFieldDef (Anchored path v) (TupleFieldDef idx _ valuedef)
  = case v Vector.!? idx of
      Just x  -> parse valuedef (Anchored (path ++ [PathElemIndex idx]) x)
      Nothing -> resultWithThrow (Anchored (path ++ [PathElemIndex idx]) "missing key")

fieldBy :: Text.Text -> Text.Text -> ValueDef a -> Ap (FieldDef s) a
fieldBy key docstring valuedef = liftAp (FieldReqDef key docstring undefined valuedef)

field :: (Unjson a) => Text.Text -> Text.Text -> Ap (FieldDef s) a
field key docstring = fieldBy key docstring valueDef

field' :: (Aeson.FromJSON a,Aeson.ToJSON a) => Text.Text -> Text.Text -> Ap (FieldDef s) a
field' key docstring = fieldBy key docstring liftAesonFromJSON

fieldOptBy :: Text.Text -> Text.Text -> ValueDef a -> Ap (FieldDef s) (Maybe a)
fieldOptBy key docstring valuedef = liftAp (FieldOptDef key docstring undefined valuedef)

fieldOpt :: (Unjson a) => Text.Text -> Text.Text -> Ap (FieldDef s) (Maybe a)
fieldOpt key docstring = fieldOptBy key docstring valueDef

fieldOpt' :: (Aeson.FromJSON a,Aeson.ToJSON a) => Text.Text -> Text.Text -> Ap (FieldDef s) (Maybe a)
fieldOpt' key docstring = fieldOptBy key docstring liftAesonFromJSON

fieldDefBy :: Text.Text -> a -> Text.Text -> ValueDef a -> Ap (FieldDef s) a
fieldDefBy key def docstring valuedef = liftAp (FieldDefDef key docstring def undefined valuedef)

fieldDef :: (Unjson a) => Text.Text -> a -> Text.Text -> Ap (FieldDef s) a
fieldDef key def docstring = fieldDefBy key def docstring valueDef

fieldDef' :: (Aeson.FromJSON a,Aeson.ToJSON a) => Text.Text -> a -> Text.Text -> Ap (FieldDef s) a
fieldDef' key def docstring = fieldDefBy key def docstring liftAesonFromJSON

arrayOf :: ValueDef a -> ValueDef [a]
arrayOf valuedef = ArrayValueDef valuedef

arrayOf' :: (Aeson.FromJSON a,Aeson.ToJSON a) => ValueDef [a]
arrayOf' = arrayOf liftAesonFromJSON

liftAesonFromJSON :: (Aeson.FromJSON a,Aeson.ToJSON a) => ValueDef a
liftAesonFromJSON = SimpleValueDef (\(Anchored path value) ->
                                        case Aeson.fromJSON value of
                                          Aeson.Success result -> Result result []
                                          Aeson.Error message -> resultWithThrow (Anchored path (Text.pack message)))
                                   Aeson.toJSON
