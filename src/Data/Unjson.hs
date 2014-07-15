{-# LANGUAGE CPP #-}

-- | @Unjson@: bidirectional JSON (de)serialization with strong error
-- reporting capabilities and automatic documentation generation.
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
-- Example:
--
-- > data Example = Example
-- >    { exampleName     :: Text.Text,
-- >      exampleArray    :: [Int],
-- >      exampleOptional :: Maybe Bool }
-- >
-- > unjsonExample :: UnjsonDef Example
-- > unjsonExample = objectOf $ pure Example
-- >   <*> field "name"
-- >           exampleName
-- >           "Name used for example"
-- >   <*> fieldDefBy "array_of_ints" []
-- >           exampleArray
-- >           "Array of integers, optional, defaults to empty list"
-- >           arrayOf'
-- >   <*> fieldOpt "optional_bool"
-- >           exampleOptional
-- >           "Optional boolean"
--
-- Rendered documentation:
--
-- > name (req):
-- >     Name used for example
-- >     Text
-- > array_of_ints (def):
-- >     Array of integers, optional, defaults to empty list
-- >     array of:
-- >         Int
-- > optional_bool (opt):
-- >     Optional boolean
-- >     Bool
--
-- Documentation has some colors that could not be reproduced in
-- haddock.
--
-- Parsing:
--
-- > let Result val iss = parse unjsonExample (Anchored mempty $
-- >                                 object [ "name" .= 123,
-- >                                          "array_of_ints" .= [toJSON 123, toJSON "abc"],
-- >                                          "optional_bool" .= True ])
--
-- Error reporting:
--
-- > mapM_ print iss
-- > > name: "when expecting a Text, encountered Number instead"
-- > > array_of_ints[1]: "when expecting a Integral, encountered String instead"
--
-- Partial results:
--
-- > print (exampleOptional val)
-- > > Just True
--
-- Bottom errors in partial results:
--
-- > print (exampleName val)
-- > > "*** Exception: name: "when expecting a Text, encountered Number instead"
--
-- Note: if list of issues is empty then there are not bottoms, guaranteed.
--
-- For more examples have a look at 'Unjson', 'parse', 'update',
-- 'serialize' and 'render'.
module Data.Unjson
( Unjson(..)
, UnjsonDef
, Problem
, Problems
, Path(..)
, PathElem(..)
, ArrayMode(..)
, serialize
, objectOf
, field
, fieldBy
, fieldOpt
, fieldOptBy
, fieldDef
, fieldDefBy
, arrayOf
, arrayWithModeOf
, arrayWithPrimaryKeyOf
, arrayWithModeAndPrimaryKeyOf
, render
, renderForPath
, renderDoc
, renderDocForPath
, unjsonAeson
, unjsonAesonWithDoc
, Result(..)
, Anchored(..)
, parse
, update

, unjsonIPv4AsWord32
)
where

import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Types as Aeson
import qualified Data.Text as Text
import qualified Data.Text.Lazy as LazyText
import qualified Data.Vector as Vector
import qualified Data.Vector.Storable
import qualified Data.Vector.Unboxed
import qualified Data.Vector.Primitive
import qualified Data.Vector.Generic
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.IntMap as IntMap
import qualified Data.IntSet as IntSet
import qualified Data.HashSet as HashSet
import Data.Typeable
import Data.Monoid
import Data.Primitive.Types
import Data.Hashable
import Data.Scientific
import Data.Attoparsec.Number
import Data.Time.LocalTime
import Data.Time.Clock
import Data.Fixed
import Data.Tree
import Foreign.Storable
import Control.Applicative
import Control.Applicative.Free
import qualified Data.HashMap.Strict as HashMap
import qualified Data.HashMap.Lazy as LazyHashMap
import Control.Exception
import Data.Traversable

import Data.Bits
import Data.Word
import Data.Int
import Data.Ratio
import Data.List
import qualified Text.ParserCombinators.ReadP as ReadP
import Data.Char
import Control.Monad

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
newtype Path = Path [PathElem]
  deriving (Eq, Ord, Typeable, Monoid)

instance Show Path where
  show (Path p) = Text.unpack (showPath True p)

showPath :: Bool -> [PathElem] -> Text.Text
showPath _ [] = ""
showPath True (PathElemKey key : rest) = key <> showPath False rest
showPath False (PathElemKey key : rest) = "." <> key <> showPath False rest
showPath _ (PathElemIndex key : rest) = "[" <> Text.pack (show key) <> "]" <> showPath False rest

-- | A value at a specific position in JSON object.
data Anchored a = Anchored Path a
  deriving (Typeable, Functor, Eq, Ord)

instance (Show a) => Show (Anchored a) where
  show (Anchored (Path path) value) = Text.unpack (showPath True path) ++ ": " ++ show value

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
--
-- Examples of list of problems:
--
-- > [Anchored [PathElemKey "credentials",PathElemKey "password"] "missing key",
-- >  Anchored [PathElemKey "tuple"] "cannot parse array of length 3 into tuple of size 4",
-- >  Anchored [PathElemKey "text_array",PathElemIndex 0.PathElemKey "value"]
-- >                                  "when expecting a Text, encountered Boolean instead"]
--
-- conveniently rendered as:
--
-- > "credentials.password": "missing key"
-- > "tuple": "cannot parse array of length 3 into tuple of size 4"
-- > "text_array[0].value": "when expecting a Text, encountered Boolean instead"

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
-- >     unjsonDef = objectOf $ pure Thing
-- >         <*> field "key1"
-- >               thingField1
-- >               "Required field of type with Unjson instance"
-- >         <*> fieldBy "key2"
-- >               thingField2
-- >               "Required field with parser given below"
-- >               unjsonForKey2
-- >         <*> fieldOpt "key4"
-- >               thingField4
-- >               "Optional field of type with Unjson instance"
-- >         <*> fieldOptBy "key5"
-- >               thingField5
-- >               "Optional field with parser given below"
-- >               unjsonForKey5
-- >         <*> fieldDef "key7"
-- >               thingField7
-- >               "Optional field with default of type with Unjson instance"
-- >         <*> fieldDefBy "key8"
-- >               thingField8
-- >               "Optional field with default with parser given below"
-- >               unjsonForKey8
class Unjson a where
  -- | Definition of a bidirectional parser for a type 'a'. See
  -- 'parse', 'update', 'serialize' and 'render' to see how to use
  -- 'UnjsonDef'.
  unjsonDef :: UnjsonDef a

instance (Unjson a) => Unjson [a] where
  unjsonDef = arrayOf unjsonDef

instance Unjson String where
  unjsonDef = unjsonAesonWithDoc "String"

instance Unjson Bool             where unjsonDef = unjsonAeson
instance Unjson Char             where unjsonDef = unjsonAeson
instance Unjson Double           where unjsonDef = unjsonAeson
instance Unjson Float            where unjsonDef = unjsonAeson
instance Unjson Int              where unjsonDef = unjsonAeson
instance Unjson Int8             where unjsonDef = unjsonAeson
instance Unjson Int16            where unjsonDef = unjsonAeson
instance Unjson Int32            where unjsonDef = unjsonAeson
instance Unjson Int64            where unjsonDef = unjsonAeson
instance Unjson Integer          where unjsonDef = unjsonAeson
instance Unjson Word             where unjsonDef = unjsonAeson
instance Unjson Word8            where unjsonDef = unjsonAeson
instance Unjson Word16           where unjsonDef = unjsonAeson
instance Unjson Word32           where unjsonDef = unjsonAeson
instance Unjson Word64           where unjsonDef = unjsonAeson
instance Unjson ()               where unjsonDef = unjsonAeson
instance Unjson Text.Text        where unjsonDef = unjsonAeson
instance Unjson Number           where unjsonDef = unjsonAeson
instance Unjson IntSet.IntSet    where unjsonDef = unjsonAeson
#if MIN_VERSION_aeson(0,7,0)
instance Unjson Scientific       where unjsonDef = unjsonAeson
#endif
instance Unjson LazyText.Text    where unjsonDef = unjsonAeson
instance Unjson ZonedTime        where unjsonDef = unjsonAeson
instance Unjson UTCTime          where unjsonDef = unjsonAeson
instance Unjson Aeson.DotNetTime where unjsonDef = unjsonAeson
instance Unjson Aeson.Value      where unjsonDef = unjsonAeson
instance Unjson (Ratio Integer)  where unjsonDef = unjsonAeson
instance (HasResolution a, Typeable a, Aeson.FromJSON a, Aeson.ToJSON a) => Unjson (Fixed a) where unjsonDef = unjsonAeson
instance Unjson a => Unjson (Dual a)  where unjsonDef = dibimapUnjsonDef Dual getDual unjsonDef
{-

-- these work only when 'Maybe a' and 'a' instances are conflated. we do not want this really, do we?
-- First and Last are Monoids here, not sure if/how Unjson should be a monoid or something
instance Unjson a => Unjson (First a)  where unjsonDef = dibimapUnjsonDef First getFirst unjsonDef
instance Unjson a => Unjson (Last a)  where unjsonDef = unjsonAeson

-- what is this tree instance thing?
instance Unjson v => Unjson (Tree v)  where unjsonDef = unjsonAeson

-- disjoint unions require special setup
instance (Unjson a, Unjson b) => Unjson (Either a b)  where unjsonDef = unjsonAeson
-}

instance Unjson a => Unjson (IntMap.IntMap a)
  where unjsonDef = dibimapUnjsonDef IntMap.fromList IntMap.toList unjsonDef
instance (Ord a, Unjson a) => Unjson (Set.Set a)
  where unjsonDef = dibimapUnjsonDef Set.fromList Set.toList unjsonDef
instance (Eq a, Hashable a, Unjson a) => Unjson (HashSet.HashSet a)
  where unjsonDef = dibimapUnjsonDef HashSet.fromList HashSet.toList unjsonDef
instance Unjson a => Unjson (Vector.Vector a)
  where unjsonDef = dibimapUnjsonDef Vector.fromList Vector.toList unjsonDef
instance (Data.Vector.Generic.Vector Data.Vector.Unboxed.Vector a, Unjson a, Data.Vector.Unboxed.Unbox a) => Unjson (Data.Vector.Unboxed.Vector a)
  where unjsonDef = dibimapUnjsonDef Data.Vector.Unboxed.fromList Data.Vector.Unboxed.toList unjsonDef
instance (Storable a, Unjson a) => Unjson (Data.Vector.Storable.Vector a)
  where unjsonDef = dibimapUnjsonDef Data.Vector.Storable.fromList Data.Vector.Storable.toList unjsonDef
instance (Prim a, Unjson a) => Unjson (Data.Vector.Primitive.Vector a)
  where unjsonDef = dibimapUnjsonDef Data.Vector.Primitive.fromList Data.Vector.Primitive.toList unjsonDef

{-

-- these are no good, seems like map behavior needs special construct

instance Unjson v => Unjson (Map.Map String v)
  where unjsonDef = dibimapUnjsonDef Map.fromList Map.toList unjsonDef
instance Unjson v => Unjson (Map.Map Text.Text v)
  where unjsonDef = dibimapUnjsonDef Map.fromList Map.toList unjsonDef
instance Unjson v => Unjson (Map.Map LazyText.Text v)
  where unjsonDef = dibimapUnjsonDef Map.fromList Map.toList unjsonDef
instance Unjson v => Unjson (HashMap.HashMap String v)
  where unjsonDef = dibimapUnjsonDef HashMap.fromList HashMap.toList unjsonDef
instance Unjson v => Unjson (HashMap.HashMap Text.Text v)
  where unjsonDef = dibimapUnjsonDef HashMap.fromList HashMap.toList unjsonDef
instance Unjson v => Unjson (HashMap.HashMap LazyText.Text v)
  where unjsonDef = dibimapUnjsonDef HashMap.fromList HashMap.toList unjsonDef
-}

instance (Unjson a,Unjson b) => Unjson (a,b) where
  unjsonDef = TupleUnjsonDef
                 $ pure (,)
               <*> liftAp (TupleFieldDef 0 (\(p,_) -> p) unjsonDef)
               <*> liftAp (TupleFieldDef 1 (\(_,p) -> p) unjsonDef)

instance (Unjson a,Unjson b,Unjson c) => Unjson (a,b,c) where
  unjsonDef = TupleUnjsonDef
               $ pure (,,)
               <*> liftAp (TupleFieldDef 0 (\(p,_,_) -> p) unjsonDef)
               <*> liftAp (TupleFieldDef 1 (\(_,p,_) -> p) unjsonDef)
               <*> liftAp (TupleFieldDef 2 (\(_,_,p) -> p) unjsonDef)

instance (Unjson a,Unjson b,Unjson c,Unjson d) => Unjson (a,b,c,d) where
  unjsonDef = TupleUnjsonDef
               $ pure (,,,)
               <*> liftAp (TupleFieldDef 0 (\(p,_,_,_) -> p) unjsonDef)
               <*> liftAp (TupleFieldDef 1 (\(_,p,_,_) -> p) unjsonDef)
               <*> liftAp (TupleFieldDef 2 (\(_,_,p,_) -> p) unjsonDef)
               <*> liftAp (TupleFieldDef 3 (\(_,_,_,p) -> p) unjsonDef)

instance (Unjson a,Unjson b,Unjson c,Unjson d
         ,Unjson e) => Unjson (a,b,c,d
                              ,e) where
  unjsonDef = TupleUnjsonDef
               $ pure (,,,,)
               <*> liftAp (TupleFieldDef 0 (\(p,_,_,_,_) -> p) unjsonDef)
               <*> liftAp (TupleFieldDef 1 (\(_,p,_,_,_) -> p) unjsonDef)
               <*> liftAp (TupleFieldDef 2 (\(_,_,p,_,_) -> p) unjsonDef)
               <*> liftAp (TupleFieldDef 3 (\(_,_,_,p,_) -> p) unjsonDef)
               <*> liftAp (TupleFieldDef 4 (\(_,_,_,_,p) -> p) unjsonDef)

instance (Unjson a,Unjson b,Unjson c,Unjson d
         ,Unjson e,Unjson f)
       => Unjson (a,b,c,d
                 ,e,f) where
  unjsonDef = TupleUnjsonDef
               $ pure (,,,,,)
               <*> liftAp (TupleFieldDef 0 (\(p,_,_,_,_,_) -> p) unjsonDef)
               <*> liftAp (TupleFieldDef 1 (\(_,p,_,_,_,_) -> p) unjsonDef)
               <*> liftAp (TupleFieldDef 2 (\(_,_,p,_,_,_) -> p) unjsonDef)
               <*> liftAp (TupleFieldDef 3 (\(_,_,_,p,_,_) -> p) unjsonDef)
               <*> liftAp (TupleFieldDef 4 (\(_,_,_,_,p,_) -> p) unjsonDef)
               <*> liftAp (TupleFieldDef 5 (\(_,_,_,_,_,p) -> p) unjsonDef)

instance (Unjson a,Unjson b,Unjson c,Unjson d
         ,Unjson e,Unjson f,Unjson g)
       => Unjson (a,b,c,d
                 ,e,f,g) where
  unjsonDef = TupleUnjsonDef
               $ pure (,,,,,,)
               <*> liftAp (TupleFieldDef 0 (\(p,_,_,_,_,_,_) -> p) unjsonDef)
               <*> liftAp (TupleFieldDef 1 (\(_,p,_,_,_,_,_) -> p) unjsonDef)
               <*> liftAp (TupleFieldDef 2 (\(_,_,p,_,_,_,_) -> p) unjsonDef)
               <*> liftAp (TupleFieldDef 3 (\(_,_,_,p,_,_,_) -> p) unjsonDef)
               <*> liftAp (TupleFieldDef 4 (\(_,_,_,_,p,_,_) -> p) unjsonDef)
               <*> liftAp (TupleFieldDef 5 (\(_,_,_,_,_,p,_) -> p) unjsonDef)
               <*> liftAp (TupleFieldDef 6 (\(_,_,_,_,_,_,p) -> p) unjsonDef)

instance (Unjson a,Unjson b,Unjson c,Unjson d
         ,Unjson e,Unjson f,Unjson g,Unjson h)
       => Unjson (a,b,c,d
                 ,e,f,g,h) where
  unjsonDef = TupleUnjsonDef
               $ pure (,,,,,,,)
               <*> liftAp (TupleFieldDef 0 (\(p,_,_,_,_,_,_,_) -> p) unjsonDef)
               <*> liftAp (TupleFieldDef 1 (\(_,p,_,_,_,_,_,_) -> p) unjsonDef)
               <*> liftAp (TupleFieldDef 2 (\(_,_,p,_,_,_,_,_) -> p) unjsonDef)
               <*> liftAp (TupleFieldDef 3 (\(_,_,_,p,_,_,_,_) -> p) unjsonDef)
               <*> liftAp (TupleFieldDef 4 (\(_,_,_,_,p,_,_,_) -> p) unjsonDef)
               <*> liftAp (TupleFieldDef 5 (\(_,_,_,_,_,p,_,_) -> p) unjsonDef)
               <*> liftAp (TupleFieldDef 6 (\(_,_,_,_,_,_,p,_) -> p) unjsonDef)
               <*> liftAp (TupleFieldDef 7 (\(_,_,_,_,_,_,_,p) -> p) unjsonDef)

instance (Unjson a,Unjson b,Unjson c,Unjson d
         ,Unjson e,Unjson f,Unjson g,Unjson h
         ,Unjson i)
       => Unjson (a,b,c,d
                 ,e,f,g,h
                 ,i) where
  unjsonDef = TupleUnjsonDef
               $ pure (,,,,,,,,)
               <*> liftAp (TupleFieldDef 0 (\(p,_,_,_,_,_,_,_,_) -> p) unjsonDef)
               <*> liftAp (TupleFieldDef 1 (\(_,p,_,_,_,_,_,_,_) -> p) unjsonDef)
               <*> liftAp (TupleFieldDef 2 (\(_,_,p,_,_,_,_,_,_) -> p) unjsonDef)
               <*> liftAp (TupleFieldDef 3 (\(_,_,_,p,_,_,_,_,_) -> p) unjsonDef)
               <*> liftAp (TupleFieldDef 4 (\(_,_,_,_,p,_,_,_,_) -> p) unjsonDef)
               <*> liftAp (TupleFieldDef 5 (\(_,_,_,_,_,p,_,_,_) -> p) unjsonDef)
               <*> liftAp (TupleFieldDef 6 (\(_,_,_,_,_,_,p,_,_) -> p) unjsonDef)
               <*> liftAp (TupleFieldDef 7 (\(_,_,_,_,_,_,_,p,_) -> p) unjsonDef)
               <*> liftAp (TupleFieldDef 8 (\(_,_,_,_,_,_,_,_,p) -> p) unjsonDef)

instance (Unjson a,Unjson b,Unjson c,Unjson d
         ,Unjson e,Unjson f,Unjson g,Unjson h
         ,Unjson i,Unjson j)
       => Unjson (a,b,c,d
                 ,e,f,g,h
                 ,i,j) where
  unjsonDef = TupleUnjsonDef
               $ pure (,,,,,,,,,)
               <*> liftAp (TupleFieldDef 0 (\(p,_,_,_,_,_,_,_,_,_) -> p) unjsonDef)
               <*> liftAp (TupleFieldDef 1 (\(_,p,_,_,_,_,_,_,_,_) -> p) unjsonDef)
               <*> liftAp (TupleFieldDef 2 (\(_,_,p,_,_,_,_,_,_,_) -> p) unjsonDef)
               <*> liftAp (TupleFieldDef 3 (\(_,_,_,p,_,_,_,_,_,_) -> p) unjsonDef)
               <*> liftAp (TupleFieldDef 4 (\(_,_,_,_,p,_,_,_,_,_) -> p) unjsonDef)
               <*> liftAp (TupleFieldDef 5 (\(_,_,_,_,_,p,_,_,_,_) -> p) unjsonDef)
               <*> liftAp (TupleFieldDef 6 (\(_,_,_,_,_,_,p,_,_,_) -> p) unjsonDef)
               <*> liftAp (TupleFieldDef 7 (\(_,_,_,_,_,_,_,p,_,_) -> p) unjsonDef)
               <*> liftAp (TupleFieldDef 8 (\(_,_,_,_,_,_,_,_,p,_) -> p) unjsonDef)
               <*> liftAp (TupleFieldDef 9 (\(_,_,_,_,_,_,_,_,_,p) -> p) unjsonDef)

instance (Unjson a,Unjson b,Unjson c,Unjson d
         ,Unjson e,Unjson f,Unjson g,Unjson h
         ,Unjson i,Unjson j,Unjson k)
       => Unjson (a,b,c,d
                 ,e,f,g,h
                 ,i,j,k) where
  unjsonDef = TupleUnjsonDef
               $ pure (,,,,,,,,,,)
               <*> liftAp (TupleFieldDef 0 (\(p,_,_,_,_,_,_,_,_,_,_) -> p) unjsonDef)
               <*> liftAp (TupleFieldDef 1 (\(_,p,_,_,_,_,_,_,_,_,_) -> p) unjsonDef)
               <*> liftAp (TupleFieldDef 2 (\(_,_,p,_,_,_,_,_,_,_,_) -> p) unjsonDef)
               <*> liftAp (TupleFieldDef 3 (\(_,_,_,p,_,_,_,_,_,_,_) -> p) unjsonDef)
               <*> liftAp (TupleFieldDef 4 (\(_,_,_,_,p,_,_,_,_,_,_) -> p) unjsonDef)
               <*> liftAp (TupleFieldDef 5 (\(_,_,_,_,_,p,_,_,_,_,_) -> p) unjsonDef)
               <*> liftAp (TupleFieldDef 6 (\(_,_,_,_,_,_,p,_,_,_,_) -> p) unjsonDef)
               <*> liftAp (TupleFieldDef 7 (\(_,_,_,_,_,_,_,p,_,_,_) -> p) unjsonDef)
               <*> liftAp (TupleFieldDef 8 (\(_,_,_,_,_,_,_,_,p,_,_) -> p) unjsonDef)
               <*> liftAp (TupleFieldDef 9 (\(_,_,_,_,_,_,_,_,_,p,_) -> p) unjsonDef)
               <*> liftAp (TupleFieldDef 10 (\(_,_,_,_,_,_,_,_,_,_,p) -> p) unjsonDef)

instance (Unjson a,Unjson b,Unjson c,Unjson d
         ,Unjson e,Unjson f,Unjson g,Unjson h
         ,Unjson i,Unjson j,Unjson k,Unjson l)
       => Unjson (a,b,c,d
                 ,e,f,g,h
                 ,i,j,k,l) where
  unjsonDef = TupleUnjsonDef
               $ pure (,,,,,,,,,,,)
               <*> liftAp (TupleFieldDef 0 (\(p,_,_,_,_,_,_,_,_,_,_,_) -> p) unjsonDef)
               <*> liftAp (TupleFieldDef 1 (\(_,p,_,_,_,_,_,_,_,_,_,_) -> p) unjsonDef)
               <*> liftAp (TupleFieldDef 2 (\(_,_,p,_,_,_,_,_,_,_,_,_) -> p) unjsonDef)
               <*> liftAp (TupleFieldDef 3 (\(_,_,_,p,_,_,_,_,_,_,_,_) -> p) unjsonDef)
               <*> liftAp (TupleFieldDef 4 (\(_,_,_,_,p,_,_,_,_,_,_,_) -> p) unjsonDef)
               <*> liftAp (TupleFieldDef 5 (\(_,_,_,_,_,p,_,_,_,_,_,_) -> p) unjsonDef)
               <*> liftAp (TupleFieldDef 6 (\(_,_,_,_,_,_,p,_,_,_,_,_) -> p) unjsonDef)
               <*> liftAp (TupleFieldDef 7 (\(_,_,_,_,_,_,_,p,_,_,_,_) -> p) unjsonDef)
               <*> liftAp (TupleFieldDef 8 (\(_,_,_,_,_,_,_,_,p,_,_,_) -> p) unjsonDef)
               <*> liftAp (TupleFieldDef 9 (\(_,_,_,_,_,_,_,_,_,p,_,_) -> p) unjsonDef)
               <*> liftAp (TupleFieldDef 10 (\(_,_,_,_,_,_,_,_,_,_,p,_) -> p) unjsonDef)
               <*> liftAp (TupleFieldDef 11 (\(_,_,_,_,_,_,_,_,_,_,_,p) -> p) unjsonDef)

-- | Specify how arrays should be handled. Default is
-- 'ArrayModeStrict' that does not do anything special with
-- arrays.
--
-- 'ArrayMode' is used in 'arrayWithModeAndPrimaryKeyOf' and
-- 'arrayWithModeOf'.
data ArrayMode
  -- | Require JSON array. On output always output array.
  = ArrayModeStrict

  -- | Allow non-array element, in that case it will be treated as a
  -- single element array. On output always output array.
  | ArrayModeParseSingle

  -- | Allow non-array element, in that case it will be treated as a
  -- single element array. On output output single element if array
  -- has one element.
  | ArrayModeParseAndOutputSingle
  deriving (Eq, Ord, Show, Typeable)

-- | 'PrimaryKeyExtraction' is needed to keep 'Ord pk' constraint
-- attached. Elements of array may be matched based on a primary
-- key. A primary key has to be extracted both from existing array
-- structure and from JSON array elements. Then a 'Set' is constructed
-- so that lookups are efficient. Then for each element in JSON a
-- corresponding element in old object is looked for. If found the
-- element is updated, if not found it is parsed fresh.
data PrimaryKeyExtraction k = forall pk . (Ord pk) => PrimaryKeyExtraction (k -> pk) (UnjsonDef pk)

-- | Opaque 'UnjsonDef' defines a bidirectional JSON parser.
data UnjsonDef a where
  SimpleUnjsonDef :: Text.Text -> (Anchored Aeson.Value -> Result k) -> (k -> Aeson.Value) -> UnjsonDef k
  ArrayUnjsonDef  :: Maybe (PrimaryKeyExtraction k) -> ArrayMode -> ([k] -> v) -> (v -> [k]) -> UnjsonDef k -> UnjsonDef v
  ObjectUnjsonDef :: Ap (FieldDef k) k -> UnjsonDef k
  TupleUnjsonDef  :: Ap (TupleFieldDef k) k -> UnjsonDef k
  -- DisjointUnjsonDef :: Ap (FieldDef k) k -> UnjsonDef k

-- This is Profunctor, but I really do not want to depend on lens here, sorry.
dibimapUnjsonDef :: (a -> b) -> (b -> a) -> UnjsonDef a -> UnjsonDef b
dibimapUnjsonDef f g (SimpleUnjsonDef name p s) = SimpleUnjsonDef name (fmap f . p) (s . g)
dibimapUnjsonDef f g (ArrayUnjsonDef mpk am n k d) = ArrayUnjsonDef mpk am (f . n) (k . g) d
dibimapUnjsonDef f g (ObjectUnjsonDef fd) = ObjectUnjsonDef (fmap f (hoistAp (dibimapFieldDef g) fd))
dibimapUnjsonDef f g (TupleUnjsonDef td) = TupleUnjsonDef (fmap f (hoistAp (dibimapTupleFieldDef g) td))

dibimapFieldDef :: (b -> a) -> FieldDef a x -> FieldDef b x
dibimapFieldDef f (FieldReqDef name doc ext d) = FieldReqDef name doc (ext . f) d
dibimapFieldDef f (FieldOptDef name doc ext d) = FieldOptDef name doc (ext . f) d
dibimapFieldDef f (FieldDefDef name doc def ext d) = FieldDefDef name doc def (ext . f) d

dibimapTupleFieldDef :: (b -> a) -> TupleFieldDef a x -> TupleFieldDef b x
dibimapTupleFieldDef f (TupleFieldDef i e d) = TupleFieldDef i (e . f) d

-- | Define a relation between a field of an object in JSON and a
-- field in a Haskell record structure.  'FieldDef' holds information
-- about a documentation string, key name, Haskell data accessor and
-- parsing definition.  'FieldDef' has three cases for fields that are
-- required, optional (via 'Maybe') or jave default value.
data FieldDef s a where
  FieldReqDef :: Text.Text -> Text.Text -> (s -> a) -> UnjsonDef a -> FieldDef s a
  FieldOptDef :: Text.Text -> Text.Text -> (s -> Maybe a) -> UnjsonDef a -> FieldDef s (Maybe a)
  FieldDefDef :: Text.Text -> Text.Text -> a -> (s -> a) -> UnjsonDef a -> FieldDef s a

-- | Define a tuple element. 'TupleFieldDef' holds information about
-- index, accessor function and a parser definition.
data TupleFieldDef s a where
  TupleFieldDef :: Int -> (s -> a) -> UnjsonDef a -> TupleFieldDef s a

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
--
-- Example:
--
-- > let v = Thing { ... }
-- > let json = serialize unjsonThing v
--
serialize :: UnjsonDef a -> a -> Aeson.Value
serialize (SimpleUnjsonDef _ _ g) a = g a
serialize (ArrayUnjsonDef _ m g k f) a =
  case (m, k a) of
    (ArrayModeParseAndOutputSingle,[b]) -> serialize f b
    (_,c) -> Aeson.toJSON (map (serialize f) c)
serialize (ObjectUnjsonDef f) a =
  Aeson.object (objectDefToArray a f)
serialize (TupleUnjsonDef f) a =
  Aeson.toJSON (tupleDefToArray a f)

-- | Count how many applications there are. Useful for error
-- reporting.
countAp :: Int -> Ap x a -> Int
countAp !n (Pure _) = n
countAp n (Ap _ r) = countAp (succ n) r

parseUpdating :: UnjsonDef a -> Maybe a -> Anchored Aeson.Value -> Result a
parseUpdating (SimpleUnjsonDef _ f _) _ov v = f v
parseUpdating (ArrayUnjsonDef (Just (PrimaryKeyExtraction pk_from_object pk_from_json)) m g k f) (Just ov) (Anchored path v)
  = case Aeson.parseEither Aeson.parseJSON v of
      Right v -> fmap g $
        sequenceA (zipWith (\v i -> (lookupObjectByJson (Anchored (path <> Path [PathElemIndex i]) v)) >>= \ov ->
                                        parseUpdating f ov
                                        (Anchored (path <> Path [PathElemIndex i]) v))
                                    (Vector.toList v) [0..])
      Left e -> case m of
          ArrayModeStrict ->
            resultWithThrow (Anchored path (Text.pack e))
          _ -> fmap g $
            sequenceA [(lookupObjectByJson (Anchored (path <> Path [PathElemIndex 0]) v)) >>= \ov ->
                                        parseUpdating f ov
                                        (Anchored (path <> Path [PathElemIndex 0]) v)]
  where
    -- Note: Map.fromList is right-biased, so that Map.fromList [(1,1),(1,2)] is [(1,2)]
    -- we need it to be left-biased, so we use Map.fromListWith (flip const)
    objectMap = Map.fromListWith (flip const) (map (\o -> (pk_from_object o, o)) (k ov))
    lookupObjectByJson js = parseUpdating pk_from_json Nothing js >>= \val -> return (Map.lookup val objectMap)

parseUpdating (ArrayUnjsonDef _ m g k f) _ov (Anchored path v)
  = case Aeson.parseEither Aeson.parseJSON v of
      Right v -> fmap g $
        sequenceA (zipWith (\v i -> parseUpdating f Nothing (Anchored (path <> Path [PathElemIndex i]) v)) (Vector.toList v) [0..])
      Left e -> case m of
          ArrayModeStrict ->
            resultWithThrow (Anchored path (Text.pack e))
          _ -> fmap g $
            sequenceA [parseUpdating f Nothing (Anchored (path <> Path [PathElemIndex 0]) v)]

parseUpdating (ObjectUnjsonDef f) ov (Anchored path v)
  = case Aeson.parseEither Aeson.parseJSON v of
      Right v ->
        runAp (lookupByFieldDef (Anchored path v) ov) f
      Left e ->
        resultWithThrow (Anchored path (Text.pack e))
parseUpdating (TupleUnjsonDef f) ov (Anchored path v)
  = case Aeson.parseEither Aeson.parseJSON v of
      Right v ->
        let r@(Result g h) = runAp (lookupByTupleFieldDef (Anchored path v) ov) f
            tupleSize = countAp 0 f
            arrayLength = Vector.length v
        in if tupleSize == arrayLength
             then r
             else Result g (h <> [Anchored path ("cannot parse array of length " <> Text.pack (show arrayLength) <>
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
-- Error reporting is a strong side of Unjson, see 'Result'.
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
parse :: UnjsonDef a -> Anchored Aeson.Value -> Result a
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
-- Error reporting is a strong side of Unjson, see 'Result'.
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
update :: a -> UnjsonDef a -> Anchored Aeson.Value -> Result a
update a vd = parseUpdating vd (Just a)

lookupByFieldDef :: Anchored Aeson.Object -> Maybe s -> FieldDef s a -> Result a
lookupByFieldDef (Anchored path v) ov (FieldReqDef name docstring f valuedef)
  = case HashMap.lookup name v of
      Just x  -> parseUpdating valuedef (fmap f ov) (Anchored (path <> Path [PathElemKey name]) x)
      Nothing -> case ov of
                   Just xov -> Result (f xov) []
                   Nothing -> resultWithThrow (Anchored (path <> Path [PathElemKey name]) "missing key")
lookupByFieldDef (Anchored path v) ov (FieldDefDef name docstring def f valuedef)
  = case HashMap.lookup name v of
      Just Aeson.Null -> Result def []
      Just x  -> parseUpdating valuedef (fmap f ov) (Anchored (path <> Path [PathElemKey name]) x)
      Nothing -> case ov of
                   Just xov -> Result (f xov) []
                   Nothing -> Result def []
lookupByFieldDef (Anchored path v) ov (FieldOptDef name docstring f valuedef)
  = case HashMap.lookup name v of
      Just Aeson.Null -> Result Nothing []
      Just x  -> case ov of
                   Just xov -> fmap Just (parseUpdating valuedef (f xov) (Anchored (path <> Path [PathElemKey name]) x))
                   Nothing -> fmap Just (parseUpdating valuedef Nothing (Anchored (path <> Path [PathElemKey name]) x))
      Nothing -> case ov of
                   Just xov -> Result (f xov) []
                   Nothing -> Result Nothing []


lookupByTupleFieldDef :: Anchored Aeson.Array -> Maybe s -> TupleFieldDef s a -> Result a
lookupByTupleFieldDef (Anchored path v) ov (TupleFieldDef idx f valuedef)
  = case v Vector.!? idx of
      Just x  -> parseUpdating valuedef (fmap f ov) (Anchored (path <> Path [PathElemIndex idx]) x)
      Nothing -> resultWithThrow (Anchored (path <> Path [PathElemIndex idx]) "missing key")

-- | Declare a required field with definition given inline by valuedef.
--
-- Example:
--
-- > unjsonThing :: UnjsonDef Thing
-- > unjsonThing = objectOf $ pure Thing
-- >    <*> fieldBy "credentials"
-- >          thingCredentials
-- >          "Credentials to use"
-- >          unjsonCredentials
-- >
-- > data Thing = Thing { thingCredentials :: Credentials, ... }
-- > unjsonCredentials :: UnjsonDef Credentials
fieldBy :: Text.Text -> (s -> a) -> Text.Text -> UnjsonDef a -> Ap (FieldDef s) a
fieldBy key f docstring valuedef = liftAp (FieldReqDef key docstring f valuedef)

-- | Declare a required field with definition from 'Unjson' typeclass.
--
-- Example:
--
-- > unjsonThing :: UnjsonDef Thing
-- > unjsonThing = objectOf $ pure Thing
-- >    <*> field "credentials"
-- >          thingCredentials
-- >          "Credentials to use"
-- >
-- > data Thing = Thing { thingCredentials :: Credentials, ... }
-- > instance Unjson Credentials where ...
field :: (Unjson a) => Text.Text -> (s -> a) -> Text.Text -> Ap (FieldDef s) a
field key f docstring = fieldBy key f docstring unjsonDef

-- | Declare an optional field and definition by valuedef.
--
-- Example:
--
-- > unjsonThing :: UnjsonDef Thing
-- > unjsonThing = objectOf $ pure Thing
-- >    <*> fieldOptBy "credentials"
-- >          thingCredentials
-- >          "Optional credentials to use"
-- >          unjsonCredentials
-- >
-- > data Thing = Thing { thingCredentials :: Credentials, ... }
-- > unjsonCredentials :: UnjsonDef Credentials
fieldOptBy :: Text.Text -> (s -> Maybe a) -> Text.Text -> UnjsonDef a -> Ap (FieldDef s) (Maybe a)
fieldOptBy key f docstring valuedef = liftAp (FieldOptDef key docstring f valuedef)

-- | Declare an optional field and definition by 'Unjson' typeclass.
--
-- Example:
--
-- > unjsonThing :: UnjsonDef Thing
-- > unjsonThing = objectOf $ pure Thing
-- >    <*> fieldOpt "credentials"
-- >          thingCredentials
-- >          "Optional credentials to use"
-- >
-- > data Thing = Thing { thingCredentials :: Credentials, ... }
-- > instance Unjson Credentials where ...
fieldOpt :: (Unjson a) => Text.Text -> (s -> Maybe a) -> Text.Text -> Ap (FieldDef s) (Maybe a)
fieldOpt key f docstring = fieldOptBy key f docstring unjsonDef

-- | Declare a field with default value and definition by valuedef.
--
-- Example:
--
-- > unjsonThing :: UnjsonDef Thing
-- > unjsonThing = objectOf $ pure Thing
-- >    <*> fieldDefBy "credentials" defaultCredentials
-- >          thingCredentials
-- >          "Credentials to use, defaults to defaultCredentials"
-- >          unjsonCredentials
-- >
-- > data Thing = Thing { thingCredentials :: Credentials, ... }
-- > unjsonCredentials :: UnjsonDef Credentials
fieldDefBy :: Text.Text -> a -> (s -> a) -> Text.Text -> UnjsonDef a -> Ap (FieldDef s) a
fieldDefBy key def f docstring valuedef = liftAp (FieldDefDef key docstring def f valuedef)

-- | Declare a field with default value and definition by 'Unjson' typeclass.
--
-- Example:
--
-- > unjsonThing :: UnjsonDef Thing
-- > unjsonThing = objectOf $ pure Thing
-- >    <*> fieldDef "port" 80
-- >          thingPort
-- >          "Port to listen on, defaults to 80"
-- >
-- > data Thing = Thing { thingPort :: Int, ... }
fieldDef :: (Unjson a) => Text.Text -> a -> (s -> a) -> Text.Text -> Ap (FieldDef s) a
fieldDef key def f docstring = fieldDefBy key def f docstring unjsonDef

-- | Declare an object as bidirectional mapping from JSON object to Haskell record and back.
--
-- Example:
--
-- > unjsonThing :: UnjsonDef Thing
-- > unjsonThing = objectOf $ pure Thing
-- >    ...field definitions go here
--
-- Use field functions to specify fields of an object: 'field',
-- 'fieldBy', 'fieldOpt', 'fieldOptBy',
-- 'fieldDef' or 'fieldDefBy'.
objectOf :: Ap (FieldDef a) a -> UnjsonDef a
objectOf fields = ObjectUnjsonDef fields

-- | Declare array of values where each of them is described by valuedef. Use 'unjsonAeson' to parse
--
-- Example:
--
-- > unjsonArrayOfThings :: UnjsonDef [Thing]
-- > unjsonArrayOfThings = arrayOf unjsonThing
-- >
-- > unjsonThing :: UnjsonDef Thing
-- > unjsonThing = ...
arrayOf :: UnjsonDef a -> UnjsonDef [a]
arrayOf = arrayWithModeOf ArrayModeStrict

-- | Declare array of values where each of them is described by
-- valuedef. Accepts mode specifier.
--
-- Example:
--
-- > unjsonArrayOfThings :: UnjsonDef [Thing]
-- > unjsonArrayOfThings = arrayOf unjsonThing
-- >
-- > unjsonThing :: UnjsonDef Thing
-- > unjsonThing = ...
arrayWithModeOf :: ArrayMode -> UnjsonDef a -> UnjsonDef [a]
arrayWithModeOf mode valuedef = ArrayUnjsonDef Nothing mode id id valuedef

-- | Declare array of primitive values lifed from 'Aeson'.
--
-- Example:
--
-- > unjsonArrayOfInt :: UnjsonDef [Int]
-- > unjsonArrayOfInt = arrayOf'
arrayOf' :: (Aeson.FromJSON a,Aeson.ToJSON a, Typeable a) => UnjsonDef [a]
arrayOf' = arrayOf unjsonAeson

-- | Declare array of primitive values lifed from 'Aeson'. Accepts
-- mode specifier.
--
-- Example:
--
-- > unjsonArrayOfIntOrSimpleInt :: UnjsonDef [Int]
-- > unjsonArrayOfIntOrSimpleInt = arrayWithModeOf'
arrayWithModeOf' :: (Aeson.FromJSON a,Aeson.ToJSON a, Typeable a)
                 => ArrayMode
                 -> UnjsonDef [a]
arrayWithModeOf' mode = arrayWithModeOf mode unjsonAeson


-- | Declare array of objects with given parsers that should be
-- matched by a primary key and accepts mode specifier.
--
-- For discussion of primary key see 'arrayWithPrimaryKeyOf'. For
-- discussion of array modes see 'ArrayMode'.
--
-- Example:
--
-- > unjsonArrayOfIntToInt :: UnjsonDef [(Int,Int)]
-- > unjsonArrayOfIntToInt = arrayWithPrimaryKeyOf ArrayModeParseSingle
-- >                              (fst)
-- >                              (objectOf $ pure id
-- >                                 <*> field "key" id "Key in mapping")
-- >                              (objectOf $ pure (,)
-- >                                 <*> field "key" fst "Key in mapping"
-- >                                 <*> field "value" fst "Value in mapping")
arrayWithModeAndPrimaryKeyOf :: (Ord pk)
                             => ArrayMode
                             -> (a -> pk)
                             -> UnjsonDef pk
                             -> UnjsonDef a
                             -> UnjsonDef [a]
arrayWithModeAndPrimaryKeyOf mode pk1 pk2 valuedef =
  ArrayUnjsonDef (Just (PrimaryKeyExtraction pk1 pk2)) mode id id valuedef

-- | Declare array of objects with given parsers that should be
-- matched by a primary key. Uses 'ArrayModeStrict'.
--
-- Primary key:
--
-- Primary keys are used to match objects in 'update' mode. When a
-- request to update array is issued and array has primary key
-- specification then the following steps are used:
--
-- 1. primary keys from old array elements are extracted and a mapping
--   from primary key to element is created. Mapping is left biased
--   meaning that first element with specific primary key in array is
--   used
--
-- 2. for each object in json array primary key is extracted and is
--   looked up in old elements mapping
--
-- 3. if mapping is found then element is 'update'd, if mapping is not
--   found then element is 'parse'd
--
-- 4. in all cases the order of elements in the *new* array is respected
--
-- Example:
--
-- > unjsonArrayOfIntToInt :: UnjsonDef [(Int,Int)]
-- > unjsonArrayOfIntToInt = arrayWithPrimaryKeyOf
-- >                              (fst)
-- >                              (objectOf $ pure id
-- >                                 <*> field "key" id "Key in mapping")
-- >                              (objectOf $ pure (,)
-- >                                 <*> field "key" fst "Key in mapping"
-- >                                 <*> field "value" fst "Value in mapping")
arrayWithPrimaryKeyOf :: (Ord pk)
                      => (a -> pk)
                      -> UnjsonDef pk
                      -> UnjsonDef a
                      -> UnjsonDef [a]
arrayWithPrimaryKeyOf pk1 pk2 valuedef =
  arrayWithModeAndPrimaryKeyOf ArrayModeStrict pk1 pk2 valuedef

-- | Use 'Aeson.fromJSON' and 'Aeson.toJSON' to create a
-- 'UnjsonDef'. This function is useful when lifted type is one of the
-- primitives. Although it can be used to lift user defined instances,
-- it is not advisable as there is too much information lost in the
-- process and proper error infomation is not possible. Use full
-- 'UnjsonDef' instance whenever possible.
--
-- Example:
--
-- > instance FromJSON MyType where ...
-- > instance ToJSON MyType where ...
-- > instance Unjson MyType where
-- >     unjsonDef = unjsonAeson
unjsonAeson :: forall a . (Aeson.FromJSON a,Aeson.ToJSON a, Typeable a) => UnjsonDef a
unjsonAeson = unjsonAesonWithDoc (Text.pack (show (typeOf (undefined :: a))))

-- | Like 'unjsonAeson' but accepts docstring as additional parameter
-- that should identify type.
unjsonAesonWithDoc :: (Aeson.FromJSON a,Aeson.ToJSON a) => Text.Text -> UnjsonDef a
unjsonAesonWithDoc docstring = SimpleUnjsonDef docstring
              (\(Anchored path value) ->
                case Aeson.fromJSON value of
                  Aeson.Success result -> Result result []
                  Aeson.Error message -> resultWithThrow (Anchored path (Text.pack message)))
              Aeson.toJSON

-- | Rename @[Char]@ to @String@ everywhere.
unjsonAesonFixCharArrayToString :: forall a . (Aeson.FromJSON a,Aeson.ToJSON a, Typeable a) => UnjsonDef a
unjsonAesonFixCharArrayToString =
  unjsonAesonWithDoc (Text.pack typeNameFixed)
  where
    typeName = show (typeOf (undefined :: a))
    typeNameFixed = fixup typeName
    fixup [] = []
    fixup ('[':'C':'h':'a':'r':']':rest) = "String" ++ fixup rest
    fixup (x:xs) = x : fixup xs

-- | Renders documentation for a parser into a multiline string. It is
-- expected that this string is a human readable representation that
-- can go directly to console.
--
-- Example rendering:
--
-- > hostname (req):
-- >     The hostname this service is visible as
-- >     Text
-- > port (def):
-- >     Port to listen on, defaults to 80
-- >     Int
-- > credentials (req):
-- >     User admin credentials
-- >     username (req):
-- >         Name of the user
-- >         Text
-- >     password (req):
-- >         Password for the user
-- >         Text
-- >     domain (opt):
-- >         Domain for user credentials
-- >         Text
-- > comment (opt):
-- >     Optional comment, free text
-- >     Text
-- > options (def):
-- >     Additional options, defaults to empty
-- >     array of:
-- >         Text
-- > alternates (opt):
-- >     Alternate names for this server
-- >     tuple of size 2 with elements:
-- >     0:
-- >         Text
-- >     1:
-- >         username (req):
-- >             Name of the user
-- >             Text
-- >         password (req):
-- >             Password for the user
-- >             Text
-- >         domain (opt):
-- >             Domain for user credentials
-- >             Text
render :: UnjsonDef a -> String
render = P.render . renderDoc

-- | Render only selected part of structure documentation. Path should
-- point to a subtree, if it does not then Nothing is returned.
renderForPath :: (Functor m, Monad m) => Path -> UnjsonDef a -> m String
renderForPath path def = fmap P.render (renderDocForPath path def)

-- | Renders documentation for a parser into a 'P.Doc'. See 'render'
-- for example.
renderDoc :: UnjsonDef a -> P.Doc
renderDoc (SimpleUnjsonDef doc _ _) = P.text (ansiDimmed ++ Text.unpack doc ++ ansiReset)
renderDoc (ArrayUnjsonDef _ _m g k f) = P.text (ansiDimmed ++ "array of" ++ ansiReset ++ ":") P.$+$
             P.nest 4 (renderDoc f)
renderDoc (ObjectUnjsonDef f) =
             P.vcat (renderFields f)
renderDoc (TupleUnjsonDef f) = P.text (ansiDimmed ++ "tuple of size " ++ show (countAp 0 f) ++ " with elements:" ++ ansiReset) P.$+$
             P.vcat (renderTupleFields f)

-- | Render only selected part of structure documentation as
-- 'P.Doc'. Path should point to a subtree, if it does not then
-- Nothing is returned.
renderDocForPath :: (Monad m) => Path -> UnjsonDef a -> m P.Doc
renderDocForPath path def = findNestedUnjson path def


renderField :: FieldDef s a -> P.Doc
renderField (FieldReqDef key docstring _f d) =
  P.text (ansiBold ++ Text.unpack key ++ ansiReset) P.<> P.text " (req):" P.$+$ P.nest 4 (P.text (Text.unpack docstring) P.$+$ renderDoc d)
renderField (FieldOptDef key docstring _f d) =
  P.text (ansiBold ++ Text.unpack key ++ ansiReset) P.<> P.text " (opt):" P.$+$ P.nest 4 (P.text (Text.unpack docstring) P.$+$ renderDoc d)
renderField (FieldDefDef key docstring _f _ d) =
  P.text (ansiBold ++ Text.unpack key ++ ansiReset) P.<> P.text " (def):" P.$+$ P.nest 4 (P.text (Text.unpack docstring) P.$+$ renderDoc d)


renderFields :: Ap (FieldDef s) a -> [P.Doc]
renderFields (Pure _) = []
renderFields (Ap f r) =
  renderField f : renderFields r

renderTupleFields :: Ap (TupleFieldDef s) a -> [P.Doc]
renderTupleFields (Pure _) = []
renderTupleFields (Ap f r) =
  renderTupleField f : renderTupleFields r

renderTupleField :: TupleFieldDef s a -> P.Doc
renderTupleField (TupleFieldDef index _f d) =
  P.text (ansiBold ++ show index ++ ansiReset) P.<> P.text ":" P.$+$ P.nest 4 s
  where
    s = renderDoc d

findNestedUnjson :: (Monad m) => Path -> UnjsonDef a -> m P.Doc
findNestedUnjson (Path []) u = return (renderDoc u)
findNestedUnjson (Path (PathElemIndex n : rest)) (TupleUnjsonDef d) = findNestedTupleUnjson n (Path rest) d
findNestedUnjson (Path (PathElemIndex _ : rest)) (ArrayUnjsonDef _ _ _ _ d) = findNestedUnjson (Path rest) d
findNestedUnjson (Path (PathElemKey k : rest)) (ObjectUnjsonDef d) = findNestedFieldUnjson k (Path rest) d
findNestedUnjson _ _ = fail "cannot find crap"

findNestedTupleUnjson :: (Monad m) => Int -> Path -> Ap (TupleFieldDef s) a -> m P.Doc
findNestedTupleUnjson n path (Ap (TupleFieldDef index _f d) _r) | n == index = findNestedUnjson path d
findNestedTupleUnjson n path (Ap (TupleFieldDef _index _f _d) r) =
  findNestedTupleUnjson n path r
findNestedTupleUnjson _ _ _ = fail "findNestedTupleUnjson"

findNestedFieldUnjson :: (Monad m) => Text.Text -> Path -> Ap (FieldDef s) a -> m P.Doc
findNestedFieldUnjson key (Path []) (Ap f@(FieldReqDef k _ _ _d) _r) | k==key = return (renderField f)
findNestedFieldUnjson key (Path []) (Ap f@(FieldOptDef k _ _ _d) _r) | k==key = return (renderField f)
findNestedFieldUnjson key (Path []) (Ap f@(FieldDefDef k _ _ _ _d) _r) | k==key = return (renderField f)
findNestedFieldUnjson key path (Ap (FieldReqDef k _ _ d) _r) | k==key = findNestedUnjson path d
findNestedFieldUnjson key path (Ap (FieldOptDef k _ _ d) _r) | k==key = findNestedUnjson path d
findNestedFieldUnjson key path (Ap (FieldDefDef k _ _ _ d) _r) | k==key = findNestedUnjson path d
findNestedFieldUnjson key path (Ap _ r) =
  findNestedFieldUnjson key path r
findNestedFieldUnjson _ _ _ = fail "findNestedFieldUnjson"

-- Add some colors to the mix

ansiReset :: String
ansiReset = "\ESC[0m"

ansiBold :: String
ansiBold = "\ESC[1m"

ansiDimmed :: String
ansiDimmed = "\ESC[2m"

parseIPv4 :: ReadP.ReadP Word32
parseIPv4 = do
  d1 <- ReadP.munch1 isDigit
  _ <- ReadP.char '.'
  d2 <- ReadP.munch1 isDigit
  _ <- ReadP.char '.'
  d3 <- ReadP.munch1 isDigit
  _ <- ReadP.char '.'
  d4 <- ReadP.munch1 isDigit
  ReadP.eof
  let r = map read [d1,d2,d3,d4]
  when (any (>255) r) ReadP.pfail
  return (sum (zipWith shiftL r [24,16,8,0]))


-- | Parse and serialize dotted decimal notation for IPv4 addresses
-- and uses 'Word32' as representation type. Note that network byte
-- order applies, so 127.0.0.1 is 0x7F000001.
unjsonIPv4AsWord32 :: UnjsonDef Word32
unjsonIPv4AsWord32 = SimpleUnjsonDef "IPv4 in decimal dot notation A.B.C.D"
              (\(Anchored path value) ->
                case Aeson.fromJSON value of
                  Aeson.Success result ->
                    -- a number, treat it as is, for example 0x7f000001 = 2130706433 = 127.0.0.1
                    Result result []
                  Aeson.Error _ ->
                    case Aeson.fromJSON value of
                      Aeson.Success result -> case ReadP.readP_to_S parseIPv4 result of
                        [(r,"")] -> Result r []
                        _ -> resultWithThrow (Anchored path (Text.pack "cannot parse as decimal dot IPv4"))
                      Aeson.Error _ ->
                        resultWithThrow (Anchored path (Text.pack "expected IPv4 as decimal dot string or a single integer")))
              (Aeson.toJSON . showAsIPv4)
  where
    showAsIPv4 :: Word32 -> String
    showAsIPv4 v = intercalate "." [show (shiftR v b .&. 255) | b <- [24,16,8,0]]
