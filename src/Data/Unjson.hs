module Data.Unjson
  ( fieldBy
  , fieldOptBy
  , fieldDefBy
  , Documentation(..)
  , document
  , parse
  , Result(..)
  )
where

import qualified Data.Aeson as Aeson
import qualified Data.Text as Text
import Data.Typeable
import Data.Monoid
import Control.Applicative
import Control.Applicative.Free
import Data.Scientific
import Data.Attoparsec.Number
import qualified Data.HashMap.Strict as HashMap
import Control.Exception

data PathElem = PathElemKey Text.Text
              | PathElemIndex Int
  deriving (Typeable, Eq, Ord, Show)

type Path = [PathElem]

data Anchored a = Anchored Path a
  deriving (Typeable, Functor, Eq, Ord, Show)


instance (Typeable a, Show a) => Exception (Anchored a)

data UnjsonX' a
  = Field Text.Text Text.Text (Maybe Aeson.Value -> Result a)
  deriving (Typeable, Functor)

type UnjsonX a = Ap UnjsonX' a

data Result a = Result a Problems
  deriving (Functor, Show, Ord, Eq)

type Problems = [Problem]

type Problem = Text.Text

data Documentation
  = Documentation Text.Text                    -- ^ description of this particular item
                  [(Text.Text,Documentation)]  -- ^ description of its parts, key-value
  deriving (Eq, Ord, Show, Typeable)


fieldBy :: (Aeson.Value -> Result a) -> Text.Text -> Text.Text -> UnjsonX a
fieldBy f name docstring = liftAp (Field name docstring f2)
  where
    f2 (Just v) = f v
    f2 Nothing = Result (error "key does not exists in object") ["key does not exists in object"]

fieldOptBy :: (Aeson.Value -> Result a) -> Text.Text -> Text.Text -> UnjsonX (Maybe a)
fieldOptBy f name docstring = liftAp (Field name docstring f2)
  where
    f2 (Just v) = fmap Just (f v)
    f2 Nothing = Result Nothing []

fieldDefBy :: (Aeson.Value -> Result a) -> a -> Text.Text -> Text.Text -> UnjsonX a
fieldDefBy f def name docstring = liftAp (Field name docstring f2)
  where
    f2 (Just v) = f v
    f2 Nothing = Result def []

documentF :: UnjsonX' a -> Documentation
documentF (Field key docstring p) = Documentation "" [(key,Documentation docstring [])]

document :: UnjsonX a -> Documentation
document (Pure x) = Documentation "" []
document (Ap a b) = Documentation (a1 <> b1) (a2 <> b2)
  where
    Documentation a1 a2 = documentF a
    Documentation b1 b2 = document b

parseF (Field key _ ap) (Aeson.Object o) = ap (HashMap.lookup key o)
parseF _ _ = Result (error "trying to lookup a key in non-object") ["trying to lookup a key in non-object"]

parse :: UnjsonX a -> Aeson.Value -> Result a
parse (Pure v) _ = Result v []
parse (Ap ff b) v = Result (bv av) (aproblems <> bproblems)
  where
    Result av aproblems = parseF ff v
    Result bv bproblems = parse b v
