module Data.Unjson
  ( fieldBy
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


data UnjsonX' a
  = Field Text.Text Text.Text (Aeson.Value -> Result a)
  deriving (Typeable, Functor)

{-
instance Functor UnjsonX' where
  fmap f (Field a b k) = Field a b ((fmap . fmap) f k)
-}

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
fieldBy f name docstring = liftAp (Field name docstring f)

documentF :: UnjsonX' a -> Documentation
documentF (Field key docstring p) = Documentation "" [(key,Documentation docstring [])]
--documentF (FieldOpt key docstring) = Documentation "" [(key,Documentation docstring [])]

document :: UnjsonX a -> Documentation
document (Pure x) = Documentation "" []
document (Ap a b) = Documentation (a1 <> b1) (a2 <> b2)
  where
    Documentation a1 a2 = documentF a
    Documentation b1 b2 = document b

parse :: UnjsonX a -> Aeson.Value -> Result a
parse (Pure v) _ = Result v []
parse (Ap (Field key _ ap) b) v = Result (bv av) (aproblems <> bproblems)
  where
    Result av aproblems = case v of
                            Aeson.Object o -> case HashMap.lookup key o of
                                                Just v2 -> ap v2
                                                Nothing -> Result (error "key does not exists in object") ["key does not exists in object"]
                            _ -> Result (error "trying to lookup a key in non-object") ["trying to lookup a key in non-object"]
    Result bv bproblems = parse b v
