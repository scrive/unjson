module Data.Unjson
  ( Unjson(..)
  , field
  , fieldOpt
  )
where

import qualified Data.Aeson as Aeson
import qualified Data.Text as Text
import Data.Typeable
import Control.Applicative

class Unjson a where
  unjson :: UnjsonX a

data UnjsonX a = UnjsonX
  deriving (Eq, Ord, Show, Typeable)

instance Functor UnjsonX where
  fmap _f UnjsonX = UnjsonX

instance Applicative UnjsonX where
  pure _a = UnjsonX
  _a <*> _b = UnjsonX

field :: String -> UnjsonX a
field _name = UnjsonX

fieldOpt :: String -> UnjsonX (Maybe a)
fieldOpt _name = UnjsonX
