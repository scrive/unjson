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
import Control.Applicative.Free

class Unjson a where
  unjson :: UnjsonX a

data UnjsonX' a
  = Field Text.Text
  | FieldOpt Text.Text
  deriving (Eq, Ord, Show, Typeable)

type UnjsonX = Ap UnjsonX'

field :: Text.Text -> UnjsonX a
field name = liftAp (Field name)

fieldOpt :: Text.Text -> UnjsonX (Maybe a)
fieldOpt name = liftAp (FieldOpt name)
