module Test where

import System.Exit (exitFailure)

import qualified Data.Text as Text
import Data.Typeable
import Data.Unjson
import Control.Applicative
import qualified Data.Aeson as Aeson
import Data.Aeson ((.=))
import Data.Scientific
import Data.Attoparsec.Number
import Data.Monoid
import System.IO
import Control.Applicative.Free
import Control.Exception
import Test.HUnit

default (Text.Text)

-- As an example we will use a hypothetical configuration data.
-- There are some mandatory fields and some optional fields.
data Konfig =
     Konfig { konfigHostname    :: Text.Text
            , konfigPort        :: Int
            , konfigCredentials :: Credentials
            , konfigComment     :: Maybe Text.Text
            , konfigAlternates  :: Maybe [Text.Text]
            }
  deriving (Eq,Ord,Show,Typeable)

data Credentials =
     Credentials { credentialsUsername :: Text.Text
                 , credentialsPassword :: Text.Text
                 }
  deriving (Eq,Ord,Show,Typeable)

unjsonKonfig :: Ap FieldDef Konfig
unjsonKonfig = pure Konfig
           <*> field' "hostname"
                 "The hostname this service is visible as"
           <*> fieldDef' "port" 80
                 "Port to listen on"
           <*> fieldBy "credentials" unjsonCredentials
           <*> fieldOpt' "comment"
                 "Optional comment, free text"
           <*> fieldOptBy "alternates" arrayOf'

unjsonCredentials :: Ap FieldDef Credentials
unjsonCredentials = pure Credentials
                    <*> field' "username"
                          "Name of the user"
                    <*> field' "password"
                          "Password for the user"

instance Unjson Credentials where
  valueDef = toValueDef unjsonCredentials

test1 :: Test
test1 = "Proper parsing of a complex structure" ~: do
  let json = Aeson.object
               [ "hostname" .= ("www.example.com" :: Text.Text)
               , "port" .= (12345 :: Int)
               , "comment" .= ("nice server" :: Text.Text)
               , "credentials" .= Aeson.object
                   [ "username" .= ("usr1" :: Text.Text)
                   , "password" .= ("pass1" :: Text.Text)
                   ]
               ]
  let expect = Konfig
               { konfigHostname = "www.example.com"
               , konfigPort = 12345
               , konfigComment = Just "nice server"
               , konfigCredentials = Credentials "usr1" "pass1"
               , konfigAlternates = Nothing
               }

  let Result val iss = parse unjsonKonfig (Anchored [] json)
  assertEqual "There are no issues in parsing" [] iss
  assertEqual "Value parsed is the one expected" expect val
  return ()

tests = test [test1]

main = runTestTT tests
