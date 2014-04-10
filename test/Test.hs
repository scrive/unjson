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
           <*> fieldBy "credentials"
                 "User admin credentials"
                 (ObjectValueDef unjsonCredentials)
           <*> fieldOpt' "comment"
                 "Optional comment, free text"
           <*> fieldOptBy "alternates"
                 "Alternate names for this server"
                 arrayOf'

unjsonCredentials :: Ap FieldDef Credentials
unjsonCredentials = pure Credentials
                    <*> field' "username"
                          "Name of the user"
                    <*> field' "password"
                          "Password for the user"

instance Unjson Credentials where
  valueDef = ObjectValueDef unjsonCredentials

test_proper_parse :: Test
test_proper_parse = "Proper parsing of a complex structure" ~: do
  let json = Aeson.object
               [ "hostname" .= ("www.example.com" :: Text.Text)
               , "comment" .= ("nice server" :: Text.Text)
               , "credentials" .= Aeson.object
                   [ "username" .= ("usr1" :: Text.Text)
                   , "password" .= ("pass1" :: Text.Text)
                   ]
               ]
  let expect = Konfig
               { konfigHostname = "www.example.com"
               , konfigPort = 80
               , konfigComment = Just "nice server"
               , konfigCredentials = Credentials "usr1" "pass1"
               , konfigAlternates = Nothing
               }

  let Result val iss = parse (ObjectValueDef unjsonKonfig) (Anchored [] json)
  assertEqual "There are no issues in parsing" [] iss
  assertEqual "Value parsed is the one expected" expect val
  return ()

test_missing_key :: Test
test_missing_key = "Key missing" ~: do
  let json = Aeson.object
               [ "hostname" .= ("www.example.com" :: Text.Text)
               , "port" .= (12345 :: Int)
               , "comment" .= ("nice server" :: Text.Text)
               , "credentials" .= Aeson.object
                   [ "username" .= ("usr1" :: Text.Text)
                   ]
               ]
  let expect = Konfig
               { konfigHostname = "www.example.com"
               , konfigPort = 12345
               , konfigComment = Just "nice server"
               , konfigCredentials = Credentials "usr1" "pass1"
               , konfigAlternates = Nothing
               }

  let Result val iss = parse (ObjectValueDef unjsonKonfig) (Anchored [] json)
  assertEqual "There is one issue in parsing" [Anchored [ PathElemKey "credentials"
                                                        , PathElemKey "password"
                                                        ] "missing key"] iss
  assertEqual "Value is accesible in parsed parts" "usr1" (credentialsUsername (konfigCredentials val))
  ((credentialsPassword (konfigCredentials val) `seq` return False) `catch` \(Anchored _ (t :: Text.Text)) -> return True) @? "Evaluating not parsed parts throws exception"
  return ()

test_wrong_value_type :: Test
test_wrong_value_type = "Value at key is wrong type" ~: do
  let json = Aeson.object
               [ "hostname" .= (12345 :: Int)
               , "port" .= Aeson.object
                   [ "username" .= ("usr1" :: Text.Text)
                   ]
               , "credentials" .= ("www.example.com" :: Text.Text)
               ]

  let Result val iss = parse (ObjectValueDef unjsonKonfig) (Anchored [] json)
  assertEqual "Number of issues in parsing" 3 (length iss)
  assertEqual "Hostname must be string error info is present"
                (Anchored [ PathElemKey "hostname"
                          ] "when expecting a Text, encountered Number instead") (iss!!0)
  assertEqual "Port must be number error info is present"
                (Anchored [ PathElemKey "port"
                          ] "when expecting a Integral, encountered Object instead") (iss!!1)
  assertEqual "Credentials must be object error info is present"
                (Anchored [ PathElemKey "credentials"
                          ] "when expecting a HashMap Text a, encountered String instead") (iss!!2)
  return ()

tests = test [ test_proper_parse
             , test_missing_key
             , test_wrong_value_type
             ]

main = runTestTT tests
