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
            , konfigAlternates  :: Maybe (Text.Text,Credentials)
            }
  deriving (Eq,Ord,Show,Typeable)

data Credentials =
     Credentials { credentialsUsername :: Text.Text
                 , credentialsPassword :: Text.Text
                 , credentialsDomain   :: Maybe Text.Text
                 }
  deriving (Eq,Ord,Show,Typeable)

data ExtendedTest =
     ExtendedTest { extendedTestEither    :: Either Int Text.Text
                  }
  deriving (Eq,Ord,Show,Typeable)

unjsonKonfig :: ValueDef Konfig
unjsonKonfig = ObjectValueDef $ pure Konfig
           <*> field' "hostname"
                 konfigHostname
                 "The hostname this service is visible as"
           <*> fieldDef' "port" 80
                 konfigPort
                 "Port to listen on"
           <*> fieldBy "credentials"
                 konfigCredentials
                 "User admin credentials"
                 unjsonCredentials
           <*> fieldOpt' "comment"
                 konfigComment
                 "Optional comment, free text"
           <*> fieldOpt "alternates"
                 konfigAlternates
                 "Alternate names for this server"

unjsonCredentials :: ValueDef Credentials
unjsonCredentials = ObjectValueDef $ pure Credentials
                    <*> field' "username"
                          credentialsUsername
                          "Name of the user"
                    <*> field' "password"
                          credentialsPassword
                          "Password for the user"
                    <*> fieldOpt' "domain"
                          credentialsDomain
                          "Domain for user credentials"


unjsonExtendedTest :: ValueDef ExtendedTest
unjsonExtendedTest = ObjectValueDef $ pure ExtendedTest
                    <*> (pure maybeMaybeToEither
                          <*> fieldOpt' "numerical_value"
                                (either Just (const Nothing) . extendedTestEither)
                                "Numerical value"
                          <*> fieldOpt' "text_value"
                                (either (const Nothing) Just . extendedTestEither)
                                "Text value")
  where
    maybeMaybeToEither (Just x) _ = Left x
    maybeMaybeToEither _ (Just x) = Right x
    maybeMaybeToEither _ _ = error "Disjoint unions need special support that is not available yet"

instance Unjson Credentials where
  valueDef = unjsonCredentials

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
               , konfigCredentials = Credentials "usr1" "pass1" Nothing
               , konfigAlternates = Nothing
               }

  let Result val iss = parse unjsonKonfig (Anchored [] json)
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
               , konfigCredentials = Credentials "usr1" "pass1" Nothing
               , konfigAlternates = Nothing
               }

  let Result val iss = parse unjsonKonfig (Anchored [] json)
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

  let Result val iss = parse unjsonKonfig (Anchored [] json)
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

test_tuple_parsing :: Test
test_tuple_parsing = "Tuple parsing" ~: do
  let json = Aeson.toJSON
               [ ("hostname" :: Aeson.Value)
               , ("port" :: Aeson.Value)
               , (Aeson.toJSON (123 :: Int))
               ]

  let Result (val1 :: String, val2 :: Text.Text, val3 ::Int) iss = parse valueDef (Anchored [] json)
  assertEqual "Number of issues in parsing" [] iss
  assertEqual "First element of tuple" "hostname" val1
  assertEqual "Second element of tuple" "port" val2
  assertEqual "Third element of tuple" 123 val3

  let Result (xval1 :: String, xval2 :: Text.Text, xval3 :: Int, xval4 :: Int) iss = parse valueDef (Anchored [] json)
  assertEqual "Issue in parsing" [Anchored [PathElemIndex 3] "missing key"
                                 ,Anchored [] "cannot parse array of length 3 into tuple of size 4"] iss

  ((xval4 `seq` return False) `catch` \(Anchored _ (t :: Text.Text)) -> return True) @? "Evaluating not parsed parts throws exception"

  let Result (yval1 :: Int, yval2 :: Int, yval3 :: Text.Text) iss = parse valueDef (Anchored [] json)
  assertEqual "Issues in parsing"
                [ Anchored [PathElemIndex 0] "when expecting a Integral, encountered String instead"
                , Anchored [PathElemIndex 1] "when expecting a Integral, encountered String instead"
                , Anchored [PathElemIndex 2] "when expecting a Text, encountered Number instead"
                ] iss

  let Result (zval1 :: String, zval2 :: Text.Text) iss = parse valueDef (Anchored [] json)
  assertEqual "Array too long for 2-tuple" [Anchored [] "cannot parse array of length 3 into tuple of size 2"] iss

  return ()

test_symmetry_of_serialization :: Test
test_symmetry_of_serialization = "Key missing" ~: do
  let expect = Konfig
               { konfigHostname = "www.example.com"
               , konfigPort = 12345
               , konfigComment = Just "nice server"
               , konfigCredentials = Credentials "usr1" "pass1" Nothing
               , konfigAlternates = Nothing
               }

  let json = serialize1 unjsonKonfig expect
  let Result val iss = parse unjsonKonfig (Anchored [] json)
  assertEqual "Serialize-parse produces no problems" expect val
  assertEqual "Serialize-parse is identity" expect val
  return ()

test_parse_either_field :: Test
test_parse_either_field = "test_parse_either_field" ~: do
  do
    let json = Aeson.object
                 [ "numerical_value" .= (12345 :: Int)
                 ]
    let Result val iss = parse unjsonExtendedTest (Anchored [] json)
    assertEqual "Serialize-parse produces no problems" [] iss
    assertEqual "Serialize-parse produces no problems" (Left 12345) (extendedTestEither val)
  do
    let json = Aeson.object
                 [ "text_value" .= ("asfsdfaf" :: Text.Text)
                 ]
    let Result val iss = parse unjsonExtendedTest (Anchored [] json)
    assertEqual "Serialize-parse produces no problems" [] iss
    assertEqual "Serialize-parse produces no problems" (Right "asfsdfaf") (extendedTestEither val)
  do
    let json = Aeson.object
                 [ "text_value" .= (False)
                 , "numerical_value" .= (12345 :: Int)
                 ]
    let Result val iss = parse unjsonExtendedTest (Anchored [] json)
    assertEqual "Serialize-parse produces no problems" [Anchored [PathElemKey "text_value"] "when expecting a Text, encountered Boolean instead"] iss
    assertEqual "Serialize-parse produces no problems" (Left 12345) (extendedTestEither val)
  do
    let json = Aeson.object
                 [ "text_value" .= ("asfsdfaf" :: Text.Text)
                 , "numerical_value" .= (12345 :: Int)
                 ]
    let Result val iss = parse unjsonExtendedTest (Anchored [] json)
    assertEqual "Serialize-parse produces no problems" [] iss
    assertEqual "Serialize-parse produces no problems" (Left 12345) (extendedTestEither val)
  {-
    This is not yet working as disjoint unions need special support that is not avialable yet
  do
    let json = Aeson.object []
    let Result val iss = parse unjsonExtendedTest (Anchored [] json)
    assertEqual "Serialize-parse produces no problems" [] iss
    assertEqual "Serialize-parse produces no problems" (Left 12345) (extendedTestEither val)
  -}
  return ()


test_update_from_serialization :: Test
test_update_from_serialization = "test_update_from_serialization" ~: do
  let initial = Konfig
               { konfigHostname = "old-www.server.com"
               , konfigPort = 12345
               , konfigComment = Just "nice server"
               , konfigCredentials = Credentials "usr1" "pass1" Nothing
               , konfigAlternates = Nothing
               }
  let expect = Konfig
               { konfigHostname = "www.example.com"
               , konfigPort = 999
               , konfigComment = Just "a better server"
               , konfigCredentials = Credentials "usr2" "pass1" (Just "domain")
               , konfigAlternates = Nothing
               }

  let json = Aeson.object
               [ "hostname" .= ("www.example.com" :: Text.Text)     -- mandatory field
               , "port" .= (999 :: Int)                             -- optional with default
               , "comment" .= ("a better server" :: Text.Text)      -- optional field
               , "credentials" .= Aeson.object
                               [ "domain" .= ("domain" :: Text.Text)
                               , "username" .= ("usr2" :: Text.Text) ]
               ]
  let Result val iss = parseUpdating unjsonKonfig initial (Anchored [] json)
  assertEqual "Serialize-parse produces no problems" [] iss
  assertEqual "Serialize-parse is identity" expect val
  return ()

test_update_from_serialization_with_reset_to_default :: Test
test_update_from_serialization_with_reset_to_default = "test_update_from_serialization_with_reset_to_default" ~: do
  let initial = Konfig
               { konfigHostname = "old-www.server.com"
               , konfigPort = 12345
               , konfigComment = Just "nice server"
               , konfigCredentials = Credentials "usr1" "pass1" (Just "domain")
               , konfigAlternates = Nothing
               }
  let expect = Konfig
               { konfigHostname = "www.example.com"
               , konfigPort = 80
               , konfigComment = Nothing
               , konfigCredentials = Credentials "usr1" "pass1" (Nothing)
               , konfigAlternates = Just ("abc", Credentials "usrx" "passx" Nothing)
               }

  let json = Aeson.object
               [ "hostname" .= Aeson.Null     -- mandatory field
               , "port" .= Aeson.Null         -- optional with default
               , "comment" .= Aeson.Null      -- optional field
               , "credentials" .= Aeson.object
                               [ "domain" .= Aeson.Null ]
               , "alternates" .= [ Aeson.toJSON ("abc" :: Text.Text)
                                 , Aeson.object
                                   [ "username" .= ("usrx" :: Text.Text)
                                   , "password" .= ("passx" :: Text.Text)
                                   ]
                                 ]
               ]
  let Result val iss = parseUpdating unjsonKonfig initial (Anchored [] json)
  assertEqual "Serialize-parse produces no problems"
                [Anchored [PathElemKey "hostname"] "when expecting a Text, encountered Null instead"] iss
  assertEqual "Serialize-parse is identity" expect (val {konfigHostname = "www.example.com"})
  return ()

tests = test [ test_proper_parse
             , test_missing_key
             , test_wrong_value_type
             , test_tuple_parsing
             , test_symmetry_of_serialization
             , test_parse_either_field
             , test_update_from_serialization
             , test_update_from_serialization_with_reset_to_default
             ]

main = runTestTT tests
