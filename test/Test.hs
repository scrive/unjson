module Main where

import qualified Data.Text as Text
import Data.Typeable
import Data.Unjson
import Control.Applicative
import qualified Data.Aeson as Aeson
import Data.Aeson ((.=))
import Control.Exception
import Test.HUnit
import Data.Monoid
import Data.List

default (Text.Text, String, Int, Double)

-- As an example we will use a hypothetical configuration data.
-- There are some mandatory fields and some optional fields.
data Konfig =
     Konfig { konfigHostname    :: Text.Text
            , konfigPort        :: Int
            , konfigCredentials :: Credentials
            , konfigComment     :: Maybe Text.Text
            , konfigOptions     :: [Text.Text]
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

unjsonKonfig :: UnjsonDef Konfig
unjsonKonfig = objectOf $ pure Konfig
           <*> field' "hostname"
                 konfigHostname
                 "The hostname this service is visible as"
           <*> fieldDef' "port" 80
                 konfigPort
                 "Port to listen on, defaults to 80"
           <*> fieldBy "credentials"
                 konfigCredentials
                 "User admin credentials"
                 unjsonCredentials
           <*> fieldOpt' "comment"
                 konfigComment
                 "Optional comment, free text"
           <*> fieldDef "options" []
                 konfigOptions
                 "Additional options, defaults to empty"
           <*> fieldOpt "alternates"
                 konfigAlternates
                 "Alternate names for this server"

unjsonCredentials :: UnjsonDef Credentials
unjsonCredentials = objectOf $ pure Credentials
                    <*> field' "username"
                          credentialsUsername
                          "Name of the user"
                    <*> field' "password"
                          credentialsPassword
                          "Password for the user"
                    <*> fieldOpt' "domain"
                          credentialsDomain
                          "Domain for user credentials"


unjsonExtendedTest :: UnjsonDef ExtendedTest
unjsonExtendedTest = objectOf $ pure ExtendedTest
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
  unjsonDef = unjsonCredentials

test_proper_parse :: Test
test_proper_parse = "Proper parsing of a complex structure" ~: do
  let json = Aeson.object
               [ "hostname" .= "www.example.com"
               , "comment" .= "nice server"
               , "credentials" .= Aeson.object
                   [ "username" .= "usr1"
                   , "password" .= "pass1"
                   ]
               ]
  let expect = Konfig
               { konfigHostname = "www.example.com"
               , konfigPort = 80
               , konfigComment = Just "nice server"
               , konfigCredentials = Credentials "usr1" "pass1" Nothing
               , konfigAlternates = Nothing
               , konfigOptions = []
               }

  let Result val iss = parse unjsonKonfig (Anchored mempty json)
  assertEqual "There are no issues in parsing" [] iss
  assertEqual "Value parsed is the one expected" expect val
  return ()

test_missing_key :: Test
test_missing_key = "Key missing" ~: do
  let json = Aeson.object
               [ "hostname" .= "www.example.com"
               , "port" .= 12345
               , "comment" .= "nice server"
               , "credentials" .= Aeson.object
                   [ "username" .= "usr1"
                   ]
               ]

  let Result val iss = parse unjsonKonfig (Anchored mempty json)
  assertEqual "There is one issue in parsing" [Anchored (Path [ PathElemKey "credentials"
                                                              , PathElemKey "password"
                                                              ]) "missing key"] iss
  assertEqual "Value is accesible in parsed parts" "usr1" (credentialsUsername (konfigCredentials val))
  ((credentialsPassword (konfigCredentials val) `seq` return False) `catch` \(Anchored _ (t :: Text.Text)) -> return True) @? "Evaluating not parsed parts throws exception"
  return ()

test_wrong_value_type :: Test
test_wrong_value_type = "Value at key is wrong type" ~: do
  let json = Aeson.object
               [ "hostname" .= 12345
               , "port" .= Aeson.object
                   [ "username" .= "usr1"
                   ]
               , "credentials" .= "www.example.com"
               ]

  let Result val iss = parse unjsonKonfig (Anchored mempty json)
  assertEqual "Number of issues in parsing" 3 (length iss)
  assertEqual "Hostname must be string error info is present"
                (Anchored (Path [ PathElemKey "hostname"
                                ]) "when expecting a Text, encountered Number instead") (iss!!0)
  assertEqual "Port must be number error info is present"
                (Anchored (Path [ PathElemKey "port"
                                ]) "when expecting a Integral, encountered Object instead") (iss!!1)
  assertEqual "Credentials must be object error info is present"
                (Anchored (Path [ PathElemKey "credentials"
                                ]) "when expecting a HashMap Text a, encountered String instead") (iss!!2)
  return ()

test_tuple_parsing :: Test
test_tuple_parsing = "Tuple parsing" ~: do
  let json = Aeson.toJSON
               [ ("hostname" :: Aeson.Value)
               , ("port" :: Aeson.Value)
               , (Aeson.toJSON 123)
               ]

  let Result (val1 :: String, val2 :: Text.Text, val3 ::Int) iss = parse unjsonDef (Anchored mempty json)
  assertEqual "Number of issues in parsing" [] iss
  assertEqual "First element of tuple" "hostname" val1
  assertEqual "Second element of tuple" "port" val2
  assertEqual "Third element of tuple" 123 val3

  let Result (xval1 :: String, xval2 :: Text.Text, xval3 :: Int, xval4 :: Int) iss = parse unjsonDef (Anchored mempty json)
  assertEqual "Issue in parsing" [Anchored (Path [PathElemIndex 3]) "missing key"
                                 ,Anchored mempty "cannot parse array of length 3 into tuple of size 4"] iss

  ((xval4 `seq` return False) `catch` \(Anchored _ (t :: Text.Text)) -> return True) @? "Evaluating not parsed parts throws exception"

  let Result (yval1 :: Int, yval2 :: Int, yval3 :: Text.Text) iss = parse unjsonDef (Anchored mempty json)
  assertEqual "Issues in parsing"
                [ Anchored (Path [PathElemIndex 0]) "when expecting a Integral, encountered String instead"
                , Anchored (Path [PathElemIndex 1]) "when expecting a Integral, encountered String instead"
                , Anchored (Path [PathElemIndex 2]) "when expecting a Text, encountered Number instead"
                ] iss

  let Result (zval1 :: String, zval2 :: Text.Text) iss = parse unjsonDef (Anchored mempty json)
  assertEqual "Array too long for 2-tuple" [Anchored mempty "cannot parse array of length 3 into tuple of size 2"] iss

  return ()

test_symmetry_of_serialization :: Test
test_symmetry_of_serialization = "Key missing" ~: do
  let expect = Konfig
               { konfigHostname = "www.example.com"
               , konfigPort = 12345
               , konfigComment = Just "nice server"
               , konfigCredentials = Credentials "usr1" "pass1" Nothing
               , konfigAlternates = Nothing
               , konfigOptions = []
               }

  let json = serialize unjsonKonfig expect
  let Result val iss = parse unjsonKonfig (Anchored mempty json)
  assertEqual "Serialize-parse produces no problems" expect val
  assertEqual "Serialize-parse is identity" expect val
  return ()

test_parse_either_field :: Test
test_parse_either_field = "test_parse_either_field" ~: do
  do
    let json = Aeson.object
                 [ "numerical_value" .= 12345
                 ]
    let Result val iss = parse unjsonExtendedTest (Anchored mempty json)
    assertEqual "No problems" [] iss
    assertEqual "Just numerical_value present" (Left 12345) (extendedTestEither val)
  do
    let json = Aeson.object
                 [ "text_value" .= "asfsdfaf"
                 ]
    let Result val iss = parse unjsonExtendedTest (Anchored mempty json)
    assertEqual "No problems" [] iss
    assertEqual "Just text_value present" (Right "asfsdfaf") (extendedTestEither val)
  do
    let json = Aeson.object
                 [ "text_value" .= False
                 , "numerical_value" .= 12345
                 ]
    let Result val iss = parse unjsonExtendedTest (Anchored mempty json)
    assertEqual "Problem when text_value is not text" [Anchored (Path [PathElemKey "text_value"]) "when expecting a Text, encountered Boolean instead"] iss
    assertEqual "Returns numerical_value" (Left 12345) (extendedTestEither val)
  do
    let json = Aeson.object
                 [ "text_value" .= "asfsdfaf"
                 , "numerical_value" .= 12345
                 ]
    let Result val iss = parse unjsonExtendedTest (Anchored mempty json)
    assertEqual "No problems" [] iss
    assertEqual "Returns numerical_value" (Left 12345) (extendedTestEither val)
  {-
    This is not yet working as disjoint unions need special support that is not avialable yet
  do
    let json = Aeson.object []
    let Result val iss = parse unjsonExtendedTest (Anchored mempty json)
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
               , konfigOptions = []
               }
  let expect = Konfig
               { konfigHostname = "www.example.com"
               , konfigPort = 999
               , konfigComment = Just "a better server"
               , konfigCredentials = Credentials "usr2" "pass1" (Just "domain")
               , konfigAlternates = Nothing
               , konfigOptions = []
               }

  let json = Aeson.object
               [ "hostname" .= "www.example.com"     -- mandatory field
               , "port" .= 999                       -- optional with default
               , "comment" .= "a better server"      -- optional field
               , "credentials" .= Aeson.object
                               [ "domain" .= "domain"
                               , "username" .= "usr2" ]
               ]
  let Result val iss = update initial unjsonKonfig (Anchored mempty json)
  assertEqual "No problems" [] iss
  assertEqual "Object updated with json" expect val
  return ()

test_update_from_serialization_with_reset_to_default :: Test
test_update_from_serialization_with_reset_to_default = "test_update_from_serialization_with_reset_to_default" ~: do
  let initial = Konfig
               { konfigHostname = "old-www.server.com"
               , konfigPort = 12345
               , konfigComment = Just "nice server"
               , konfigCredentials = Credentials "usr1" "pass1" (Just "domain")
               , konfigAlternates = Nothing
               , konfigOptions = []
               }
  let expect = Konfig
               { konfigHostname = "www.example.com"
               , konfigPort = 80
               , konfigComment = Nothing
               , konfigCredentials = Credentials "usr1" "pass1" (Nothing)
               , konfigAlternates = Just ("abc", Credentials "usrx" "passx" Nothing)
               , konfigOptions = []
               }

  let json = Aeson.object
               [ "hostname" .= Aeson.Null     -- mandatory field
               , "port" .= Aeson.Null         -- optional with default
               , "comment" .= Aeson.Null      -- optional field
               , "credentials" .= Aeson.object
                               [ "domain" .= Aeson.Null ]
               , "alternates" .= [ Aeson.toJSON "abc"
                                 , Aeson.object
                                   [ "username" .= "usrx"
                                   , "password" .= "passx"
                                   ]
                                 ]
               ]
  let Result val iss = update initial unjsonKonfig (Anchored mempty json)
  assertEqual "Cannot reset mangatory field without default"
                [Anchored (Path [PathElemKey "hostname"]) "when expecting a Text, encountered Null instead"] iss
  assertEqual "Can reset value with default" (konfigPort expect) (konfigPort val)
  assertEqual "Can reset optional value" (konfigComment expect) (konfigComment val)
  return ()

test_array_modes :: Test
test_array_modes = "test_array_modes" ~: do

  let json = Aeson.object
               [ "hostname" .= ("www.example.com" ::Text.Text)
               ]
  let json1 = Aeson.object
               [ "hostname" .= ["www.example.com" ::Text.Text]
               ]
  let p0 :: UnjsonDef [Text.Text]
      p0 = objectOf $ pure id
         <*> fieldBy "hostname" id
                 "Single value or array"
                 (arrayOf')
  let p1 :: UnjsonDef [Text.Text]
      p1 = objectOf $ pure id
         <*> fieldBy "hostname" id
                 "Single value or array"
                 (arrayWithModeOf ArrayModeParseSingle liftAeson)
  let p2 :: UnjsonDef [Text.Text]
      p2 = objectOf $ pure id
         <*> fieldBy "hostname" id
                 "Single value or array"
                 (arrayWithModeOf' ArrayModeParseAndOutputSingle)
  let Result val0 iss0 = parse p0 (Anchored mempty json)
  assertEqual "Does not parse value in strict array mode" [Anchored (Path [PathElemKey "hostname"]) "when expecting a Vector a, encountered String instead"] iss0
  let Result val1 iss1 = parse p1 (Anchored mempty json)
  assertEqual "No problems" [] iss1
  assertEqual "Accepts singel value in ArrayModeParseSingle" ["www.example.com"] val1
  let sjson1 = serialize p1 val1
  assertEqual "Same json" json1 sjson1

  let Result val2 iss2 = parse p2 (Anchored mempty json)
  assertEqual "No problems" [] iss2
  assertEqual "Array fetch produced result" ["www.example.com"] val2
  let sjson2 = serialize p2 val2
  assertEqual "Same json" json sjson2
  return ()

test_array_update_by_primary_key :: Test
test_array_update_by_primary_key = "test_array_update_by_primary_key" ~: do

  let json = Aeson.object
               [ "array" .= [ Aeson.object
                              [ "id" .= 12
                              , "value" .= "for 12"
                              ]
                            , Aeson.object
                              [ "id" .= 17
                              , "value" .= "for 17"
                              ]
                            , Aeson.object
                              [ "id" .= 3
                              , "value" .= "for 3"
                              ]
                            , Aeson.object
                              [ "id" .= 17
                              , "value" .= "wrong value for 17"
                              ]
                            ]
               ]
  let json1 = Aeson.object
               [ "array" .= [ Aeson.object  -- 17 is first now, value left intact
                              [ "id" .= 17
                              ]
                            , Aeson.object       -- 3 is not there, but 4 is new
                              [ "id" .= 4
                              , "value" .= "for 4"
                              ]
                            , Aeson.object
                              [ "id" .= 12 -- 12 got new value
                              , "value" .= "for 12 new value"
                              ]
                            ]
               ]
  let unjsonPair = objectOf $ pure (,)
         <*> field "id"
               fst
               "Unique id"
         <*> field "value"
               snd
               "Value"
  let pk1 = fst
      pk2 = objectOf $ field "id" id "Unique id"
  let p0 :: UnjsonDef [(Int,Text.Text)]
      p0 = objectOf $ pure id
         <*> fieldBy "array"
                 id
                 "Array updated by primary key"
                 (arrayWithPrimaryKeyOf pk1 pk2 unjsonPair)
  let Result val0 iss0 = parse p0 (Anchored mempty json)
  assertEqual "No problems" [] iss0
  assertEqual "Parsing keeps proper order" [(12,"for 12"),(17,"for 17"),(3,"for 3"),(17,"wrong value for 17")] val0
  let Result val1 iss1 = update val0 p0 (Anchored mempty json1)
  assertEqual "No problems" [] iss1
  assertEqual "Update keeps proper order" [(17,"for 17"),(4,"for 4"),(12,"for 12 new value")] val1
  return ()

tests :: Test
tests = test [ test_proper_parse
             , test_missing_key
             , test_wrong_value_type
             , test_tuple_parsing
             , test_symmetry_of_serialization
             , test_parse_either_field
             , test_update_from_serialization
             , test_update_from_serialization_with_reset_to_default
             , test_array_modes
             , test_array_update_by_primary_key
             ]

main :: IO Counts
main = runTestTT tests

updateExampleRendering :: IO ()
updateExampleRendering = do
  contents <- readFile "src/Data/Unjson.hs"
  let (before,exampleAndRest) = break (=="-- Example rendering:") (lines contents)
      (example,after) = break ("render ::" `isPrefixOf`) exampleAndRest
  _ <- return $! length after
  writeFile "src/Data/Unjson.hs"
     (unlines (before ++ ["-- Example rendering:", "--"] ++
               (map ((++)"-- > ") $ lines $ filterOutAnsi $ render unjsonKonfig) ++
               after))

filterOutAnsi :: String -> String
filterOutAnsi "" = ""
filterOutAnsi ('\ESC' : '[' : rest) = filterOutAnsiTillEndOfMulticharSequence rest
filterOutAnsi ('\ESC' : _ : rest) = filterOutAnsi rest
filterOutAnsi (c : rest)  = c : filterOutAnsi rest

filterOutAnsiTillEndOfMulticharSequence :: String -> String
filterOutAnsiTillEndOfMulticharSequence (c : rest) | c >= '@' = filterOutAnsi rest
filterOutAnsiTillEndOfMulticharSequence (c : rest) =
  filterOutAnsiTillEndOfMulticharSequence rest
filterOutAnsiTillEndOfMulticharSequence [] = []
