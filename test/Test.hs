module Main where

import qualified Data.Text as Text
import qualified Data.ByteString.Lazy.Char8 as BSL
import Data.Typeable
import Data.Unjson
import Control.Applicative
import qualified Data.Aeson as Aeson
import Data.Aeson ((.=))
import Control.Exception
import Test.HUnit
import Data.Monoid
import Data.List
import Data.Data
import Data.Functor.Invariant
import qualified Data.HashMap.Strict as HashMap
import qualified Data.HashMap.Lazy as LazyHashMap
import qualified Data.Map as Map

default (Text.Text, String, Int, Double)

-- As an example we will use a hypothetical configuration data.
-- There are some mandatory fields and some optional fields.
data Konfig =
     Konfig { konfigHostname    :: Text.Text
            , konfigPort        :: Integer
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


unjsonKonfig :: UnjsonDef Konfig
unjsonKonfig = objectOf $ pure Konfig
           <*> field "hostname"
                 konfigHostname
                 "The hostname this service is visible as"
           <*> fieldDef "port" 80
                 konfigPort
                 "Port to listen on, defaults to 80"
           <*> fieldBy "credentials"
                 konfigCredentials
                 "User admin credentials"
                 unjsonCredentials
           <*> fieldOpt "comment"
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
                    <*> field "username"
                          credentialsUsername
                          "Name of the user"
                    <*> field "password"
                          credentialsPassword
                          "Password for the user"
                    <*> fieldOpt "domain"
                          credentialsDomain
                          "Domain for user credentials"



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

  let Result val iss = parse unjsonKonfig json
  assertEqual "There are no issues in parsing" [] iss
  assertEqual "Value parsed is the one expected" expect val
  return ()

test_missing_key :: Test
test_missing_key = "Key missing" ~: do

  -- Note:
  --
  -- This test is strange with respect to what is returned as
  -- exceptions. We would expect to have whole path to problematic
  -- place returned, for some reason only last part of the path is
  -- returned. This is good enough to keep this mechanism in place but
  -- overall it is unknown why not everything is returned.
  --
  -- Underlying mechanism is supposed to use mapException inside of
  -- mapException and that should stack them on top concatenating the
  -- path. Tests with external setup prove this is really the case,
  -- but for some reason does not happen in general scenario.
  --
  -- *Data.Unjson> resultPrependKey "a" (resultPrependKey "b" (fail "d" :: Result ()))
  -- Result *** Exception: a.b: "d"
  --
  let json1 = Aeson.object
              [ "hostname" .= "www.example.com"
              , "port" .= 12345
              , "comment" .= "nice server"
              , "credentials" .= Aeson.object
                                [ "username" .= "usr1"
                                ]
              ]
  let json = Aeson.object
               [ "payload" .= json1
               ]
  let unjsonEnvelope :: UnjsonDef Konfig
      unjsonEnvelope = objectOf $ pure id
                       <*> fieldBy "payload"
                           id
                           "Enveloped Konfig"
                           unjsonKonfig
  do
       let Result val iss = parse unjsonEnvelope json
       assertEqual "There is one issue in parsing" [Anchored (Path [ PathElemKey "payload"
                                                                   , PathElemKey "credentials"
                                                                   , PathElemKey "password"
                                                                   ]) "missing key"] iss
       assertEqual "Value is accessible in parsed parts" "usr1" (credentialsUsername (konfigCredentials val))
       catch
         (do
             _ <- return $! credentialsPassword (konfigCredentials val)
             assertFailure "Should have thrown an exception")
         (\(Anchored path (msg :: Text.Text)) -> do
             assertEqual "Path to problematic key"
                           (Path [ PathElemKey "password"
                                 ]) path
             assertEqual "Message about the problem" (Text.pack "missing key") msg)
       return ()

  do
       let Result val iss = parse unjsonKonfig json1
       assertEqual "There is one issue in parsing" [Anchored (Path [ PathElemKey "credentials"
                                                                   , PathElemKey "password"
                                                                   ]) "missing key"] iss
       assertEqual "Value is accessible in parsed parts" "usr1" (credentialsUsername (konfigCredentials val))
       catch
         (do
             _ <- return $! credentialsPassword (konfigCredentials val)
             assertFailure "Should have thrown an exception")
         (\(Anchored path (msg :: Text.Text)) -> do
             assertEqual "Path to problematic key"
                           (Path [ PathElemKey "password"
                                 ]) path
             assertEqual "Message about the problem" (Text.pack "missing key") msg)
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

  let Result val iss = parse unjsonKonfig json
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

  let Result (val1 :: String, val2 :: Text.Text, val3 ::Integer) iss = parse unjsonDef json
  assertEqual "Number of issues in parsing" [] iss
  assertEqual "First element of tuple" "hostname" val1
  assertEqual "Second element of tuple" "port" val2
  assertEqual "Third element of tuple" 123 val3

  let Result (xval1 :: String, xval2 :: Text.Text, xval3 :: Int, xval4 :: Int) iss = parse unjsonDef json
  assertEqual "Issue in parsing" [Anchored mempty "cannot parse array of length 3 into tuple of size 4"
                                 ,Anchored (Path [PathElemIndex 3]) "missing key"] iss

  catch
    (do
        _ <- return $! xval4
        assertFailure "Should have thrown an exception")
    (\(Anchored path (msg :: Text.Text)) -> do
        assertEqual "Path to problematic key"
                      (Path [PathElemIndex 3]) path
        assertEqual "Message about the problem" (Text.pack "missing key") msg)

  let Result (yval1 :: Integer, yval2 :: Integer, yval3 :: Text.Text) iss = parse unjsonDef json
  assertEqual "Issues in parsing"
                [ Anchored (Path [PathElemIndex 0]) "when expecting a Integral, encountered String instead"
                , Anchored (Path [PathElemIndex 1]) "when expecting a Integral, encountered String instead"
                , Anchored (Path [PathElemIndex 2]) "when expecting a Text, encountered Number instead"
                ] iss

  let Result (zval1 :: String, zval2 :: Text.Text) iss = parse unjsonDef json
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

  let json = unjsonToJSON unjsonKonfig expect
  let Result val iss = parse unjsonKonfig json
  assertEqual "Serialize-parse produces no problems" expect val
  assertEqual "Serialize-parse is identity" expect val
  return ()

test_pretty_serialization :: Test
test_pretty_serialization = "Pretty serialization" ~: do
  let konfig = Konfig
               { konfigHostname = "www.example.com"
               , konfigPort = 12345
               , konfigComment = Just "nice server"
               , konfigCredentials = Credentials "usr1" "pass1" Nothing
               , konfigAlternates = Nothing
               , konfigOptions = []
               }

  let jsonstr = BSL.unpack $ unjsonToByteStringLazy' (Options { nulls = False, indent = 4, pretty = True }) unjsonKonfig konfig
  let expect = intercalate "\n"
        [ "{"
        , "    \"hostname\": \"www.example.com\","
        , "    \"port\": 12345,"
        , "    \"credentials\": {"
        , "        \"username\": \"usr1\","
        , "        \"password\": \"pass1\""
        , "    },"
        , "    \"comment\": \"nice server\","
        , "    \"options\": []"
        , "}"
        ]
  assertEqual "Serialize pretty prints proper indents" expect jsonstr
  let jsonstr5 = BSL.unpack $ unjsonToByteStringLazy' (Options { nulls = False, indent = 5, pretty = True }) unjsonKonfig konfig
  let expect5 = intercalate "\n"
        [ "{"
        , "     \"hostname\": \"www.example.com\","
        , "     \"port\": 12345,"
        , "     \"credentials\": {"
        , "          \"username\": \"usr1\","
        , "          \"password\": \"pass1\""
        , "     },"
        , "     \"comment\": \"nice server\","
        , "     \"options\": []"
        , "}"
        ]
  assertEqual "Serialize pretty prints proper indents" expect5 jsonstr5
  let jsonstr3 = BSL.unpack $ unjsonToByteStringLazy' (Options { nulls = False, indent = 3, pretty = False }) unjsonKonfig konfig
  let expect3 = concat
        [ "{"
        , "\"hostname\":\"www.example.com\","
        , "\"port\":12345,"
        , "\"credentials\":{"
        , "\"username\":\"usr1\","
        , "\"password\":\"pass1\""
        , "},"
        , "\"comment\":\"nice server\","
        , "\"options\":[]"
        , "}"
        ]
  assertEqual "Serialize pretty prints proper indents" expect3 jsonstr3
  return ()

test_serialize_with_nulls :: Test
test_serialize_with_nulls = "Serialize with nulls" ~: do
  let konfig = Konfig
               { konfigHostname = "www.example.com"
               , konfigPort = 12345
               , konfigComment = Just "nice server"
               , konfigCredentials = Credentials "usr1" "pass1" Nothing
               , konfigAlternates = Nothing
               , konfigOptions = []
               }

  let jsonstr = BSL.unpack $ unjsonToByteStringLazy' (Options { nulls = True, indent = 4, pretty = True }) unjsonKonfig konfig
  let expect = intercalate "\n"
        [ "{"
        , "    \"hostname\": \"www.example.com\","
        , "    \"port\": 12345,"
        , "    \"credentials\": {"
        , "        \"username\": \"usr1\","
        , "        \"password\": \"pass1\","
        , "        \"domain\": null"
        , "    },"
        , "    \"comment\": \"nice server\","
        , "    \"options\": [],"
        , "    \"alternates\": null"
        , "}"
        ]
  assertEqual "Serialize pretty prints proper indents" expect jsonstr

unjsonButThirteen :: UnjsonDef Int
unjsonButThirteen = objectOf $ pure id
    <*> fieldBy "value" id "Integer but god forbid 13" (unjsonInvmapR whenParse id $ unjsonDef)
  where
    whenParse 13 = fail "13 is a bad luck number"
    whenParse x = return x

test_semantic_errors_on_values :: Test
test_semantic_errors_on_values = "test_semantic_errors_on_values" ~: do
  do
    let json = Aeson.object
                 [ "value" .= (13 :: Int)
                 ]
    let Result _val iss = parse unjsonButThirteen json
    assertEqual "Problem is reported" [Anchored (Path [PathElemKey "value"]) "13 is a bad luck number"] iss
    -- assertEqual "Just numerical_value present" (13) val

unjsonEitherIntText :: UnjsonDef (Either Int Text.Text)
unjsonEitherIntText = disjointUnionOf "mode"
                     [ ("number", unjsonIsConstrByName "Left",
                        pure Left
                          <*> field "numerical_value"
                          fromLeft
                          "Numerical value")
                     , ("text", unjsonIsConstrByName "Right",
                        pure Right
                          <*> field "text_value"
                          fromRight
                          "Text value")]
  where fromLeft ~(Left x) = x
        fromRight ~(Right x) = x


test_parse_either_field :: Test
test_parse_either_field = "test_parse_either_field" ~: do
  do
    let json = Aeson.object
                 [ "mode" .= "number"
                 , "numerical_value" .= 12345
                 ]
    let Result val iss = parse unjsonEitherIntText json
    assertEqual "No problems" [] iss
    assertEqual "Just numerical_value present" (Left 12345) val
  do
    let json = Aeson.object
                 [ "mode" .= "text"
                 , "text_value" .= "asfsdfaf"
                 ]
    let Result val iss = parse unjsonEitherIntText json
    assertEqual "No problems" [] iss
    assertEqual "Just text_value present" (Right "asfsdfaf") val
  do
    let json = Aeson.object
                 [ "text_value" .= False
                 , "numerical_value" .= 12345
                 ]
    let Result val iss = parse unjsonEitherIntText json
    assertEqual "Problem when mode is missing" [Anchored (Path [PathElemKey "mode"]) "missing key"] iss
  do
    let json = Aeson.object
                 [ "mode" .= "something else"
                 ]
    let Result val iss = parse unjsonEitherIntText json
    assertEqual "Problem when mode is missing" [Anchored (Path [PathElemKey "mode"]) "value 'something else' is not one of the allowed for enumeration [number,text]"] iss
  do
    let json = Aeson.object
                 [ "mode" .= "number"
                 , "numerical_value" .= 123
                 ]
    let ex = Left 123
    let js = unjsonToJSON unjsonEitherIntText ex
    assertEqual "Serialized makes what expected" json js
  do
    let docstr = render unjsonEitherIntText
    assertBool "Documentation generates" (length docstr > 0)
  return ()

data AB = A | B
   deriving (Show, Eq, Ord)


unjsonEnumAB :: UnjsonDef AB
unjsonEnumAB = enumOf "mode"
                     [ ("A", A)
                     , ("B", B)]


test_enum_field :: Test
test_enum_field = "test_enum_field" ~: do
  do
    let json = Aeson.object
                 [ "mode" .= "A"
                 ]
    let Result val iss = parse unjsonEnumAB json
    assertEqual "No problems" [] iss
    assertEqual "Proper value present" A val
  do
    let json = Aeson.object
                 [ "mode" .= "B"
                 ]
    let Result val iss = parse unjsonEnumAB json
    assertEqual "No problems" [] iss
    assertEqual "Proper value present" B val
  do
    let json = Aeson.object
                 [ "mode" .= "wrong"
                 ]
    let Result val iss = parse unjsonEnumAB json
    assertEqual "No problems" [Anchored (Path [PathElemKey "mode"]) "value 'wrong' is not one of the allowed for enumeration [A,B]"] iss
    catch
         (do
             _ <- return $! val
             assertFailure "Should have thrown an exception")
         (\(Anchored path (msg :: Text.Text)) -> do
             assertEqual "Path to problematic key"
                           (Path [ PathElemKey "mode"
                                 ]) path
             assertEqual "Message about the problem" (Text.pack "value 'wrong' is not one of the allowed for enumeration [A,B]") msg)

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
  let Result val iss = update initial unjsonKonfig json
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
  let Result val iss = update initial unjsonKonfig json
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
                 (arrayOf unjsonDef)
  let p1 :: UnjsonDef [Text.Text]
      p1 = objectOf $ pure id
         <*> fieldBy "hostname" id
                 "Single value or array"
                 (arrayWithModeOf ArrayModeParseSingle unjsonAeson)
  let p2 :: UnjsonDef [Text.Text]
      p2 = objectOf $ pure id
         <*> fieldBy "hostname" id
                 "Single value or array"
                 (arrayWithModeOf ArrayModeParseAndOutputSingle unjsonDef)
  let Result val0 iss0 = parse p0 json
  assertEqual "Does not parse value in strict array mode" [Anchored (Path [PathElemKey "hostname"]) "when expecting a Vector a, encountered String instead"] iss0
  let Result val1 iss1 = parse p1 json
  assertEqual "No problems" [] iss1
  assertEqual "Accepts singel value in ArrayModeParseSingle" ["www.example.com"] val1
  let sjson1 = unjsonToJSON p1 val1
  assertEqual "Same json" json1 sjson1

  let Result val2 iss2 = parse p2 json
  assertEqual "No problems" [] iss2
  assertEqual "Array fetch produced result" ["www.example.com"] val2
  let sjson2 = unjsonToJSON p2 val2
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
  let Result val0 iss0 = parse p0 json
  assertEqual "No problems" [] iss0
  assertEqual "Parsing keeps proper order" [(12,"for 12"),(17,"for 17"),(3,"for 3"),(17,"wrong value for 17")] val0
  let Result val1 iss1 = update val0 p0 json1
  assertEqual "No problems" [] iss1
  assertEqual "Update keeps proper order" [(17,"for 17"),(4,"for 4"),(12,"for 12 new value")] val1
  return ()

test_maps :: Test
test_maps = "test_maps" ~: do

  let json = Aeson.object
               [ "k1" .= (12 :: Int)
               , "k2" .= (1122 :: Int)
               , "a4" .= (666 :: Int)
               ]
      jsonEmbedded = Aeson.object
                     [ "a_map" .= json ]
  let unjsonMapByInstance :: (Unjson a) => UnjsonDef a
      unjsonMapByInstance = objectOf $ pure id
         <*> field "a_map"
             id
             "The only map"
  let unjsonMapByExplicit :: (Unjson a) => UnjsonDef (HashMap.HashMap Text.Text a)
      unjsonMapByExplicit = objectOf $ pure id
         <*> fieldBy "a_map"
             id
             "The only map"
             (mapOf unjsonDef)
  let Result val0 iss0 = parse unjsonMapByInstance jsonEmbedded
  assertEqual "No problems" [] iss0
  assertEqual "Parsing keeps proper order in Data.Map" (Map.fromList [("k1"::String, 12::Int),("k2", 1122), ("a4", 666)]) val0
  let Result val1 iss1 = parse unjsonMapByInstance jsonEmbedded
  assertEqual "No problems" [] iss1
  assertEqual "Parsing keeps proper order" (HashMap.fromList [("k1"::String, 12::Int),("k2", 1122), ("a4", 666)]) val1
  let Result val2 iss2 = parse unjsonMapByExplicit jsonEmbedded
  assertEqual "No problems" [] iss2
  assertEqual "Parsing keeps proper order" (LazyHashMap.fromList [("k1"::Text.Text, 12::Int),("k2", 1122), ("a4", 666)]) val2
  return ()


data PlainUnion
  = PlainUnionA
    { plainUnionKey1 :: String
    , plainUnionKey2 :: Maybe Int
    }
  | PlainUnionB
    { plainUnionKey3 :: Int
    , plainUnionKey4 :: Int
    }
    deriving (Eq, Show, Typeable, Data)

unjsonPlainUnion :: UnjsonDef PlainUnion
unjsonPlainUnion = unionOf
                   [ (unjsonIsConstrByName "PlainUnionA",
                      pure PlainUnionA
                      <*> field "key1" plainUnionKey1 ""
                      <*> fieldOpt "key2" plainUnionKey2 "")
                   , (unjsonIsConstrByName "PlainUnionB",
                      pure PlainUnionB
                      <*> field "key3" plainUnionKey3 ""
                      <*> fieldDef "key4" 123 plainUnionKey4 "")
                   ]

test_plain_unions :: Test
test_plain_unions = "test_maps" ~: do

  -- simplest case
  let json1 = Aeson.object
               [ "key1" .= ("abc" :: String)
               ]

  let Result val1 iss1 = parse unjsonPlainUnion json1
  assertEqual "No problems" [] iss1
  assertEqual "Got expected value" (PlainUnionA "abc" Nothing) val1

  -- anyway choose first object on list, because 'key1' is present
  let json2 = Aeson.object
               [ "key1" .= ("abc" :: String)
               , "key3" .= ("abc" :: String)
               , "key4" .= ("abc" :: String)
               ]

  let Result val2 iss2 = parse unjsonPlainUnion json2
  assertEqual "No problems" [] iss2
  assertEqual "Got expected value" (PlainUnionA "abc" Nothing) val2

  -- key is present so PlainUnionA will be chosen and then fail
  -- because of wrong types
  let json3 = Aeson.object
               [ "key1" .= (123 :: Int)
               ]

  let Result val3 iss3 = parse unjsonPlainUnion json3
  assertEqual "Cannot parse PlainUnionA" [Anchored (Path [PathElemKey "key1"]) "when expecting a String, encountered Number instead"] iss3


  -- choose PlainUnionB
  let json4 = Aeson.object
               [ "xx" .= (123 :: Int)
               , "key3" .= (15523 :: Int)
               , "key4" .= (13 :: Int)
               ]

  let Result val4 iss4 = parse unjsonPlainUnion json4
  assertEqual "No issues" [] iss4
  assertEqual "Got expected value" (PlainUnionB 15523 13) val4

  return ()


tests :: Test
tests = test [ test_proper_parse
             , test_missing_key
             , test_wrong_value_type
             , test_tuple_parsing
             , test_symmetry_of_serialization
             , test_serialize_with_nulls
             , test_parse_either_field
             , test_enum_field
             , test_update_from_serialization
             , test_update_from_serialization_with_reset_to_default
             , test_array_modes
             , test_array_update_by_primary_key
             , test_pretty_serialization
             , test_semantic_errors_on_values
             , test_maps
             , test_plain_unions
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


data Example = Example
   { exampleName :: Text.Text,
     exampleArray :: [Int],
     exampleOptional :: Maybe Bool,
     exampleIntAsString :: Int }

unjsonExample :: UnjsonDef Example
unjsonExample = objectOf $ pure Example
  <*> field "name"
          exampleName
          "Name used for example"
  <*> fieldDefBy "array_of_ints" []
          exampleArray
          "Array of integers, optional, defaults to empty list"
          (arrayOf unjsonDef)
  <*> fieldOpt "optional_bool"
          exampleOptional
          "Optional boolean"
  <*> fieldBy "int_as_string"
          exampleIntAsString
          "Integer value serialized as a string value in json"
          (invmap (read :: String -> Int) (show :: Int -> String) unjsonDef)

newtype Theme = Theme { unTheme :: Int }

unjsonTheme :: UnjsonDef Theme
unjsonTheme = invmap (Theme . read :: String -> Theme) (show . unTheme :: Theme -> String) unjsonDef

instance Unjson Theme where
  unjsonDef = unjsonTheme
