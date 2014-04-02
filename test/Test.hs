module Main where

import System.Exit (exitFailure)

import qualified Data.Text as Text
import Data.Typeable
import Data.Unjson
import Control.Applicative
import qualified Data.Aeson as Aeson
import Data.Scientific
import Data.Attoparsec.Number
import Data.Monoid
import System.IO
import Control.Applicative.Free

-- As an example we will use a hypothetical configuration data.
-- There are some mandatory fields and some optional fields.
data Konfig =
     Konfig { konfigHostname    :: Text.Text
            , konfigPort        :: Int
            , konfigCredentials :: Credentials
            , konfigCredentials2 :: Credentials
            , konfigComment     :: Maybe Text.Text
            , konfigAlternates  :: Maybe [Text.Text]
            , konfigAlternates2 :: Maybe [Text.Text]
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
           <*> field "credentials2"
           <*> fieldOpt' "comment"
                 "Optional comment, free text"
           <*> fieldOptBy "alternates" arrayOf'
           <*> fieldOpt "alternates"

unjsonCredentials :: Ap FieldDef Credentials
unjsonCredentials = pure Credentials
                    <*> field' "username"
                          "Name of the user"
                    <*> field' "password"
                          "Password for the user"

instance Unjson Credentials where
  valueDef = toValueDef unjsonCredentials

json1 :: Aeson.Value
Just json1 = Aeson.decode "{\"hostname\": \"www.example.com\", \"arr\": [\"1\",\"2\"], \"port\": 12345, \"comment\": \"nice server\", \"credentials\": { \"username\": \"usr1\", \"password\": \"pass1\" } }"

json2 :: Aeson.Value
Just json2 = Aeson.decode "{\"hostname\": \"www.example.com\", \"port\": 12345 }"

json3 :: Aeson.Value
Just json3 = Aeson.decode "{\"hostname\": \"www.example.com\" }"

issues (Result _ is) = is

main = do

    --print (document unjsonKonfig)
    print (parse (field "arr") (Anchored [] json1) :: Result (Text.Text,Text.Text))
    print (parse unjsonKonfig (Anchored [] json1))
    print (issues (parse unjsonKonfig (Anchored [] json2)))
    print (parse unjsonKonfig (Anchored [] json2))
    let g = (show (parse unjsonKonfig (Anchored [] json3)))
    mapM_ (\c -> putStr [c] >> hFlush stdout) g
    print (fmap konfigHostname $ parse unjsonKonfig (Anchored [] json3))
    putStrLn "This test always fails!"
    exitFailure
