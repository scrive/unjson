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

-- As an example we will use a hypothetical configuration data.
-- There are some mandatory fields and some optional fields.
data Konfig =
     Konfig { konfigHostname :: Text.Text
            , konfigPort     :: Int
            , konfigUsername :: Maybe Text.Text
            }
  deriving (Eq,Ord,Show,Typeable)


gText (Aeson.String s) = Result s []
gText _ = Result msg [msg]
  where
    msg = "trying to get a string from something that is not a string, sorry"

gInt (Aeson.Number v) | base10Exponent v >= 0 = Result (fromIntegral $ coefficient v) []
gInt (Aeson.Number v) = Result (error msg) [Text.pack msg]
  where msg = "number is not integer: " <> show v <> ", base10Exponent=" <> show (base10Exponent v)
gInt _ = Result (error msg) [msg]
  where
    msg = "trying to get a string from something that is not a number, sorry"

unjsonKonfig = pure Konfig
           <*> fieldBy gText "hostname" "docstring for hostname"
           <*> fieldBy gInt "port" "docstring for port"
           <*> fieldOptBy gText "username" "docstring for username"

parsedKonfig = parse unjsonKonfig undefined

json1 :: Aeson.Value
Just json1 = Aeson.decode "{\"hostname\": \"www.example.com\", \"port\": 12345, \"username\": \"user1\"}"

json2 :: Aeson.Value
Just json2 = Aeson.decode "{\"hostname\": \"www.example.com\", \"port\": 12345 }"

json3 :: Aeson.Value
Just json3 = Aeson.decode "{\"hostname\": \"www.example.com\" }"

main = do
    print (document unjsonKonfig)
    print (parse unjsonKonfig json1)
    print (parse unjsonKonfig json2)
    let g = (show (parse unjsonKonfig json3))
    mapM_ (\c -> putStr [c] >> hFlush stdout) g
    print (fmap konfigHostname $ parse unjsonKonfig json3)
    putStrLn "This test always fails!"
    exitFailure
