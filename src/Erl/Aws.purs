module Erl.Aws
  ( InstanceDescription(..)
  , describeInstances
  ) where

import Prelude
import Control.Monad.Except (ExceptT(..), except)
import Data.Bifunctor (bimap)
import Data.DateTime (DateTime(..))
import Data.DateTime.Parsing (parseFullDateTime, toUTC)
import Data.Either (Either)
import Data.Foldable (foldl)
import Data.Generic.Rep (class Generic)
import Data.Identity (Identity(..))
import Data.List.NonEmpty (NonEmptyList(..), singleton)
import Data.Maybe (maybe)
import Data.Show.Generic (genericShow)
import Debug.Trace (spy)
import Erl.Data.List (List)
import Erl.Data.List as List
import Erl.Data.Map (Map)
import Erl.Data.Map as Map
import Foreign (Foreign, ForeignError(..), readString)
import Simple.JSON (class ReadForeign, readImpl, readJSON)
import Text.Parsing.Parser (ParserT, fail, parseErrorMessage, runParser)
import Unsafe.Coerce (unsafeCoerce)

type InstanceDescription
  = { instanceId :: String
    , instanceType :: String
    , imageId :: String
    , tags :: Map String String
    , launchTime :: DateTime
    }

type InstanceDescriptionInt
  = { "InstanceId" :: String
    , "InstanceType" :: String
    , "ImageId" :: String
    , "Tags" :: List TagInt
    , "LaunchTime" :: DateTimeInt
    }

type TagInt
  = { "Key" :: String
    , "Value" :: String
    }

tagsIntToTags :: List TagInt -> Map String String
tagsIntToTags = foldl insertTag Map.empty
  where
  insertTag acc { "Key": key, "Value": value } = Map.insert key value acc

newtype DateTimeInt
  = DateTimeInt DateTime

instance readForeignDateTimeInt :: ReadForeign DateTimeInt where
  readImpl =
    readString >=> parseFDT
    where
    parseFDT s =
      except
        $ bimap (singleton <<< ForeignError <<< parseErrorMessage) DateTimeInt (runParser s parseDateTime)

fromInstanceDescriptionInt :: InstanceDescriptionInt -> InstanceDescription
fromInstanceDescriptionInt
  { "InstanceId": instanceId
  , "InstanceType": instanceType
  , "ImageId": imageId
  , "Tags": tagsInt
  , "LaunchTime": (DateTimeInt launchTime)
  } =
  let
    _ = spy "LaunchTime" $ launchTime
  in
    { instanceId, instanceType, imageId, tags: tagsIntToTags tagsInt, launchTime }

type DescribeInstancesInt
  = { "Reservations" :: List ReservationInt
    }

type ReservationInt
  = { "Instances" :: List InstanceDescriptionInt }

bar :: DescribeInstancesInt -> List InstanceDescription
bar { "Reservations": reservations } = join $ foo <$> reservations

foo :: ReservationInt -> List InstanceDescription
foo { "Instances": instancesInt } = fromInstanceDescriptionInt <$> instancesInt

describeInstances :: String → Either (NonEmptyList ForeignError) (List InstanceDescription)
describeInstances s = do
  intRep <- readJSON s
  pure
    $ bar intRep

parseDateTime :: forall m. Monad m => ParserT String m DateTime
parseDateTime = parseFullDateTime >>= (\full -> maybe (fail "Invalid datetime offset") (pure) $ toUTC full)
