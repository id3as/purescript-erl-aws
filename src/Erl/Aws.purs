module Erl.Aws
  ( InstanceDescription(..)
  , Tags
  , describeInstances
  ) where

import Prelude
import Control.Monad.Except (ExceptT(..))
import Data.Either (Either)
import Data.Foldable (foldl)
import Data.Generic.Rep (class Generic)
import Data.Identity (Identity(..))
import Data.List.NonEmpty (NonEmptyList(..))
import Data.Show.Generic (genericShow)
import Erl.Data.List (List)
import Erl.Data.List as List
import Erl.Data.Map (Map)
import Erl.Data.Map as Map
import Foreign (Foreign, ForeignError(..))
import Simple.JSON (class ReadForeign, readImpl, readJSON)
import Unsafe.Coerce (unsafeCoerce)

newtype InstanceDescription
  = InstanceDescription
  { instanceId :: String
  , instanceType :: String
  , imageId :: String
  , tags :: Tags
  }

derive instance eqInstanceDescription :: Eq InstanceDescription

derive instance genericInstanceDescription :: Generic InstanceDescription _

instance showInstanceDescription :: Show InstanceDescription where
  show = genericShow

type InstanceDescriptionInt
  = { "InstanceId" :: String
    , "InstanceType" :: String
    , "ImageId" :: String
    , "Tags" :: List TagInt
    }

type TagInt
  = { "Key" :: String
    , "Value" :: String
    }

type Tags
  = Map String String

tagsIntToTags :: List TagInt -> Tags
tagsIntToTags = foldl insertTag Map.empty
  where
  insertTag acc { "Key": key, "Value": value } = Map.insert key value acc

renameInstanceDescriptionInt :: InstanceDescriptionInt -> InstanceDescription
renameInstanceDescriptionInt
  { "InstanceId": instanceId
  , "InstanceType": instanceType
  , "ImageId": imageId
  , "Tags": tagsInt
  } = InstanceDescription { instanceId, instanceType, imageId, tags: tagsIntToTags tagsInt }

type DescribeInstancesInt
  = { "Reservations" :: List ReservationInt
    }

type ReservationInt
  = { "Instances" :: List InstanceDescriptionInt }

bar :: DescribeInstancesInt -> List InstanceDescription
bar { "Reservations": reservations } = join $ foo <$> reservations

foo :: ReservationInt -> List InstanceDescription
foo { "Instances": instancesInt } = renameInstanceDescriptionInt <$> instancesInt

describeInstances :: String → Either (NonEmptyList ForeignError) (List InstanceDescription)
describeInstances s = do
  intRep <- readJSON s
  pure
    $ bar intRep
