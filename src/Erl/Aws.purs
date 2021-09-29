module Erl.Aws
  ( InstanceDescription(..)
  , describeInstances
  ) where

import Prelude
import Control.Monad.Except (ExceptT(..))
import Data.Either (Either)
import Data.Identity (Identity(..))
import Data.Generic.Rep (class Generic)
import Data.List.NonEmpty (NonEmptyList(..))
import Data.Show.Generic (genericShow)
import Erl.Data.List (List)
import Foreign (Foreign, ForeignError(..))
import Simple.JSON (class ReadForeign, readImpl, readJSON)
import Unsafe.Coerce (unsafeCoerce)

newtype InstanceDescription
  = InstanceDescription
  { instanceId :: String
  , instanceType :: String
  , imageId :: String
  }

derive instance eqInstanceDescription :: Eq InstanceDescription

derive instance genericInstanceDescription :: Generic InstanceDescription _

instance showInstanceDescription :: Show InstanceDescription where
  show = genericShow

type InstanceDescriptionInt
  = { "InstanceId" :: String
    , "InstanceType" :: String
    , "ImageId" :: String
    }

renameInstanceDescriptionInt :: InstanceDescriptionInt -> InstanceDescription
renameInstanceDescriptionInt
  { "InstanceId": instanceId
  , "InstanceType": instanceType
  , "ImageId": imageId
  } = InstanceDescription { instanceId, instanceType, imageId }

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
