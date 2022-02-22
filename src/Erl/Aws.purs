module Erl.Aws
  ( ClientToken(..)
  , ImageId(..)
  , InstanceDescription(..)
  , InstanceId(..)
  , InstanceState(..)
  , InstanceType(..)
  , KeyName(..)
  , Region(..)
  , RunningInstance
  , describeInstances
  , runInstances
  , stopInstances
  , terminateInstances
  ) where

import Prelude
import Control.Monad.Except (ExceptT(..), except, runExcept, runExceptT)
import Data.Bifunctor (bimap)
import Data.DateTime (DateTime(..))
import Data.DateTime.Parsing (parseFullDateTime, toUTC)
import Data.Either (Either(..), fromRight, note)
import Data.Foldable (foldl)
import Data.Generic.Rep (class Generic)
import Data.Identity (Identity(..))
import Data.List.NonEmpty (NonEmptyList(..), singleton)
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.Newtype (class Newtype, unwrap)
import Data.Set (fromMap)
import Data.Show.Generic (genericShow)
import Data.String.Regex.Flags (dotAll)
import Data.Traversable (traverse)
import Data.Tuple (Tuple(..))
import Debug (spy, traceM)
import Effect (Effect)
import Erl.Data.Binary.UTF8 (toBinary)
import Erl.Data.List (List)
import Erl.Data.List as List
import Erl.Data.Map (Map)
import Erl.Data.Map as Map
import Erl.Kernel.File (FileName(..), readFile)
import Erl.Kernel.Inet (Hostname)
import Erl.Kernel.Os as Os
import Foreign (F, Foreign, ForeignError(..), MultipleErrors, readString)
import Simple.JSON (class ReadForeign, class WriteForeign, E, readImpl, readJSON, readJSON', writeJSON)
import Text.Parsing.Parser (ParserT, fail, parseErrorMessage, runParser)
import Unsafe.Coerce (unsafeCoerce)

newtype InstanceId
  = InstanceId String

derive newtype instance Eq InstanceId
derive newtype instance Ord InstanceId
derive newtype instance ReadForeign InstanceId
derive newtype instance WriteForeign InstanceId
derive instance Newtype InstanceId _
derive instance Generic InstanceId _
instance Show InstanceId where
  show = genericShow

newtype InstanceType
  = InstanceType String

derive newtype instance Eq InstanceType
derive newtype instance Ord InstanceType
derive newtype instance ReadForeign InstanceType
derive newtype instance WriteForeign InstanceType
derive instance Newtype InstanceType _
derive instance Generic InstanceType _
instance Show InstanceType where
  show = genericShow

newtype ImageId
  = ImageId String

derive newtype instance Eq ImageId
derive newtype instance Ord ImageId
derive newtype instance ReadForeign ImageId
derive newtype instance WriteForeign ImageId
derive instance Newtype ImageId _
derive instance Generic ImageId _
instance Show ImageId where
  show = genericShow

newtype Region
  = Region String

derive newtype instance Eq Region
derive newtype instance Ord Region
derive newtype instance ReadForeign Region
derive newtype instance WriteForeign Region
derive instance Newtype Region _
derive instance Generic Region _
instance Show Region where
  show = genericShow

newtype KeyName
  = KeyName String

derive newtype instance Eq KeyName
derive newtype instance Ord KeyName
derive newtype instance ReadForeign KeyName
derive newtype instance WriteForeign KeyName
derive instance Newtype KeyName _
derive instance Generic KeyName _
instance Show KeyName where
  show = genericShow

newtype ClientToken
  = ClientToken String

derive newtype instance Eq ClientToken
derive newtype instance Ord ClientToken
derive newtype instance ReadForeign ClientToken
derive newtype instance WriteForeign ClientToken
derive instance Newtype ClientToken _
derive instance Generic ClientToken _
instance Show ClientToken where
  show = genericShow

type RunningInstance
  = { publicDnsName :: Maybe String
    , privateDnsName :: String
    , privateIpAddress :: String
    }

data InstanceState
  = Pending
  | Running RunningInstance
  | ShuttingDown
  | Terminated
  | Stopping
  | Stopped

type InstanceDescription
  = { instanceId :: InstanceId
    , instanceType :: InstanceType
    , imageId :: ImageId
    , tags :: Map String String
    , launchTime :: DateTime
    , state :: InstanceState
    , clientToken :: Maybe ClientToken
    }

type TagInt
  = { "Key" :: String
    , "Value" :: String
    }

tagIntsToTags :: List TagInt -> Map String String
tagIntsToTags = foldl insertTag Map.empty
  where
  insertTag acc { "Key": key, "Value": value } = Map.insert key value acc

tagsToTagInts :: Map String String -> List TagInt
tagsToTagInts tags = (\(Tuple key value) -> { "Key": key, "Value": value }) <$> Map.toUnfoldable tags

newtype DateTimeInt
  = DateTimeInt DateTime

instance readForeignDateTimeInt :: ReadForeign DateTimeInt where
  readImpl =
    readString >=> parseFDT
    where
    parseFDT s =
      except
        $ bimap (singleton <<< ForeignError <<< parseErrorMessage) DateTimeInt (runParser s parseDateTime)

parseDateTime :: forall m. Monad m => ParserT String m DateTime
parseDateTime = parseFullDateTime >>= (\full -> maybe (fail "Invalid datetime offset") (pure) $ toUTC full)

type DescribeInstancesInt
  = { "Reservations" :: List ReservationInt
    }

type ReservationInt
  = { "Instances" :: List InstanceDescriptionInt
    }

type InstanceDescriptionInt
  = { "InstanceId" :: String
    , "InstanceType" :: String
    , "ImageId" :: String
    , "Tags" :: Maybe (List TagInt)
    , "LaunchTime" :: DateTimeInt
    , "PrivateDnsName" :: Maybe String
    , "PrivateIpAddress" :: Maybe String
    , "PublicDnsName" :: Maybe String
    , "State" :: StateInt
    , "ClientToken" :: String
    }

type StateInt
  = { "Code" :: Int
    , "Name" :: String
    }

type InstanceStateChangeInt
  = { "CurrentState" :: StateInt
    , "PreviousState" :: StateInt
    , "InstanceId" :: String
    }

fromDescribeInstancesInt :: DescribeInstancesInt -> F (List InstanceDescription)
fromDescribeInstancesInt { "Reservations": reservations } = join <$> traverse fromReservationInt reservations

fromReservationInt :: ReservationInt -> F (List InstanceDescription)
fromReservationInt { "Instances": instances } = traverse fromInstanceDescriptionInt instances

fromInstanceDescriptionInt :: InstanceDescriptionInt -> F InstanceDescription
fromInstanceDescriptionInt
  instanceDescriptionInt@
    { "InstanceId": instanceId
    , "InstanceType": instanceType
    , "ImageId": imageId
    , "Tags": tagsInt
    , "LaunchTime": (DateTimeInt launchTime)
    , "State": stateInt
    , "ClientToken": clientToken
    } = ado
  instanceState <- stateIntToInstanceState stateInt instanceDescriptionInt
  in { instanceId: InstanceId instanceId
  , instanceType: InstanceType instanceType
  , imageId: ImageId imageId
  , tags: tagIntsToTags $ fromMaybe List.nil tagsInt
  , launchTime
  , state: instanceState
  , clientToken: ClientToken <$> emptyStringToNothing clientToken
  }

mandatory :: forall a. String -> Maybe a -> F a
mandatory name Nothing = except $ Left $ singleton $ ForeignError $ name <> " is mandatory"
mandatory _name (Just a) = pure a

emptyStringToNothing :: String -> Maybe String
emptyStringToNothing "" = Nothing
emptyStringToNothing str = Just str

stateIntToInstanceState :: StateInt -> InstanceDescriptionInt -> F InstanceState
stateIntToInstanceState { "Name": "pending" } _ =
  pure Pending
stateIntToInstanceState { "Name": "running" }
  { "PrivateDnsName": privateDnsName
  , "PrivateIpAddress": privateIpAddress
  , "PublicDnsName": publicDnsName
  } = ado
  privateDnsName <- mandatory "PrivateDnsName" privateDnsName
  privateIpAddress <- mandatory "PrivateIpAddress" privateIpAddress
  in Running { privateDnsName, privateIpAddress, publicDnsName: emptyStringToNothing =<< publicDnsName }
stateIntToInstanceState { "Name": "shutting-down" } _ =
  pure ShuttingDown
stateIntToInstanceState { "Name": "terminated" } _ =
  pure Terminated
stateIntToInstanceState { "Name": "stopping" } _ =
  pure Stopping
stateIntToInstanceState { "Name": "stopped" } _ =
  pure Stopped
stateIntToInstanceState { "Name": unknown } _ =
  except $ Left $ singleton $ ForeignError $ "Unknown state: " <> unknown

type BaseRequest a
  = { region :: Maybe Region
    , profile :: Maybe String
    | a
    }

type DescribeInstancesRequest
  = BaseRequest ( instanceIds :: Maybe (List InstanceId)
    )

type FilterInt
  = { "Name" :: String
    , "Values" :: List String
    }

type DescribeInstancesRequestInt
  = { "Filters" :: List FilterInt
    , "InstanceIds" :: List InstanceId
    }

describeInstances :: DescribeInstancesRequest → Effect (Either MultipleErrors (List InstanceDescription))
describeInstances req@{ instanceIds } = do
  let
    requestInt :: DescribeInstancesRequestInt
    requestInt =
      { "Filters": List.nil
      , "InstanceIds": fromMaybe List.nil instanceIds
      }
    requestJson = writeJSON requestInt
    cli =
      awsCliBase req "describe-instances"
        <> " --cli-input-json '"
        <> requestJson
        <> "'"
  outputJson <- runAwsCli cli
  pure $ runExcept $ fromDescribeInstancesInt =<< readJSON' =<< outputJson

type RunInstancesRequest
  = BaseRequest ( clientToken :: ClientToken
    , ebsOptimized :: Boolean
    , imageId :: ImageId
    , instanceType :: InstanceType
    , keyName :: KeyName
    , count :: Int
    , userData :: String
    , tags :: Map String String
    )

type TagSpecificationsInt
  = { "ResourceType" :: String
    , "Tags" :: List TagInt
    }

type RunInstancesRequestInt
  = { "ClientToken" :: ClientToken
    , "EbsOptimized" :: Boolean
    , "ImageId" :: ImageId
    , "InstanceType" :: InstanceType
    , "KeyName" :: KeyName
    , "MinCount" :: Int
    , "MaxCount" :: Int
    , "UserData" :: String
    , "TagSpecifications" :: List TagSpecificationsInt
    }

type RunInstancesResponse
  = { instances :: List InstanceDescription
    , ownerId :: String
    , reservationId :: String
    }

type RunInstancesResponseInt
  = { "Instances" :: List InstanceDescriptionInt
    , "OwnerId" :: String
    , "ReservationId" :: String
    }

fromRunInstancesResponseInt :: RunInstancesResponseInt -> F RunInstancesResponse
fromRunInstancesResponseInt { "Instances": instances, "OwnerId": ownerId, "ReservationId": reservationId } = ado
  instances <- traverse fromInstanceDescriptionInt instances
  in { instances, ownerId, reservationId }

runInstances :: RunInstancesRequest -> Effect (Either MultipleErrors RunInstancesResponse)
runInstances
  req@
    { clientToken
    , ebsOptimized
    , imageId
    , instanceType
    , keyName
    , count
    , userData
    , tags
    } = do
  let
    requestInt :: RunInstancesRequestInt
    requestInt =
      { "ClientToken": clientToken
      , "EbsOptimized": ebsOptimized
      , "ImageId": imageId
      , "InstanceType": instanceType
      , "KeyName": keyName
      , "MinCount": count
      , "MaxCount": count
      , "UserData": userData
      , "TagSpecifications": List.singleton { "ResourceType": "instance", "Tags": tagsToTagInts tags }
      }
    requestJson = writeJSON requestInt
    cli =
      awsCliBase req "run-instances"
        <> " --cli-input-json '"
        <> requestJson
        <> "'"
  outputJson <- runAwsCli cli
  pure $ runExcept $ fromRunInstancesResponseInt =<< readJSON' =<< outputJson

type TerminateInstancesRequest
  = BaseRequest ( instanceIds :: List InstanceId
    )

type TerminateInstancesRequestInt
  = { "InstanceIds" :: List InstanceId
    }

type TerminateInstancesResponseInt
  = { "TerminatingInstances" :: List InstanceStateChangeInt
    }

terminateInstances :: TerminateInstancesRequest -> Effect (Either MultipleErrors (List InstanceId))
terminateInstances
  req@
    { instanceIds
    } = do
  let
    requestInt :: TerminateInstancesRequestInt
    requestInt =
      { "InstanceIds": instanceIds
      }
    requestJson = writeJSON requestInt
    cli =
      awsCliBase req "terminate-instances"
        <> " --cli-input-json '"
        <> requestJson
        <> "'"
  outputJson <- runAwsCli cli
  let
    response :: F TerminateInstancesResponseInt
    response = readJSON' =<< outputJson
  pure $ runExcept $ (map (InstanceId <<< _."InstanceId")) <$> (_."TerminatingInstances") <$> response

type StopInstancesRequest
  = BaseRequest ( instanceIds :: List InstanceId
    )

type StopInstancesRequestInt
  = { "InstanceIds" :: List InstanceId
    }

type StopInstancesResponseInt
  = { "TerminatingInstances" :: List InstanceStateChangeInt
    }

stopInstances :: StopInstancesRequest -> Effect (E String)
stopInstances
  req@
    { instanceIds
    } = do
  let
    requestInt :: StopInstancesRequestInt
    requestInt =
      { "InstanceIds": instanceIds
      }
    requestJson = writeJSON requestInt
    cli =
      awsCliBase req "stop-instances"
        <> " --cli-input-json '"
        <> requestJson
        <> "'"
  outputJson <- runAwsCli cli
  pure $ runExcept outputJson
-- let
--   response :: F StopInstancesResponseInt
--   response = readJSON' outputJson
-- pure $ runExcept $ (map (InstanceId <<< _."InstanceId")) <$> (_."TerminatingInstances") <$> response
awsCliBase :: forall t. BaseRequest t -> String -> String
awsCliBase { profile, region } command = do
  "aws ec2 "
    <> command
    <> " --output json --color off "
    <> (fromMaybe "" $ (\r -> " --region " <> r) <$> unwrap <$> region)
    <> (fromMaybe "" $ (\p -> " --profile " <> p) <$> profile)

runAwsCli :: String -> Effect (F String)
runAwsCli cmd = do
  res <- runCommand cmd
  case res of
    Left { output } -> pure $ except $ Left $ singleton $ ForeignError $ "aws cli failure: " <> output
    Right output -> pure $ except $ Right output

type CmdError
  = { exitStatus :: Number
    , output :: String
    }

foreign import runCommand :: String -> Effect (Either CmdError String)
