module Erl.Aws
  ( ClientToken(..)
  , IamRole(..)
  , ImageId(..)
  , InstanceDescription(..)
  , InstanceId(..)
  , InstanceState(..)
  , InstanceType(..)
  , KeyName(..)
  , OptInStatus(..)
  , Profile(..)
  , Region(..)
  , RegionDescription(..)
  , RunningInstance
  , SecurityGroupId(..)
  , SubnetId(..)
  , UserData(..)
  , defaultMetadataOptions
  , describeInstances
  , describeInstanceUserData
  , describeRegions
  , runInstances
  , stopInstances
  , terminateInstances
  , createTags
  ) where

import Prelude

import Common.Shared.Json (genericTaggedWriteForeign)
import Control.Alt ((<|>))
import Control.Monad.Except (except, runExcept, withExcept)
import Data.Bifunctor (bimap)
import Data.DateTime (DateTime)
import Data.DateTime.Parsing (parseFullDateTime, toUTC)
import Data.Either (Either(..), note)
import Data.Foldable (foldl)
import Data.Generic.Rep (class Generic, Argument(..), Constructor(..), NoArguments(..), Sum(..), to)
import Data.List.NonEmpty (singleton)
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.Newtype (class Newtype, unwrap)
import Data.Show.Generic (genericShow)
import Data.Symbol (class IsSymbol, reflectSymbol)
import Data.Traversable (traverse)
import Data.Tuple (Tuple(..))
import Debug (spy, traceM)
import Effect (Effect)
import Erl.Data.List (List)
import Erl.Data.List as List
import Erl.Data.Map (Map)
import Erl.Data.Map as Map
import Erl.Kernel.Inet (Hostname, IpAddress, parseIpAddress)
import Foreign (F, Foreign, ForeignError(..), MultipleErrors, readString)
import Foreign as Foreign
import JsonLd as JsonLd
import Simple.JSON (class ReadForeign, class WriteForeign, class WriteForeignKey, E, read', readImpl, readJSON', writeImpl, writeJSON)
import Text.Parsing.Parser (ParserT, fail, parseErrorMessage, runParser)
import Type.Prelude (Proxy(..))

newtype InstanceId = InstanceId String

derive newtype instance Eq InstanceId
derive newtype instance Ord InstanceId
derive newtype instance ReadForeign InstanceId
derive newtype instance WriteForeign InstanceId
derive newtype instance WriteForeignKey InstanceId
derive instance Newtype InstanceId _
derive instance Generic InstanceId _
instance Show InstanceId where
  show = genericShow

newtype InstanceType = InstanceType String

derive newtype instance Eq InstanceType
derive newtype instance Ord InstanceType
derive newtype instance ReadForeign InstanceType
derive newtype instance WriteForeign InstanceType
derive instance Newtype InstanceType _
derive instance Generic InstanceType _
instance Show InstanceType where
  show = genericShow

newtype ImageId = ImageId String

derive newtype instance Eq ImageId
derive newtype instance Ord ImageId
derive newtype instance ReadForeign ImageId
derive newtype instance WriteForeign ImageId
derive instance Newtype ImageId _
derive instance Generic ImageId _
instance Show ImageId where
  show = genericShow

newtype Region = Region String

derive newtype instance Eq Region
derive newtype instance Ord Region
derive newtype instance ReadForeign Region
derive newtype instance WriteForeign Region
derive newtype instance WriteForeignKey Region
derive instance Newtype Region _
derive instance Generic Region _
instance Show Region where
  show = genericShow

instance JsonLd.JsonLdContext Region where
  getContextValue _ = JsonLd.ContextValue "Region"

newtype Profile = Profile String

derive newtype instance Eq Profile
derive newtype instance Ord Profile
derive newtype instance ReadForeign Profile
derive newtype instance WriteForeign Profile
derive instance Newtype Profile _
derive instance Generic Profile _
instance Show Profile where
  show = genericShow

newtype SecurityGroupId = SecurityGroupId String

derive newtype instance Eq SecurityGroupId
derive newtype instance Ord SecurityGroupId
derive newtype instance ReadForeign SecurityGroupId
derive newtype instance WriteForeign SecurityGroupId
derive instance Newtype SecurityGroupId _
derive instance Generic SecurityGroupId _
instance Show SecurityGroupId where
  show = genericShow

newtype SubnetId = SubnetId String

derive newtype instance Eq SubnetId
derive newtype instance Ord SubnetId
derive newtype instance ReadForeign SubnetId
derive newtype instance WriteForeign SubnetId
derive instance Newtype SubnetId _
derive instance Generic SubnetId _
instance Show SubnetId where
  show = genericShow

newtype KeyName = KeyName String

derive newtype instance Eq KeyName
derive newtype instance Ord KeyName
derive newtype instance ReadForeign KeyName
derive newtype instance WriteForeign KeyName
derive instance Newtype KeyName _
derive instance Generic KeyName _
instance Show KeyName where
  show = genericShow

newtype ClientToken = ClientToken String

derive newtype instance Eq ClientToken
derive newtype instance Ord ClientToken
derive newtype instance ReadForeign ClientToken
derive newtype instance WriteForeign ClientToken
derive instance Newtype ClientToken _
derive instance Generic ClientToken _
instance Show ClientToken where
  show = genericShow

newtype UserData = UserData String

derive newtype instance Eq UserData
derive newtype instance Ord UserData
derive newtype instance ReadForeign UserData
derive newtype instance WriteForeign UserData
derive instance Newtype UserData _
derive instance Generic UserData _
instance Show UserData where
  show = genericShow

type RunningInstance =
  { publicDnsName :: Maybe Hostname
  , privateDnsName :: Hostname
  , privateIpAddress :: IpAddress
  }

data InstanceState
  = Pending
  | Running RunningInstance
  | ShuttingDown
  | Terminated
  | Stopping
  | Stopped

derive instance Eq InstanceState

instance WriteForeign InstanceState where
  writeImpl s =
    let
      instanceData = case s of
        Running i -> Just i
        _ -> Nothing
      state = case s of
        Pending -> "pending"
        Running _ -> "running"
        ShuttingDown -> "shutting-down"
        Terminated -> "terminated"
        Stopping -> "stopping"
        Stopped -> "stopped"
    in
      writeImpl { state, instanceData }

type InstanceDescription =
  { instanceId :: InstanceId
  , instanceType :: InstanceType
  , imageId :: ImageId
  , tags :: Map String String
  , launchTime :: DateTime
  , state :: InstanceState
  , clientToken :: Maybe ClientToken
  }

data OptInStatus
  = OptInNotRequired
  | OptedIn
  | NotOptedIn

type RegionDescription =
  { regionName :: Region
  , endpoint :: String
  , optInStatus :: OptInStatus
  }

type TagInt =
  { "Key" :: String
  , "Value" :: String
  }

tagIntsToTags :: List TagInt -> Map String String
tagIntsToTags = foldl insertTag Map.empty
  where
  insertTag acc { "Key": key, "Value": value } = Map.insert key value acc

tagsToTagInts :: Map String String -> List TagInt
tagsToTagInts tags = (\(Tuple key value) -> { "Key": key, "Value": value }) <$> Map.toUnfoldable tags

newtype DateTimeInt = DateTimeInt DateTime

instance readForeignDateTimeInt :: ReadForeign DateTimeInt where
  readImpl =
    readString >=> parseFDT
    where
    parseFDT s =
      except
        $ bimap (singleton <<< ForeignError <<< parseErrorMessage) DateTimeInt (runParser s parseDateTime)

parseDateTime :: forall m. Monad m => ParserT String m DateTime
parseDateTime = parseFullDateTime >>= (\full -> maybe (fail "Invalid datetime offset") (pure) $ toUTC full)

type DescribeRegionsInt =
  { "Regions" :: List RegionDescriptionInt
  }

type RegionDescriptionInt =
  { "Endpoint" :: String
  , "RegionName" :: String
  , "OptInStatus" :: String
  }

type DescribeInstancesInt =
  { "Reservations" :: List ReservationInt
  }

type ReservationInt =
  { "Instances" :: List InstanceDescriptionInt
  }

type InstanceDescriptionInt =
  { "InstanceId" :: String
  , "InstanceType" :: String
  , "ImageId" :: String
  , "Tags" :: Maybe (List TagInt)
  , "LaunchTime" :: DateTimeInt
  , "PrivateDnsName" :: Maybe String
  , "PrivateIpAddress" :: Maybe String
  , "PublicDnsName" :: Maybe String
  , "State" :: StateInt
  , "ClientToken" :: String
  , "UserData" :: Maybe String
  }

type StateInt =
  { "Code" :: Int
  , "Name" :: String
  }

type InstanceStateChangeInt =
  { "CurrentState" :: StateInt
  , "PreviousState" :: StateInt
  , "InstanceId" :: String
  }

fromDescribeRegionsInt :: DescribeRegionsInt -> F (List RegionDescription)
fromDescribeRegionsInt { "Regions": regions } = traverse fromRegionInt regions

fromRegionInt :: RegionDescriptionInt -> F RegionDescription
fromRegionInt
  { "Endpoint": endpoint
  , "RegionName": regionName
  , "OptInStatus": optInStatus
  } = ado
  optInStatus' <- case optInStatus of
    "opt-in-not-required" -> pure OptInNotRequired
    "opted-in" -> pure OptedIn
    "not-opted-in" -> pure NotOptedIn
    _ -> except $ Left $ singleton $ ForeignError $ optInStatus <> " is not recognised"
  in
    { regionName: Region regionName
    , endpoint
    , optInStatus: optInStatus'
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
  in
    { instanceId: InstanceId instanceId
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
stateIntToInstanceState
  { "Name": "running" }
  { "PrivateDnsName": privateDnsName
  , "PrivateIpAddress": privateIpAddress
  , "PublicDnsName": publicDnsName
  } = ado
  privateDnsName <- mandatory "PrivateDnsName" privateDnsName
  privateIpAddress <- (\addr -> except $ note (singleton $ ForeignError $ "Invalid IpAddress: addr") $ parseIpAddress addr) =<< mandatory "PrivateIpAddress" privateIpAddress
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

type BaseRequest a =
  { region :: Maybe Region
  , profile :: Maybe Profile
  , dryRun :: Boolean
  | a
  }

type DescribeInstancesRequest = BaseRequest
  ( instanceIds :: Maybe (List InstanceId)
  )

type FilterInt =
  { "Name" :: String
  , "Values" :: List String
  }

type DescribeInstancesRequestInt =
  { "Filters" :: List FilterInt
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

type DescribeInstanceUserDataRequest = BaseRequest (instanceId :: InstanceId)

type DescribeInstanceAttributeRequestInt =
  { "InstanceId" :: InstanceId
  , "Attribute" :: String
  }

type DescribeInstanceUserDataResponseInt =
  { "InstanceId" :: String
  , "UserData" :: { "Value" :: Maybe String }
  }

describeInstanceUserData :: DescribeInstanceUserDataRequest -> Effect (Either MultipleErrors (Maybe UserData))
describeInstanceUserData req@{ instanceId } = do
  let
    requestInt :: DescribeInstanceAttributeRequestInt
    requestInt =
      { "InstanceId": instanceId
      , "Attribute": "userData"
      }
    requestJson = writeJSON requestInt
    cli =
      awsCliBase req "describe-instance-attribute"
        <> " --cli-input-json '"
        <> requestJson
        <> "'"
  outputJson <- runAwsCli cli
  let
    parsed :: F DescribeInstanceUserDataResponseInt
    parsed = readJSON' =<< outputJson

    userData :: F (Maybe UserData)
    userData = (\{ "UserData": { "Value": userData } } -> (map UserData <<< emptyStringToNothing) =<< base64Decode =<< userData) <$> parsed
  pure $ runExcept $ userData

describeRegions :: BaseRequest () -> Effect (Either MultipleErrors (List RegionDescription))
describeRegions req = do
  let
    cli = awsCliBase req "describe-regions"

  outputJson <- runAwsCli cli
  pure $ runExcept $ fromDescribeRegionsInt =<< readJSON' =<< outputJson

data IamRole
  = RoleArn String
  | RoleName String

derive instance Eq IamRole
derive instance Generic IamRole _
instance WriteForeign IamRole where
  writeImpl = genericTaggedWriteForeign

instance ReadForeign IamRole where
  readImpl = genericTaggedReadForeign

iamProfileToInt :: IamRole -> IamInstanceProfileSpecificationInt
iamProfileToInt (RoleArn arn) = { "Arn": Just arn, "Name": Nothing }
iamProfileToInt (RoleName name) = { "Arn": Nothing, "Name": Just name }

defaultMetadataOptions :: MetadataOptions
defaultMetadataOptions =
  { httpEndpoint: true
  , httpProtocolIpv6: true
  , httpPutResponseHopLimit: 1
  , httpTokens: false
  , instanceMetadataTags: false
  }

type MetadataOptions =
  { httpEndpoint :: Boolean
  , httpProtocolIpv6 :: Boolean
  , httpPutResponseHopLimit :: Int
  , httpTokens :: Boolean
  , instanceMetadataTags :: Boolean
  }

type RunInstancesRequest = BaseRequest
  ( clientToken :: ClientToken
  , ebsOptimized :: Boolean
  , imageId :: ImageId
  , instanceType :: InstanceType
  , keyName :: Maybe KeyName
  , count :: Int
  , userData :: String
  , tags :: Map String String
  , iamRole :: Maybe IamRole
  , securityGroups :: List SecurityGroupId
  , metadataOptions :: Maybe MetadataOptions
  , subnetId :: Maybe SubnetId
  )

type TagSpecificationsInt =
  { "ResourceType" :: String
  , "Tags" :: List TagInt
  }

type IamInstanceProfileSpecificationInt =
  { "Name" :: Maybe String
  , "Arn" :: Maybe String
  }

type InstanceMetadataOptionsRequestInt =
  { "HttpEndpoint" :: String
  , "HttpProtocolIpv6" :: String
  , "HttpPutResponseHopLimit" :: Int
  , "HttpTokens" :: String
  , "InstanceMetadataTags" :: String
  }

metadataOptionsToInt :: MetadataOptions -> InstanceMetadataOptionsRequestInt
metadataOptionsToInt
  { httpEndpoint
  , httpProtocolIpv6
  , httpPutResponseHopLimit
  , httpTokens
  , instanceMetadataTags
  } =
  { "HttpEndpoint": booleanToDisabledEnabled httpEndpoint
  , "HttpProtocolIpv6": booleanToDisabledEnabled httpProtocolIpv6
  , "HttpPutResponseHopLimit": httpPutResponseHopLimit
  , "HttpTokens": if httpTokens then "required" else "optional"
  , "InstanceMetadataTags": booleanToDisabledEnabled instanceMetadataTags
  }

type RunInstancesRequestInt =
  { "ClientToken" :: ClientToken
  , "EbsOptimized" :: Boolean
  , "ImageId" :: ImageId
  , "InstanceType" :: InstanceType
  , "KeyName" :: Maybe KeyName
  , "MinCount" :: Int
  , "MaxCount" :: Int
  , "UserData" :: String
  , "TagSpecifications" :: List TagSpecificationsInt
  , "SecurityGroupIds" :: List SecurityGroupId
  , "IamInstanceProfile" :: Maybe IamInstanceProfileSpecificationInt
  , "MetadataOptions" :: Maybe InstanceMetadataOptionsRequestInt
  , "SubnetId" :: Maybe SubnetId
  }

type RunInstancesResponse =
  { instances :: List InstanceDescription
  , ownerId :: String
  , reservationId :: String
  }

type RunInstancesResponseInt =
  { "Instances" :: List InstanceDescriptionInt
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
    , iamRole
    , securityGroups
    , metadataOptions
    , subnetId
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
      , "IamInstanceProfile": iamProfileToInt <$> iamRole
      , "MetadataOptions": metadataOptionsToInt <$> metadataOptions
      , "SecurityGroupIds": securityGroups
      , "SubnetId": subnetId
      }
    requestJson = writeJSON requestInt
    cli =
      awsCliBase req "run-instances"
        <> " --cli-input-json '"
        <> requestJson
        <> "'"
  traceM cli
  outputJson <- runAwsCli cli
  pure $ runExcept $ fromRunInstancesResponseInt =<< readJSON' =<< outputJson

type TerminateInstancesRequest = BaseRequest
  ( instanceIds :: List InstanceId
  )

type TerminateInstancesRequestInt =
  { "InstanceIds" :: List InstanceId
  }

type TerminateInstancesResponseInt =
  { "TerminatingInstances" :: List InstanceStateChangeInt
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

type StopInstancesRequest = BaseRequest
  ( instanceIds :: List InstanceId
  )

type StopInstancesRequestInt =
  { "InstanceIds" :: List InstanceId
  }

type StopInstancesResponseInt =
  { "StoppingInstances" :: List InstanceStateChangeInt
  }

stopInstances :: StopInstancesRequest -> Effect (E (List InstanceId))
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
  let
    response :: F StopInstancesResponseInt
    response = readJSON' =<< outputJson
  pure $ runExcept $ (map (InstanceId <<< _."InstanceId")) <$> (_."StoppingInstances") <$> response

type CreateTagsRequest = BaseRequest
  ( instanceId :: InstanceId
  , tags :: Map String String
  )

type CreateTagsRequestInt =
  { "Resources" :: List InstanceId
  , "Tags" :: List TagInt
  }

createTags :: CreateTagsRequest -> Effect (E Unit)
createTags req@{ instanceId, tags } = do
  let
    requestInt :: CreateTagsRequestInt
    requestInt =
      { "Resources": List.singleton instanceId
      , "Tags": tagsToTagInts tags
      }
    requestJson = writeJSON requestInt
    cli =
      awsCliBase req "create-tags"
        <> " --cli-input-json '"
        <> requestJson
        <> "'"
  output <- runAwsCli cli
  pure $ runExcept $ (const unit) <$> output

booleanToDisabledEnabled :: Boolean -> String
booleanToDisabledEnabled true = "enabled"
booleanToDisabledEnabled false = "disabled"

awsCliBase :: forall t. BaseRequest t -> String -> String
awsCliBase { profile, region, dryRun } command = do
  "aws ec2 "
    <> command
    <> " --output json --color off "
    <> (if dryRun then " --dry-run" else "")
    <> (fromMaybe "" $ (\r -> " --region " <> r) <$> unwrap <$> region)
    <> (fromMaybe "" $ (\p -> " --profile " <> p) <$> unwrap <$> profile)

runAwsCli :: String -> Effect (F String)
runAwsCli cmd = do
  res <- runCommand $ spy "CMD:" cmd
  case res of
    Left { output } -> pure $ except $ Left $ singleton $ ForeignError $ "aws cli failure: " <> output
    Right output -> pure $ except $ Right output

type CmdError =
  { exitStatus :: Number
  , output :: String
  }

foreign import runCommand :: String -> Effect (Either CmdError String)

------------------------------------------------------------------------------
-- GenericTaggedReadForeign
genericTaggedReadForeign
  :: forall a rep
   . Generic a rep
  => GenericTaggedReadForeign rep
  => Foreign
  -> Foreign.F a
genericTaggedReadForeign f = to <$> genericTaggedReadForeignImpl f

class GenericTaggedReadForeign rep where
  genericTaggedReadForeignImpl :: Foreign -> Foreign.F rep

instance genericTaggedReadForeignSum ::
  ( GenericTaggedReadForeign a
  , GenericTaggedReadForeign b
  ) =>
  GenericTaggedReadForeign (Sum a b) where
  genericTaggedReadForeignImpl f =
    Inl
      <$> genericTaggedReadForeignImpl f
      <|> Inr
        <$> genericTaggedReadForeignImpl f

instance genericTaggedReadForeignConstructor ::
  ( GenericTaggedReadForeign a
  , IsSymbol name
  ) =>
  GenericTaggedReadForeign (Constructor name a) where
  genericTaggedReadForeignImpl f = do
    r :: { "type" :: String, value :: Foreign } <- read' f
    if r."type" == name then
      withExcept (map $ ErrorAtProperty name) $ Constructor <$> genericTaggedReadForeignImpl r.value
    else
      Foreign.fail $ ForeignError $ "Wrong type tag " <> r."type" <> " where " <> name <> " was expected."
    where
    nameP = Proxy :: Proxy name
    name = reflectSymbol nameP

instance genericTaggedReadForeignArgument ::
  ( ReadForeign a
  ) =>
  GenericTaggedReadForeign (Argument a) where
  genericTaggedReadForeignImpl f = Argument <$> readImpl f

instance genericTaggedReadForeignNoArgument ::
  GenericTaggedReadForeign NoArguments where
  genericTaggedReadForeignImpl _ = pure NoArguments

foreign import base64Decode :: String -> Maybe String
