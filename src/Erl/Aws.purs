module Erl.Aws
  ( ClientToken(..)
  , EbsInfo(..)
  , EbsOptimizedInfo(..)
  , IamRole(..)
  , ImageId(..)
  , InstanceDescription(..)
  , InstanceTypeDescription(..)
  , InstanceId(..)
  , InstanceState(..)
  , InstanceType(..)
  , KeyName(..)
  , LaunchTemplateIdentifier(..)
  , LaunchTemplate
  , OptInStatus(..)
  , MemoryInfo(..)
  , NetworkCard(..)
  , NetworkInfo(..)
  , PlacementGroupInfo(..)
  , PlacementGroupStrategies(..)
  , ProcessorInfo(..)
  , Profile(..)
  , Region(..)
  , RegionDescription(..)
  , RunningInstance
  , SecurityGroupId(..)
  , SubnetId(..)
  , SupportedArchitectures(..)
  , SupportedBootModes(..)
  , SupportedRootDeviceTypes(..)
  , SupportedUsageClasses(..)
  , SupportedVirtualizationTypes(..)
  , TypeOffering
  , UserData(..)
  , VCpuInfo(..)
  , createTags
  , defaultMetadataOptions
  , describeInstanceTypes
  , describeInstanceUserData
  , describeInstances
  , describeRegions
  , describeTypeOfferings
  , runInstances
  , stopInstances
  , terminateInstances
  ) where

import Prelude

import Control.Monad.Except (except, runExcept)
import Data.Bifunctor (bimap)
import Data.DateTime (DateTime)
import Data.DateTime.Parsing (parseFullDateTime, toUTC)
import Data.Either (Either(..), note)
import Data.Foldable (foldl, intercalate)
import Data.Generic.Rep (class Generic)
import Data.List.NonEmpty (singleton)
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.Newtype (class Newtype, unwrap)
import Data.Show.Generic (genericShow)
import Data.Traversable (traverse)
import Data.Tuple (Tuple(..))
import Debug (spy)
import Effect (Effect)
import Erl.Data.List (List, nil)
import Erl.Data.List as List
import Erl.Data.Map (Map)
import Erl.Data.Map as Map
import Erl.Json (genericTaggedReadForeign, genericTaggedWriteForeign, genericEnumReadForeign, genericEnumWriteForeign)
import Erl.Kernel.Inet (Hostname, IpAddress, parseIpAddress)
import Foreign (F, ForeignError(..), MultipleErrors, readString, unsafeFromForeign)
import Partial.Unsafe (unsafeCrashWith)
import Simple.JSON (class ReadForeign, class WriteForeign, class WriteForeignKey, E, readJSON', writeImpl, writeJSON)
import Text.Parsing.Parser (ParserT, fail, parseErrorMessage, runParser)

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

derive instance Generic OptInStatus _
instance WriteForeign OptInStatus where
  writeImpl = genericEnumWriteForeign

instance ReadForeign OptInStatus where
  readImpl = genericEnumReadForeign

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
  , additionalCliArgs :: List String
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

data LaunchTemplateIdentifier
  = LaunchTemplateName String
  | LaunchTemplateId String

derive instance Eq LaunchTemplateIdentifier

type LaunchTemplate =
  { templateIdentifier :: LaunchTemplateIdentifier
  , version :: Maybe String
  }

type RunInstancesRequest = BaseRequest
  ( clientToken :: ClientToken
  , ebsOptimized :: Maybe Boolean
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
  , template :: Maybe LaunchTemplate
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
  , "EbsOptimized" :: Maybe Boolean
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
  , "LaunchTemplate" :: Maybe LaunchTemplateInt
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
    , template
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
      , "LaunchTemplate": launchTemplateToInt <$> template
      }
    requestJson = writeJSON requestInt
    cli =
      awsCliBase req "run-instances"
        <> " --cli-input-json '"
        <> requestJson
        <> "'"
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

type TypeOfferingsRequest = BaseRequest ()

type TypeOfferingsInt =
  { "InstanceTypeOfferings" :: List TypeOfferingInt
  }

type TypeOfferingInt =
  { "InstanceType" :: String
  , "LocationType" :: String
  , "Location" :: String
  }

type TypeOffering =
  { instanceType :: InstanceType
  , locationType :: String
  , location :: String
  }

fromDescribeTypeOfferingsResponseInt :: TypeOfferingsInt -> F (List TypeOffering)
fromDescribeTypeOfferingsResponseInt { "InstanceTypeOfferings": typeOfferings } = ado
  types <- traverse fromTypeOfferingInt typeOfferings
  in types

fromTypeOfferingInt :: TypeOfferingInt -> F TypeOffering
fromTypeOfferingInt
  { "InstanceType": instanceType
  , "LocationType": locationType
  , "Location": location
  } = do
  pure $
    { instanceType: InstanceType instanceType
    , locationType
    , location
    }

describeTypeOfferings :: TypeOfferingsRequest -> Effect (Either MultipleErrors (List TypeOffering))
describeTypeOfferings req = do
  let
    cli =
      awsCliBase req "describe-instance-type-offerings"
  outputJson <- runAwsCli cli
  pure $ runExcept $ fromDescribeTypeOfferingsResponseInt =<< readJSON' =<< outputJson

type InstanceTypeRequest = BaseRequest ()

data SupportedUsageClasses = OnDemand | Spot

derive instance Eq SupportedUsageClasses
derive instance Generic SupportedUsageClasses _
instance ReadForeign SupportedUsageClasses where
  readImpl f =
    case unsafeFromForeign f of
      "on-demand" -> pure OnDemand
      "spot" -> pure Spot
      _ -> unsafeCrashWith "Unexpected SupportedUsageClasses"

instance WriteForeign SupportedUsageClasses where
  writeImpl OnDemand = writeImpl "on-demand"
  writeImpl Spot = writeImpl "spot"

instance Show SupportedUsageClasses where
  show = genericShow

data SupportedRootDeviceTypes = Ebs | InstanceStore

derive instance Eq SupportedRootDeviceTypes
derive instance Generic SupportedRootDeviceTypes _
instance ReadForeign SupportedRootDeviceTypes where
  readImpl f =
    case unsafeFromForeign f of
      "ebs" -> pure Ebs
      "instance-store" -> pure InstanceStore
      _ -> unsafeCrashWith "Unexpected SupportedRootDeviceTypes"

instance WriteForeign SupportedRootDeviceTypes where
  writeImpl Ebs = writeImpl "ebs"
  writeImpl InstanceStore = writeImpl "instance-store"

instance Show SupportedRootDeviceTypes where
  show = genericShow

data SupportedVirtualizationTypes = Hvm | Paravirtual

derive instance Eq SupportedVirtualizationTypes
derive instance Generic SupportedVirtualizationTypes _
instance ReadForeign SupportedVirtualizationTypes where
  readImpl f =
    case unsafeFromForeign f of
      "hvm" -> pure Hvm
      "paravirtual" -> pure Paravirtual
      _ -> unsafeCrashWith "Unexpected SupportedVirtualizationTypes"

instance WriteForeign SupportedVirtualizationTypes where
  writeImpl Hvm = writeImpl "hvm"
  writeImpl Paravirtual = writeImpl "paravirtual"

instance Show SupportedVirtualizationTypes where
  show = genericShow

data SupportedArchitectures = I386 | X86_64 | Arm64 | X86_64_mac | Arm64_mac

derive instance Eq SupportedArchitectures
derive instance Generic SupportedArchitectures _
instance ReadForeign SupportedArchitectures where
  readImpl f =
    case unsafeFromForeign f of
      "i386" -> pure I386
      "x86_64" -> pure X86_64
      "arm64" -> pure Arm64
      "x86_64_mac" -> pure X86_64_mac
      "arm64_mac" -> pure Arm64_mac
      _ -> unsafeCrashWith "Unexpected SupportedArchitectures"

instance WriteForeign SupportedArchitectures where
  writeImpl I386 = writeImpl "i386"
  writeImpl X86_64 = writeImpl "x86_64"
  writeImpl Arm64 = writeImpl "arm64"
  writeImpl X86_64_mac = writeImpl "x86_64_mac"
  writeImpl Arm64_mac = writeImpl "arm64_mac"

instance Show SupportedArchitectures where
  show = genericShow

data PlacementGroupStrategies = Cluster | Partition | Spread

derive instance Eq PlacementGroupStrategies
derive instance Generic PlacementGroupStrategies _
instance ReadForeign PlacementGroupStrategies where
  readImpl f =
    case unsafeFromForeign f of
      "cluster" -> pure Cluster
      "partition" -> pure Partition
      "spread" -> pure Spread
      _ -> unsafeCrashWith "Unexpected PlacementGroupStrategies"

instance WriteForeign PlacementGroupStrategies where
  writeImpl Cluster = writeImpl "cluster"
  writeImpl Partition = writeImpl "partition"
  writeImpl Spread = writeImpl "spread"

instance Show PlacementGroupStrategies where
  show = genericShow

data SupportedBootModes = LegacyBios | Uefi

derive instance Eq SupportedBootModes
derive instance Generic SupportedBootModes _
instance ReadForeign SupportedBootModes where
  readImpl f =
    case unsafeFromForeign f of
      "legacy-bios" -> pure LegacyBios
      "uefi" -> pure Uefi
      _ -> unsafeCrashWith "Unexpected SupportedBootModes"

instance WriteForeign SupportedBootModes where
  writeImpl LegacyBios = writeImpl "legacy-bios"
  writeImpl Uefi = writeImpl "uefi"

instance Show SupportedBootModes where
  show = genericShow

type ProcessorInfo =
  { supportedArchitectures :: List SupportedArchitectures
  , sustainedClockSpeedInGhz :: Maybe Number
  }

type ProcessorInfoInt =
  { "SupportedArchitectures" :: List SupportedArchitectures
  , "SustainedClockSpeedInGhz" :: Maybe Number
  }

fromProcessorInfoInt :: ProcessorInfoInt -> F ProcessorInfo
fromProcessorInfoInt
  { "SupportedArchitectures": supportedArchitectures
  , "SustainedClockSpeedInGhz": sustainedClockSpeedInGhz
  } = do
  pure $
    { supportedArchitectures
    , sustainedClockSpeedInGhz
    }

type VCpuInfo =
  { defaultCores :: Int
  , defaultThreadsPerCore :: Int
  , defaultVCpus :: Maybe Int
  , validCores :: Maybe (List Int)
  , validThreadsPerCore :: Maybe (List Int)
  }

type VCpuInfoInt =
  { "DefaultCores" :: Int
  , "DefaultThreadsPerCore" :: Int
  , "DefaultVCpus" :: Maybe Int
  , "ValidCores" :: Maybe (List Int)
  , "ValidThreadsPerCore" :: Maybe (List Int)
  }

fromVCpuInfoInt :: Maybe VCpuInfoInt -> F (Maybe VCpuInfo)
fromVCpuInfoInt info =
  case info of
    Just
      { "DefaultCores": defaultCores
      , "DefaultThreadsPerCore": defaultThreadsPerCore
      , "DefaultVCpus": defaultVCpus
      , "ValidCores": validCores
      , "ValidThreadsPerCore": validThreadsPerCore
      } -> do
      pure $ Just
        { defaultCores
        , defaultThreadsPerCore
        , defaultVCpus
        , validCores
        , validThreadsPerCore
        }
    Nothing -> pure Nothing

type MemoryInfo =
  { sizeInMiB :: Int
  }

type MemoryInfoInt =
  { "SizeInMiB" :: Int
  }

fromMemoryInfoInt :: MemoryInfoInt -> F MemoryInfo
fromMemoryInfoInt
  { "SizeInMiB": sizeInMiB
  } = do
  pure $
    { sizeInMiB
    }

type EbsOptimizedInfo =
  { baselineBandwidthInMbps :: Int
  , baselineThroughputInMBps :: Number
  , baselineIops :: Int
  , maximumBandwidthInMbps :: Int
  , maximumThroughputInMBps :: Number
  , maximumIops :: Int
  }

type EbsOptimizedInfoInt =
  { "BaselineBandwidthInMbps" :: Int
  , "BaselineThroughputInMBps" :: Number
  , "BaselineIops" :: Int
  , "MaximumBandwidthInMbps" :: Int
  , "MaximumThroughputInMBps" :: Number
  , "MaximumIops" :: Int
  }

fromEbsOptimizedInfoInt :: Maybe EbsOptimizedInfoInt -> F (Maybe EbsOptimizedInfo)
fromEbsOptimizedInfoInt info =
  case info of
    Just
      { "BaselineBandwidthInMbps": baselineBandwidthInMbps
      , "BaselineThroughputInMBps": baselineThroughputInMBps
      , "BaselineIops": baselineIops
      , "MaximumBandwidthInMbps": maximumBandwidthInMbps
      , "MaximumThroughputInMBps": maximumThroughputInMBps
      , "MaximumIops": maximumIops
      } -> do
      pure
        $ Just
        $
          { baselineBandwidthInMbps
          , baselineThroughputInMBps
          , baselineIops
          , maximumBandwidthInMbps
          , maximumThroughputInMBps
          , maximumIops
          }
    Nothing -> do
      pure Nothing

type EbsInfo =
  { ebsOptimizedSupport :: String
  , encryptionSupport :: String
  , ebsOptimizedInfo :: Maybe EbsOptimizedInfo
  , nvmeSupport :: String
  }

type EbsInfoInt =
  { "EbsOptimizedSupport" :: String
  , "EncryptionSupport" :: String
  , "EbsOptimizedInfo" :: Maybe EbsOptimizedInfoInt
  , "NvmeSupport" :: String
  }

fromEbsInfoInt :: EbsInfoInt -> F EbsInfo
fromEbsInfoInt
  { "EbsOptimizedSupport": ebsOptimizedSupport
  , "EncryptionSupport": encryptionSupport
  , "EbsOptimizedInfo": ebsOptimizedInfo
  , "NvmeSupport": nvmeSupport
  } = ado
  info <- fromEbsOptimizedInfoInt ebsOptimizedInfo
  in
    { ebsOptimizedSupport
    , encryptionSupport
    , ebsOptimizedInfo: info
    , nvmeSupport
    }

type NetworkCard =
  { networkCardIndex :: Int
  , networkPerformance :: String
  , maximumNetworkInterfaces :: Int
  }

type NetworkCardInt =
  { "NetworkCardIndex" :: Int
  , "NetworkPerformance" :: String
  , "MaximumNetworkInterfaces" :: Int
  }

fromNetworkCardInt :: NetworkCardInt -> F NetworkCard
fromNetworkCardInt
  { "NetworkCardIndex": networkCardIndex
  , "NetworkPerformance": networkPerformance
  , "MaximumNetworkInterfaces": maximumNetworkInterfaces
  } = do
  pure $
    { networkCardIndex
    , networkPerformance
    , maximumNetworkInterfaces
    }

type NetworkInfo =
  { networkPerformance :: String
  , maximumNetworkInterfaces :: Int
  , maximumNetworkCards :: Int
  , defaultNetworkCardIndex :: Int
  , networkCards :: List NetworkCard
  , ipv4AddressesPerInterface :: Int
  , ipv6AddressesPerInterface :: Int
  , ipv6Supported :: Boolean
  , enaSupport :: String
  , efaSupported :: Boolean
  , encryptionInTransitSupported :: Boolean
  }

type NetworkInfoInt =
  { "NetworkPerformance" :: String
  , "MaximumNetworkInterfaces" :: Int
  , "MaximumNetworkCards" :: Int
  , "DefaultNetworkCardIndex" :: Int
  , "NetworkCards" :: List NetworkCardInt
  , "Ipv4AddressesPerInterface" :: Int
  , "Ipv6AddressesPerInterface" :: Int
  , "Ipv6Supported" :: Boolean
  , "EnaSupport" :: String
  , "EfaSupported" :: Boolean
  , "EncryptionInTransitSupported" :: Boolean
  }

fromNetworkInfoInt :: NetworkInfoInt -> F NetworkInfo
fromNetworkInfoInt
  { "NetworkPerformance": networkPerformance
  , "MaximumNetworkInterfaces": maximumNetworkInterfaces
  , "MaximumNetworkCards": maximumNetworkCards
  , "DefaultNetworkCardIndex": defaultNetworkCardIndex
  , "NetworkCards": networkCardsInt
  , "Ipv4AddressesPerInterface": ipv4AddressesPerInterface
  , "Ipv6AddressesPerInterface": ipv6AddressesPerInterface
  , "Ipv6Supported": ipv6Supported
  , "EnaSupport": enaSupport
  , "EfaSupported": efaSupported
  , "EncryptionInTransitSupported": encryptionInTransitSupported
  } = ado

  networkCards <- traverse fromNetworkCardInt networkCardsInt

  in
    { networkPerformance
    , maximumNetworkInterfaces
    , maximumNetworkCards
    , defaultNetworkCardIndex
    , networkCards
    , ipv4AddressesPerInterface
    , ipv6AddressesPerInterface
    , ipv6Supported
    , enaSupport
    , efaSupported
    , encryptionInTransitSupported
    }

type PlacementGroupInfo =
  { supportedStrategies :: List PlacementGroupStrategies
  }

type PlacementGroupInfoInt =
  { "SupportedStrategies" :: List PlacementGroupStrategies
  }

fromPlacementGroupInfo :: PlacementGroupInfoInt -> F PlacementGroupInfo
fromPlacementGroupInfo
  { "SupportedStrategies": supportedStrategies
  } = do
  pure
    { supportedStrategies
    }

type InstanceTypeDescription =
  { instanceType :: InstanceType
  , currentGeneration :: Maybe Boolean
  , freeTierEligible :: Boolean
  , supportedUsageClasses :: List SupportedUsageClasses
  , supportedRootDeviceTypes :: List SupportedRootDeviceTypes
  , supportedVirtualizationTypes :: List SupportedVirtualizationTypes
  , bareMetal :: Boolean
  , hypervisor :: Maybe String
  , processorInfo :: ProcessorInfo
  , vCpuInfo :: Maybe VCpuInfo
  , memoryInfo :: MemoryInfo
  , instanceStorageSupported :: Boolean
  , ebsInfo :: Maybe EbsInfo
  , networkInfo :: Maybe NetworkInfo
  , placementGroupInfo :: Maybe PlacementGroupInfo
  , hibernationSupported :: Boolean
  , burstablePerformanceSupported :: Maybe Boolean
  , dedicatedHostsSupported :: Maybe Boolean
  , autoRecoverySupported :: Maybe Boolean
  , supportedBootModes :: List SupportedBootModes
  }

type InstanceTypeDescriptionInt =
  { "InstanceType" :: String
  , "CurrentGeneration" :: Maybe Boolean
  , "FreeTierEligible" :: Maybe Boolean
  , "SupportedUsageClasses" :: Maybe (List SupportedUsageClasses)
  , "SupportedRootDeviceTypes" :: Maybe (List SupportedRootDeviceTypes)
  , "SupportedVirtualizationTypes" :: Maybe (List SupportedVirtualizationTypes)
  , "BareMetal" :: Maybe Boolean
  , "Hypervisor" :: Maybe String
  , "ProcessorInfo" :: ProcessorInfoInt
  , "VCpuInfo" :: Maybe VCpuInfoInt
  , "MemoryInfo" :: MemoryInfoInt
  , "InstanceStorageSupported" :: Maybe Boolean
  , "EbsInfo" :: Maybe EbsInfoInt
  , "NetworkInfo" :: Maybe NetworkInfoInt
  , "PlacementGroupInfo" :: Maybe PlacementGroupInfoInt
  , "HibernationSupported" :: Maybe Boolean
  , "BurstablePerformanceSupported" :: Maybe Boolean
  , "DedicatedHostsSupported" :: Maybe Boolean
  , "AutoRecoverySupported" :: Maybe Boolean
  , "SupportedBootModes" :: Maybe (List SupportedBootModes)
  }

fromInstanceTypeDescriptionInt :: InstanceTypeDescriptionInt -> F InstanceTypeDescription
fromInstanceTypeDescriptionInt
  { "InstanceType": instanceType
  , "CurrentGeneration": currentGeneration
  , "FreeTierEligible": freeTierEligible
  , "SupportedUsageClasses": supportedUsageClasses
  , "SupportedRootDeviceTypes": supportedRootDeviceTypes
  , "SupportedVirtualizationTypes": supportedVirtualizationTypes
  , "BareMetal": bareMetal
  , "Hypervisor": hypervisor
  , "ProcessorInfo": processorInfoInt
  , "VCpuInfo": vCpuInfoInt
  , "MemoryInfo": memoryInfoInt
  , "InstanceStorageSupported": instanceStorageSupported
  , "EbsInfo": ebsInfoInt
  , "NetworkInfo": networkInfoInt
  , "PlacementGroupInfo": placementGroupInfoInt
  , "HibernationSupported": hibernationSupported
  , "BurstablePerformanceSupported": burstablePerformanceSupported
  , "DedicatedHostsSupported": dedicatedHostsSupported
  , "AutoRecoverySupported": autoRecoverySupported
  , "SupportedBootModes": supportedBootModes
  } = ado
  processorInfo <- fromProcessorInfoInt processorInfoInt
  vCpuInfo <- fromVCpuInfoInt vCpuInfoInt
  ebsInfo <- traverse fromEbsInfoInt ebsInfoInt
  memoryInfo <- fromMemoryInfoInt memoryInfoInt
  networkInfo <- traverse fromNetworkInfoInt networkInfoInt
  placementGroupInfo <- traverse fromPlacementGroupInfo placementGroupInfoInt
  in
    { instanceType: InstanceType instanceType
    , currentGeneration
    , freeTierEligible: fromMaybe false freeTierEligible
    , supportedUsageClasses: fromMaybe nil supportedUsageClasses
    , supportedRootDeviceTypes: fromMaybe nil supportedRootDeviceTypes
    , supportedVirtualizationTypes: fromMaybe nil supportedVirtualizationTypes
    , bareMetal: fromMaybe false bareMetal
    , hypervisor
    , processorInfo
    , vCpuInfo
    , memoryInfo
    , instanceStorageSupported: fromMaybe false instanceStorageSupported
    , ebsInfo
    , networkInfo
    , placementGroupInfo
    , hibernationSupported: fromMaybe false hibernationSupported
    , burstablePerformanceSupported
    , dedicatedHostsSupported
    , autoRecoverySupported
    , supportedBootModes: fromMaybe nil supportedBootModes
    }

type InstanceTypesResponse =
  { "InstanceTypes" :: List InstanceTypeDescriptionInt
  }

fromInstanceTypesResponseInt :: InstanceTypesResponse -> F (List InstanceTypeDescription)
fromInstanceTypesResponseInt { "InstanceTypes": instanceTypes } = ado
  types <- traverse fromInstanceTypeDescriptionInt instanceTypes
  in types


type LaunchTemplateInt =
  { "LaunchTemplateId" :: Maybe String
  , "LaunchTemplateName" :: Maybe String
  , "Version" :: Maybe String
  }

launchTemplateToInt :: LaunchTemplate -> LaunchTemplateInt
launchTemplateToInt {templateIdentifier,version} =
  {"LaunchTemplateId": case templateIdentifier of
      LaunchTemplateId val -> Just val
      LaunchTemplateName _ -> Nothing
  , "LaunchTemplateName": case templateIdentifier of
      LaunchTemplateId _ -> Nothing
      LaunchTemplateName val -> Just val
  , "Version": version
  }

describeInstanceTypes :: InstanceTypeRequest -> Effect (Either MultipleErrors (List InstanceTypeDescription))
describeInstanceTypes req = do
  let
    cli =
      awsCliBase req "describe-instance-types" -- <> " --query \"InstanceTypes[?InstanceType=='m2.4xlarge']\""
  outputJson <- runAwsCli cli
  pure $ runExcept $ fromInstanceTypesResponseInt =<< readJSON' =<< outputJson

booleanToDisabledEnabled :: Boolean -> String
booleanToDisabledEnabled true = "enabled"
booleanToDisabledEnabled false = "disabled"

awsCliBase :: forall t. BaseRequest t -> String -> String
awsCliBase { profile, region, dryRun, additionalCliArgs } command = do
  "aws ec2 "
    <> command
    <> " --output json --color off "
    <> (if dryRun then " --dry-run" else "")
    <> (fromMaybe "" $ (\r -> " --region " <> r) <$> unwrap <$> region)
    <> (fromMaybe "" $ (\p -> " --profile " <> p) <$> unwrap <$> profile)
    <> " "
    <> (intercalate " " additionalCliArgs)

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

foreign import base64Decode :: String -> Maybe String
