module Test.Main where

import Prelude
import Control.Monad.Free (Free)
import Data.Either (Either(..), fromRight')
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe(..), fromMaybe')
import Data.Show.Generic (genericShow)
import Effect (Effect)
import Effect.Class (liftEffect)
import Erl.Aws (describeInstances)
import Erl.Data.Binary.IOData (fromBinary)
import Erl.Data.Binary.UTF8 (toBinary)
import Erl.Data.List as List
import Erl.Data.Tuple (tuple4)
import Erl.Kernel.Inet (ConnectAddress(..), HostAddress(..), IpAddress(..), SocketActive(..), SocketAddress(..), ActiveError(..))
import Erl.Kernel.Tcp (TcpMessage(..))
import Erl.Kernel.Tcp as Tcp
import Erl.Kernel.Udp (UdpMessage(..))
import Erl.Kernel.Udp as Udp
import Erl.Process (Process, ProcessM, receive, spawnLink, unsafeRunProcessM, (!))
import Erl.Process.Class (self)
import Erl.Test.EUnit (TestF, runTests, suite, test)
import Erl.Types (Timeout(..))
import Erl.Untagged.Union (class RuntimeType, type (|$|), type (|+|), Nil, RTLiteralAtom, RTOption, RTTuple1, Union, inj, prj)
import Partial.Unsafe (unsafeCrashWith)
import Test.Assert (assertEqual, assertTrue)

main :: Effect Unit
main =
  void
    $ runTests do
        describeTests

describeTests :: Free TestF Unit
describeTests = do
  suite "describe-instances tests" do
    test "Can parse response" do
      unsafeRunProcessM
        $ do
            let actual = describeInstances describeResp
            liftEffect
              $ assertEqual { expected: Right List.nil, actual }

describeResp =
  """
{
  "Reservations": [
    {
      "Groups": [],
      "Instances": [
        {
          "AmiLaunchIndex": 0,
          "ImageId": "ami-bb373ddf",
          "InstanceId": "i-08ecb111c3d83fac2",
          "InstanceType": "t2.micro",
          "KeyName": "JumpBox",
          "LaunchTime": "2017-03-07T15:41:05+01:00",
          "Monitoring": {
            "State": "disabled"
          },
          "Placement": {
            "AvailabilityZone": "eu-west-2b",
            "GroupName": "",
            "Tenancy": "default"
          },
          "PrivateDnsName": "ip-172-31-29-98.eu-west-2.compute.internal",
          "PrivateIpAddress": "172.31.29.98",
          "ProductCodes": [
            {
              "ProductCodeId": "aw0evgkw8e5c1q413zgy5pjce",
              "ProductCodeType": "marketplace"
            }
          ],
          "PublicDnsName": "ec2-52-56-170-12.eu-west-2.compute.amazonaws.com",
          "PublicIpAddress": "52.56.170.12",
          "State": {
            "Code": 16,
            "Name": "running"
          },
          "StateTransitionReason": "",
          "SubnetId": "subnet-6141502b",
          "VpcId": "vpc-16e3287f",
          "Architecture": "x86_64",
          "BlockDeviceMappings": [
            {
              "DeviceName": "/dev/sda1",
              "Ebs": {
                "AttachTime": "2017-03-07T15:41:06+00:00",
                "DeleteOnTermination": false,
                "Status": "attached",
                "VolumeId": "vol-0f1c280c8aa7015bf"
              }
            }
          ],
          "ClientToken": "148890125928466306",
          "EbsOptimized": false,
          "Hypervisor": "xen",
          "NetworkInterfaces": [
            {
              "Association": {
                "IpOwnerId": "160572248038",
                "PublicDnsName": "ec2-52-56-170-12.eu-west-2.compute.amazonaws.com",
                "PublicIp": "52.56.170.12"
              },
              "Attachment": {
                "AttachTime": "2017-03-07T15:41:05+00:00",
                "AttachmentId": "eni-attach-1b870f78",
                "DeleteOnTermination": true,
                "DeviceIndex": 0,
                "Status": "attached",
                "NetworkCardIndex": 0
              },
              "Description": "",
              "Groups": [
                {
                  "GroupName": "JumpBox",
                  "GroupId": "sg-e24aea8b"
                }
              ],
              "Ipv6Addresses": [],
              "MacAddress": "0a:85:b0:5f:76:b1",
              "NetworkInterfaceId": "eni-b89b37f5",
              "OwnerId": "160572248038",
              "PrivateDnsName": "ip-172-31-29-98.eu-west-2.compute.internal",
              "PrivateIpAddress": "172.31.29.98",
              "PrivateIpAddresses": [
                {
                  "Association": {
                    "IpOwnerId": "160572248038",
                    "PublicDnsName": "ec2-52-56-170-12.eu-west-2.compute.amazonaws.com",
                    "PublicIp": "52.56.170.12"
                  },
                  "Primary": true,
                  "PrivateDnsName": "ip-172-31-29-98.eu-west-2.compute.internal",
                  "PrivateIpAddress": "172.31.29.98"
                }
              ],
              "SourceDestCheck": true,
              "Status": "in-use",
              "SubnetId": "subnet-6141502b",
              "VpcId": "vpc-16e3287f",
              "InterfaceType": "interface"
            }
          ],
          "RootDeviceName": "/dev/sda1",
          "RootDeviceType": "ebs",
          "SecurityGroups": [
            {
              "GroupName": "JumpBox",
              "GroupId": "sg-e24aea8b"
            }
          ],
          "SourceDestCheck": true,
          "Tags": [
            {
              "Key": "Name",
              "Value": "JumpBox"
            }
          ],
          "VirtualizationType": "hvm",
          "CpuOptions": {
            "CoreCount": 1,
            "ThreadsPerCore": 1
          },
          "CapacityReservationSpecification": {
            "CapacityReservationPreference": "open"
          },
          "HibernationOptions": {
            "Configured": false
          },
          "MetadataOptions": {
            "State": "applied",
            "HttpTokens": "optional",
            "HttpPutResponseHopLimit": 1,
            "HttpEndpoint": "enabled"
          },
          "EnclaveOptions": {
            "Enabled": false
          }
        }
      ],
      "OwnerId": "160572248038",
      "ReservationId": "r-08b1a38dbf1e545fd"
    },
    {
      "Groups": [],
      "Instances": [
        {
          "AmiLaunchIndex": 0,
          "ImageId": "ami-0eab3a90fc693af19",
          "InstanceId": "i-0561d09728cb9b920",
          "InstanceType": "t2.medium",
          "KeyName": "id3as-pulumi-pubkey-6341432",
          "LaunchTime": "2021-02-06T06:48:01-01:00",
          "Monitoring": {
            "State": "disabled"
          },
          "Placement": {
            "AvailabilityZone": "eu-west-2b",
            "GroupName": "",
            "Tenancy": "default"
          },
          "PrivateDnsName": "ip-172-31-17-108.eu-west-2.compute.internal",
          "PrivateIpAddress": "172.31.17.108",
          "ProductCodes": [
            {
              "ProductCodeId": "aw0evgkw8e5c1q413zgy5pjce",
              "ProductCodeType": "marketplace"
            }
          ],
          "PublicDnsName": "",
          "State": {
            "Code": 80,
            "Name": "stopped"
          },
          "StateTransitionReason": "User initiated",
          "SubnetId": "subnet-6141502b",
          "VpcId": "vpc-16e3287f",
          "Architecture": "x86_64",
          "BlockDeviceMappings": [
            {
              "DeviceName": "/dev/sda1",
              "Ebs": {
                "AttachTime": "2019-12-20T15:26:33+00:00",
                "DeleteOnTermination": true,
                "Status": "attached",
                "VolumeId": "vol-05f2b0f26ce8f2b0b"
              }
            }
          ],
          "ClientToken": "",
          "EbsOptimized": false,
          "EnaSupport": true,
          "Hypervisor": "xen",
          "IamInstanceProfile": {
            "Arn": "arn:aws:iam::160572248038:instance-profile/id3as-build-role-profile-b3bc41f",
            "Id": "AIPASKYW2B7TJUDVAZUGX"
          },
          "NetworkInterfaces": [
            {
              "Attachment": {
                "AttachTime": "2019-12-20T15:26:32+00:00",
                "AttachmentId": "eni-attach-0b2bef67357a71ead",
                "DeleteOnTermination": true,
                "DeviceIndex": 0,
                "Status": "attached",
                "NetworkCardIndex": 0
              },
              "Description": "",
              "Groups": [
                {
                  "GroupName": "id3as-webserver-secgrp-49b4271",
                  "GroupId": "sg-0e9d42091fcfe5bfc"
                }
              ],
              "Ipv6Addresses": [],
              "MacAddress": "0a:a3:33:7b:2f:fa",
              "NetworkInterfaceId": "eni-0e6c9093a8be38788",
              "OwnerId": "160572248038",
              "PrivateDnsName": "ip-172-31-17-108.eu-west-2.compute.internal",
              "PrivateIpAddress": "172.31.17.108",
              "PrivateIpAddresses": [
                {
                  "Primary": true,
                  "PrivateDnsName": "ip-172-31-17-108.eu-west-2.compute.internal",
                  "PrivateIpAddress": "172.31.17.108"
                }
              ],
              "SourceDestCheck": true,
              "Status": "in-use",
              "SubnetId": "subnet-6141502b",
              "VpcId": "vpc-16e3287f",
              "InterfaceType": "interface"
            }
          ],
          "RootDeviceName": "/dev/sda1",
          "RootDeviceType": "ebs",
          "SecurityGroups": [
            {
              "GroupName": "id3as-webserver-secgrp-49b4271",
              "GroupId": "sg-0e9d42091fcfe5bfc"
            }
          ],
          "SourceDestCheck": true,
          "StateReason": {
            "Code": "Client.InstanceInitiatedShutdown",
            "Message": "Client.InstanceInitiatedShutdown: Instance initiated shutdown"
          },
          "Tags": [
            {
              "Key": "Company",
              "Value": "id3as"
            },
            {
              "Key": "Name",
              "Value": "Arqiva Build Box"
            },
            {
              "Key": "Application",
              "Value": "arqiva-ps"
            }
          ],
          "VirtualizationType": "hvm",
          "CpuOptions": {
            "CoreCount": 2,
            "ThreadsPerCore": 1
          },
          "CapacityReservationSpecification": {
            "CapacityReservationPreference": "open"
          },
          "HibernationOptions": {
            "Configured": false
          },
          "MetadataOptions": {
            "State": "applied",
            "HttpTokens": "optional",
            "HttpPutResponseHopLimit": 1,
            "HttpEndpoint": "enabled"
          },
          "EnclaveOptions": {
            "Enabled": false
          }
        }
      ],
      "OwnerId": "160572248038",
      "ReservationId": "r-0261a83ff57f7257b"
    },
    {
      "Groups": [],
      "Instances": [
        {
          "AmiLaunchIndex": 0,
          "ImageId": "ami-bb373ddf",
          "InstanceId": "i-034f518c48652f138",
          "InstanceType": "t2.medium",
          "KeyName": "perform-pulumi-pubkey-0a065b0",
          "LaunchTime": "2021-09-02T10:13:59+00:00",
          "Monitoring": {
            "State": "disabled"
          },
          "Placement": {
            "AvailabilityZone": "eu-west-2b",
            "GroupName": "",
            "Tenancy": "default"
          },
          "PrivateDnsName": "ip-172-31-20-131.eu-west-2.compute.internal",
          "PrivateIpAddress": "172.31.20.131",
          "ProductCodes": [
            {
              "ProductCodeId": "aw0evgkw8e5c1q413zgy5pjce",
              "ProductCodeType": "marketplace"
            }
          ],
          "PublicDnsName": "",
          "State": {
            "Code": 80,
            "Name": "stopped"
          },
          "StateTransitionReason": "User initiated",
          "SubnetId": "subnet-6141502b",
          "VpcId": "vpc-16e3287f",
          "Architecture": "x86_64",
          "BlockDeviceMappings": [
            {
              "DeviceName": "/dev/sda1",
              "Ebs": {
                "AttachTime": "2020-02-06T12:44:11+00:00",
                "DeleteOnTermination": true,
                "Status": "attached",
                "VolumeId": "vol-047c3d51314caf834"
              }
            }
          ],
          "ClientToken": "",
          "EbsOptimized": false,
          "Hypervisor": "xen",
          "IamInstanceProfile": {
            "Arn": "arn:aws:iam::160572248038:instance-profile/perform-build-role-profile-5908c59",
            "Id": "AIPASKYW2B7TPZJS7TBN5"
          },
          "NetworkInterfaces": [
            {
              "Attachment": {
                "AttachTime": "2020-02-06T12:44:10+00:00",
                "AttachmentId": "eni-attach-0cdf2133413374b98",
                "DeleteOnTermination": true,
                "DeviceIndex": 0,
                "Status": "attached",
                "NetworkCardIndex": 0
              },
              "Description": "",
              "Groups": [
                {
                  "GroupName": "id3as-webserver-secgrp-c20b27a",
                  "GroupId": "sg-0dbe10ec22277de25"
                }
              ],
              "Ipv6Addresses": [],
              "MacAddress": "0a:a0:cc:33:ee:0c",
              "NetworkInterfaceId": "eni-0ce97a79ce6cc63f2",
              "OwnerId": "160572248038",
              "PrivateDnsName": "ip-172-31-20-131.eu-west-2.compute.internal",
              "PrivateIpAddress": "172.31.20.131",
              "PrivateIpAddresses": [
                {
                  "Primary": true,
                  "PrivateDnsName": "ip-172-31-20-131.eu-west-2.compute.internal",
                  "PrivateIpAddress": "172.31.20.131"
                }
              ],
              "SourceDestCheck": true,
              "Status": "in-use",
              "SubnetId": "subnet-6141502b",
              "VpcId": "vpc-16e3287f",
              "InterfaceType": "interface"
            }
          ],
          "RootDeviceName": "/dev/sda1",
          "RootDeviceType": "ebs",
          "SecurityGroups": [
            {
              "GroupName": "id3as-webserver-secgrp-c20b27a",
              "GroupId": "sg-0dbe10ec22277de25"
            }
          ],
          "SourceDestCheck": true,
          "StateReason": {
            "Code": "Client.InstanceInitiatedShutdown",
            "Message": "Client.InstanceInitiatedShutdown: Instance initiated shutdown"
          },
          "Tags": [
            {
              "Key": "Name",
              "Value": "Perform Build Box"
            },
            {
              "Key": "Company",
              "Value": "id3as"
            },
            {
              "Key": "Application",
              "Value": "perform"
            }
          ],
          "VirtualizationType": "hvm",
          "CpuOptions": {
            "CoreCount": 2,
            "ThreadsPerCore": 1
          },
          "CapacityReservationSpecification": {
            "CapacityReservationPreference": "open"
          },
          "HibernationOptions": {
            "Configured": false
          },
          "MetadataOptions": {
            "State": "applied",
            "HttpTokens": "optional",
            "HttpPutResponseHopLimit": 1,
            "HttpEndpoint": "enabled"
          },
          "EnclaveOptions": {
            "Enabled": false
          }
        }
      ],
      "OwnerId": "160572248038",
      "ReservationId": "r-037159e162b7d7618"
    },
    {
      "Groups": [],
      "Instances": [
        {
          "AmiLaunchIndex": 0,
          "ImageId": "ami-0ff4c8fb495a5a50d",
          "InstanceId": "i-03981a9c8f046397e",
          "InstanceType": "t3.small",
          "KeyName": "llhls",
          "LaunchTime": "2021-02-08T15:06:25+00:00",
          "Monitoring": {
            "State": "disabled"
          },
          "Placement": {
            "AvailabilityZone": "eu-west-2c",
            "GroupName": "",
            "Tenancy": "default"
          },
          "PrivateDnsName": "ip-172-31-47-200.eu-west-2.compute.internal",
          "PrivateIpAddress": "172.31.47.200",
          "ProductCodes": [],
          "PublicDnsName": "ec2-18-132-168-223.eu-west-2.compute.amazonaws.com",
          "PublicIpAddress": "18.132.168.223",
          "State": {
            "Code": 16,
            "Name": "running"
          },
          "StateTransitionReason": "",
          "SubnetId": "subnet-90e120f9",
          "VpcId": "vpc-16e3287f",
          "Architecture": "x86_64",
          "BlockDeviceMappings": [
            {
              "DeviceName": "/dev/sda1",
              "Ebs": {
                "AttachTime": "2021-02-04T10:02:23+00:00",
                "DeleteOnTermination": true,
                "Status": "attached",
                "VolumeId": "vol-02363c779ec170c30"
              }
            }
          ],
          "ClientToken": "",
          "EbsOptimized": true,
          "EnaSupport": true,
          "Hypervisor": "xen",
          "NetworkInterfaces": [
            {
              "Association": {
                "IpOwnerId": "160572248038",
                "PublicDnsName": "ec2-18-132-168-223.eu-west-2.compute.amazonaws.com",
                "PublicIp": "18.132.168.223"
              },
              "Attachment": {
                "AttachTime": "2021-02-04T10:02:22+00:00",
                "AttachmentId": "eni-attach-0d5215f553bc33b32",
                "DeleteOnTermination": true,
                "DeviceIndex": 0,
                "Status": "attached",
                "NetworkCardIndex": 0
              },
              "Description": "",
              "Groups": [
                {
                  "GroupName": "llhls",
                  "GroupId": "sg-0f036655aea987463"
                }
              ],
              "Ipv6Addresses": [],
              "MacAddress": "02:15:3c:2a:c4:e6",
              "NetworkInterfaceId": "eni-08f928be40d46fd8e",
              "OwnerId": "160572248038",
              "PrivateDnsName": "ip-172-31-47-200.eu-west-2.compute.internal",
              "PrivateIpAddress": "172.31.47.200",
              "PrivateIpAddresses": [
                {
                  "Association": {
                    "IpOwnerId": "160572248038",
                    "PublicDnsName": "ec2-18-132-168-223.eu-west-2.compute.amazonaws.com",
                    "PublicIp": "18.132.168.223"
                  },
                  "Primary": true,
                  "PrivateDnsName": "ip-172-31-47-200.eu-west-2.compute.internal",
                  "PrivateIpAddress": "172.31.47.200"
                }
              ],
              "SourceDestCheck": true,
              "Status": "in-use",
              "SubnetId": "subnet-90e120f9",
              "VpcId": "vpc-16e3287f",
              "InterfaceType": "interface"
            }
          ],
          "RootDeviceName": "/dev/sda1",
          "RootDeviceType": "ebs",
          "SecurityGroups": [
            {
              "GroupName": "llhls",
              "GroupId": "sg-0f036655aea987463"
            }
          ],
          "SourceDestCheck": true,
          "Tags": [
            {
              "Key": "Name",
              "Value": "LL HLS Demo"
            },
            {
              "Key": "llhls",
              "Value": ""
            }
          ],
          "VirtualizationType": "hvm",
          "CpuOptions": {
            "CoreCount": 1,
            "ThreadsPerCore": 2
          },
          "CapacityReservationSpecification": {
            "CapacityReservationPreference": "open"
          },
          "HibernationOptions": {
            "Configured": false
          },
          "MetadataOptions": {
            "State": "applied",
            "HttpTokens": "optional",
            "HttpPutResponseHopLimit": 1,
            "HttpEndpoint": "enabled"
          },
          "EnclaveOptions": {
            "Enabled": false
          }
        }
      ],
      "OwnerId": "160572248038",
      "ReservationId": "r-0bcc2755fee5f2f26"
    },
    {
      "Groups": [],
      "Instances": [
        {
          "AmiLaunchIndex": 0,
          "ImageId": "ami-098828924dc89ea4a",
          "InstanceId": "i-0a9f27cb43dec9986",
          "InstanceType": "t3.large",
          "KeyName": "tva-pulumi-pubkey-eb10e45",
          "LaunchTime": "2021-05-13T08:14:48+00:00",
          "Monitoring": {
            "State": "disabled"
          },
          "Placement": {
            "AvailabilityZone": "eu-west-2c",
            "GroupName": "",
            "Tenancy": "default"
          },
          "PrivateDnsName": "ip-172-31-40-232.eu-west-2.compute.internal",
          "PrivateIpAddress": "172.31.40.232",
          "ProductCodes": [],
          "PublicDnsName": "",
          "State": {
            "Code": 80,
            "Name": "stopped"
          },
          "StateTransitionReason": "User initiated",
          "SubnetId": "subnet-90e120f9",
          "VpcId": "vpc-16e3287f",
          "Architecture": "x86_64",
          "BlockDeviceMappings": [
            {
              "DeviceName": "/dev/xvda",
              "Ebs": {
                "AttachTime": "2021-02-05T16:30:01+00:00",
                "DeleteOnTermination": true,
                "Status": "attached",
                "VolumeId": "vol-0c47fda74727ae0aa"
              }
            }
          ],
          "ClientToken": "AAF977E7-58AD-4AEF-BAAD-51EC94140392",
          "EbsOptimized": false,
          "EnaSupport": true,
          "Hypervisor": "xen",
          "IamInstanceProfile": {
            "Arn": "arn:aws:iam::160572248038:instance-profile/tva-build-role-profile-6e2f3d3",
            "Id": "AIPASKYW2B7TBQKA7Q3MT"
          },
          "NetworkInterfaces": [
            {
              "Attachment": {
                "AttachTime": "2021-02-05T16:30:00+00:00",
                "AttachmentId": "eni-attach-00f01b5674f3416ea",
                "DeleteOnTermination": true,
                "DeviceIndex": 0,
                "Status": "attached",
                "NetworkCardIndex": 0
              },
              "Description": "",
              "Groups": [
                {
                  "GroupName": "tva-webserver-secgrp-2d26d73",
                  "GroupId": "sg-09ad92cccaf282b0e"
                }
              ],
              "Ipv6Addresses": [],
              "MacAddress": "02:b0:5b:ec:22:40",
              "NetworkInterfaceId": "eni-03f6e6a6a7aa09768",
              "OwnerId": "160572248038",
              "PrivateDnsName": "ip-172-31-40-232.eu-west-2.compute.internal",
              "PrivateIpAddress": "172.31.40.232",
              "PrivateIpAddresses": [
                {
                  "Primary": true,
                  "PrivateDnsName": "ip-172-31-40-232.eu-west-2.compute.internal",
                  "PrivateIpAddress": "172.31.40.232"
                }
              ],
              "SourceDestCheck": true,
              "Status": "in-use",
              "SubnetId": "subnet-90e120f9",
              "VpcId": "vpc-16e3287f",
              "InterfaceType": "interface"
            }
          ],
          "RootDeviceName": "/dev/xvda",
          "RootDeviceType": "ebs",
          "SecurityGroups": [
            {
              "GroupName": "tva-webserver-secgrp-2d26d73",
              "GroupId": "sg-09ad92cccaf282b0e"
            }
          ],
          "SourceDestCheck": true,
          "StateReason": {
            "Code": "Client.InstanceInitiatedShutdown",
            "Message": "Client.InstanceInitiatedShutdown: Instance initiated shutdown"
          },
          "Tags": [
            {
              "Key": "Application",
              "Value": "tva_sonlife-ps"
            },
            {
              "Key": "Company",
              "Value": "tva"
            },
            {
              "Key": "Name",
              "Value": "TVA Build Box"
            }
          ],
          "VirtualizationType": "hvm",
          "CpuOptions": {
            "CoreCount": 1,
            "ThreadsPerCore": 2
          },
          "CapacityReservationSpecification": {
            "CapacityReservationPreference": "open"
          },
          "HibernationOptions": {
            "Configured": false
          },
          "MetadataOptions": {
            "State": "applied",
            "HttpTokens": "optional",
            "HttpPutResponseHopLimit": 1,
            "HttpEndpoint": "enabled"
          },
          "EnclaveOptions": {
            "Enabled": false
          }
        }
      ],
      "OwnerId": "160572248038",
      "ReservationId": "r-053faba6f45cd3966"
    }
  ]
}
"""
