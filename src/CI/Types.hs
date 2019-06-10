{-# LANGUAGE DeriveLift #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module CI.Types where

import           Control.Lens               ( (^.) )

import qualified Data.Aeson                 as Aeson
import qualified Data.Aeson.Casing          as Aeson
import qualified Data.Aeson.TH              as Aeson
import           Data.HashMap.Strict        ( HashMap )
import qualified Data.HashMap.Strict        as HashMap
import           Data.Text                  ( Text )
import qualified Data.Text                  as T

import           Instances.TH.Lift          ()

import qualified Language.Haskell.TH.Syntax as TH

import qualified Network.Wreq               as Wreq

data CI =
      AWSCodeBuild -- https://aws.amazon.com/codebuild/
    | AppVeyor -- http://www.appveyor.com/
    | AzurePipelines -- https://azure.microsoft.com/en-us/services/devops/pipelines/
    | Bamboo -- https://www.atlassian.com/software/bamboo/
    | BitbucketPipelines -- https://bitbucket.org/product/features/pipelines/
    | Bitrise -- https://www.bitrise.io/
    | Buddy -- https://buddy.works/
    | Buildkite -- https://buildkite.com/
    | CircleCI -- http://circleci.com/
    | CirrusCI -- https://cirrus-ci.org/
    | Codeship -- https://codeship.com/
    | Drone -- https://drone.io/
    | Dsari -- https://github.com/rfinnie/dsari/
    | GitLabCI -- https://about.gitlab.com/gitlab-ci/
    | GoCD -- https://www.go.cd/
    | Hudson -- http://hudson-ci.org/
    | Jenkins -- https://jenkins-ci.org/
    | MagnumCI -- https://magnum-ci.com/
    | NetlifyCI -- https://www.netlify.com/
    | Nevercode -- http://nevercode.io/
    | SailCI -- https://sail.ci/
    | Semaphore -- https://semaphoreci.com/
    | Shippable -- https://www.shippable.com/
    | SolanoCI -- https://www.solanolabs.com/
    | StriderCD -- https://strider-cd.github.io/
    | TaskCluster -- http://docs.taskcluster.net/
    | TeamCity -- https://www.jetbrains.com/teamcity/
    | TravisCI -- http://travis-ci.org/
    | UnknownVendor
    deriving ( Eq, Show, TH.Lift )

instance Aeson.FromJSON CI where
    parseJSON = Aeson.withText "String" $ \case
        "APPVEYOR" -> return AppVeyor
        "AZURE_PIPELINES" -> return AzurePipelines
        "BAMBOO" -> return Bamboo
        "BITBUCKET" -> return BitbucketPipelines
        "BITRISE" -> return Bitrise
        "BUDDY" -> return Buddy
        "BUILDKITE" -> return Buildkite
        "CIRCLE" -> return CircleCI
        "CIRRUS" -> return CirrusCI
        "CODEBUILD" -> return AWSCodeBuild
        "CODESHIP" -> return Codeship
        "DRONE" -> return Drone
        "DSARI" -> return Dsari
        "GITLAB" -> return GitLabCI
        "GOCD" -> return GoCD
        "HUDSON" -> return Hudson
        "JENKINS" -> return Jenkins
        "MAGNUM" -> return MagnumCI
        "NETLIFY" -> return NetlifyCI
        "NEVERCODE" -> return Nevercode
        "SAIL" -> return SailCI
        "SEMAPHORE" -> return Semaphore
        "SHIPPABLE" -> return Shippable
        "SOLANO" -> return SolanoCI
        "STRIDER" -> return StriderCD
        "TASKCLUSTER" -> return TaskCluster
        "TEAMCITY" -> return TeamCity
        "TRAVIS" -> return TravisCI
        unknownVendor ->
            fail $ "Unknown vendor name: " <> T.unpack unknownVendor

instance (TH.Lift k, TH.Lift v) => TH.Lift (HashMap k v) where
    lift hashMap = [|HashMap.fromList $(TH.lift $ HashMap.toList hashMap)|]

data VendorEnv = VendorEnvString !Text
               | VendorEnvList ![Text]
               | VendorEnvObject !(HashMap Text Text)
    deriving ( Eq, Show, TH.Lift )

instance Aeson.FromJSON VendorEnv where
    parseJSON val = case val of
        Aeson.String text -> return $ VendorEnvString text
        Aeson.Array _ -> VendorEnvList <$> Aeson.parseJSON val
        Aeson.Object _ -> VendorEnvObject <$> Aeson.parseJSON val
        _ -> fail "expected String, List[String], or Map[String, String] in vendor env"

data Vendor =
    Vendor { vendorName :: Text, vendorConstant :: CI, vendorEnv :: VendorEnv }
    deriving ( Eq, Show, TH.Lift )

$(Aeson.deriveFromJSON (Aeson.aesonDrop (T.length "vendor")
                                        Aeson.snakeCase) { Aeson.omitNothingFields = True
                                                         }
                       ''Vendor)

getVendors :: IO [Vendor]
getVendors = do
    response <- Wreq.get vendorsUrl
    either fail return $ Aeson.eitherDecode $ response ^. Wreq.responseBody
  where
    vendorsUrl =
        "https://raw.githubusercontent.com/watson/ci-info/master/vendors.json"
