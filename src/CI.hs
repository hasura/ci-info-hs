{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module CI ( isCI, getCI ) where

import           CI.Types                   ( CI )
import qualified CI.Types                   as CI

import           Control.Arrow              ( (***) )

import           Data.Bool                  ( bool )
import           Data.Foldable              ( find )
import qualified Data.HashMap.Strict        as HashMap
import           Data.Maybe                 ( isJust )
import qualified Data.Text                  as T

import qualified Language.Haskell.TH        as TH
import qualified Language.Haskell.TH.Syntax as TH

import           System.Environment         ( getEnvironment )

vendors :: [CI.Vendor]
vendors = $(TH.runIO CI.getVendors >>= TH.lift)

getCI :: IO (Maybe CI)
getCI = do
    env <- HashMap.fromList . map (T.pack *** T.pack) <$> getEnvironment
    let maybeVendor = find (checkVendor env) vendors
    return $ case maybeVendor of
        Nothing -> bool Nothing (Just CI.UnknownVendor) $
            any (`HashMap.member` env) -- check vendor neutral environment variables
                [ "CI" -- Travis CI, CircleCI, Cirrus CI, Gitlab CI, Appveyor, CodeShip, dsari
                , "CONTINUOUS_INTEGRATION" -- Travis CI, Cirrus CI
                , "BUILD_NUMBER" -- Jenkins, TeamCity
                , "RUN_ID" -- TaskCluster, dsari
                ]
        Just vendor -> Just $ CI.vendorConstant vendor
  where
    checkVendor env vendor = case CI.vendorEnv vendor of
        (CI.VendorEnvString text) -> HashMap.member text env
        (CI.VendorEnvList list) -> all (`HashMap.member` env) list
        (CI.VendorEnvObject hashMap) ->
            all (\(k, v) -> HashMap.lookup k env == Just v) $
            HashMap.toList hashMap

isCI :: IO Bool
isCI = isJust <$> getCI
