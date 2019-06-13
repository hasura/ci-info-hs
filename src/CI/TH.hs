module CI.TH
  ( getVendors
  ) where

import qualified Data.Aeson                 as Aeson
import qualified Language.Haskell.TH        as TH
import qualified Language.Haskell.TH.Syntax as TH

import qualified CI.Types                   as Types


getVendors :: TH.Q TH.Exp
getVendors = TH.runIO readVendors >>= TH.lift
  where
    vendorsPath = "res/vendors.json"

    readVendors :: IO [Types.Vendor]
    readVendors = do
      vendors <- Aeson.eitherDecodeFileStrict' vendorsPath
      case vendors of
        Left e  -> fail $ "parsing vendors.json failed: " <> e
        Right v -> return v
