
module Types where

  data AssetClass = AssetClass { policyId :: String
                               , assetName :: String
                               } deriving (Eq, Ord)

  data AssetTotal = AssetTotal { assetClass :: AssetClass
                               , total :: Int
                               } deriving (Eq, Ord)