module Properties where

minUtxoLovelace = 1700000 :: Int
batchLimit = 1000 :: Int
targetFileName = "batched-tx.sh"

{-testnet or mainnet-}
cardanoNetwork = "testnet"

{-This is the address where your tokens to be sent are stored-}
storageAddress = "addr_test1vph70x04mfnxjg2jme07q4qalgpkdc4q52rge82q7s53kecslmqdf"

{-This is the comma separated list of policy id, asset name and total quantity at the storage address -}
assetInfo = [  "08e81885037e4e6a7b3960b551260a4d29ed856bc10857f1bbac3a8f,TitanCoin,95055"
              ,"19bb9f9b1f89d54ea8f6274af15e2bfbf2730362d711cb26c8a92eb7,tClay,97549"
              ,"84e2b7e0ec32f81091c30f7a38b32c595f79ba8b54f0f98e2658873e,NanoNet,96712"
              ,"9f5b8c9fb2efa75cf3e8b0406213bcdbe27e280fe25e28717b369b7d,QuantumPulse,95537"
              ,"a0922a11cc9dde96fe7285f6856e23616e8c12da674ecda5a0b7cada,Agrochain,95610"
              ,"b2ae7d28fcc067c62342b4ea4073987c0e13159cf10e73ed762d5f69,NuTron,96683"
              ,"b5b9b76694768dd99362ff2e48d6f42b91cd0af37a9db7fbe7907c32,JungleApes,96411"
              ,"e02aa67d70e1be127a528b09e38b73882d824766df74a7dc126cece2,StellarWars,96522"
              ,"e66b8b16476aaa28d81a56dd75151c44a3aafa0e6b7189cbd6e545d6,BurstZK,94908"
              ,"f50e49cd95d6b6d9c3f4d760321c8eced1e89f461a495be76849d789,SpaceCats,95522"
            ]

{-The tx in should contain tx-ins of all native tokens at the storage address and tx-in for sufficient ADA-}
txInList = [ "92d2ba3c95bbf25c6a0f12526d941f7816abb7187f6b7055e85ed5ae8a1ad18e#0"
            ,"cdf444b788f4cd02d5b365916bd8192febb89c676198314aa40d71ade2cc02a5#180"
            ,"cdf444b788f4cd02d5b365916bd8192febb89c676198314aa40d71ade2cc02a5#181"
            ,"c39bfaf534e9195391423f4a14d613d24115dab52836d8f9176b99bb40c80fe0#0"
           ]
