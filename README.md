# cardano-crosschain-evo
Evolution of Cardano crosschain contracts.


## GroupNFT
Responsible to mint a NFT Token such as GroupNFTToken and AdminNFTToken. 
#### GroupNFTToken
GroupNFTToken stores important parameters for crosschain ,such as GPK, in inline datum
#### AdminNFTToken
AdminNFTToken stores Administator parameters ,such as all Administators's PK of and the Authorization thresholds, in inline datum

## GroupNFTHolder
The contract holds the GroupNFT.

## Treasury
The contract locks the crosschain assets (only cardano native token and ada). User send assets to Treasury when cross assets from Cardano to orther chains, and transfer assets from Treasury when crossing back.

## TreasuryCheck
The contract holds the TreasuryCheckToken minted via CheckToken, is responsible for verifying the spend operation of Treasury is authorized. 

## MappingToken
Responsible to mint Mapping Token of the crosschain assets which is from orther chain.

## MintCheck
The contract holds the MintCheckToken minted via CheckToken, is responsible for verifying the Mint operation of MappingToken is authorized. 

## NFTTreasury
The contract locks the NFT assets (only cardano native token and ada). User send assets to NFTTreasury when cross assets from Cardano to orther chains, and transfer assets from NFTTreasury when crossing back.

## NFTTreasuryCheck
The contract holds the NFTTreasuryCheckToken minted via CheckToken, is responsible for verifying the spend operation of NFTTreasury is authorized. 

## NFTMappingToken
Responsible to mint NFT Mapping Token of the crosschain NFT assets which is from orther chain.

## NFTMintCheck
The contract holds the NFTMintCheckToken minted via CheckToken, is responsible for verifying the Mint operation of NFT MappingToken is authorized. 

## StoremanStake
The contract address is usually used as the stake address for orther payment address such as Treasury.

## StakeCheck
Responsible for verifying the operations of StoremanStake is is authorized by Admin.

## AdminNFTHolder
The contract holds the AdminNFTToken minted via GroupNFT. Any managerment operations must be authorized by the AdminNFTHolder.