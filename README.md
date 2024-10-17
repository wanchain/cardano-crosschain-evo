# cardano-crosschain-evo
Evolution of Cardano crosschain contracts.

# Contracts Description
* #### GroupNFT
     Responsible to mint a NFT Token such as GroupNFTToken and AdminNFTToken. 
* #### GroupNFTToken
    GroupNFTToken stores important parameters for crosschain ,such as GPK, in inline datum
* #### AdminNFTToken
    AdminNFTToken stores Administator parameters ,such as all Administators's PK of and the Authorization thresholds, in inline datum

* ### GroupNFTHolder
    The contract holds the GroupNFT.

* ### Treasury
    The contract locks the crosschain assets (only cardano native token and ada). User send assets to Treasury when cross assets from Cardano to orther chains, and transfer assets from Treasury when crossing back.

* ### TreasuryCheck
    The contract holds the TreasuryCheckToken minted via CheckToken, is responsible for verifying the spend operation of Treasury is authorized. 

* ### MappingToken
    Responsible to mint Mapping Token of the crosschain assets which is from orther chain.

* ### MintCheck
    The contract holds the MintCheckToken minted via CheckToken, is responsible for verifying the Mint operation of MappingToken is authorized. 

* ### NFTTreasury
    The contract locks the NFT assets (only cardano native token and ada). User send assets to NFTTreasury when cross assets from Cardano to orther chains, and transfer assets from NFTTreasury when crossing back.

* ### NFTTreasuryCheck
    The contract holds the NFTTreasuryCheckToken minted via CheckToken, is responsible for verifying the spend operation of NFTTreasury is authorized. 

* ### NFTMappingToken
    Responsible to mint NFT Mapping Token of the crosschain NFT assets which is from orther chain.

* ### NFTMintCheck
    The contract holds the NFTMintCheckToken minted via CheckToken, is responsible for verifying the Mint operation of NFT MappingToken is authorized. 

* ### StoremanStake
    The contract address is usually used as the stake address for orther payment address such as Treasury.

* ### StakeCheck
    Responsible for verifying the operations of StoremanStake is is authorized by Admin.

* ### AdminNFTHolder
    The contract holds the AdminNFTToken minted via GroupNFT. Any managerment operations must be authorized by the AdminNFTHolder.

      
<br />
<br />


# Scenario Description

|Scenario|Input|Referece Input|Output|Validator Rules|
| -----------------------------------------------| ---------------------------------------------------------------------------------------------| ---------------------------| ---------------------------------------------------------------------------------------------------------------------------| ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
|Mint GroupNFT Token|1. utxo which is specified by the contract parameters||1. utxo with GroupNFT Token (whose owner is usually the GroupNFTHolder)|1. must spend the utxo which is specified by GroupNFT ‘s parameter|
|Update GroupNFT Token|1. utxo with GroupNFT Token||1. utxo with GroupInfoNFT Token|1. must spend utxo with AdminNFT when update any one of the parameters or signed by oracle-worker when update GPK.<br />2. update only one of the parameter at a time except when setting a new version(upgrading the GroupNFTHolder contract)<br />3. check the owner of GroupNFT is not changed except when setting a new version.|
|Mint AdminNFT Token|1.utxo which is specified by the contract parameters||1.utxo with AdminNFT Token (whose owner is usually the AdminNFTHolder)|1. must spend the utxo which is specified by GroupNFT‘s parameter( an another utxo is different with in Mint GrouNFT )|
|Spend AdminNFT Token|1. utxo with AdminNFT Token||1. utxo with AdminNFT Token|1. satisfies m/n multi-signatures<br />2. check AdminNFT in outputs:<br />       i) if action is Update ,check the new datum of the AdminNFT is valid. And the owner is AdminNFTHolder<br />      ii) if action is Use , check the datum of the AdminNFT is not changed.And the owner is AdminNFTHolder<br />      iii) if action is Upgrade ,the owner of AdminNFT must be a contract|
|Mint XXXCheckToken|1.utxo with AdminNFT Token||1.utxo with AdminNFT <br />|1. must spend AdminNFT<br />2. check the owner of XXXCheckToken in outputs:<br />    i) TreasuryCheckToken is owner By TreasuryCheck<br />    ii) MintCheckToken is owner By MintCheck<br />    iii) NFTTreasuryCheckToken is owner By NFTTreasuryCheck<br />    iiii) NFTMintCheckToken is owner By NFTMintCheck<br />3. check the amount of XXXCheckToken in each utxo is no bigger than 1|
|Burn XXXCheckToken|1.utxo with AdminNFT Token||1.utxo with AdminNFT|1. must spend AdminNFT<br />2. check all XXXCheckToken has been burned|
|Operations about Stake|1.utxo with AdminNFT Token||1.utxo with AdminNFT<br />2. utxo owner by StackCheck if needed|1. must spend AdminNFT<br />2. check one of the outputs owner by StackCheck if redeemer is SpendU.|
||||||
|Transfer assets from Treasury to user|1. utxos owned by Treasury<br />2. utxo with TreasuryCheckToken owner by TreasuryCheck|1. utxo with GroupInfoNFT|1. utxos with assets transferd from Treasury<br />2. utxo with TreasuryCheckToken 3. utxos as changes if needed<br />|1. check the redeemer is signed by MPC<br />2. check the tx  must spend the utxo specified by the redeemer<br />3. check the transfer assets is no bigger than the amount specified by the redeemer.<br />4. check the assets received by user is no less than the amount specified by the redeemer.<br />5. check the tx is not expired.<br />6.  check the owner of TresuryCheckToken is still the TreasuryCheck.<br />7. check the datum of the use's output is the userData specified by redeemer if the user is a contract|
|Mint MappingToken to user|1. utxo with MintCheckToken owner By MintCheck|1. utxo with GroupInfoNFT|1. utxos with MappingToken <br />2. utxo with MintCheckToken <br />3. utxos as changes if needed<br />|1. check the redeemer is signed by MPC<br />2. check the tx  must spend the utxo specified by the redeemer<br />3. check the minted assets is equal the amount specified by the redeemer.<br />4. check the assets received by user is no less then the amount specified by the redeemer.<br />5. check the tx is not expired.<br />6.  check the owner of MintCheckToken is still the MintCheck.<br />7. check the datum of the use's output is the userData specified by redeemer if the user is a contract<br />|
|Transfer NFT assets from NFTTtreasury to user|1. utxos owned by NFTTreasury<br />2. utxo with NFTTreasuryCheckToken owner by NFTTreasuryCheck|1. utxo with GroupInfoNFT|1. utxos with NFT assets <br />2. utxo with TreasuryCheckToken <br />3. utxos as changes if needed<br />|1. check the redeemer is signed by MPC<br />2. check the tx  must spend the utxo specified by the redeemer<br />3. check the transfer assets is no bigger than the amount specified by the redeemer.<br />4. check the assets received by user is no less than the amount specified by the redeemer.<br />5. check the tx is not expired.<br />6.  check the owner of NFTTresuryCheckToken is still the NFTTreasuryCheck.<br />7. check the datum of the use's output is the userData specified by redeemer if the user is a contract<br />8. only the assets has the same policy can be tranfered together except that if txType == 2|
|Mint NFTMappingToken to user|1. utxo with MintNFTCheckToken owner By NFTMintCheck|1. utxo with GroupInfoNFT|1. utxos with NFTMappingToken ,and NFT Reference Token <br />2. utxo with NFTMintCheckToken <br />3. utxos as changes if needed<br />|1. check the redeemer is signed by MPC<br />2. check the tx  must spend the utxo specified by the redeemer<br />3. check the amount , datum and owner fo the minted NFT Mapping Token match the redeemer<br />4. check the types and datums of the minted NFT Reference Token match the redeemer, and the owner is NFTRefHolder (specified by GrouNFT's datum)<br />5. check the tx is not expired.<br />6.  check the owner of NFTMintCheckToken is still the NFTMintCheck<br />7. only the NFT assets has the same policy can be minted together<br />|