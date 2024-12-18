- # [issue WAN-101]
    - ## descript: 
    Treasury UTxOs can contain more than one token
    - ## solution
    `` isSingleAsset v cs tk = all (\(cs',tk',_) -> (cs' == cs && tk' == tk) || (cs' == Ada.adaSymbol  && tk' == Ada.adaToken)) $ flattenValue v ``

- # [issue WAN-001]
    - ## descript: 
    Multiple check tokens can be locked in the same UTxO( `checkOutput` in CheckToken.hs )
    - ## solution
    **Not fixing** for reasons below:
    1. All CheckToken can only be burned, not transfered.
    2. The amount of CheckToken in inputs of a transaction is limited to 1. (in Treasury.hs NFTTreasury.hs MappingToken.hs NFTMappingToken.hs)