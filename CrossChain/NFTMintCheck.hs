{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies       #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE NamedFieldPuns       #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ViewPatterns         #-}
-- {-# LANGUAGE FlexibleContexts   #-}
-- {-# LANGUAGE NamedFieldPuns     #-}
-- {-# LANGUAGE OverloadedStrings  #-}
-- {-# LANGUAGE TypeOperators      #-}
-- {-# OPTIONS_GHC -fno-ignore-interface-pragmas #-}
-- {-# OPTIONS_GHC -fno-specialise #-}
{-# OPTIONS_GHC -fplugin-opt PlutusTx.Plugin:profile-all #-}
{-# OPTIONS_GHC -fplugin-opt PlutusTx.Plugin:dump-uplc #-}

module CrossChain.NFTMintCheck
  ( mintNFTCheckScript
  -- , authorityCheckScriptShortBs
  ,mintNFTCheckScriptHash
  -- ,authorityCheckScriptHashStr
  ,mintNFTCheckAddress
  , NFTMintCheckProofData (..)
  , NFTMintCheckProof (..)
  , NFTMintCheckRedeemer (..)
  -- , GroupAdminNFTCheckTokenInfo (..)
  -- , GroupAdminNFTCheckTokenInfo
  ) where

import Data.Aeson (FromJSON, ToJSON)
import GHC.Generics (Generic)
import Cardano.Api.Shelley (PlutusScript (..), PlutusScriptV2)
import Prelude hiding (($),(<>), (&&), (&&), (==),(||),(>=),(<=),(+),(<),(-),not,length,filter,(>),(!!),map,head,reverse,any,elem,snd,mconcat,negate,all,fst)

import Codec.Serialise
import Data.ByteString.Lazy qualified as LBS
import Data.ByteString.Short qualified as SBS

-- import Plutus.Script.Utils.V2.Typed.Scripts.Validators as Scripts
import Plutus.Script.Utils.V2.Typed.Scripts qualified as PV2
import Plutus.Script.Utils.V2.Scripts as Scripts
import Plutus.V2.Ledger.Api qualified as Plutus
import Plutus.V2.Ledger.Contexts as V2
import PlutusTx qualified
-- import PlutusTx.Builtins
import PlutusTx.Builtins
-- import PlutusTx.Eq as PlutusTx
-- import PlutusTx.Eq()
import PlutusTx.Prelude hiding (SemigroupInfo (..), unless, (.))
-- import PlutusTx.Prelude qualified as PlutusPrelude
import           Ledger               hiding (validatorHash,validatorHash)
import Plutus.V2.Ledger.Tx (isPayToScriptOut,OutputDatum (..))
import Ledger.Typed.Scripts (ValidatorTypes (..), TypedValidator (..),mkTypedValidator,mkTypedValidatorParam) --,mkUntypedValidator )
-- import Plutus.Script.Utils.Typed (validatorScript,validatorAddress,validatorHash)

import Data.ByteString qualified as ByteString
import Ledger.Crypto (PubKey (..), PubKeyHash, pubKeyHash)
import Plutus.V1.Ledger.Bytes (LedgerBytes (LedgerBytes),fromBytes,getLedgerBytes)
import Ledger.Ada  as Ada
import Plutus.V1.Ledger.Value (valueOf) --,currencySymbol,tokenName,symbols,flattenValue,assetClass)
import PlutusTx.Builtins --(decodeUtf8,sha3_256,appendByteString)
import Ledger.Address 
import Ledger.Value
import Plutus.V2.Ledger.Contexts as V2
import Ledger.Typed.Scripts qualified as Scripts hiding (validatorHash)
import Plutus.V1.Ledger.Tx
import CrossChain.Types hiding (uniqueId)
-- ===================================================
-- import Plutus.V1.Ledger.Value
-- import Ledger.Address (PaymentPrivateKey (PaymentPrivateKey, unPaymentPrivateKey), PaymentPubKey (PaymentPubKey),PaymentPubKeyHash (..),unPaymentPubKeyHash,toPubKeyHash,toValidatorHash)

import Ledger hiding (validatorHash) --singleton



data NFTMintCheckProofData = NFTMintCheckProofData
  {
    uniqueId :: BuiltinByteString
    , nonce :: TxOutRef
    , mode :: Integer
    , toAddr :: Address
    , policy :: BuiltinByteString -- which token , zero indicated only transfer ada
    , nftAssets :: [(BuiltinByteString, Integer)]
    , userData :: OutputDatum
    , nftRefAssets:: [(Integer, BuiltinByteString, OutputDatum)]
    , ttl :: Integer
  }deriving (Prelude.Eq, Show)

PlutusTx.unstableMakeIsData ''NFTMintCheckProofData
PlutusTx.makeLift ''NFTMintCheckProofData

data NFTMintCheckProof = NFTMintCheckProof
  {
    proof :: NFTMintCheckProofData
    , signature :: BuiltinByteString
  }deriving (Prelude.Eq, Show)


PlutusTx.unstableMakeIsData ''NFTMintCheckProof
PlutusTx.makeLift ''NFTMintCheckProof


data NFTMintCheckRedeemer = BurnNFTMintCheckToken | NFTMintCheckRedeemer NFTMintCheckProof
    deriving (Show, Prelude.Eq)
PlutusTx.unstableMakeIsData ''NFTMintCheckRedeemer


data TreasuryType
instance Scripts.ValidatorTypes TreasuryType where
    type instance DatumType TreasuryType = ()
    type instance RedeemerType TreasuryType = NFTMintCheckRedeemer

-- data GroupAdminNFTCheckTokenInfo
--   = GroupAdminNFTCheckTokenInfo
--       { groupNftInfo :: GroupNFTTokenInfo
--         , adminNft :: AdminNftTokenInfo
--         , checkToken:: CheckTokenInfo
--       } deriving stock (Generic)
--         deriving anyclass (ToJSON, FromJSON)

-- PlutusTx.unstableMakeIsData ''GroupAdminNFTCheckTokenInfo
-- PlutusTx.makeLift ''GroupAdminNFTCheckTokenInfo

{-# INLINABLE burnTokenCheck #-}
burnTokenCheck :: GroupAdminNFTCheckTokenInfo -> V2.ScriptContext -> Bool
burnTokenCheck (GroupAdminNFTCheckTokenInfo (GroupNFTTokenInfo groupInfoCurrency groupInfoTokenName) (AdminNftTokenInfo adminNftSymbol adminNftName) (CheckTokenInfo checkTokenSymbol checkTokenName)) ctx = 
  traceIfFalse "a"  hasAdminNftInInput
  -- && traceIfFalse "b" checkOutput
  where 
    info :: V2.TxInfo
    !info = V2.scriptContextTxInfo ctx

    groupInfo :: GroupInfoParams
    !groupInfo = getGroupInfo info groupInfoCurrency groupInfoTokenName

    hasAdminNftInInput :: Bool
    hasAdminNftInInput = 
      let !totalInputValue = V2.valueSpent info
          !amount = valueOf totalInputValue adminNftSymbol adminNftName
      in amount == 1

    checkOutput :: Bool
    checkOutput = 
      let outputValue = V2.valueProduced info
      in valueOf outputValue checkTokenSymbol checkTokenName == 0
      -- let !totalAmountOfCheckTokenInOutput = getAmountOfCheckTokenInOutput ctx checkTokenSymbol checkTokenName
      --     outputsAtChecker = map snd $ scriptOutputsAt' (ValidatorHash (getGroupInfoParams groupInfo NFTMintCheckVH)) (getGroupInfoParams groupInfo StkVh) info True
      --     outputAtCheckerSum = valueOf (mconcat outputsAtChecker) checkTokenSymbol checkTokenName
      -- in totalAmountOfCheckTokenInOutput == outputAtCheckerSum && (length outputsAtChecker) == outputAtCheckerSum




{-# INLINABLE mintSpendCheck #-}
mintSpendCheck :: GroupAdminNFTCheckTokenInfo -> NFTMintCheckProof -> V2.ScriptContext -> Bool
mintSpendCheck (GroupAdminNFTCheckTokenInfo (GroupNFTTokenInfo groupInfoCurrency groupInfoTokenName) (AdminNftTokenInfo adminNftSymbol adminNftName) (CheckTokenInfo checkTokenSymbol checkTokenName)) NFTMintCheckProof{proof= p@NFTMintCheckProofData{uniqueId, nonce, mode, toAddr, policy, nftAssets, userData, nftRefAssets, ttl}, signature} ctx = 
  traceIfFalse "1" hasUTxO  && 
  traceIfFalse "2" (amountOfCheckTokeninOwnOutput == 1) && 
  traceIfFalse "3" checkSignature &&  
  -- traceIfFalse "m3" checkMint && 
  traceIfFalse "4" checkOutput &&
  traceIfFalse "5" checkTtl
  where
    
    info :: V2.TxInfo
    !info = V2.scriptContextTxInfo ctx

    hasUTxO :: Bool
    hasUTxO = 
      let V2.ScriptContext{V2.scriptContextPurpose=Spending txOutRef} = ctx in txOutRef == nonce

    groupInfo :: GroupInfoParams
    !groupInfo = getGroupInfo info groupInfoCurrency groupInfoTokenName

    stkVh :: BuiltinByteString
    !stkVh = getGroupInfoParams groupInfo StkVh

    amountOfCheckTokeninOwnOutput :: Integer
    amountOfCheckTokeninOwnOutput = getAmountOfCheckTokeninOwnOutput ctx checkTokenSymbol checkTokenName stkVh

    -- hashRedeemer :: BuiltinByteString
    -- hashRedeemer = 
    --     let tmp1 = serialiseData $ PlutusTx.toBuiltinData p
    --     in sha3_256 tmp1


    -- checkSignature :: Bool
    -- !checkSignature -- mode pk hash signature
    --   | mode == 0 = verifyEcdsaSecp256k1Signature (getGroupInfoParams groupInfo GPK) hashRedeemer signature
    --   | mode == 1 = verifySchnorrSecp256k1Signature (getGroupInfoParams groupInfo GPK) hashRedeemer signature
    --   | mode == 2 = verifyEd25519Signature (getGroupInfoParams groupInfo GPK) hashRedeemer signature

    checkSignature :: Bool
    checkSignature =
      let gpk = getGroupInfoParams groupInfo GPK
          hashRedeemer = sha3_256 (serialiseData (PlutusTx.toBuiltinData p))
      in 
        if mode == 0 then verifyEcdsaSecp256k1Signature gpk hashRedeemer signature
        else if mode == 1 then verifySchnorrSecp256k1Signature gpk hashRedeemer signature
        else verifyEd25519Signature gpk hashRedeemer signature


    -- nftCrossValue :: Value
    -- nftCrossValue = mconcat $ map (\(cname,amount) -> Plutus.singleton (CurrencySymbol policy) (TokenName cname) amount) nftAssets

    -- nftRefCrossValue :: Value
    -- nftRefCrossValue = mconcat $ map (\(_,cname,amount) -> Plutus.singleton (CurrencySymbol policy) (TokenName cname) 1) nftRefAssets



    -- outputs :: [V2.TxOut]
    -- !outputs = V2.txInfoOutputs info

    -- nftRefHolderAddress :: Address
    -- nftRefHolderAddress = Address (Plutus.ScriptCredential  (ValidatorHash (getGroupInfoParams groupInfo NFTRefHolderVH)) ) (Just (Plutus.StakingHash (Plutus.ScriptCredential (ValidatorHash stkVh))))


    getNftRefOutputValue :: (Integer,BuiltinByteString,OutputDatum) -> Value
    getNftRefOutputValue (i,_,d) = 
      let 
          V2.TxOut{V2.txOutAddress = Address (Plutus.ScriptCredential s) _, V2.txOutValue, V2.txOutDatum} = (V2.txInfoOutputs info) !! i
          nftRefHolderVH = ValidatorHash (getGroupInfoParams groupInfo NFTRefHolderVH)
          -- V2.TxOut{V2.txOutAddress, V2.txOutValue, V2.txOutDatum} = (V2.txInfoOutputs info) !! i
          -- nftRefHolderAddress = Address (Plutus.ScriptCredential  (ValidatorHash (getGroupInfoParams groupInfo NFTRefHolderVH)) ) Nothing --(Just (Plutus.StakingHash (Plutus.ScriptCredential (ValidatorHash stkVh))))
      in 
        -- case (nftRefHolderAddress == txOutAddress) && (txOutDatum == d) of
        --   True -> txOutValue
        -- if (nftRefHolderAddress == txOutAddress) && (txOutDatum == d) then txOutValue
        if (nftRefHolderVH == s) && (txOutDatum == d) then txOutValue
        else Ada.lovelaceValueOf 0


    -- 1. check the reciever ,including toaddr and nftrefholder ,are correct
    -- 2. check the datums ,including userData and nft metadata, are right
    checkOutput :: Bool
    checkOutput = 
        -- let receivedNftValue =  mconcat (map snd $ filter (\item -> (fst item) == userData) (outputsOf toAddr info))
        let !receivedNftValue = mconcat $ scriptOutputsAt2 toAddr info userData
            receivedNftRefValue = mconcat (map getNftRefOutputValue nftRefAssets)
            !mintValue = V2.txInfoMint info
            !nftCrossValue = mconcat $ map (\(cname,amount) -> Plutus.singleton (CurrencySymbol policy) (TokenName cname) amount) nftAssets
            !nftRefCrossValue = mconcat $ map (\(_,cname,_) -> Plutus.singleton (CurrencySymbol policy) (TokenName cname) 1) nftRefAssets
        in 
            mintValue == (nftCrossValue <> nftRefCrossValue)
            && receivedNftValue `geq` nftCrossValue
            && receivedNftRefValue `geq` nftRefCrossValue
    
    checkTtl :: Bool
    checkTtl = 
      let range = V2.txInfoValidRange info
          ttlRange = to (Plutus.POSIXTime ttl)
      in ttlRange == range 


{-# INLINABLE mkValidator #-}
mkValidator :: GroupAdminNFTCheckTokenInfo ->() -> NFTMintCheckRedeemer  -> V2.ScriptContext -> Bool
mkValidator storeman _ redeemer ctx = 
  case redeemer of
    BurnNFTMintCheckToken -> burnTokenCheck storeman ctx
    NFTMintCheckRedeemer mintCheckProof -> mintSpendCheck storeman mintCheckProof ctx


typedValidator :: GroupAdminNFTCheckTokenInfo -> PV2.TypedValidator TreasuryType
typedValidator = PV2.mkTypedValidatorParam @TreasuryType
    $$(PlutusTx.compile [|| mkValidator ||])
    $$(PlutusTx.compile [|| wrap ||])
    where
        wrap = PV2.mkUntypedValidator


validator :: GroupAdminNFTCheckTokenInfo -> Validator
validator = PV2.validatorScript . typedValidator

script :: GroupAdminNFTCheckTokenInfo -> Plutus.Script
script = unValidatorScript . validator


mintNFTCheckScript :: GroupAdminNFTCheckTokenInfo ->  PlutusScript PlutusScriptV2
mintNFTCheckScript p = PlutusScriptSerialised
  . SBS.toShort
  . LBS.toStrict
  $ serialise 
  (script p)

mintNFTCheckScriptHash :: GroupAdminNFTCheckTokenInfo -> Plutus.ValidatorHash
mintNFTCheckScriptHash = PV2.validatorHash .typedValidator


mintNFTCheckAddress ::GroupAdminNFTCheckTokenInfo -> Ledger.Address
mintNFTCheckAddress = PV2.validatorAddress . typedValidator
