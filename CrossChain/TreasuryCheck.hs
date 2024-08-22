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

module CrossChain.TreasuryCheck
  ( treasuryCheckScript
  -- , authorityCheckScriptShortBs
  ,treasuryCheckScriptHash
  -- ,authorityCheckScriptHashStr
  ,treasuryCheckAddress
  , TreasuryCheckProof (..)
  -- ,TreasuryCheckProof
  , TreasuryCheckRedeemer(..)
  ,TreasuryCheckParams (..)
  ,TreasuryCheckParams
  ) where

import Data.Aeson (FromJSON, ToJSON)
import GHC.Generics (Generic)
import Cardano.Api.Shelley (PlutusScript (..), PlutusScriptV2)
import Prelude hiding (($),(<>), (&&), (==),(||),(>=),(<=),(<),(-),(/=),not,length,filter,(>),(+),map,any,elem,fst,snd,mconcat)

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
import PlutusTx.Prelude hiding (SemigroupInfo (..), (.))--unless
-- import PlutusTx.Prelude qualified as PlutusPrelude
import           Ledger               hiding (validatorHash)
import Plutus.V2.Ledger.Tx (OutputDatum (..)) -- isPayToScriptOut
import Ledger.Typed.Scripts (ValidatorTypes (..), TypedValidator (..),mkTypedValidatorParam) --mkTypedValidator,mkUntypedValidator )
-- import Plutus.Script.Utils.Typed (validatorScript,validatorAddress,validatorHash)

-- import Data.ByteString qualified as ByteString
import Ledger.Crypto (PubKey (..), PubKeyHash)--, pubKeyHash
-- import Plutus.V1.Ledger.Bytes (LedgerBytes (LedgerBytes),fromBytes,getLedgerBytes)
import Ledger.Ada  as Ada
import Plutus.V1.Ledger.Value (valueOf,flattenValue)--,currencySymbol,tokenName,symbols,)
import PlutusTx.Builtins --(decodeUtf8,sha3_256,appendByteString)
import Ledger.Address 
import Ledger.Value
import Plutus.V2.Ledger.Contexts as V2
import Ledger.Typed.Scripts qualified as Scripts hiding (validatorHash)
import Plutus.V1.Ledger.Tx
import CrossChain.Types 
-- ===================================================
-- import Plutus.V1.Ledger.Value
-- import Ledger.Address (PaymentPrivateKey (PaymentPrivateKey, unPaymentPrivateKey), PaymentPubKey (PaymentPubKey),PaymentPubKeyHash (..),unPaymentPubKeyHash,toPubKeyHash,toValidatorHash)

import Ledger hiding (validatorHash) --singleton




data TreasuryCheckProof = TreasuryCheckProof
  {
    toPkhPay :: BuiltinByteString -- send toPkh 
    , toPkhStk :: BuiltinByteString
    , policy :: BuiltinByteString -- which token , zero indicated only transfer ada
    , assetName :: BuiltinByteString
    , amount :: Integer  -- token amount
    , adaAmount :: Integer -- addtional ada amount
    , txHash :: BuiltinByteString
    , index :: Integer
    , mode :: Integer
    , uniqueId :: BuiltinByteString
    , txType :: Integer
    , ttl :: Integer
    , outputCount :: Integer
    , signature :: BuiltinByteString
  }deriving (Prelude.Eq, Show)


PlutusTx.unstableMakeIsData ''TreasuryCheckProof
PlutusTx.makeLift ''TreasuryCheckProof


data TreasuryCheckProof2 = TreasuryCheckProof2
  {
    proofPart :: TreasuryCheckProof
    , userData2 :: BuiltinByteString
  }deriving (Prelude.Eq, Show)


PlutusTx.unstableMakeIsData ''TreasuryCheckProof2
PlutusTx.makeLift ''TreasuryCheckProof2

data TreasuryCheckRedeemer = BurnTreasuryCheckToken | TreasuryCheckRedeemer TreasuryCheckProof | TreasuryCheckRedeemer2 TreasuryCheckProof2
    deriving (Show, Prelude.Eq)
PlutusTx.unstableMakeIsData ''TreasuryCheckRedeemer

data TreasuryType
instance Scripts.ValidatorTypes TreasuryType where
    type instance DatumType TreasuryType = ()
    type instance RedeemerType TreasuryType = TreasuryCheckRedeemer

data TreasuryCheckParams
  = TreasuryCheckParams
      { tokenInfos :: GroupAdminNFTCheckTokenInfo
        , treasury :: ValidatorHash 
      } deriving (Generic, Prelude.Eq)
        -- deriving anyclass (ToJSON, FromJSON)

PlutusTx.unstableMakeIsData ''TreasuryCheckParams
PlutusTx.makeLift ''TreasuryCheckParams


{-# INLINABLE burnTokenCheck #-}
burnTokenCheck :: TreasuryCheckParams -> V2.ScriptContext -> Bool
burnTokenCheck (TreasuryCheckParams (GroupAdminNFTCheckTokenInfo (GroupNFTTokenInfo groupInfoCurrency groupInfoTokenName) (AdminNftTokenInfo adminNftSymbol adminNftName) (CheckTokenInfo checkTokenSymbol checkTokenName)) treasury) ctx = 
  traceIfFalse "a" hasAdminNftInInput 
  && traceIfFalse "b" checkOutPut
  && traceIfFalse "ti" (not hasTreasuryInput)
  where 
    info :: V2.TxInfo
    !info = V2.scriptContextTxInfo ctx

    groupInfo :: GroupInfoParams
    !groupInfo = getGroupInfo info groupInfoCurrency groupInfoTokenName

    hasAdminNftInInput :: Bool
    !hasAdminNftInInput = 
      let !totalInputValue = V2.valueSpent info
          !amount = valueOf totalInputValue adminNftSymbol adminNftName
      in amount == 1

    checkOutPut :: Bool
    !checkOutPut = 
      let outputValue = V2.valueProduced info
      in valueOf outputValue checkTokenSymbol checkTokenName == 0
    -- !checkOutPut = 
    --   let !totalAmountOfCheckTokenInOutput = getAmountOfCheckTokenInOutput ctx checkTokenSymbol checkTokenName
    --       !outputsAtChecker = map snd $ scriptOutputsAt' (ValidatorHash (getGroupInfoParams groupInfo TreasuryCheckVH)) (getGroupInfoParams groupInfo StkVh) info True
    --       !outputAtCheckerSum = valueOf (mconcat outputsAtChecker) checkTokenSymbol checkTokenName
    --   in totalAmountOfCheckTokenInOutput == outputAtCheckerSum && (length outputsAtChecker) == outputAtCheckerSum
    
    isTreasuryInput:: V2.TxInInfo -> Bool
    isTreasuryInput (V2.TxInInfo _ (V2.TxOut (Address addressCredential _) _ _ _)) = 
      case addressCredential of
        (Plutus.ScriptCredential s) -> s == treasury
        _ -> False

    hasTreasuryInput :: Bool
    !hasTreasuryInput = any (isTreasuryInput) $ V2.txInfoInputs info

{-# INLINABLE isExpectedValue #-}
isExpectedValue :: Value -> CurrencySymbol -> TokenName -> Bool
isExpectedValue v cs tk = 
  if cs == Ada.adaSymbol && tk == Ada.adaToken then v == Plutus.singleton Plutus.adaSymbol Plutus.adaToken assetAmount
  else (v == ((Plutus.singleton Plutus.adaSymbol Plutus.adaToken (valueOf v Ada.adaSymbol Ada.adaToken)) <> Plutus.singleton cs tk assetAmount)) 
  && (assetAmount > 0)
  where
    assetAmount = valueOf v cs tk

{-# INLINABLE isMultiAsset #-}
isMultiAsset :: Value ->Bool
isMultiAsset v = (length $ flattenValue v) > 2

{-# INLINABLE verify #-}
verify :: Integer -> BuiltinByteString -> BuiltinByteString -> BuiltinByteString-> Bool
verify mode pk hash signature
  | mode == 0 = verifyEcdsaSecp256k1Signature pk hash signature
  | mode == 1 = verifySchnorrSecp256k1Signature pk hash signature
  | mode == 2 = verifyEd25519Signature pk hash signature
  -- | otherwise = traceError "m"


{-# INLINABLE hasUTxO #-}
hasUTxO :: V2.ScriptContext -> BuiltinByteString -> Integer -> Bool
hasUTxO V2.ScriptContext{V2.scriptContextPurpose=Spending txOutRef} txHash index = (V2.txOutRefId txOutRef) == (Plutus.TxId txHash) && (V2.txOutRefIdx txOutRef) == index


{-# INLINABLE isValidValue #-}
isValidValue :: Value -> Integer -> CurrencySymbol -> TokenName -> Bool
isValidValue v txType targetSymbol targetTokenName--v = isExpectedValue v targetSymbol targetTokenName
  | txType == 2 = isMultiAsset v
  | otherwise = isExpectedValue v targetSymbol targetTokenName

{-# INLINABLE treasuryInputValue #-}
treasuryInputValue :: V2.TxInfo -> ValidatorHash -> Integer -> CurrencySymbol -> TokenName ->Value
treasuryInputValue info treasury txType symbol name = go (Plutus.singleton Plutus.adaSymbol Plutus.adaToken 0) (V2.txInfoInputs info)
  where
    go v [] = v
    go v (V2.TxInInfo{V2.txInInfoResolved=V2.TxOut{V2.txOutValue,V2.txOutAddress}} : rest) = case txOutAddress of
      Address{addressCredential} -> case addressCredential of
        Plutus.ScriptCredential s -> 
          if s == treasury then 
            if isValidValue txOutValue txType symbol name then go (v <> txOutValue) rest
            else traceError "bi"
          else go v rest
        _ -> go v rest


-- {-# INLINABLE packgeData #-}
-- packgeData :: BuiltinByteString -> BuiltinByteString-> BuiltinByteString -> BuiltinByteString ->Integer -> Integer -> BuiltinByteString -> Integer -> Integer -> BuiltinByteString -> Integer -> Integer -> Integer -> BuiltinByteString
-- packgeData toPkhPay toPkhStk policy assetName amount adaAmount txHash index mode uniqueId txType ttl outputCount =
--   let tmp1 = appendByteString (appendByteString (appendByteString (appendByteString (appendByteString toPkhPay toPkhStk) policy) assetName) (packInteger amount)) (packInteger adaAmount)
--       tmp2 = appendByteString (appendByteString (appendByteString tmp1 txHash) (packInteger index)) (packInteger mode)
--       tmp3 = appendByteString tmp2 uniqueId
--       tmp4 = appendByteString tmp3 (packInteger txType)
--       tmp5 = appendByteString tmp4 (packInteger ttl)
--       tmp6 = appendByteString tmp5 (packInteger outputCount)
--   in tmp6


{-# INLINABLE checkTtl #-}
checkTtl :: V2.TxInfo -> Integer -> Bool
checkTtl info ttl = 
  let !range = V2.txInfoValidRange info
      !ttlRange = to (Plutus.POSIXTime ttl)
  in ttlRange == range 

{-# INLINABLE treasurySpendCheck #-}
treasurySpendCheck :: TreasuryCheckParams -> TreasuryCheckProof-> V2.ScriptContext -> Bool
treasurySpendCheck (TreasuryCheckParams (GroupAdminNFTCheckTokenInfo (GroupNFTTokenInfo groupInfoCurrency groupInfoTokenName) (AdminNftTokenInfo adminNftSymbol adminNftName) (CheckTokenInfo checkTokenSymbol checkTokenName)) treasury) (TreasuryCheckProof toPkhPay toPkhStk policy assetName amount adaAmount txHash index mode uniqueId txType ttl outputCount signature) ctx = 
  -- traceIfFalse "l" (V2.txSignedBy info  (PubKeyHash  (getGroupInfoParams groupInfo BalanceWorker))) && 
  traceIfFalse "ht" (hasUTxO ctx txHash index) && 
  traceIfFalse "hot" (amountOfCheckTokeninOwnOutput == 1) && 
  traceIfFalse "cs" checkSignature && 
  traceIfFalse "tt" checkTxInOut  && -- check asset balance between input and output
  traceIfFalse "notr" hasTreasuryInput && -- check has treasury input 
  traceIfFalse "ttl" (checkTtl info ttl)
  where 
    info :: V2.TxInfo
    !info = V2.scriptContextTxInfo ctx

    hashRedeemer :: BuiltinByteString
    hashRedeemer = 
        let tmp1 = appendByteString (appendByteString (appendByteString (appendByteString (appendByteString toPkhPay toPkhStk) policy) assetName) (packInteger amount)) (packInteger adaAmount)
            tmp2 = appendByteString (appendByteString (appendByteString tmp1 txHash) (packInteger index)) (packInteger mode)
            tmp3 = appendByteString tmp2 uniqueId
            tmp4 = appendByteString tmp3 (packInteger txType)
            tmp5 = appendByteString tmp4 (packInteger ttl)
            tmp6 = appendByteString tmp5 (packInteger outputCount)
        in sha3_256 tmp6

    -- hashRedeemer :: BuiltinByteString
    -- !hashRedeemer = 
    --   let tmp = packgeData toPkhPay toPkhStk policy assetName amount adaAmount txHash index mode uniqueId txType ttl outputCount
    --   in sha3_256 tmp
    
    -- hasUTxO :: V2.ScriptContext -> Bool
    -- hasUTxO V2.ScriptContext{V2.scriptContextPurpose=Spending txOutRef} = (V2.txOutRefId txOutRef) == (Plutus.TxId txHash) && (V2.txOutRefIdx txOutRef) == index

    -- verify :: Integer -> BuiltinByteString -> BuiltinByteString -> BuiltinByteString-> Bool
    -- verify mode pk hash signatureGroupInfoParams
    --   | mode == 0 = verifyEcdsaSecp256k1Signature pk hash signature
    --   | mode == 1 = verifySchnorrSecp256k1Signature pk hash signature
    --   | mode == 2 = verifyEd25519Signature pk hash signature
    --   -- | otherwise = traceError "m"
    
    groupInfo :: GroupInfoParams
    !groupInfo = getGroupInfo info groupInfoCurrency groupInfoTokenName

    amountOfCheckTokeninOwnOutput :: Integer
    amountOfCheckTokeninOwnOutput = getAmountOfCheckTokeninOwnOutput ctx checkTokenSymbol checkTokenName (getGroupInfoParams groupInfo StkVh)

    checkSignature :: Bool
    !checkSignature = 
      let !groupInfoPk = getGroupInfoParams groupInfo GPK
      in verify mode groupInfoPk hashRedeemer signature

    -- isTreasuryInput:: V2.TxInInfo -> Bool
    -- isTreasuryInput (V2.TxInInfo _ (V2.TxOut (Address addressCredential _) _ _ _)) = 
    --   case addressCredential of
    --     (Plutus.ScriptCredential s) -> s == treasury
    --     _ -> False
        

    -- treasuryInputValue :: Value
    -- !treasuryInputValue = go (Plutus.singleton Plutus.adaSymbol Plutus.adaToken 0) (V2.txInfoInputs info)
    --   where
    --     go v [] = v
    --     go v (V2.TxInInfo{V2.txInInfoResolved=V2.TxOut{V2.txOutValue,V2.txOutAddress}} : rest) = case txOutAddress of
    --       Address{addressCredential} -> case addressCredential of
    --         Plutus.ScriptCredential s -> 
    --           if s == treasury then 
    --             if isValidValue txOutValue then go (v <> txOutValue) rest
    --             else traceError "bi"
    --           else go v rest
    --         _ -> go v rest


    hasTreasuryInput :: Bool
    !hasTreasuryInput = ((valueOf totalTreasurySpendValue Ada.adaSymbol Ada.adaToken) > 0)

    targetSymbol :: CurrencySymbol 
    !targetSymbol = CurrencySymbol policy
    
    targetTokenName :: TokenName
    !targetTokenName = TokenName assetName

    totalTreasurySpendValue :: Value
    !totalTreasurySpendValue= treasuryInputValue info treasury txType targetSymbol targetTokenName 

    valuePaidToTarget :: Value
    !valuePaidToTarget 
      | txType == 1 = valueLockedBy' info treasury (getGroupInfoParams groupInfo StkVh)
        -- let outValues = scriptOutputsAt' treasury (getGroupInfoParams groupInfo StkVh) info True
        -- in 
        --   if any (\v -> not $ isSingleAsset v targetSymbol targetTokenName) outValues then traceError "bo"
        --   else mconcat outValues
      | otherwise = valuePaidTo' info (PubKeyHash toPkhPay) toPkhStk


    -- isExpectedValue :: Value -> CurrencySymbol -> TokenName -> Bool
    -- isExpectedValue v cs tk = 
    --   if cs == Ada.adaSymbol && tk == Ada.adaToken then v == Plutus.singleton Plutus.adaSymbol Plutus.adaToken assetAmount
    --   else (v == ((Plutus.singleton Plutus.adaSymbol Plutus.adaToken (valueOf v Ada.adaSymbol Ada.adaToken)) <> Plutus.singleton cs tk assetAmount)) 
    --   && (assetAmount > 0)
    --   where
    --     assetAmount = valueOf v cs tk

    -- isSingleAsset :: Value -> CurrencySymbol -> TokenName -> Bool
    -- isSingleAsset v cs tk = not $ any (\(cs',tk',_) -> cs' /= cs && cs' /= Ada.adaSymbol && tk' /= tk && tk' /= Ada.adaToken) $ flattenValue v

    -- isMultiAsset :: Value ->Bool
    -- isMultiAsset v = (length $ flattenValue v) > 2

    -- isValidValue :: Value -> Bool
    -- isValidValue v
    --   | txType == 2 = isMultiAsset v
    --   | otherwise = isExpectedValue v targetSymbol targetTokenName

    crossValue :: Value
    !crossValue
      | (txType == 0) = Ada.lovelaceValueOf adaAmount <> Plutus.singleton targetSymbol targetTokenName amount
      | otherwise = Ada.lovelaceValueOf 0

    -- balanceWorker :: PubKeyHash
    -- !balanceWorker = PubKeyHash (getGroupInfoParams groupInfo BalanceWorker)

    -- 1. 
    checkTx :: Bool 
    !checkTx = 
        let !receivedValue = valuePaidToTarget -- V2.valuePaidTo info (PubKeyHash toPkhPay)
            !inputValue = totalTreasurySpendValue -- treasuryInputValue
            !changeValues = map snd $ scriptOutputsAt' treasury (getGroupInfoParams groupInfo StkVh) info True
            !remainValue = mconcat changeValues
            !valueSum = crossValue <> remainValue
            -- !checkFoundationTransfer = 
            --   if txType == 3 then 
            --     ((getGroupInfoParams groupInfo BalanceWorker) == toPkhPay) 
            --     && (amount == 0)
            --     && (isExpectedValue receivedValue Plutus.adaSymbol Plutus.adaToken)
            --   else (isExpectedValue receivedValue targetSymbol targetTokenName)
        in 
          (receivedValue `geq` crossValue) 
          && (valueSum `geq` inputValue) 
          && (length changeValues == outputCount) 
          && (isSingleAsset receivedValue targetSymbol targetTokenName)
          -- && (not $ any (\v -> (valueOf v targetSymbol targetTokenName) <= 0) changeValues)
          && (isSingleAsset remainValue targetSymbol targetTokenName)
          -- if targetSymbol == Plutus.adaSymbol then 
          --   (receivedValue `geq` crossValue) 
          --   && (valueSum `geq` inputValue) 
          --   && (length changeValues == outputCount) 
          --   && (isSingleAsset receivedValue targetSymbol targetTokenName)
          --   && (isExpectedValue remainValue targetSymbol targetTokenName) 
          -- else 
          --   (receivedValue `geq` crossValue) 
          --   && (valueSum `geq` inputValue) 
          --   && (length changeValues == outputCount) 
          --   && (isExpectedValue remainValue targetSymbol targetTokenName)
          --   && (isSingleAsset receivedValue targetSymbol targetTokenName)
          --   && (not $ any (\v -> (valueOf v targetSymbol targetTokenName) <= 0) changeValues)
      

    checkTxInOut:: Bool
    !checkTxInOut  
      | txType == 0 = checkTx 
      | txType == 1 = ((ValidatorHash toPkhPay) == treasury ) && (toPkhStk == (getGroupInfoParams groupInfo StkVh)) && checkTx
      | txType == 2 = (valuePaidTo' info (PubKeyHash toPkhPay) toPkhStk ) `geq` totalTreasurySpendValue --treasuryInputValue

    -- checkTtl :: Bool
    -- !checkTtl = 
    --   let !range = V2.txInfoValidRange info
    --       !ttlRange = to (Plutus.POSIXTime ttl)
    --   in ttlRange == range 

{-# INLINABLE treasurySpendCheck2 #-}
treasurySpendCheck2 :: TreasuryCheckParams -> TreasuryCheckProof2 -> V2.ScriptContext -> Bool
treasurySpendCheck2 (TreasuryCheckParams (GroupAdminNFTCheckTokenInfo (GroupNFTTokenInfo groupInfoCurrency groupInfoTokenName) (AdminNftTokenInfo adminNftSymbol adminNftName) (CheckTokenInfo checkTokenSymbol checkTokenName)) treasury) (TreasuryCheckProof2 (TreasuryCheckProof toPkhPay toPkhStk policy assetName amount adaAmount txHash index mode uniqueId txType ttl outputCount  signature) userData) ctx = 
  -- traceIfFalse "l" (V2.txSignedBy info  (PubKeyHash  (getGroupInfoParams groupInfo BalanceWorker))) && 
  traceIfFalse "rt2" (txType == 0) && 
  traceIfFalse "ht2" (hasUTxO ctx txHash index) && 
  traceIfFalse "hot2" (amountOfCheckTokeninOwnOutput == 1) && 
  traceIfFalse "cs2" checkSignature && 
  traceIfFalse "tt2" checkTxInOut  && -- check asset balance between input and output
  traceIfFalse "notr2" hasTreasuryInput && -- check has treasury input 
  -- traceIfFalse "ttl2" checkTtl
  traceIfFalse "ttl2" (checkTtl info ttl)
  where 
    info :: V2.TxInfo
    !info = V2.scriptContextTxInfo ctx

    hashRedeemer :: BuiltinByteString
    hashRedeemer = 
        let tmp1 = appendByteString (appendByteString (appendByteString (appendByteString (appendByteString toPkhPay toPkhStk) policy) assetName) (packInteger amount)) (packInteger adaAmount)
            tmp2 = appendByteString (appendByteString (appendByteString tmp1 txHash) (packInteger index)) (packInteger mode)
            tmp3 = appendByteString tmp2 uniqueId
            tmp4 = appendByteString tmp3 (packInteger txType)
            tmp5 = appendByteString tmp4 (packInteger ttl)
            tmp6 = appendByteString tmp5 (packInteger outputCount)
            tmp7 = appendByteString tmp6 userData
        in sha3_256 tmp7

    -- hashRedeemer :: BuiltinByteString
    -- hashRedeemer = 
    --   let tmp1 = packgeData toPkhPay2 toPkhStk2 policy2 assetName2 amount2 adaAmount2 txHash2 index2 mode2 uniqueId2 txType2 ttl2  
    --       tmp2 = appendByteString tmp1 userData2
    --   in sha3_256 tmp2
    
    -- hasUTxO :: V2.ScriptContext -> Bool
    -- hasUTxO V2.ScriptContext{V2.scriptContextPurpose=Spending txOutRef} = (V2.txOutRefId txOutRef) == (Plutus.TxId txHash2) && (V2.txOutRefIdx txOutRef) == index2

    -- verify :: Integer -> BuiltinByteString -> BuiltinByteString -> BuiltinByteString-> Bool
    -- verify mode2 pk hash signatureGroupInfoParams
    --   | mode2 == 0 = verifyEcdsaSecp256k1Signature pk hash signature2
    --   | mode2 == 1 = verifySchnorrSecp256k1Signature pk hash signature2
    --   | mode2 == 2 = verifyEd25519Signature pk hash signature2
    --   -- | otherwise = traceError "m"
    
    groupInfo :: GroupInfoParams
    !groupInfo = getGroupInfo info groupInfoCurrency groupInfoTokenName

    amountOfCheckTokeninOwnOutput :: Integer
    !amountOfCheckTokeninOwnOutput = getAmountOfCheckTokeninOwnOutput ctx checkTokenSymbol checkTokenName (getGroupInfoParams groupInfo StkVh)

    checkSignature :: Bool
    !checkSignature = 
      let !groupInfoPk = getGroupInfoParams groupInfo GPK
      in verify mode groupInfoPk hashRedeemer signature

    -- isTreasuryInput:: V2.TxInInfo -> Bool
    -- isTreasuryInput (V2.TxInInfo _ (V2.TxOut (Address addressCredential _) _ _ _)) = 
    --   case addressCredential of
    --     (Plutus.ScriptCredential s) -> s == treasury
    --     _ -> False
        

    -- treasuryInputValue :: Value
    -- !treasuryInputValue = go (Plutus.singleton Plutus.adaSymbol Plutus.adaToken 0) (V2.txInfoInputs info)
    --   where
    --     go v [] = v
    --     go v (V2.TxInInfo{V2.txInInfoResolved=V2.TxOut{V2.txOutValue,V2.txOutAddress}} : rest) = case txOutAddress of
    --       Address{addressCredential} -> case addressCredential of
    --         Plutus.ScriptCredential s -> 
    --           if s == treasury then 
    --             if isValidValue txOutValue then go (v <> txOutValue) rest
    --             else traceError "bi"
    --           else go v rest
    --         _ -> go v rest


    hasTreasuryInput :: Bool
    !hasTreasuryInput = ((valueOf totalTreasurySpendValue Ada.adaSymbol Ada.adaToken) > 0)

    targetSymbol :: CurrencySymbol 
    !targetSymbol = CurrencySymbol policy
    
    targetTokenName :: TokenName
    !targetTokenName = TokenName assetName

    totalTreasurySpendValue :: Value
    !totalTreasurySpendValue= treasuryInputValue info  treasury txType targetSymbol targetTokenName

    -- valuePaidToTarget :: Value
    -- valuePaidToTarget = 
    --   let (totalValue,count) =  valueLockedByAndCheckDatum info (ValidatorHash toPkhPay) toPkhStk userData
    --   in 
    --     if count <=1 then totalValue
    --     else traceError "t1"



    -- isExpectedValue :: Value -> CurrencySymbol -> TokenName -> Bool
    -- isExpectedValue v cs tk = 
    --   if cs == Ada.adaSymbol && tk == Ada.adaToken then v == Plutus.singleton Plutus.adaSymbol Plutus.adaToken assetAmount
    --   else (v == ((Plutus.singleton Plutus.adaSymbol Plutus.adaToken (valueOf v Ada.adaSymbol Ada.adaToken)) <> Plutus.singleton cs tk assetAmount)) 
    --   && (assetAmount > 0)
    --   where
    --     assetAmount = valueOf v cs tk

    -- isSingleAsset :: Value -> CurrencySymbol -> TokenName -> Bool
    -- isSingleAsset v cs tk = not $ any (\(cs',tk',_) -> cs' /= cs && cs' /= Ada.adaSymbol && tk' /= tk && tk' /= Ada.adaToken) $ flattenValue v

    -- isMultiAsset :: Value ->Bool
    -- isMultiAsset v = (length $ flattenValue v) > 2

    -- isValidValue :: Value -> Bool
    -- isValidValue v = isExpectedValue v targetSymbol targetTokenName

    crossValue :: Value
    !crossValue = Ada.lovelaceValueOf adaAmount <> Plutus.singleton targetSymbol targetTokenName amount

    -- 1. 
    checkTxInOut :: Bool 
    !checkTxInOut = 
        let (receivedValue,count) =  valueLockedByAndCheckDatum info (ValidatorHash toPkhPay) toPkhStk userData -- V2.valuePaidTo info (PubKeyHash toPkhPay2)
            inputValue = totalTreasurySpendValue
            changeValues = map snd $ scriptOutputsAt' treasury (getGroupInfoParams groupInfo StkVh) info True
            remainValue = mconcat changeValues
            valueSum = crossValue <> remainValue
        in 
          (receivedValue `geq` crossValue) 
          && (valueSum `geq` inputValue) 
          && (length changeValues == outputCount) 
          && (isSingleAsset receivedValue targetSymbol targetTokenName)
          -- && (not $ any (\v -> (valueOf v targetSymbol targetTokenName) <= 0) changeValues)
          && (isSingleAsset remainValue targetSymbol targetTokenName)
          && (count == 1)


    -- checkTtl :: Bool
    -- !checkTtl = 
    --   let !range = V2.txInfoValidRange info
    --       !ttlRange = to (Plutus.POSIXTime ttl2)
    --   in ttlRange == range 


{-# INLINABLE mkValidator #-}
mkValidator :: TreasuryCheckParams ->() -> TreasuryCheckRedeemer -> V2.ScriptContext -> Bool
mkValidator storeman _ redeemer ctx = 
  case redeemer of
    BurnTreasuryCheckToken -> burnTokenCheck storeman ctx
    TreasuryCheckRedeemer treasuryRedeemer -> treasurySpendCheck storeman treasuryRedeemer ctx
    TreasuryCheckRedeemer2 treasuryRedeemer2 -> treasurySpendCheck2 storeman treasuryRedeemer2 ctx


typedValidator :: TreasuryCheckParams -> PV2.TypedValidator TreasuryType
typedValidator = PV2.mkTypedValidatorParam @TreasuryType
    $$(PlutusTx.compile [|| mkValidator ||])
    $$(PlutusTx.compile [|| wrap ||])
    where
        wrap = PV2.mkUntypedValidator


validator :: TreasuryCheckParams -> Validator
validator = PV2.validatorScript . typedValidator

script :: TreasuryCheckParams -> Plutus.Script
script = unValidatorScript . validator

-- authorityCheckScriptShortBs :: TreasuryCheckParams -> SBS.ShortByteString
-- authorityCheckScriptShortBs = SBS.toShort . LBS.toStrict $ serialise . script

-- treasuryCheckScript :: CurrencySymbol -> PlutusScript PlutusScriptV2
-- treasuryCheckScript = PlutusScriptSerialised . authorityCheckScriptShortBs

treasuryCheckScript :: TreasuryCheckParams ->  PlutusScript PlutusScriptV2
treasuryCheckScript p = PlutusScriptSerialised
  . SBS.toShort
  . LBS.toStrict
  $ serialise 
  (script p)

treasuryCheckScriptHash :: TreasuryCheckParams -> Plutus.ValidatorHash
treasuryCheckScriptHash = PV2.validatorHash .typedValidator

-- authorityCheckScriptHashStr :: TreasuryCheckParams -> BuiltinByteString
-- authorityCheckScriptHashStr = case PlutusTx.fromBuiltinData $ PlutusTx.toBuiltinData . treasuryCheckScriptHash of 
--   Just s -> s
--   Nothing -> ""

treasuryCheckAddress ::TreasuryCheckParams -> Ledger.Address
treasuryCheckAddress = PV2.validatorAddress . typedValidator
