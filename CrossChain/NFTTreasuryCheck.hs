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
                  
module CrossChain.NFTTreasuryCheck
  ( nftTreasuryCheckScript
  -- , authorityCheckScriptShortBs
  ,nftTreasuryCheckScriptHash
  -- ,authorityCheckScriptHashStr
  ,nftTreasuryCheckAddress
  , NFTTreasuryCheckProof (..)
  , NFTTreasuryCheckProofData (..)
  , NFTTreasuryCheckRedeemer (..)
  ,NFTTreasuryCheckParams (..)
  ,NFTTreasuryCheckParams
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
import Plutus.V1.Ledger.Credential (Credential (..), StakingCredential (..))
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
import CrossChain.Types hiding (uniqueId)
-- ===================================================
-- import Plutus.V1.Ledger.Value
-- import Ledger.Address (PaymentPrivateKey (PaymentPrivateKey, unPaymentPrivateKey), PaymentPubKey (PaymentPubKey),PaymentPubKeyHash (..),unPaymentPubKeyHash,toPubKeyHash,toValidatorHash)

import Ledger hiding (validatorHash) --singleton


-- data UserData = 

data NFTTreasuryCheckProofData = NFTTreasuryCheckProofData
  {
    uniqueId :: BuiltinByteString
    , nonce :: TxOutRef
    , mode :: Integer
    , toAddr :: Address
    , policy :: BuiltinByteString
    , crossValue :: Value
    , userData :: OutputDatum
    , txType :: Integer
    , ttl :: Integer
    -- , signature :: BuiltinByteString
  }deriving (Prelude.Eq, Show)


PlutusTx.unstableMakeIsData ''NFTTreasuryCheckProofData
PlutusTx.makeLift ''NFTTreasuryCheckProofData


data NFTTreasuryCheckProof = NFTTreasuryCheckProof
  {
    proof :: NFTTreasuryCheckProofData
    , signature :: BuiltinByteString
  }deriving (Prelude.Eq, Show)


PlutusTx.unstableMakeIsData ''NFTTreasuryCheckProof
PlutusTx.makeLift ''NFTTreasuryCheckProof

data NFTTreasuryCheckRedeemer = BurnTreasuryCheckToken | NFTTreasuryCheckRedeemer NFTTreasuryCheckProof -- | NFTMerge BuiltinByteString
    deriving (Show, Prelude.Eq)
PlutusTx.unstableMakeIsData ''NFTTreasuryCheckRedeemer

data TreasuryType
instance Scripts.ValidatorTypes TreasuryType where
    type instance DatumType TreasuryType = ()
    type instance RedeemerType TreasuryType = NFTTreasuryCheckRedeemer

data NFTTreasuryCheckParams
  = NFTTreasuryCheckParams
      { tokenInfos :: GroupAdminNFTCheckTokenInfo
        , treasury :: ValidatorHash 
      } deriving (Generic, Prelude.Eq)
        -- deriving anyclass (ToJSON, FromJSON)

PlutusTx.unstableMakeIsData ''NFTTreasuryCheckParams
PlutusTx.makeLift ''NFTTreasuryCheckParams



{-# INLINABLE burnTokenCheck #-}
burnTokenCheck :: NFTTreasuryCheckParams -> V2.ScriptContext -> Bool
burnTokenCheck (NFTTreasuryCheckParams (GroupAdminNFTCheckTokenInfo (GroupNFTTokenInfo groupInfoCurrency groupInfoTokenName) (AdminNftTokenInfo adminNftSymbol adminNftName) (CheckTokenInfo checkTokenSymbol checkTokenName)) treasury) ctx = 
  traceIfFalse "a" hasAdminNftInInput 
  && traceIfFalse "b" checkOutPut
  && traceIfFalse "c" (not hasTreasuryInput)
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

    treasuryCheckAddress :: Address
    treasuryCheckAddress = Address (Plutus.ScriptCredential (ValidatorHash (getGroupInfoParams groupInfo NFTTreasuryCheckVH))) (Just (Plutus.StakingHash (Plutus.ScriptCredential (ValidatorHash (getGroupInfoParams groupInfo StkVh)))))

    checkOutPut :: Bool
    !checkOutPut = 
      let outputValue = V2.valueProduced info
      in valueOf outputValue checkTokenSymbol checkTokenName == 0
      -- let totalAmountOfCheckTokenInOutput = getAmountOfCheckTokenInOutput ctx checkTokenSymbol checkTokenName
      --     -- !outputsAtChecker = map snd $ scriptOutputsAt' (ValidatorHash (getGroupInfoParams groupInfo TreasuryCheckVH)) (getGroupInfoParams groupInfo StkVh) info True
      --     outputsAtChecker = scriptOutputsAt2 treasuryCheckAddress info nonsenseDatum
      --     outputAtCheckerSum = valueOf (mconcat outputsAtChecker) checkTokenSymbol checkTokenName
      -- in totalAmountOfCheckTokenInOutput == outputAtCheckerSum && (length outputsAtChecker) == outputAtCheckerSum
    
    isTreasuryInput:: V2.TxInInfo -> Bool
    isTreasuryInput (V2.TxInInfo _ (V2.TxOut (Address addressCredential _) _ _ _)) = 
      case addressCredential of
        (Plutus.ScriptCredential s) -> s == treasury
        _ -> False

    hasTreasuryInput :: Bool
    !hasTreasuryInput = any (isTreasuryInput) $ V2.txInfoInputs info

-- {-# INLINABLE isExpectedValue #-}
-- isExpectedValue :: Value -> CurrencySymbol -> TokenName -> Bool
-- isExpectedValue v cs tk = 
--   if cs == Ada.adaSymbol && tk == Ada.adaToken then v == Plutus.singleton Plutus.adaSymbol Plutus.adaToken assetAmount
--   else (v == ((Plutus.singleton Plutus.adaSymbol Plutus.adaToken (valueOf v Ada.adaSymbol Ada.adaToken)) <> Plutus.singleton cs tk assetAmount)) 
--   && (assetAmount > 0)
--   where
--     assetAmount = valueOf v cs tk


-- {-# INLINABLE verify #-}
-- verify :: Integer -> BuiltinByteString -> BuiltinByteString -> BuiltinByteString-> Bool
-- verify mode pk hash signature
--   | mode == 0 = verifyEcdsaSecp256k1Signature pk hash signature
--   | mode == 1 = verifySchnorrSecp256k1Signature pk hash signature
--   | mode == 2 = verifyEd25519Signature pk hash signature
--   -- | otherwise = traceError "m"


-- {-# INLINABLE hasUTxO #-}
-- hasUTxO :: V2.ScriptContext -> V2.TxOutRef -> Bool
-- hasUTxO V2.ScriptContext{V2.scriptContextPurpose=Spending txOutRef} o = txOutRef == o


{-# INLINABLE isValidValue #-}
isValidValue :: Value -> Bool -> CurrencySymbol -> Bool
isValidValue v bSingleSymbol targetSymbol 
  | bSingleSymbol == False = True
  | otherwise = not $ any (\(cs',_,_) -> cs' /= targetSymbol && cs' /= Ada.adaSymbol) $ flattenValue v



-- {-# INLINABLE treasuryInputValue #-}
-- -- caculate the total input value of treasury
-- -- 1. owner is treasury 
-- -- 2. input value must be signle asset(ada + nomore than one token)
-- -- txType: 0 - cross 1: manual
-- treasuryInputValue :: V2.TxInfo -> ValidatorHash -> Integer -> CurrencySymbol -> Value
-- treasuryInputValue info treasury txType symbol = go (Plutus.singleton Plutus.adaSymbol Plutus.adaToken 0) (V2.txInfoInputs info)
--   where
--     go v [] = v
--     go v (V2.TxInInfo{V2.txInInfoResolved=V2.TxOut{V2.txOutValue,V2.txOutAddress= Address addressCredential _}} : rest) =
--       case addressCredential of
--         Plutus.ScriptCredential s -> 
--           if s == treasury then 
--             if isValidValue txOutValue (txType == 0) symbol then go (v <> txOutValue) rest
--             else traceError "bi"
--           else go v rest
--         _ -> go v rest


-- {-# INLINABLE nftMerge #-}
-- nftMerge ::NFTTreasuryCheckParams -> V2.ScriptContext -> BuiltinByteString -> Bool
-- nftMerge (NFTTreasuryCheckParams (GroupAdminNFTCheckTokenInfo (GroupNFTTokenInfo groupInfoCurrency groupInfoTokenName) _ _) treasury) ctx policy = 
--   traceIfFalse "f" (V2.txSignedBy info  (PubKeyHash  (getGroupInfoParams groupInfo BalanceWorker))) &&
--   traceIfFalse "v" (totalTreasurySpendValue == totalTreasuryChangeValue)
--   where
--     info :: V2.TxInfo
--     info = V2.scriptContextTxInfo ctx

--     groupInfo :: GroupInfoParams
--     groupInfo = getGroupInfo info groupInfoCurrency groupInfoTokenName

--     totalTreasurySpendValue :: Value
--     totalTreasurySpendValue = treasuryInputValue info treasury 1 (CurrencySymbol policy)

--     totalTreasuryChangeValue :: Value
--     totalTreasuryChangeValue = 
--       let os = map snd $ scriptOutputsAt' treasury (getGroupInfoParams groupInfo StkVh) info True
--       in mconcat os

{-# INLINABLE treasurySpendCheck #-}
treasurySpendCheck :: NFTTreasuryCheckParams -> NFTTreasuryCheckProof-> V2.ScriptContext -> Bool
treasurySpendCheck (NFTTreasuryCheckParams (GroupAdminNFTCheckTokenInfo (GroupNFTTokenInfo groupInfoCurrency groupInfoTokenName) (AdminNftTokenInfo adminNftSymbol adminNftName) (CheckTokenInfo checkTokenSymbol checkTokenName)) treasury) NFTTreasuryCheckProof{proof= p@NFTTreasuryCheckProofData{uniqueId, nonce, mode, toAddr, policy, crossValue, userData, txType, ttl}, signature} ctx = 
  traceIfFalse "1" (hasUTxO) && 
  traceIfFalse "2" (amountOfCheckTokeninOwnOutput == 1) && 
  traceIfFalse "3" checkSignature && 
  traceIfFalse "4" checkTx  &&
  traceIfFalse "5" hasTreasuryInput && -- check has treasury input 
  traceIfFalse "6" checkTtl
  where 
    info :: V2.TxInfo
    !info = V2.scriptContextTxInfo ctx

    hasUTxO :: Bool
    hasUTxO = 
      let V2.ScriptContext{V2.scriptContextPurpose=Spending txOutRef} = ctx in txOutRef == nonce
    -- hasUTxO V2.ScriptContext{V2.scriptContextPurpose=Spending txOutRef} = txOutRef == nonce

    hashRedeemer :: BuiltinByteString
    !hashRedeemer = 
        let !tmp1 = serialiseData $ PlutusTx.toBuiltinData p --NFTTreasuryCheckProofData{uniqueId, nonce, mode, toAddr, policy, crossValue, userData, txType, ttl}
        in sha3_256 tmp1

    
    groupInfo :: GroupInfoParams
    !groupInfo = getGroupInfo info groupInfoCurrency groupInfoTokenName

    amountOfCheckTokeninOwnOutput :: Integer
    amountOfCheckTokeninOwnOutput = getAmountOfCheckTokeninOwnOutput ctx checkTokenSymbol checkTokenName (getGroupInfoParams groupInfo StkVh)


    verify :: Bool
    verify -- mode pk hash signature
      | mode == 0 = verifyEcdsaSecp256k1Signature (getGroupInfoParams groupInfo GPK) hashRedeemer signature
      | mode == 1 = verifySchnorrSecp256k1Signature (getGroupInfoParams groupInfo GPK) hashRedeemer signature
      | mode == 2 = verifyEd25519Signature (getGroupInfoParams groupInfo GPK) hashRedeemer signature
  -- | otherwise = traceError "m"

    checkSignature :: Bool
    !checkSignature =
      if txType /=1 then  verify
        -- let !groupInfoPk = getGroupInfoParams groupInfo GPK
        -- in verify --mode groupInfoPk hashRedeemer signature
      else 
        (V2.txSignedBy info  (PubKeyHash  (getGroupInfoParams groupInfo BalanceWorker)))
      

    hasTreasuryInput :: Bool
    hasTreasuryInput = ((valueOf treasuryInputValue Ada.adaSymbol Ada.adaToken) > 0)

    targetSymbol :: CurrencySymbol 
    !targetSymbol = CurrencySymbol policy

    treasuryInputValue :: Value
    !treasuryInputValue = go (Plutus.singleton Plutus.adaSymbol Plutus.adaToken 0) (V2.txInfoInputs info)
      where
        go v [] = v
        go v (V2.TxInInfo{V2.txInInfoResolved=V2.TxOut{V2.txOutValue,V2.txOutAddress= Address addressCredential _}} : rest) =
          case addressCredential of
            Plutus.ScriptCredential s -> 
              if s == treasury then 
                if isValidValue txOutValue (txType /= 2) targetSymbol then go (v <> txOutValue) rest
                else traceError "b"
              else go v rest
            _ -> go v rest

    -- valuePaidToTarget :: Address -> Value
    -- valuePaidToTarget target@Address{addressCredential, addressStakingCredential} = 
    --   let datumAndValues = map snd $ filter (\item -> (fst item) == userData) (outputsOf target info)
    --       totalValue = 
    --         case addressCredential of
    --           Plutus.ScriptCredential vh -> 
    --             case userData of -- if target is contract then the outout toAddr target must have datum
    --               NoOutputDatum -> Ada.lovelaceValueOf 0
    --               _ -> mconcat datumAndValues
    --           _ -> mconcat datumAndValues
    --   in  totalValue

    valuePaidToTarget :: Address -> Value
    valuePaidToTarget target@Address{addressCredential} =
      let values = scriptOutputsAt2 target info userData
          totalValue = 
            case addressCredential of
              Plutus.ScriptCredential _ ->
                case userData of
                  NoOutputDatum -> Ada.lovelaceValueOf 0
                  _ -> mconcat values
              _ -> mconcat values
      in  totalValue


    treasuryAddress :: Address
    !treasuryAddress = Address (Plutus.ScriptCredential treasury) (Just (Plutus.StakingHash (Plutus.ScriptCredential (ValidatorHash (getGroupInfoParams groupInfo StkVh)))))

    changeValue :: Value
    !changeValue = 
      let vs = map snd $ scriptOutputsAt' treasury (getGroupInfoParams groupInfo StkVh) info True
      in mconcat vs

    checkTx :: Bool 
    !checkTx =
      if txType /= 1 then 
        let !receivedValue = valuePaidToTarget toAddr
            -- !inputValue = treasuryInputValue
            -- !changeValues = map snd $ scriptOutputsAt' treasury (getGroupInfoParams groupInfo StkVh) info True
            -- changeValues = scriptOutputsAt2 treasuryAddress info nonsenseDatum
            -- !remainValue = mconcat changeValues
            !valueSum = crossValue <> changeValue  -- <> (Ada.lovelaceValueOf (valueOf treasuryInputValue Ada.adaSymbol Ada.adaToken))
        in 
          (isValidValue crossValue (txType /= 2) targetSymbol) -- only cross one kind token(the policies of all assets is the same one)
          && (receivedValue `geq` crossValue) 
          && (valueSum `geq` treasuryInputValue) 
          -- && (length changeValues == 1) -- treasury has only 1 change output 
          && (isValidValue receivedValue (txType /= 2) targetSymbol)
          && (isValidValue changeValue (txType /= 2) targetSymbol)
      else 
        treasuryInputValue `geq` changeValue
        -- let changeValues = map snd $ scriptOutputsAt' treasury (getGroupInfoParams groupInfo StkVh) info True
        -- let changeValues = scriptOutputsAt2 treasuryAddress info nonsenseDatum
        -- in treasuryInputValue == (mconcat changeValues)

    checkTtl :: Bool
    !checkTtl = 
      let range = V2.txInfoValidRange info
          ttlRange = to (Plutus.POSIXTime ttl)
      in ttlRange == range 


{-# INLINABLE mkValidator #-}
mkValidator :: NFTTreasuryCheckParams ->() -> NFTTreasuryCheckRedeemer -> V2.ScriptContext -> Bool
mkValidator storeman _ redeemer ctx = 
  case redeemer of
    BurnTreasuryCheckToken -> burnTokenCheck storeman ctx
    NFTTreasuryCheckRedeemer treasuryRedeemer -> treasurySpendCheck storeman treasuryRedeemer ctx
    -- NFTMerge policy -> nftMerge storeman ctx policy-- TBD


typedValidator :: NFTTreasuryCheckParams -> PV2.TypedValidator TreasuryType
typedValidator = PV2.mkTypedValidatorParam @TreasuryType
    $$(PlutusTx.compile [|| mkValidator ||])
    $$(PlutusTx.compile [|| wrap ||])
    where
        wrap = PV2.mkUntypedValidator


validator :: NFTTreasuryCheckParams -> Validator
validator = PV2.validatorScript . typedValidator

script :: NFTTreasuryCheckParams -> Plutus.Script
script = unValidatorScript . validator

-- authorityCheckScriptShortBs :: NFTTreasuryCheckParams -> SBS.ShortByteString
-- authorityCheckScriptShortBs = SBS.toShort . LBS.toStrict $ serialise . script

-- nftTreasuryCheckScript :: CurrencySymbol -> PlutusScript PlutusScriptV2
-- nftTreasuryCheckScript = PlutusScriptSerialised . authorityCheckScriptShortBs

nftTreasuryCheckScript :: NFTTreasuryCheckParams ->  PlutusScript PlutusScriptV2
nftTreasuryCheckScript p = PlutusScriptSerialised
  . SBS.toShort
  . LBS.toStrict
  $ serialise 
  (script p)

nftTreasuryCheckScriptHash :: NFTTreasuryCheckParams -> Plutus.ValidatorHash
nftTreasuryCheckScriptHash = PV2.validatorHash .typedValidator

-- authorityCheckScriptHashStr :: NFTTreasuryCheckParams -> BuiltinByteString
-- authorityCheckScriptHashStr = case PlutusTx.fromBuiltinData $ PlutusTx.toBuiltinData . nftTreasuryCheckScriptHash of 
--   Just s -> s
--   Nothing -> ""

nftTreasuryCheckAddress ::NFTTreasuryCheckParams -> Ledger.Address
nftTreasuryCheckAddress = PV2.validatorAddress . typedValidator
