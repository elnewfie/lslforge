{-# LANGUAGE FlexibleInstances, FlexibleContexts, TypeOperators,
    GeneralizedNewtypeDeriving, MultiParamTypeClasses, TypeSynonymInstances,
    TemplateHaskell #-}
{-# OPTIONS_GHC -fwarn-unused-binds -fwarn-unused-imports #-}
module Language.Lsl.Internal.WorldState(
    durationToTicks,      -- :: (RealFrac t, Integral b) => t -> b
    dyn,                  -- ::
    evalWorldE,
    findAsset,            -- :: (Monad m) => t -> m (Maybe a)
    findTextureAsset,     -- :: (MonadPlus m) => [Char] -> m InventoryItem
    findTexture,
    fromWorldE,
    getActualPrimScripts,
    getObjectPosition,
    getObjectRotation,
    getObjectVelocity,
    getParcelByPosition,
    getPos,
    getPrimAnimations,
    getPrimBodyParts,
    getPrimClothing,
    getPrimFaceAlpha,
    getPrimFaceColor,
    getPrimFaceTextureInfo,
    getPrimGestures,
    getPrimInventory,
    getPrimLandmarks,
    getPrimLinkNum,
    getPrimNotecards,
    getPrimObjects,
    getPrimParcel,
    getPrimRegion,
    getPrimScripts,
    getPrimSounds,
    getPrimTextures,
    getPrimTypeInfo,
    getRootPrim,
    insertScriptChannelPair,
    isSensorEvent,
    isSoundAsset,
    logAMessage,
    logTrace,
    lookupDataChannel,
    lookupScriptFromChan,
    newKey,
    primAttachment',
    primHasActiveHandler,
    pushDeferredScriptEvent,
    pushDeferredScriptEventToPrim,
    pushDeferredScriptEventToObject,
    pushChangedEventToObject,
    pushAttachEvent,
    putHTTPTimeoutEvent,
    putHTTPResponseEvent,
    putParcel,
    putManyWQ,
    putWQ,
    putWorldEvent,
    runAndLogIfErr,
    runErrFace,
    runErrPrim,
    scriptHasActiveHandler,
    setPrimFaceAlpha,
    setPrimFaceColor,
    setPrimInventory,
    setTexture,
    takeWQ,
    ticksToDuration,
    updatePrimFace,
    wprim,
    wrand,
    lscript,
    sortByInvName,
    wav,
    module Language.Lsl.Internal.WorldStateTypes,
    getM,
    setM,
    modM,
    (=:)
    ) where

import Prelude hiding(id,(.))
import Control.Category
import Control.Monad(MonadPlus(..))
import Control.Monad.State(StateT(..))
import Control.Monad.Error(Error,ErrorT(..),MonadError(..),strMsg)
import Data.Label
import Data.LabelExtras
import Data.List(elemIndex)
import Data.Maybe(fromMaybe)
import qualified Data.Map as M

import Language.Lsl.Internal.Evaluation(Event(..))
import Language.Lsl.Internal.Exec(hasActiveHandler)
import Language.Lsl.Internal.Key(mkKey,LSLKey(..),nullKey)
import Language.Lsl.Internal.Log(LogMessage(..),LogLevel(..))
import Language.Lsl.Internal.Type(LSLValue(..),vec2VVal)
import Language.Lsl.Internal.Util(lookupByIndex)
import Language.Lsl.WorldDef(Attachment,InventoryItem(..),InventoryItemIdentification(..),
    Region(..),Parcel(..),Prim,isInvNotecardItem,isInvLandmarkItem,
    isInvClothingItem,isInvBodyPartItem,isInvGestureItem,isInvSoundItem,
    isInvAnimationItem,isInvTextureItem,isInvScriptItem,isInvObjectItem,
    sortByInvName,findByInvName)
import Language.Lsl.Internal.WorldStateTypes

import System.Random(Random(..))

primAttachment' :: (MonadError e m, Error e) => m Prim :-> m Attachment
primAttachment' = rjoinV (strMsg "not attached") primAttachment

-- extracting/updating the world state ----------------------------------------
wav k = lm k.worldAvatars

wprim k = lm k.wprims
getRootPrim k = fromMaybe k <$> getM (primParent.wprim k)
getPrimTypeInfo k = getM $ primTypeInfo.wprim k

getPrimInventory k = getM $ primInventory.wprim k

getPrimNotecards k = filter isInvNotecardItem <$> getPrimInventory k
getPrimLandmarks k = filter isInvLandmarkItem <$> getPrimInventory k
getPrimClothing k = filter isInvClothingItem <$> getPrimInventory k
getPrimBodyParts k = filter isInvBodyPartItem <$> getPrimInventory k
getPrimObjects k = filter isInvObjectItem <$> getPrimInventory k
getPrimGestures k = filter isInvGestureItem <$> getPrimInventory k
getPrimSounds k = filter isInvSoundItem <$> getPrimInventory k
getPrimAnimations k = filter isInvAnimationItem <$> getPrimInventory k
getPrimTextures k = filter isInvTextureItem <$> getPrimInventory k
getPrimScripts k = filter isInvScriptItem <$> getPrimInventory k

itemNameKey = inventoryItemNameKey . inventoryItemIdentification

getActualPrimScripts k = do
    scriptNames <- map (fst . itemNameKey) <$> getPrimScripts k
    allScripts <- M.toList <$> getM (worldScripts)
    return [ s | s@((pk,sn),_) <- allScripts,pk == k && sn `elem` scriptNames ]

getPrimLinkNum pk = do
    mp <- getM $ primParent.wprim pk
    case mp of
        Nothing -> do  -- this is the root prim
            links <- getM $ primKeys.lm pk.wobjects
            return (if null links then 0 else 1)
        Just ok -> do
            links <- getM $ primKeys.lm ok.wobjects
            case elemIndex pk links of
                Nothing -> throwError err
                Just i -> return (i + 1)
    where err = "internal error, can't find prim in link list of parent object"

-- TODO: temp until introduce region into Prim definition
getPrimRegion _ = return (0 :: Int, 0 :: Int)

getPos pkey = runErrPrim pkey (VVal 0.0 0.0 0.0)
    (vec2VVal <$> (getRootPrim pkey >>= getObjectPosition))

runErrFace k i defaultVal = runAndLogIfErr
    ("face " ++ (show i) ++ " or prim " ++ unLslKey k ++ " not found") defaultVal

getPrimFaceAlpha k i = getM (faceAlpha.lli i.primFaces.wprim k)
getPrimFaceColor k i = getM (faceColor.lli i.primFaces.wprim k)
getPrimFaceTextureInfo k i = getM (faceTextureInfo.lli i.primFaces.wprim k)

runErrPrim k defaultVal =
    runAndLogIfErr ("prim " ++ unLslKey k ++ " not found") defaultVal

setPrimInventory k v = primInventory.wprim k =: v

updatePrimFace k i f = (primFaces.wprim k) `modM_` update
    where update = zipWith (\ index face ->
            if (index == i) then f face else face) [0..]

setPrimFaceAlpha k i v = runErrFace k i () $ updatePrimFace k i (setI faceAlpha v)
setPrimFaceColor k i v = runErrFace k i () $ updatePrimFace k i (setI faceColor v)

isSensorEvent (SensorEvent {}) = True
isSensorEvent _ = False

takeWQ :: Int -> WorldEventQueue -> (Maybe WorldEventType,WorldEventQueue)
takeWQ i [] = (Nothing,[])
takeWQ i ((j,we):wes) | i >= j = (Just we,wes)
                      | otherwise = (Nothing,wes)

putWQ tick we wes = before ++ ((tick,we):after)
    where (before,after) = break ((>tick).fst) wes

putManyWQ [] wq = wq
putManyWQ ((tick,we):wes) wq = putWQ tick we (putManyWQ wes wq)

putWorldEvent delay we = do
   t <- getM tick
   wqueue `modM_` putWQ (t + max (durationToTicks delay) 1) we

pushDeferred event delay =
    putWorldEvent delay . DeferredScriptEvent event
pushDeferredScriptEvent event pk sn delay =
    pushDeferred event delay (DeferredScriptEventScriptTarget (pk,sn))
pushDeferredScriptEventToPrim event pk delay =
    pushDeferred event delay (DeferredScriptEventPrimTarget pk)
pushDeferredScriptEventToObject event oid delay =
    pushDeferred event delay (DeferredScriptEventObjectTarget oid)

pushChangedEventToObject oid val =
    pushDeferredScriptEventToObject (Event "changed" [IVal val] M.empty) oid 0
pushAttachEvent pk k =
    pushDeferredScriptEventToPrim (Event "attach" [KVal k] M.empty) pk 0

putHTTPTimeoutEvent pk sn key =
    pushDeferredScriptEvent (Event "http_response"
        [KVal key, IVal 499, LVal [], SVal ""] M.empty) pk sn
putHTTPResponseEvent pk sn key status metadata body =
    pushDeferredScriptEvent (Event "http_response"
        [KVal key, IVal status, LVal metadata, body] M.empty) pk sn


evalWorldE :: Monad m => WorldE m v -> StateT (World m) m (Either String v)
evalWorldE = runErrorT . unWorldE

fromWorldE def val = val `catchError` const (return def)

runAndLogIfErr msg def val =
    val `catchError` (\ s ->
        logAMessage LogWarn "sim" (msg ++ " (" ++ s ++ ")") >> return def)

logAMessage logLevel source s = do
    tick <- getM tick
    msglog `modM_` (LogMessage tick logLevel source s:)

logTrace source s = do
    tick <- getM tick
    msglog `modM_` (LogMessage tick LogTrace source s:)

newKey :: Monad m => WorldE m LSLKey
newKey = mkKey <$> (worldKeyIndex `modM` (+1))

findAsset _ = return Nothing

isSoundAsset _ = False

findTextureAsset "" = mzero
findTextureAsset _ = return $
    InventoryItem (InventoryItemIdentification ("",nullKey)) undefined undefined

primHasActiveHandler pk handler = do
    scripts <- getPrimScripts pk
    images <- imagesForScripts scripts
    return $ foldl (\ x y -> x || hasActiveHandler y handler) False images
    where imagesForScripts scriptItems = do
              scripts <- runErrPrim pk [] $ getActualPrimScripts pk
              return $ map (getI scriptImage . snd) scripts

scriptHasActiveHandler pk sn handler =  hasActiveHandler <$>
    getM (scriptImage.lscript pk sn) <*> pure handler

lookupDataChannel scriptAddr = getM $ lm scriptAddr.lsnd.worldOpenDataChannels
lookupScriptFromChan chan = getM $ lm chan.lfst.worldOpenDataChannels

insertScriptChannelPair script chan =
    ((lm chan.lfst).*(lm script.lsnd)).worldOpenDataChannels =: (script:*chan)

----- MISC ----
durationToTicks dur = floor (1000.0 * dur)
ticksToDuration ticks = fromIntegral ticks / 1000.0

wrand :: (Monad m, Random a) => WorldE m a
wrand = do
    g <- getM randGen
    let (v,g') = random g
    randGen =: g'
    return v

dyn oid = dynamics.lm oid.wobjects

getObjectPosition k = getM $ objectPosition.dyn k
getObjectRotation k = getM $ objectRotation.dyn k
getObjectVelocity k = getM $ objectVelocity.dyn k

lscript pk sn = lm (pk,sn).worldScripts

getParcelByPosition regionIndex (x,y,_) =
    getM (lm regionIndex.worldRegions) >>= findParcel 0 . regionParcels
    where findParcel _ [] = throwError "parcel not found"
          findParcel i (p:ps) =
              let (south,north,west,east) = parcelBoundaries p
                  (xc,yc) = (floor x, floor y) in
                  if xc < east && xc >= west && yc < north && yc >= south
                      then return (i,p) else findParcel (i + 1) ps

getPrimParcel pk = do
    regionIndex <- getPrimRegion pk
    pos <- getObjectPosition =<< getRootPrim pk
    (index,parcel) <- getParcelByPosition regionIndex pos
    return (regionIndex,index,parcel)

putParcel regionIndex index parcel = do
    region <- getM $ lm regionIndex.worldRegions
    let (before,after) = splitAt index (regionParcels region)
    let parcels' = if null after
            then parcel : before
            else before ++ (parcel : tail after)
    lm regionIndex.worldRegions =: region { regionParcels = parcels' }

findTexture pk id = do
    textures <- getPrimTextures pk
    case findByInvName id textures of
        Just item -> return $ itemKey item
        Nothing -> do
            result <- return $ findTextureAsset id
            case result of
                Nothing -> throwError ("cannot find texture " ++ id)
                Just item -> return $ itemKey item
    where itemKey = snd . itemNameKey

setTexture tk face pkey =
    if face == -1
        then do faces <- getM $ primFaces.wprim pkey
                let faces' = map (setI (textureKey.faceTextureInfo) tk) faces
                primFaces.wprim pkey =: faces'
        else do faces <- getM $ primFaces.wprim pkey
                f <- lookupByIndex face faces
                updatePrimFace pkey face (setI (textureKey.faceTextureInfo) tk)
