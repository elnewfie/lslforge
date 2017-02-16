{-# LANGUAGE FlexibleContexts, NoMonomorphismRestriction, ViewPatterns #-}
{-# OPTIONS_GHC -fwarn-unused-binds -fwarn-unused-imports #-}
-------------------------------------------------------------------------------
-- |
-- Module        : Language.Lsl.Internal.SimLL
--
-- This module contains implementations of the impure LSL 'll' functions for
-- the LSL Plus Sim.  There are several hundred of these functions (hence the
-- length of this module!); most of them have reasonable implementations, some
-- simply log that they've been called because no reasonable implementation
-- is possible, and a few have no implementation, which causes the Sim to fall
-- back on the a default behavior, based on the function's type.
-------------------------------------------------------------------------------
module Language.Lsl.Internal.SimLL(
    doPredef,
    defaultPredefs,
    determineActivePassiveScripted,
    getGlobalPos,
    getGlobalRot,
    getPrimBox,
    objectMass,
    putChat,
    resetScript,
    sayRange,
    unimplementedFuncs,
    loadScript
    ) where


import Prelude hiding ((.),id)
import Control.Category
import Control.Applicative
import Control.Monad(
    MonadPlus(..),foldM,forM_,forM,liftM,liftM2,unless,when,filterM)
import Control.Monad.Except(MonadError(..))
import Data.List(find,foldl',isSuffixOf,elemIndex)
import Data.Bits((.&.),(.|.),bit,clearBit,complement,setBit,shiftL,testBit)
import Data.Int()
import Data.LabelExtras
import Data.Maybe(fromMaybe,isNothing)
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.IntMap as IM
import Data.Time
import Data.Time.Clock.POSIX

import Language.Lsl.Internal.Animation(builtInAnimations)
import Language.Lsl.Internal.AvEvents(AvatarInputEvent(..))
import Language.Lsl.Internal.CodeHelper(renderCall)
import Language.Lsl.Internal.Constants
import Language.Lsl.Internal.Evaluation(ScriptInfo(..),Event(..),
    EvalResult(..))
import Language.Lsl.Internal.Exec(ExecutionState(..),ScriptImage(..),hardReset,
    initLSLScript)
import Language.Lsl.Internal.FuncSigs(funcSigs)
import Language.Lsl.Internal.InternalLLFuncs(internalLLFuncs)
import Language.Lsl.Internal.Key(nullKey,LSLKey(..))
import Language.Lsl.Internal.Log(LogLevel(..))
import Language.Lsl.Internal.Physics(calcAccel,primMassApprox)
import Language.Lsl.Syntax(Ctx(..),FuncDec(..),predefFuncs)
import Language.Lsl.Internal.Type(LSLValue(..),LSLType(..),defaultValue,
    lslBool,lslValString,rVal2Rot,rot2RVal,typeOfLSLValue,vVal2Vec,vec2VVal,
    onI,onS,toSVal)
import Language.Lsl.Internal.Util(add3d,diff3d,fac,findM,fromInt,
    generatePermutation,lookupByIndex,mag3d,mlookup,norm3d,
    quaternionMultiply,rot3d,rotationBetween,scale3d,tuplify,
    (<||>),(<??>),rotL,whenJust,filtMapM)
import Language.Lsl.WorldDef(Attachment(..),AvatarControlListener(..),
    Email(..),Flexibility(..),InventoryInfo(..),InventoryItem(..),
    InventoryItemData(..),InventoryItemIdentification(..),LightInfo(..),
    LSLObject(..),Parcel(..),PrimType(..),PositionTarget(..),Region(..),
    RotationTarget(..),TextureInfo(..),defaultCamera,defaultDynamics,
    findByInvKey,findByInvName,inventoryInfoPermValue,inventoryItemName,
    isInvAnimationItem,isInvBodyPartItem,isInvClothingItem,isInvGestureItem,
    isInvNotecardItem,isInvObjectItem,isInvScriptItem,isInvSoundItem,
    isInvTextureItem,primPhantomBit,primPhysicsBit,ItemPermissions(..),
    mkScript)
import Language.Lsl.Internal.WorldState

-- just for fun
(<<=) = (=<<)

infixl 1 <<=

-- some logging functions
slog :: Monad m => ScriptInfo Float -> String -> WorldE m ()
slog = logAMessage LogInfo . infoToLogSource
    where infoToLogSource info = unLslKey (scriptInfoPrimKey info) ++ ": " ++ scriptInfoScriptName info

-- execute a predefined ('ll') function
doPredef :: Monad m => String -> ScriptInfo Float -> [LSLValue Float] -> WorldE m (EvalResult,LSLValue Float)
doPredef name info@(ScriptInfo oid pid sid pkey event) args =
   (maybe doDefault runIt . tryPredefs name) =<< getM predefs
    where runIt (t,f) = runErrFunc info name (defaultValue t) args f
          doDefault = do
               (_,rettype,argtypes) <- either fail return $ findM (\ (x,y,z) -> x == name) funcSigs
               logAMessage LogDebug (unLslKey pkey ++ ":" ++ sid)
                   ("unimplemented predefined function called: " ++
                   renderCall name args)
               continueWith $ defaultValue rettype

tryPredefs name predefs = fmap f (M.lookup name predefs)
    where f (PredefFunc _ t predef) = (t,predef)

defaultPredef name predef =  maybe (error $ "undefined predef " ++ name)
        (\ (FuncDec _ t _) -> PredefFunc name t predef)
             (find (\ (FuncDec n _ _) -> name == ctxItem n) predefFuncs)

runErrFunc info fname defval args f =
    f info args `catchError`
        (\ s -> slog info (fname ++ ": " ++ s) >> continueWith defval)

prohibited a = maybe (return ()) (const $ throwError "prohibited") =<< a

withAvatar ak nf f = maybe nf f =<< (optional $ getM $ wav ak)

pushDataserverEvent pk sn key val =
    pushDeferredScriptEvent (Event "dataserver" [KVal key, SVal val] M.empty) pk sn 0

--- Web related functions ----------------------------------------------------

countURLs scriptId = (length . filter ((==scriptId) . snd) . M.elems) <$> (getM worldURLRegistry)

regionIndex (x,y) = base i +
  (maybe (error "impossible!") id
      $ elemIndex (x,y)
          [ (k,l) | k <- [(-ix)..ix],l <- [(-ix)..ix],isp (k,l) ])
  where i = max (abs x) (abs y)
        ix = el i `div` 2
        el = (1+) . (2*)
        base 0 = 0
        base n = el (n - 1) ^2
        isp (k,l) = abs k == ix || abs l == ix

simName pk = (("sim" ++) . show . regionIndex) <$> getPrimRegion pk

mkURL secure pk k = do
    nm <- simName pk
    return $ pref ++ nm ++ ".example.com:12043/cap/" ++ unLslKey k
    where pref = if secure then "https://" else "http://"

reqURL secure info@(ScriptInfo _ _ sn pk _) [] = do
    c <- countURLs (pk,sn)
    k <- newKey
    if c < 10
        then do
            url <- mkURL secure pk k
            pushDeferredScriptEvent (Event "http_request" [KVal k, llcUrlRequestGranted, SVal url] M.empty) pk sn 0
            lm url.worldURLRegistry =: (k,(pk,sn))
            continueK k
        else do
            slog info "url limit exceeded"
            pushDeferredScriptEvent (Event "http_request" [KVal k, llcUrlRequestDenied, SVal ""] M.empty) pk sn 0
            continueK k

llGetFreeURLs (ScriptInfo _ _ sn pk _) [] =
    ((10-) <$> countURLs (pk,sn)) >>= continueI

llReleaseURL info [SVal url] = void (worldURLRegistry `modM_` M.delete url)

llHTTPResponse (ScriptInfo _ _ sn pk mevent) [KVal k, IVal status, SVal body] = void $ do
    PendingHTTPResponse av (pk',sn') _ _ _ _ _ _ <- getM $ lm k.worldPendingHTTPResponses
    when ((pk,sn) /= (pk',sn')) $
        throwError $ "trying to repsond to a HTTP request " ++
            "received by a different script!"
    worldPendingHTTPResponses `modM_` M.delete k
    putWorldEvent 0 (AvatarInputEvent av (AvatarHTTPResponse k status (take 2048 body)))

llGetHTTPHeader (ScriptInfo _ _ sn pk _) [KVal k, SVal header] = do
    PendingHTTPResponse av (pk',sn') base path query ip ua _ <- getM $ lm k.worldPendingHTTPResponses
    when ((pk,sn) /= (pk',sn')) $
        throwError $ "trying to get headers for a HTTP request " ++
            "received by a different script!"
    case header of
        "x-script-url" -> continueS base
        "x-path-info" -> continueS path
        "x-query-string" -> continueS query
        "x-remote-ip" -> continueS ip
        "user-agent" -> continueS ua
        _ -> throwError $ "unsupported header: " ++ header

llHTTPRequest (ScriptInfo _ _ sn pk _) [SVal url, LVal params, SVal body] = do
    t <- getM tick
    -- verify the parameters
    when (length params `mod` 2 /= 0 || length params > 8)
        (throwError "invalid parameter list")
    let params' = tuplify params
    unless (all ((`elem` httpConsts) . fst) params')
        (throwError "invalid parameter list")
    method <- maybe (return "GET")
        (\ v -> if v `notElem` map SVal ["GET","POST","PUT","DELETE"]
          then throwError ("invalid HTTP method " ++ lslValString v)
          else let SVal s = v in return s)
        (lookup llcHTTPMethod params')
    mimet <- maybe (return "text/plain;charset=utf-8")
          (onS return (throwError "invalid mimetype"))
          (lookup llcHTTPMimetype params')
    maxlen <- maybe (return 2048)
          (onI return (throwError "invalid body length"))
          (lookup llcHTTPBodyMaxlength params')
    verify <- maybe (return 1)
          (onI return (throwError "invalid verify cert value"))
          (lookup llcHTTPVerifyCert params')
    -- parameters ok
    key <- newKey
    putWorldEvent 0 (HTTPRequestEvent (pk,sn) key url method mimet maxlen
        verify body)
    continueK key
    where httpConsts = [llcHTTPMethod,llcHTTPMimetype,llcHTTPBodyMaxlength,
              llcHTTPVerifyCert]
--- Sensor related functions -------------------------------------------------

llSensor info@(ScriptInfo _ _ sn pk _) [SVal name, KVal key, IVal etype, FVal range, FVal arc] =
    void $ putWorldEvent 0 (SensorEvent (pk,sn) name key etype range arc Nothing)
llSensorRepeat info@(ScriptInfo _ _ sn pk _) [SVal name, KVal key, IVal etype, FVal range, FVal arc, FVal interval] =
    void $ putWorldEvent interval (SensorEvent (pk,sn) name key etype range arc (Just interval'))
    where interval' = max 0 interval
llSensorRemove info@(ScriptInfo _ _ sn pk _) [] = void $
    modM_ wqueue $ \ q -> [ qentry | qentry@(_,event) <- q, not (isSensorEvent event) || sensorAddress event /= (pk,sn)]

------------------------------------------------------------------------------
llGetRegionAgentCount info@(ScriptInfo _ _ sn pk _) [] = do
    region <- getPrimRegion pk
    count <- (length . filter ((==region) . getI avatarRegion) . M.elems) <$> getM worldAvatars
    continueI count

llRequestAgentData info@(ScriptInfo _ _ sn pk _) [KVal ak, IVal d] =
    do key <- newKey
       av <- optional $ getM $ wav ak
       dataVal <- case av of
           Nothing -> do
               slog info ("llRequestAgentData: no such avatar - " ++ unLslKey ak)
               return "0"
           Just av -> case d of
                d | d == cDataBorn -> return "2006-01-01"
                  | d == cDataOnline -> return "1"
                  | d == cDataRating -> return "0,0,0,0,0"
                  | d == cDataName -> return $ getI avatarName av
                  | d == cDataPayinfo -> return "3"
                  | otherwise -> throwError ("invalid data requested: " ++ show d)
       pushDataserverEvent pk sn key dataVal
       continueK key

llRequestSimulatorData (ScriptInfo _ _ sn pk _) [SVal simName, IVal d] = do
    regions <- getM worldRegions
    let findRegion = either fail return $ findM ((==simName) . regionName . snd) (M.toList regions)
    let simErr = throwError ("unknown simulator " ++ simName)
    val <- case d of
        d | d == cDataSimStatus -> (findRegion >> return "up") <||>
              return "unknown"
          | d == cDataSimRating -> (findRegion >> return "PG") <||> simErr
          | d == cDataSimPos -> (do
              ((x,y),_) <- findRegion
              return $ lslValString $
                  vec2VVal (256 * fromIntegral x, 256 * fromIntegral y, 0))
              <||> simErr
          | otherwise -> throwError ("invalid data requested: " ++ show d)
    key <- newKey
    pushDataserverEvent pk sn key val
    continueK key

--- INVENTORY related functions ----------------------------------------------

getPrimInventoryInfo pk invType =
    case invType of
        i | i == llcInventoryAll ->       sortit getPrimInventory
          | i == llcInventoryAnimation -> sortit getPrimAnimations
          | i == llcInventoryBodyPart ->  sortit getPrimBodyParts
          | i == llcInventoryClothing ->  sortit getPrimClothing
          | i == llcInventoryGesture ->   sortit getPrimGestures
          | i == llcInventoryNotecard ->  sortit getPrimNotecards
          | i == llcInventoryObject ->    sortit getPrimObjects
          | i == llcInventoryScript ->    sortit getPrimScripts
          | i == llcInventorySound ->     sortit getPrimSounds
          | i == llcInventoryTexture ->   sortit getPrimTextures
          | otherwise ->
              throwError ("invalid inventory type: " ++ lslValString invType)
    where sortit f = sortByInvName <$> (f pk)

llGetInventoryNumber info@(ScriptInfo _ _ _ pk _) [invType@(IVal _)] =
    getPrimInventoryInfo pk invType >>= continueI . length

sayRange = Just 20.0

llGetInventoryCreator info@(ScriptInfo _ _ _ pk _) [SVal name] =
    do  all <- getPrimInventory pk
        continueK =<< case findByInvName name all of
            Nothing -> putChat sayRange pk 0 ("no item named '" ++ name ++ "'")
               >> return nullKey
            Just item ->
                return $ inventoryInfoCreator $ inventoryItemInfo item

llGetInventoryPermMask info@(ScriptInfo _ _ _ pk _) [SVal name, IVal mask] =
    do  all <- getPrimInventory pk
        continueI =<< case findByInvName name all of
            Nothing -> putChat sayRange pk 0 ("No item named '" ++ name ++ "'") >> throwError ("No item named '" ++ name ++ "'")
            Just item -> inventoryInfoPermValue mask (inventoryInfoPerms $ inventoryItemInfo item)

llGetInventoryKey info@(ScriptInfo _ _ _ pk _) [SVal name] =
    do  all <- getPrimInventory pk
        continueK =<< case findByInvName name all of
            Nothing -> return nullKey
            Just (InventoryItem id invInfo _) ->
                let Right perms = inventoryInfoPermValue cMaskOwner
                        (inventoryInfoPerms invInfo) in
                    if perms .&. cFullPerm == cFullPerm then
                        return $ snd (inventoryItemNameKey id)
                    else return nullKey

llGetInventoryName info@(ScriptInfo _ _ _ pk _) [invType@(IVal _), IVal index] =
    (map inventoryItemName) <$> (getPrimInventoryInfo pk invType) >>=
        continueS . fromMaybe "" . lookupByIndex index

llGetInventoryType info@(ScriptInfo _ _ _ pk _) [SVal name] =
        getPrimInventory pk >>= continueI . classify . findByInvName name
    where classifications = [(isInvAnimationItem,cInventoryAnimation),
                             (isInvBodyPartItem,cInventoryBodyPart),
                             (isInvClothingItem,cInventoryClothing),
                             (isInvGestureItem,cInventoryGesture),
                             (isInvNotecardItem,cInventoryNotecard),
                             (isInvObjectItem,cInventoryObject),
                             (isInvSoundItem,cInventorySound),
                             (isInvTextureItem,cInventoryTexture),
                             (isInvScriptItem,cInventoryScript)]
          classify Nothing = (-1)
          classify (Just item) = foldl'
              (\ cur (f,i) -> if (cur < 0) && f item then i else cur) (-1)
              classifications

llRemoveInventory info@(ScriptInfo _ _ _ pk _) [SVal name] = do
    inv <- getPrimInventory pk
    maybe (sayErr info ("Missing inventory item '" ++ name ++ "'"))
        (const $ let inv' = [ item | item <- inv, name /= inventoryItemName item] in
            setPrimInventory pk inv')
            (findByInvName name inv)
    continueV

loadScript key name script active = do
    (t:*scripts) <- getM (tick.*wscripts)
    case lookup script scripts of
        Just (Right code) -> do
            let image = mkScript $ initLSLScript code
            lm (key,name).worldScripts =:
                if active
                    then setI (scriptStartTick.*scriptLastResetTick) (t:*t) image
                    else setI scriptActive False image
        _ -> return ()

giveInventory info@(ScriptInfo _ _ _ pk _) k folder itemNames = void $ do
    let hasPerm p item = 0 /= p .&. (permMaskOwner . inventoryInfoPerms . inventoryItemInfo $ item)
    inventory <- getM $ primInventory.wprim pk
    prim <- optional $ getM $ wprim k
    srcOwner <- getM $ primOwner.wprim pk
    items <- flip filtMapM itemNames $ (\ nm ->
        case findByInvName nm inventory of
            Nothing -> sayErr info ("inventory item " ++ nm ++ " not found") >>
                return Nothing
            Just item -> if hasPerm cPermCopy item
                then return (Just item)
                else sayErr info ("inventory item " ++ nm ++ " not copyable") >>
                        return Nothing)
    unless (null items) $ case prim of
        Nothing -> do
            items' <- checkXfers srcOwner k items
            av <- (getM $ wav k) <||> throwError ("no object or avatar found with key " ++ unLslKey k)
            putWorldEvent 0 (GiveAvatarInventoryEvent k "" items') >>
               slog info ("llGiveInventory: avatar " ++ unLslKey k ++ " given option of accepting inventory")
        Just p -> do
            owner:*group <- getM $ (primOwner.*primGroup).wprim pk
            items' <- checkXfers srcOwner owner items
            let [ownerP,groupP,everybodyP] =
                    drop 1 $ take 4 (getI primPermissions p ++ repeat 0)
            let hasModifyPerm = (cPermModify .&. everybodyP /= 0) ||
                    (cPermModify .&. groupP /= 0 && not (isNothing group) && group == getI primGroup p) ||
                    (cPermModify .&. ownerP /= 0 && owner == srcOwner)
            if hasModifyPerm || getI primAllowInventoryDrop p
                then do
                    let newOwner = getI primOwner p
                    let newGroup = getI primGroup p
                    items' <- forM items $ copyInventoryItem newOwner newGroup
                    let inventory' = getI primInventory p
                    let addItem inv itm = case findByInvName (inventoryItemName itm) inv of
                            Nothing -> do
                                loadScript' itm k
                                return $ itm : inv
                            Just _ -> do
                                let itm' = changeName itm newName
                                loadScript' itm' k
                                return $ itm' : inv
                                where newName = findANewName 1 (inventoryItemName itm)
                                      findANewName i base =
                                          maybe name (const $ findANewName (i + 1) base) $
                                              findByInvName name inv
                                          where name = base ++ " " ++ show i
                    newInv <- foldM addItem inventory' items'
                    primInventory.wprim k =: newInv
                    pushDeferredScriptEventToPrim
                        (Event "changed" [if hasModifyPerm then llcChangedInventory else llcChangedAllowedDrop] M.empty) k 0
                else throwError "can't modify/drop inventory on target"
    where
        checkXfers src dst items =  flip filterM items $ \ item ->
            let perms = (permMaskOwner . inventoryInfoPerms .
                    inventoryItemInfo $ item) in checkXfer src dst perms
        checkXfer src dst perms | src == dst = return True
                                | cPermTransfer .&. perms /= 0 = return True
                                | otherwise = let err = "item not transferrable" in
                                    sayErr info err >> return False
        changeName (InventoryItem (InventoryItemIdentification (name,key)) info dat) newName =
            InventoryItem (InventoryItemIdentification (newName,key)) info dat
        copyLink newOwner newGroup prim = do
            k <- newKey
            inventory <- mapM (copyInventoryItem newOwner newGroup)
                (getI primInventory prim)
            return $ setI (primKey.*primInventory.*primOwner.*primGroup)
                (k:*inventory:*newOwner:*newGroup) prim
        copyInventoryItem newOwner newGroup item = case item of
            item | isInvObjectItem item -> do
                      let links = invObjectPrims . inventoryItemData $ item
                      links' <- mapM (copyLink newOwner newGroup) links
                      when (null links') $ throwError "empty linkset!"
                      let info = inventoryItemInfo item
                      return $ InventoryItem
                          (InventoryItemIdentification
                              (inventoryItemName item,
                                  getI primKey $ head links'))
                          info (InvObject links')
                 | otherwise -> do
                      k <- newKey
                      let info = inventoryItemInfo item
                      let dat = inventoryItemData item
                      return $ InventoryItem
                          (InventoryItemIdentification
                              (inventoryItemName item, k)) info dat
        loadScript' itm@(InventoryItem
            { inventoryItemData = InvScript { invScriptLibId = script } }) k =
                loadScript k (inventoryItemName itm) script False
        loadScript' _ _ = return ()

llGiveInventory info [KVal k, SVal s] = giveInventory info k "" [s]

llGiveInventoryList info [KVal k, SVal f, LVal l] = giveInventory info k f [ s | SVal s <- map toSVal l]

llSetRemoteScriptAccessPin (ScriptInfo _ _ _ pk _) [IVal pin] =
    void $ primRemoteScriptAccessPin.wprim pk =: pin
llRemoteLoadScript info [KVal _,SVal _, IVal _, IVal _] =
    void $ sayErr info  "Deprecated.  Please use llRemoteLoadScriptPin instead."

llRezObject = rezObject False
llRezAtRoot = rezObject True

rezObject atRoot info@(ScriptInfo _ _ _ pk _)
    [SVal inventory, VVal px py pz, VVal vx vy vz, RVal rx ry rz rs,
     IVal param] = void $ do
    objects <- getPrimObjects pk
    maybe (do putChat (Just 20) pk 0 ("Couldn't find object '" ++ inventory ++ "'")
              throwError ("no such inventory item - " ++ inventory))
        (\ item -> do
            let copy = isCopyable item
            let InvObject linkSet = inventoryItemData item
            putWorldEvent 0 (RezObjectEvent linkSet (px,py,pz) (vz,vy,vz) (rz,ry,rz,rs) param pk copy atRoot)
            unless copy $
                modM_ (primInventory.wprim pk) $ \ wholeInventory ->
                    [ item | item <- wholeInventory,
                        inventory /= inventoryItemName item]
        ) (findByInvName inventory objects)

isCopyable item = (cPermCopy .&. perm /= 0) &&
       (not (isInvObjectItem item) || all primCopyable links)
    where perm = permMaskOwner $ inventoryInfoPerms $ inventoryItemInfo item
          InvObject links = inventoryItemData item
          primCopyable = all isCopyable . (getI primInventory)

llResetScript info@(ScriptInfo _ _ sn pk _) [] =
    putWorldEvent 0 (ResetScriptEvent pk sn) >> doneV

llResetOtherScript info@(ScriptInfo _ _ sn pk _) [SVal sn'] = void $ do
    when (sn == sn') $ throwError "trying to reset the current script - must use llResetScript for this"
    (getM $ scriptActive.lscript pk sn) >>= flip unless (throwError "can't reset a stopped script")
    resetScript pk sn'

resetScript pk sn = do
    logAMessage LogDebug "sim" "resetting a script"
    img <- getM $ scriptImage.lscript pk sn
    t <- getM tick
    let doReset execState = do
            scriptImage.lscript pk sn =:
                (hardReset img) {executionState = execState}
            scriptActive.lscript pk sn =: True
            scriptPermissions.lscript pk sn =: M.empty
            scriptLastPerm.lscript pk sn =: Nothing
            scriptLastResetTick.lscript pk sn =: t
            scriptEventQueue.lscript pk sn =: [Event "state_entry" [] M.empty]
    case executionState img of
        Executing -> doReset Waiting
        Waiting -> doReset Waiting
        SleepingTil i -> doReset (WaitingTil i)
        WaitingTil i -> doReset (WaitingTil i)
        _ -> throwError "can't reset script that is crashed or suspended"

llGetScriptState info@(ScriptInfo _ _ _ pk _) [SVal sn] = do
    img:*active <- getM $ (scriptImage.*scriptActive).lscript pk sn
    continueI $ if not active then 0
        else case executionState img of
            Halted -> 0
            Erroneous _ -> 0
            Crashed _ -> 0
            _ -> 1

llSetScriptState (ScriptInfo _ _ sn pk _) [SVal sn', IVal state] = void $ do
    when (sn == sn') $ throwError "can't change your own state"
    img:*active <- getM $ (scriptImage.*scriptActive).lscript pk sn'
    if state == 0
        then when active $ scriptActive.lscript pk sn' =: False
        else case executionState img of
            Crashed _ -> throwError "can't change state of crashed script"
            Erroneous _ -> throwError "can't change state of erroneous script"
            _ | active -> throwError "script is already running"
              |otherwise -> resetScript pk sn'
------------------------------------------------------------------------------
llSetPayPrice info@(ScriptInfo _ _ sn pk _) [IVal price, LVal l] = void $ do
    hasMoney <- scriptHasActiveHandler pk sn "money"
    unless hasMoney $ throwError "no money handler, pay price info ignored"
    primPayInfo.wprim pk =: (price,i1,i2,i3,i4)
    where toI (IVal i) = if i < (-2) then (-1) else i
          toI _ = (-1)
          [i1,i2,i3,i4] = take 4 $ map toI l ++ replicate 4 (-1)

llGetStartParameter info@(ScriptInfo _ _ sn pk _) [] =
    (getM $ scriptStartParameter.lscript pk sn) >>= continueI
------------------------------------------------------------------------------
llCloud info _ =
    slog info "llCloud: returning default density" >>  continueF 0

llWind info _ =
    slog info "llWind: returning default density" >> continueVec (0,0,0)

llWater info _ =
    slog info "llWater: returning default water elevation" >> continueF 0

llSitTarget (ScriptInfo _ _ _ pk _) [v@(VVal _ _ _),r@(RVal _ _ _ _)] = void $
    primSitTarget.wprim pk =: (Just (vVal2Vec v, rVal2Rot r))

llAvatarOnSitTarget info@(ScriptInfo _ _ _ pk _) [] = do
    sitTarget <- getM $ primSitTarget.wprim pk
    when (isNothing sitTarget) (throwError "no sit target")
    (fromMaybe nullKey) <$> (getM $ primSittingAvatar.wprim pk) >>= continueK

llUnSit info@(ScriptInfo _ _ _ pk _) [KVal k] = void $ do
    val <- getM $ primSittingAvatar.wprim pk
    case val of
        Nothing -> throwError "no avatar sitting on prim (land unsit not implemented"
        Just ak | ak == k -> primSittingAvatar.wprim pk =: Nothing
                | otherwise -> throwError "unsit of avatar not sitting on prim (land unsit not implemented)"

llMessageLinked info@(ScriptInfo oid pid sid pkey _)
        [IVal link,IVal val,SVal msg,KVal key] = void $ do
    links <- getM $ primKeys.lm oid.wobjects
    when (null links) $ throwError "object is has no links!"
    let sender = if length links > 1 then pid + 1 else pid
    let targetLinkIndices = targetLinks (length links) link pid
    let targetLinks = map (links !!) targetLinkIndices
    let event = (Event "link_message" [IVal sender, IVal val, SVal msg, KVal key] M.empty)
    mapM_ (\ pk -> pushDeferredScriptEventToPrim event pk 0) targetLinks

targetLinks n link pid =
    case link of
        l | l == (-1) -> [0 .. (n - 1)] -- whole link set
          | l == (-2) -> [0..(pid - 1)] ++ [(pid + 1).. (n - 1)] -- others
          | l == (-3) -> [1..(n - 1)] -- children
          | l == (-4) -> [pid] -- this prim
          | otherwise -> [link - 1]

-- the key to 'sleep' is that instead of returning 'EvalIncomplete' as its EvalResult, it returns
-- 'YieldTil <some-tick>', which puts the script into a sleep state.
-- other functions which have a built in delay should use this mechanism as well.
llSleep _ [FVal f] = yieldV . (+ durationToTicks f) =<< getM tick

llInstantMessage info@(ScriptInfo _ _ _ pk _) [KVal ak, SVal message] = do
    tick:*name:*avname <- getM (tick.*(primName.wprim pk).*(avatarName.wav ak))
    logAMessage LogInfo "sim" ("IM to " ++ avname ++ " from " ++ name ++ ": " ++ message)
    yieldV $ tick + durationToTicks 2

llSay = chat sayRange
llWhisper = chat (Just 10.0)
llShout = chat (Just 100.0)
llRegionSay info params@[IVal chan, SVal message] =
    if chan == 0
       then void $ logAMessage LogWarn
           (unLslKey (scriptInfoPrimKey info) ++ ": " ++ scriptInfoScriptName info)
           "attempt to llRegionSay on channel 0"
       else chat Nothing info params

chat range info@(ScriptInfo oid pid sid pkey event)
        [IVal chan, SVal message] = void $ do
    slogChat info chan message
    putChat range pkey chan message

putChat range k chan message = do
    mp <- optional $ getM $ wprim k
    case mp of
        Just prim -> runErrPrim k () $ do
            region <- (getPrimRegion k)
            pos <- getObjectPosition =<< getRootPrim k
            putWorldEvent 0 $ Chat chan (getI primName prim) k message (region, pos) range
        Nothing -> do
            mav <- optional $ getM $ wav k
            case mav of
                Just avatar -> putWorldEvent 0 $
                    Chat chan (getI avatarName avatar) k message (getI avatarRegion avatar, getI avatarPosition avatar) range
                Nothing -> logAMessage LogWarn "sim" ("Trying to chat from unknown object/avatar with key " ++ unLslKey k)

slogChat info chan msg =
    slog info $ concat ["chan = ", show chan, ", message = ", msg]

sayErr info@(ScriptInfo oid _ _ pk _) msg = do
    slogChat info cDebugChannel msg
    putChat sayRange pk cDebugChannel msg

registerListener listener = do
    id <- nextListenerId `modM` (+1)
    lmi id.wlisteners =: (listener,True)
    return id

unregisterListener pk sname id = do
    listener <- getM $ lfst.lmi id.wlisteners
    when (listenerScriptName listener /= sname && listenerPrimKey listener /= pk)
        (throwError ("listener " ++ show id ++ " not registered for calling script"))
    wlisteners `modM_` IM.delete id
updateListener pk sname id active = do
    listener <- getM $ lfst.lmi id.wlisteners
    when (listenerScriptName listener /= sname && listenerPrimKey listener /= pk)
        (throwError ("listener " ++ show id ++ " not registered for calling script"))
    lmi id.wlisteners =: (listener,active)

llListen (ScriptInfo oid pid sid pkey event) [IVal chan, SVal sender, KVal key, SVal msg] =
    (registerListener $ Listener pkey sid chan sender key msg) >>=  continueI

llListenRemove (ScriptInfo _ _ sid pk _) [IVal id] =
    unregisterListener pk sid id >> continueV

llListenControl (ScriptInfo _ _ sid pk _) [IVal id, IVal active] =
    updateListener pk sid id (active /= 0) >> continueV

llFrand _ [FVal maxval] = wrand >>= continueF . (maxval *)

llTeleportAgentHome info@(ScriptInfo _ _ _ pk _) [KVal ak] = void $ do
    owner <- getM $ primOwner.wprim pk
    regionIndex <- getPrimRegion pk
    (_,_,parcel) <- getPrimParcel pk
    if parcelOwner parcel == owner
        then slog info ("llTeleportAgentHome: user " ++ unLslKey ak ++ " teleported home (in theory)")
        else slog info "llTeleportAgentHome: not permitted to teleport agents from this parcel"

llEjectFromLand info@(ScriptInfo oid _ _ pk _) [KVal user] = void $ do
    owner <- getM $ primOwner.wprim pk
    regionIndex <- getPrimRegion pk
    (_,parcel) <- getParcelByPosition regionIndex =<< getObjectPosition oid
    if parcelOwner parcel == owner
        then slog info ("llEjectFromLand: user " ++ unLslKey user ++ " ejected (in theory)")
        else slog info "llEjectFromLand: not permitted to eject from this parcel"

llBreakLink info@(ScriptInfo oid _ sid pk _) [IVal link] =
    do  perm <- getPermissions pk sid
        if perm .&. cPermissionChangeLinks /= 0
            then do
                objects <- getM wobjects
                links :* dynm <- getM $ (primKeys.*dynamics).lm oid.wobjects
                prohibited (getM $ primAttachment.wprim oid) <??> "can't change links of attached object"
                if link < 1 || link > length links then slog info "llBreakLink: invalid link id"
                    else do
                        let (dyn1,dyn2) = if link == 1 then (defaultDynamics,dynm) else (dynm,defaultDynamics)
                        let (xs,y:ys) = splitAt (link - 1) links
                        let (linkSet1,linkSet2) = (xs ++ ys, [y])
                        unless (null linkSet1) (lm (head linkSet1).wobjects =: LSLObject linkSet1 dyn1)
                        lm (head linkSet2).wobjects =: LSLObject linkSet2 dyn2
                        unless (null linkSet1) $ do
                            pushChangedEventToObject (head linkSet1) cChangedLink
                            primParent.wprim (head linkSet1) =: Nothing
                        pushChangedEventToObject (head linkSet2) cChangedLink
                        primParent.wprim (head linkSet2) =: Nothing
            else slog info "llBreakLink: no permission"
        continueV

llBreakAllLinks info@(ScriptInfo oid _ sid pk _) [] = void $ do
    perm <- getPermissions pk sid
    when (perm .&. cPermissionChangeLinks == 0) $ throwError "no permission"
    objects <- getM wobjects
    links :* dynm <- getM $ (primKeys.*dynamics).lm oid.wobjects
    prohibited (getM $ primAttachment.wprim oid) <??> "can't change links of attached object"
    when (length links > 1) $ do
        -- order of parameters to union is vital: union is left biased, so we want the
        -- new objects (in the left argument) to replace the old (in the right) where
        -- both exist (the root key of the old object is found in both)
        let objects' = M.union (M.fromList $ map (mkObj dynm) links) objects
        wobjects =: objects'
        mapM_ processLink links
    where mkObj dynm k = (k,LSLObject [k] (if k == oid then dynm else defaultDynamics))
          processLink k = do
              pushChangedEventToObject k cChangedLink
              primParent.wprim k =: Nothing

-- TODO: verify link order
llCreateLink info@(ScriptInfo oid _ sid pk _)
        [KVal target, IVal iparent] = void $ do
    perm <- getPermissions pk sid
    when (perm .&. cPermissionChangeLinks == 0) $ throwError "no permission to change links"
    (link':links'):*dynamics' <- getM ((primKeys.*dynamics).lm target.wobjects) <??> "target not found"
    (link:links):*dynamics <- getM $ (primKeys.*dynamics).lm oid.wobjects
    prohibited (getM $ primAttachment.wprim oid) <??> "can't change links of attached object"
    prohibited (getM $ primAttachment.wprim target) <??> "can't change links of attached object"
    mask <- getObjectPermMask target cMaskOwner
    ownerTarget:*owner <- getM (primOwner.wprim target.*primOwner.wprim oid)
    unless (mask .&. cPermModify /= 0 && owner == ownerTarget) $
        throwError "no modify permission on target"
    let (newLinkset,deleteKey,newDynamics) = if parent
                then ((link:link':links') ++ links, link',dynamics)
                else ((link':link:links) ++ links', link,dynamics')
    wobjects `modM_` (M.delete deleteKey)
    lm (head newLinkset).wobjects =: LSLObject newLinkset newDynamics
    pushChangedEventToObject (head newLinkset) cChangedLink
    where parent = iparent /= 0

llDie info@(ScriptInfo oid _ _ pk _) [] = do
     prohibited (getM $ primAttachment.wprim oid) <??> "attachment cannot die"
     links <- getM $ primKeys.lm oid.wobjects
     allScripts <- mapM getPrimScripts links
     let primsWithScriptNames = zip links (map (map inventoryItemName) allScripts)
     let skeys = concatMap ( \ (k,list) -> map ((,)k) list) primsWithScriptNames
     mapM_ (modM_ worldScripts . M.delete) skeys
     mapM_ (\ link -> liftM (M.delete link) (getM wprims) >>= (setM wprims)) links
     wobjects `modM_` M.delete oid
     slog info "object, and therefore this script, is dying"
     doneV

llAttachToAvatar info@(ScriptInfo oid _ sid pk _) [IVal attachPoint] = void $ do
    unless (attachPoint `elem` validAttachmentPoints) $
        throwError ("invalid attachment point: " ++ show attachPoint)
    prohibited (getM $ primAttachment.wprim oid) <??> "already attached"
    lastPerm :* perms <- getM $ (scriptLastPerm.*scriptPermissions).lscript pk sid
    k <- maybe (throwError "no permission to attach") return lastPerm
    perm <- mlookup k perms
    when (perm .&. cPermissionAttach == 0) $ throwError "no permission to attach"
    owner <- getM $ primOwner.wprim pk
    if k /= owner
        then putChat sayRange pk 0 "Script trying to attach to someone other than owner!"
        else do
            av <- getM $ wav k
            let attachments = getI avatarAttachments av
            rotL maybe (\ _ -> throwError "attachment point already occupied")
                (IM.lookup attachPoint attachments) $
                do lmi attachPoint.avatarAttachments.wav k =: oid
                   primAttachment.wprim oid =: Just (Attachment k attachPoint)
                   pushAttachEvent pk k

llDetachFromAvatar info@(ScriptInfo oid _ sid pk _) [] = void $ do
    when (oid /= pk) $ throwError "can't detach from within child prim"
    perms <- getM $ scriptPermissions.lscript pk sid
    Attachment k attachPoint <- getM $ primAttachment'.wprim oid
    perm <- mlookup k perms <||> throwError "no permission to detach"
    when (perm .&. cPermissionAttach == 0) $ throwError "no permission to attach"
    modM_ (avatarAttachments.wav k)  (IM.delete attachPoint)
    primAttachment.wprim oid =: Nothing
    pushAttachEvent oid nullKey
    putWorldEvent 1 (DetachCompleteEvent oid k)

llGetAttached info@(ScriptInfo oid _ _ _ _) [] =
    fromMaybe 0 <$> (getM $ attachmentPoint.primAttachment'.?wprim oid) >>= continueI

llGiveMoney info@(ScriptInfo _ _ sn pk _) [KVal ak, IVal amount] = do
    lastPerm :* perms <- getM $ (scriptLastPerm.*scriptPermissions).lscript pk sn
    permKey <- maybe (throwError "no permission") return lastPerm
    perm <- mlookup permKey perms
    when (perm .&. cPermissionAttach == 0) $ throwError "no permission"
    slog info ("llGiveMoney: pretending to give " ++ show amount ++ " to avatar with key " ++ unLslKey ak)
    continueI 0

llGetPermissionsKey (ScriptInfo _ _ sid pk _) [] =
    (getM $ scriptLastPerm.lscript pk sid) >>= continueK . fromMaybe nullKey

llGetPermissions (ScriptInfo _ _ sid pk _) [] = getPermissions pk sid >>= continueI

getPermissions pk s = do
    script <- getM $ lscript pk s
    lastPerm :* perms <- getM $ (scriptLastPerm.*scriptPermissions).lscript pk s
    maybe (return 0) (flip mlookup perms) lastPerm

getPermittedAvKey info@(ScriptInfo _ _ s pk _) fname perm = do
    lastPerm :* perms <- getM $ (scriptLastPerm.*scriptPermissions).lscript pk s
    case lastPerm of
        Nothing -> slog info (fname ++ ": not permitted") >> return Nothing
        Just k -> case M.lookup k perms of
            Nothing -> slog info (fname ++ ": internal error getting permissions") >> return Nothing
            Just i | i .&. perm /= 0 -> return (Just k)
                   | otherwise -> return Nothing

llRequestPermissions (ScriptInfo _ _ sid pk _) [KVal k, IVal mask] = void $
    maybe (logAMessage LogInfo (unLslKey pk ++ ":" ++ sid) ("Invalid permissions request: no such avatar: " ++ unLslKey k))
       (const $ putWorldEvent 0 (PermissionRequestEvent pk sid k mask)) =<< (optional $ getM $ wav k)

llClearCameraParams info@(ScriptInfo _ _ sid pk _) [] = void $
    maybe (return ()) clear =<<
        getPermittedAvKey info "llClearCameraParams" cPermissionControlCamera
    where clear k = avatarCameraControlParams.wav k =: defaultCamera

llGetCameraPos info@(ScriptInfo _ _ sid pk _) [] = do
    getPermittedAvKey info "llGetCameraPos" cPermissionTrackCamera >>=
        maybe (return (0,0,0)) (\ k -> getM $ avatarCameraPosition.wav k) >>=
            continueVec

llGetCameraRot info@(ScriptInfo _ _ sid pk _) [] =
    getPermittedAvKey info "llGetCameraRot" cPermissionTrackCamera >>=
        maybe (return (0,0,0,1)) (\ k -> getM $ avatarCameraRotation.wav k) >>=
            continueRot

llTakeCamera info _ = void $ slog info "llTakeCamera: deprecated!"
llReleaseCamera info _ = void $ slog info "llRelaaseCamera: deprecated!"
llPointAt info _ = void $ slog info "llPointAt: deprecated!"
llStopPointAt info _ = void $ slog info "llStopPointAt: deprecated!"

llSetPrimURL info _ = void $ slog info "llSetPrimURL: unimplemented!"
llRefreshPrimURL info _ = void $ slog info "llRefreshPrimURL: unimplemted!"

llGetRot info@(ScriptInfo oid _ _ pk _) [] = getGlobalRot pk >>= continueRot

llGetLocalRot info@(ScriptInfo oid _ _ pk _) [] =
    (if oid == pk then getObjectRotation oid  else getM $ primRotation.wprim pk)
        >>= continueRot

llGetRootRotation info@(ScriptInfo oid _ _ pk _) [] =
    getObjectRotation oid >>= continueRot

--TODO: handle attachments...
--TODO: confirm order of rotations in both of these
llSetRot info@(ScriptInfo oid _ _ pk _) [r@(RVal _ _ _ _)] =
    (if oid == pk then setRootRotation oid rot else setChildRotation oid pk rot) >> continueV
    where rot = rVal2Rot r

-- TODO: fix this!!
setChildRotation rk pk rot = do
    rootRot <- getM $ primRotation.wprim rk
    primRotation.wprim pk =: (rot `quaternionMultiply` rootRot `quaternionMultiply` rootRot)

setRootRotation rk rot = objectRotation.dynamics.lm rk.wobjects =: rot

llSetLocalRot info@(ScriptInfo oid _ _ pk _) [r@(RVal _ _ _ _)] = void $
    primRotation.wprim k =: rVal2Rot r
    where k = if oid == pk then oid else pk

llSetPos info@(ScriptInfo oid pid sid pk event) [val] = void $
    if oid == pk then setRootPos oid v else setChildPos pk v
    where v = vVal2Vec val

setChildPos pk = (primPosition.wprim pk =:)

-- updates the world coordinates of object
-- does NOT consider region boundaries (TODO: fix!)
setRootPos oid v =
   do v0 <- getObjectPosition oid
      let vec = diff3d v v0
      let dist = mag3d vec
      let vec' = if dist <= 10.0
              then vec
              else scale3d 10.0 $ norm3d vec
      objectPosition.dyn oid =: v0 `add3d` vec'

llGetOwner info [] =
    let k = scriptInfoPrimKey info in getM (primOwner.wprim k) >>= continueK
llGetCreator info [] =
    let k = scriptInfoPrimKey info in getM (primCreator.wprim k) >>= continueK
llSameGroup info@(ScriptInfo _ _ _ pk _) [KVal k] =
    do  group <- avGroup <||> getM (primGroup.wprim k) <||> throwError ("no such object or avatar: " ++ unLslKey k)
        myGroup <- getM (primGroup.wprim pk) <||> throwError ("can't find " ++ unLslKey pk)
        continueI (lslBool (group == myGroup))
    where avGroup = getM $ avatarActiveGroup.wav k

getLinkKey oid link =
    lookupByIndex (link - 1) =<< getM (primKeys.lm oid.wobjects)

llGetLinkKey (ScriptInfo oid _ _ _ _) [IVal link] =
    continueK =<< getLinkKey oid link

-- TODO: should check for av/prim in same region
-- TODO: no concept of online/offline for av
llKey2Name info [KVal k] =
    (continueS =<< getM (avatarName.wav k)) <||>
    (continueS =<< getM (primName.wprim k)) <||> continueS ""

llOwnerSay info [SVal s] = void $ do
    slog info ("Owner Say: " ++ s)
    owner <- getM $ primOwner.wprim (scriptInfoObjectKey info)
    putWorldEvent 0 (AvatarInputEvent owner (AvatarOwnerSay (scriptInfoPrimKey info) s))

llGetGeometricCenter info@(ScriptInfo oid _ _ _ _) [] = do
    links <- getM $ primKeys.lm oid.wobjects
    when (null links) $ throwError "error: empty linkset!"
    rootPos <- getObjectPosition oid
    foldM (\ v -> liftM (add3d v) . getGlobalPos) (0,0,0) links
        >>= continueVec . scale3d (1 / fromIntegral (length links))

llGetMass info@(ScriptInfo oid _ _ _ _) [] = objectMass oid >>= continueF

objectMass oid = do
    links <- getM $ primKeys.lm oid.wobjects
    foldM (\ v k -> (getM $ wprim k) >>= ( \ prim -> return $ v + primMassApprox prim)) 0.0 links

llGetObjectMass info [KVal k] =
    (((getM $ wav k) >> return 80.0) <||> objectMass k) >>= continueF

llGetVel info@(ScriptInfo oid _ _ _ _) [] =
    continueVec =<< getM (objectVelocity.dyn oid)

llGetForce info@(ScriptInfo oid _ _ _ _) [] =
    continueVec . fst =<< getM (objectForce.dyn oid)

llSetForce info@(ScriptInfo oid _ _ _ _) [v@(VVal x y z), IVal local] =
    objectForce.dyn oid =: ((x,y,z), local /= 0) >> continueV

llSetBuoyancy info@(ScriptInfo { scriptInfoObjectKey = oid }) [FVal buoy] = do
   objectBuoyancy.dyn oid =: buoy
   continueV

llApplyImpulse info@(ScriptInfo oid _ _ _ _) [v@(VVal x y z), IVal local] =
   do t <- getM tick
      objectImpulse.dyn oid =: (((x,y,z), local /= 0), t + durationToTicks 1)
      continueV

llGetAccel info@(ScriptInfo oid _ _ _ _) [] = do
    prim <- getM $ wprim oid
    dyn <- getM $ dyn oid
    let force = fst (getI objectForce dyn) `rot3d` rotation
            where rotation = if snd $ getI objectForce dyn then getI objectRotation dyn else (0,0,0,1)
    let impulse = ((fst . fst . getI objectImpulse) dyn `rot3d` rotation, (snd . getI objectImpulse) dyn)
            where rotation = if (snd . fst . getI objectImpulse) dyn then getI objectRotation dyn else (0,0,0,1)
    continueVec =<< calcAccel <$> getM tick <*> pure 0 <*> objectMass oid <*> getObjectPosition oid <*>
        pure force <*> pure impulse

llSetTorque info@(ScriptInfo { scriptInfoObjectKey = oid }) [VVal x y z, IVal local] =
    do  objectTorque.dyn oid =: ((x,y,z),local /= 0)
        continueV

llSetForceAndTorque info@(ScriptInfo { scriptInfoObjectKey = oid}) [VVal fx fy fz, VVal tx ty tz, IVal local] =
    do  let loc = local /= 0
        objectForce.dyn oid =: ((fx,fy,fz),loc)
        objectTorque.dyn oid =: ((tx,ty,tz),loc)
        continueV

llGetTorque (ScriptInfo { scriptInfoObjectKey = oid }) [] =
    continueVec . fst =<< getM (objectTorque.dyn oid)

llGetOmega info@(ScriptInfo { scriptInfoObjectKey = oid }) [] =
    continueVec =<< getM (objectOmega.dyn oid)

llApplyRotationalImpulse info@(ScriptInfo { scriptInfoObjectKey = oid }) [VVal x y z, IVal local] =
    do  t <- getM tick
        objectRotationalImpulse.dyn oid =: (((x,y,z),local /= 0), t + durationToTicks 1)
        continueV

llGetPos info@(ScriptInfo _ _ _ pk _) [] = getGlobalPos pk >>= continueVec
llGetRootPosition info@(ScriptInfo oid _ _ _ _) [] = getObjectPosition oid >>= continueVec

getGlobalPos pk = do
        oid <- getRootPrim pk
        root <- getObjectPosition oid
        rel <- getM $ primPosition.wprim pk
        rot <- getObjectRotation oid
        return (root `add3d` rot3d rel rot)

getGlobalRot pk =
    quaternionMultiply <$> getM (primRotation.wprim pk) <*> (getObjectRotation =<< getRootPrim pk)

llRotLookAt info@(ScriptInfo oid _ _ _ _) [RVal x y z s, FVal strength, FVal tau] =
    rotLookAt oid (x,y,z,s) strength tau >> continueV

llLookAt info@(ScriptInfo oid _ _ _ _) [VVal x y z, FVal strength, FVal tau] =
    do  pos <- getObjectPosition oid
        rot <- getGlobalRot oid
        let v = diff3d (x,y,z) pos
        let rot1 = rot `quaternionMultiply` rotationBetween (rot3d (0,0,1) rot) v
        rotLookAt oid rot1 strength tau
        continueV

rotLookAt oid (x,y,z,s) strength tau =
    objectRotationTarget.dyn oid =:
        Just RotationTarget {
                rotationTarget = (x,y,z,s),
                rotationTargetStrength = strength,
                rotationTargetTau = tau }

llStopLookAt info@(ScriptInfo oid _ _ _ _) [] = do
    objectRotationTarget.dyn oid =: Nothing
    continueV

llGetLocalPos (ScriptInfo _ _ _ pk _) [] =
    getM (primPosition.wprim pk) >>= continueVec

llMoveToTarget info@(ScriptInfo { scriptInfoObjectKey = oid, scriptInfoScriptName = sn, scriptInfoPrimKey = pk }) [VVal x y z, FVal tau] =
    do  tgt <- getM (objectPositionTarget.dyn oid)
        case tgt of
            Nothing -> setTarg
            Just (PositionTarget { positionTargetSetBy = scriptId })
                | scriptId == (pk,sn) -> setTarg
                | otherwise -> logAMessage LogInfo "llMoveToTarget" ("target already set by another script: " ++ show scriptId)
            Just _ -> setTarg
        continueV
    where setTarg = objectPositionTarget.dyn oid =:
              Just PositionTarget {
                  positionTargetTau = tau,
                  positionTargetLocation = (x,y,z),
                  positionTargetSetBy = (pk,sn) }

llStopMoveToTarget info@(ScriptInfo { scriptInfoObjectKey = oid }) [] = do
    objectPositionTarget.dyn oid =: Nothing
    continueV

llGroundRepel info@(ScriptInfo { scriptInfoObjectKey = oid }) [FVal height, IVal water, FVal tau] =
    do  objectPositionTarget.dyn oid =:
            Just Repel {
                positionTargetTau = tau,
                positionTargetOverWater = water /= 0,
                positionTargetHeight = height }
        continueV

llSetHoverHeight info@(ScriptInfo { scriptInfoObjectKey = oid }) [FVal height, IVal water, FVal tau] =
    do  objectPositionTarget.dyn oid =:
            Just Hover {
                    positionTargetTau = tau,
                    positionTargetOverWater = water /= 0,
                    positionTargetHeight = height }
        continueV

llStopHover info@(ScriptInfo { scriptInfoObjectKey = oid }) [] = do
    targ <- getM $ objectPositionTarget.dyn oid
    maybe (logAMessage LogWarn "llStopHover" "not hovering")
        (const (objectPositionTarget.dyn oid =: Nothing)) targ
    continueV

runFace k i action = action <||>
    throwError ("face " ++ show i ++ " or prim " ++ unLslKey k ++ " not found")

llGetAlpha (ScriptInfo _ _ _ pkey _) [IVal side] =
    computeAlpha >>= continueF
    where computeAlpha = if side /= -1
            then runFace pkey side (getPrimFaceAlpha pkey side)
            else runFace pkey side $ do
                faces <- getM $ primFaces.wprim pkey
                let n = length faces
                return $ if n > 0 then sum (map (getI faceAlpha) faces) / fromInt n
                                  else 1.0

llSetAlpha (ScriptInfo _ _ _ pkey _) [FVal alpha, IVal face] =
    setAlpha alpha face pkey

setAlpha alpha face pkey =
    if face == -1
        then runErrFace pkey face (EvalIncomplete,VoidVal) $ do
                faces <- getM $ primFaces.wprim pkey
                let faces' = map (setI faceAlpha alpha) faces
                primFaces.wprim pkey =: faces'
                continueV
        else setPrimFaceAlpha pkey face alpha >> continueV

llSetLinkAlpha (ScriptInfo oid pid _ _ _) [IVal link, FVal alpha, IVal face] = do
    mapM_ (setAlpha alpha face) =<< getTargetPrimKeys oid link pid
    continueV

getTargetPrimKeys oid link pid = do
    prims <-
        (runAndLogIfErr ("can't find object " ++ unLslKey oid) [] $ getM $ primKeys.lm oid.wobjects)
    let targetList = targetLinks (length prims) link pid
    mapM (flip lookupByIndex prims) targetList

llGetColor (ScriptInfo _ _ _ pkey _) [IVal side] =
    computeColor >>= continueVec
    where computeColor = if side /= -1
            then runFace pkey side (getPrimFaceColor pkey side)
            else runFace pkey side $ do
                faces <- getM $ primFaces.wprim pkey
                let n = length faces
                return $ if n > 0 then scale3d (1.0/ fromInt (length faces) )
                                           (foldr add3d (0.0,0.0,0.0) (map (getI faceColor) faces))
                                  else (1.0,1.0,1.0)

llSetColor (ScriptInfo _ _ _ pkey _) [color, IVal face] = setColor color face pkey

setColor color face pkey =
    if face == -1
        then runFace pkey face $ do
                let colorVal = vVal2Vec color
                (primFaces.wprim pkey) `modM` map (setI faceColor colorVal)
                continueV
        else setPrimFaceColor pkey face (vVal2Vec color) >> continueV

llSetLinkColor (ScriptInfo oid pid _ _ _) [IVal link, color, IVal face] = do
    mapM_ (setColor color face) =<< getTargetPrimKeys oid link pid
    continueV

runFaceTextureQuery pk face query =
    runFace pk face (query <$> (getPrimFaceTextureInfo pk face'))
    where face' = if face == -1 then 0 else face

llGetTextureOffset (ScriptInfo _ _ _ pkey _) [IVal face] =
    continueVec =<< runFaceTextureQuery pkey face (getI textureOffsets)
llGetTextureScale (ScriptInfo _ _ _ pkey _) [IVal face] =
    continueVec =<< runFaceTextureQuery pkey face (getI textureRepeats)
llGetTextureRot (ScriptInfo _ _ _ pkey _) [IVal face] =
    continueF =<< runFaceTextureQuery pkey face (getI textureRotation)

llSetTexture (ScriptInfo _ _ _ pk _) [SVal texture,IVal face] = do
    tk <- findTexture pk texture
    setTexture tk face pk
    continueV

-- TODO: worry about texture permissions
llGetTexture (ScriptInfo _ _ _ pkey _) [IVal face] =
    let face' = if face == -1 then 0 else face in
        runFace pkey face (do
            info <- getPrimFaceTextureInfo pkey face'
            invTextures <- getPrimTextures pkey
            let tKey = getI textureKey info
            case findByInvKey tKey invTextures of
                 Nothing -> return $ unLslKey tKey
                 Just item -> return $ inventoryItemName item) >>= continueS

llSetLinkTexture (ScriptInfo oid pid _ pk _) [IVal link, SVal texture,IVal face] = do
    tk <- findTexture pk texture
    mapM_ (setTexture tk face) =<< getTargetPrimKeys oid link pid
    continueV

updateTextureInfo update pk faceIndex =
    runFace pk faceIndex (
        if faceIndex == -1
            then do
                let f faces = map (\ face ->
                        let info = getI faceTextureInfo face in
                            setI faceTextureInfo (update info) face) faces
                (primFaces.wprim pk) `modM_` f
            else do
                f <- getM (primFaces.wprim pk) >>= lookupByIndex faceIndex
                let tInfo = update $ getI faceTextureInfo f
                updatePrimFace pk faceIndex (setI faceTextureInfo tInfo)
    ) >> continueV

llScaleTexture info@(ScriptInfo _ _ _ pk _) [FVal h,FVal v,IVal faceIndex] =
    updateTextureInfo (setI textureRepeats (h,v,0)) pk faceIndex
llOffsetTexture (ScriptInfo _ _ _ pk _) [FVal h, FVal v, IVal faceIndex] =
    updateTextureInfo (setI textureOffsets (h,v,0)) pk faceIndex
llRotateTexture (ScriptInfo _ _ _ pk _) [FVal rot, IVal faceIndex] =
    updateTextureInfo (setI textureRotation rot) pk faceIndex

groupList _ [] = []
groupList n l | n > 0 = take n l : groupList n (drop n l)
              | otherwise = groupList 1 l

genNum :: [Int] -> Integer -> Integer
genNum rands maxval =
    let rands' :: [Integer]
        rands' = map ((0x80000000-).toInteger) rands
        topval :: Rational
        topval = fromInteger $
            foldl' (.|.) 0 $ zipWith shiftL (replicate (length rands') 0xffffffff) [0,32..]
        randval :: Rational
        randval = fromInteger $ foldl' (.|.) 0 $ zipWith shiftL rands' [0,32]
    in floor ((randval / topval) * fromInteger maxval)


llListRandomize _ [LVal list, IVal stride] =
    let l1 = groupList stride list
        n = fac (toInteger $ length l1)
        randsNeeded = ceiling $ logBase 2 (fromInteger n) / 32
        wrands = replicate randsNeeded wrand
    in  do
        rands <- sequence wrands
        let permutation = genNum rands n
        continueL $ generatePermutation list permutation

llGetNumberOfPrims (ScriptInfo oid _ _ pkey _) [] =
    continueI =<< (length <$> getM (primKeys.lm oid.wobjects))

llGetObjectPrimCount info@(ScriptInfo oid _ _ pkey _) [KVal k] =
    do  region <- getPrimRegion oid
        p <- getM $ wprim k
        region' <- getPrimRegion k
        if region /= region'
            then continueI 0
            else getRootPrim k >>= primCount >>= continueI
    where primCount oid = length <$> getM (primKeys.lm oid.wobjects)

llGetNumberOfSides (ScriptInfo _ _ _ pkey _) [] =
     getM (primFaces.wprim pkey) >>= continueI . length

llGetObjectDesc (ScriptInfo _ _ _ pkey _) [] =
    continueS =<< getM (primDescription.wprim pkey)
llGetObjectName (ScriptInfo _ _ _ pkey _) [] =
    continueS =<< getM (primName.wprim pkey)

llSetObjectName (ScriptInfo _ _ _ pkey _) [SVal name] =
    (primName.wprim pkey) =: (take 255 name) >> continueV

llSetObjectDesc (ScriptInfo _ _ _ pkey _) [SVal desc] =
    primDescription.wprim pkey =: (take 127 desc) >> continueV

llGetObjectPermMask (ScriptInfo oid _ _ _ _) [IVal maskId] =
    getObjectPermMask oid maskId >>= continueI

getObjectPermMask oid maskId = do
    masks <- getM $ primPermissions.wprim oid
    let base = if null masks then 0x0008e000 else head masks
    let n = length masks
    return (if maskId `elem` [0..(n-1)] then masks !! maskId else
                  if maskId == 3 then 0 else base)

llGetScale (ScriptInfo _ _ _ pkey _) [] = continueVec =<< getM (primScale.wprim pkey)

llSetScale (ScriptInfo _ _ _ pk _) [scale] =
    if tooSmall then continueV else primScale.wprim pk =: clippedVec >> continueV
    where (x,y,z) = vVal2Vec scale
          tooSmall = x < 0.01 || y < 0.01 || z < 0.01
          clippedVec = (min x 10.0, min y 10.0, min z 10.0)

llGetBoundingBox info [KVal k] =
    do  logAMessage LogInfo "sim" "note: llGetBoundingBox does not return accurate results (yet)"
        withAvatar k notFound found
    where tup2l (x,y) = [x,y]
          found avatar = let pos = getI avatarPosition avatar in
                continueL $ map (vec2VVal . add3d pos) [(-1.0,-1.0,-1.0),(1.0,1.0,1.0)]
          notFound = getRootPrim k >>= getPrimBox >>= continueL . map vec2VVal . tup2l

getPrimBox pk = do
    pos <- getGlobalPos pk
    scale <- getM $ primScale.wprim pk
    let (xs,ys,zs) = scale3d 0.5 scale
    return (add3d pos (-xs, -ys, -zs),add3d pos (xs,ys,zs))

llSetTimerEvent (ScriptInfo _ _ sn pk _) [FVal interval] =
    -- TODO: this may not accurately reflect buggy behavior in SL
    do removePendingTimerEvent pk sn
       when (interval > 0) $ putWorldEvent interval (TimerEvent interval (pk,sn))
       continueV
    where removePendingTimerEvent pk sn = wqueue `modM_` f
              where f wq = flip filter wq $ \ e -> case e of
                      (_,TimerEvent _ (pk',sn')) -> pk /= pk' || sn /= sn'
                      _ -> True

llSetVehicleFlags info@(ScriptInfo _ _ _ pk _) [IVal flagsToSet] =
    void $ (primVehicleFlags.wprim pk) `modM_` (.|. flagsToSet)

llRemoveVehicleFlags info@(ScriptInfo _ _ _ pk _) [IVal flagsToClear] =
    void $ (primVehicleFlags.wprim pk) `modM_` (.&. complement flagsToClear)

----------------------------------------------------------------------
-- get/set prim parameters
llGetPrimitiveParams (ScriptInfo _ _ _ pk _) [LVal l] = getPrimParameters pk l >>= continueL
llSetPrimitiveParams (ScriptInfo _ _ _ pk _) [LVal l] = setPrimParameters pk l >> continueV

llSetLinkPrimitiveParams (ScriptInfo oid pid _ _ _) [IVal link,LVal l] = do
    pks <- getM $ primKeys.lm oid.wobjects
    let targetList = targetLinks (length pks) link pid
    pkList <- mapM (flip lookupByIndex pks) targetList
    mapM_ (flip setPrimParameters l) pkList
    continueV

getPrimParameters pk params =
    case params of
        [] -> return []
        (x:xs) | x `elem` [llcPrimBumpShiny,llcPrimFullbright,llcPrimColor,llcPrimTexture,llcPrimTexgen] && null xs -> return []
               | x == llcPrimBumpShiny  -> queryAndDoRest (queryPrimBumpShiny $ head xs) (tail xs)
               | x == llcPrimColor -> queryAndDoRest (queryPrimColor $ head xs) (tail xs)
               | x == llcPrimTexture -> queryAndDoRest (queryPrimTexture $ head xs) (tail xs)
               | x == llcPrimTexgen -> queryAndDoRest (queryPrimTexgen $ head xs) (tail xs)
               | x == llcPrimFullbright -> queryAndDoRest (queryPrimFullbright $ head xs) (tail xs)
               | x == llcPrimMaterial -> queryAndDoRest queryPrimMaterial xs
               | x == llcPrimPhantom -> queryAndDoRest (queryPrimStatus primPhantomBit) xs
               | x == llcPrimPhysics -> queryAndDoRest (queryPrimStatus primPhysicsBit) xs
               | x == llcPrimFlexible -> queryAndDoRest queryPrimFlexible xs
               | x == llcPrimPointLight -> queryAndDoRest queryPrimLight xs
               | x == llcPrimPosition -> queryAndDoRest queryPrimPosition xs
               | x == llcPrimRotation -> queryAndDoRest queryPrimRotation xs
               | x == llcPrimSize -> queryAndDoRest queryPrimScale xs
               | x == llcPrimTempOnRez -> queryAndDoRest queryPrimTempOnRez xs
               | x == llcPrimType -> queryAndDoRest queryPrimType xs
               | x == IVal 1 -> queryAndDoRest queryPrimTypeOldSkool xs
               | otherwise -> getPrimParameters pk xs
    where queryAndDoRest q = liftM2 (++) (q pk) . getPrimParameters pk

queryPrimTempOnRez pk = liftM ((:[]) . IVal . lslBool) $ getM $ primTempOnRez.wprim pk
queryPrimRotation = liftM ((:[]) . rot2RVal) . getGlobalRot
queryPrimPosition = liftM ((:[]) . vec2VVal) . getGlobalPos
queryPrimScale = liftM ((:[]) . vec2VVal) . (\k -> getM $ primScale.wprim k)
queryPrimFlexible k =
    getM (primFlexibility.wprim k) >>= (\ flex -> case flex of
        Nothing -> return [IVal 0, IVal 0, FVal 0, FVal 0, FVal 0, FVal 0, VVal 0.0 0.0 0.0]
        Just flex' -> return $ map ($ flex')
            [const (IVal 1),IVal . getI flexSoftness, FVal . getI flexGravity,
             FVal . getI flexFriction, FVal . getI flexWind,
             FVal . getI flexTension, vec2VVal . getI flexForce])
queryPrimLight k =
    getM (primLight.wprim k) >>= (\ light -> case light of
        Nothing -> return [IVal 0, VVal 0 0 0, FVal 0, FVal 0, FVal 0]
        Just light' -> return $ map ($ light')
            [const (IVal 1), vec2VVal . getI lightColor,
             FVal . getI lightIntensity, FVal . getI lightRadius,
             FVal . getI lightFalloff])

queryPrimMaterial k = liftM ((:[]) . IVal) $ getM $ primMaterial.wprim k
queryPrimStatus bit k =  liftM ((:[]) . IVal . (\ i -> lslBool (testBit i bit))) $ getM $ primStatus.wprim k
queryPrimBumpShiny side =  queryFaceVals bumpShiny side
    where bumpShiny face = [IVal $ getI faceShininess face, IVal $ getI faceBumpiness face]
queryPrimColor side = queryFaceVals colorAlpha side
    where colorAlpha face = [vec2VVal $ getI faceColor face, FVal $ getI faceAlpha face]
queryPrimTexture side = queryFaceVals textureInfo side
    where textureInfo face =
              let tinfo = getI faceTextureInfo face in map ($ tinfo)
                      [SVal . unLslKey . getI textureKey,vec2VVal . getI textureRepeats,
                       vec2VVal . getI textureOffsets,
                       FVal . getI textureRotation]
queryPrimTexgen = queryFaceVals (return . IVal . getI faceTextureMode)
queryPrimFullbright = queryFaceVals (\ face -> if getI faceFullbright face then [IVal 1] else [IVal 0])

queryFaceVals f (IVal side) k =
    if side == -1
        then (concatMap f) <$> getM (primFaces.wprim k)
        else getM (primFaces.wprim k) >>= fromWorldE [] . (liftM f . lookupByIndex side)

-- TODO: get these out of here
Just (Constant _ llcObjectUnknownDetail) = findConstant "OBJECT_UNKNOWN_DETAIL"
Just (Constant _ llcObjectName) = findConstant "OBJECT_NAME"
Just (Constant _ llcObjectDesc) = findConstant "OBJECT_DESC"
Just (Constant _ llcObjectPos) = findConstant "OBJECT_POS"
Just (Constant _ llcObjectRot) = findConstant "OBJECT_ROT"
Just (Constant _ llcObjectVelocity) = findConstant "OBJECT_VELOCITY"
Just (Constant _ llcObjectOwner) = findConstant "OBJECT_OWNER"
Just (Constant _ llcObjectGroup) = findConstant "OBJECT_GROUP"
Just (Constant _ llcObjectCreator) = findConstant "OBJECT_CREATOR"

queryPrimType k = do
    (PrimType version typecode holeshape cut twist holesize topshear hollow taper advancedcut roffset revs skew sculpt sculptType) <-
        getPrimTypeInfo k
    case typecode of
       i | IVal i `elem` [llcPrimTypeBox,llcPrimTypeCylinder,llcPrimTypePrism] ->
                      return [IVal i,IVal holeshape, vec2VVal cut, FVal hollow, vec2VVal twist, vec2VVal taper, vec2VVal topshear]
         | IVal i == llcPrimTypeSphere -> return [IVal i,IVal holeshape, vec2VVal cut, FVal hollow, vec2VVal twist, vec2VVal advancedcut]
         | IVal i `elem` [llcPrimTypeRing,llcPrimTypeTorus,llcPrimTypeTube] ->
                      return [IVal i,IVal holeshape, vec2VVal cut, FVal hollow, vec2VVal twist, vec2VVal holesize, vec2VVal topshear,
                    vec2VVal advancedcut, vec2VVal taper, FVal revs, FVal roffset, FVal skew]
         | IVal i == llcPrimTypeSculpt -> return [IVal i,SVal $ fromMaybe "" sculpt, IVal sculptType]
         | otherwise -> return []

queryPrimTypeOldSkool k = do
    (PrimType version typecode holeshape cut twist holesize topshear hollow taper advancedcut roffset revs skew sculpt sculptType) <-
        getPrimTypeInfo k
    case typecode of
       i | IVal i `elem` [llcPrimTypeBox,llcPrimTypeCylinder,llcPrimTypePrism] ->
                      return [IVal i, vec2VVal cut, FVal hollow, FVal $ yOf twist, vec2VVal taper, vec2VVal topshear]
         | IVal i == llcPrimTypeSphere -> return [IVal i,vec2VVal cut, FVal hollow,vec2VVal advancedcut]
         | IVal i == llcPrimTypeTorus ->
                      return [IVal i, vec2VVal cut, FVal hollow, FVal $ yOf twist, FVal $ yOf taper, vec2VVal topshear, vec2VVal advancedcut]
         | IVal i == llcPrimTypeTube ->
                      return [IVal i, vec2VVal cut, FVal hollow, FVal $ yOf twist, FVal $ xOf topshear]
         | otherwise -> return []
    where xOf (x,y,z) = x
          yOf (x,y,z) = y

setPrimParameters pk params = updatePrimParameters pk params >> return ()

updatePrimParameters pk [] = return []
updatePrimParameters pk (code:rest) = do
    result <- case code of
       i | i == llcPrimTempOnRez -> updatePrimTempOnRez pk rest
          | i == llcPrimMaterial -> updatePrimMaterial pk rest
          | i == llcPrimPhantom -> updatePrimPhantom pk rest
          | i == llcPrimPhysics -> updatePrimPhysics pk rest
          | i == llcPrimPosition -> updatePrimPosition pk rest
          | i == llcPrimRotation -> updatePrimRotation pk rest
          | i == llcPrimSize -> updatePrimScale pk rest
          | i == llcPrimFlexible -> updatePrimFlexible pk rest
          | i == llcPrimPointLight -> updatePrimLight pk rest
          | i == llcPrimBumpShiny -> updatePrimBumpShiny pk rest
          | i == llcPrimColor -> updatePrimColor pk rest
          | i == llcPrimTexture -> updatePrimTexture pk rest
          | i == llcPrimTexgen -> updatePrimTexgen pk rest
          | i == llcPrimFullbright -> updatePrimFullbright pk rest
          | i == llcPrimType -> updatePrimType pk rest
          | i == IVal 1 -> updatePrimTypeOldSkool pk rest
          | otherwise -> throwError "incorrect parameter"
    updatePrimParameters pk result

badParms = throwError . ("insufficient or incorrect parameters for " ++)

updatePrimType pk vals = (getM $ wprim pk) >>= flip updatePrimType' vals >>= (\ (prim,rest) -> (wprim pk =: prim) >> return rest)
updatePrimType' prim (primCode@(IVal i):rest) | primCode == llcPrimTypeSphere = updatePrimTypeSphere prim rest
                                              | primCode `elem` [llcPrimTypeBox,llcPrimTypeCylinder,llcPrimTypePrism] =
                                                  updatePrimTypeBoxCylPrism i prim rest
                                              | primCode `elem` [llcPrimTypeTorus,llcPrimTypeTube,llcPrimTypeRing] =
                                                  updatePrimTypeRingTorusTube i prim rest
                                              | primCode == llcPrimTypeSculpt = updatePrimTypeSculpt prim rest
                                              | otherwise = badParms "PRIM_TYPE"
updatePrimType' _ _ = badParms "PRIM_TYPE"

updatePrimTypeOldSkool pk vals = (getM $ wprim pk) >>= flip updatePrimTypeOldSkool' vals >>= (\ (prim,rest) -> (wprim pk =: prim) >> return rest)
updatePrimTypeOldSkool' prim (primCode@(IVal i):rest) | primCode == llcPrimTypeSphere = updatePrimTypeSphereOld prim rest
                                                      | primCode `elem` [llcPrimTypeBox,llcPrimTypeCylinder,llcPrimTypePrism] =
                                                          updatePrimTypeBoxCylPrismOld i prim rest
                                                      | primCode == llcPrimTypeTorus = updatePrimTypeTorusOld prim rest
                                                      | primCode == llcPrimTypeTube = updatePrimTypeTubeOld prim rest
                                                      | otherwise = badParms "deprecated prim type"
updatePrimTypeOldSkool' _ _ = badParms "deprecated prim type"

updatePrimTempOnRez pk (IVal tempOnRez:rest) =
    (primTempOnRez.wprim pk =: (tempOnRez /= 0)) >> return rest
updatePrimTempOnRez _ _ = badParms "PRIM_TEMP_ON_REZ"

updatePrimMaterial pk (IVal material:rest) = (primMaterial.wprim pk =: material) >> return rest
updatePrimMaterial _ _ = throwError "insufficient or incorrect parameters for PRIM_MATERIAL"

updatePrimPhantom = updatePrimStatus "insufficient or incorrect parameters for PRIM_PHANTOM" primPhantomBit
updatePrimPhysics = updatePrimStatus "insufficient or incorrect parameters for PRIM_PHYSICS" primPhysicsBit

updatePrimStatus _ bit pk (IVal i:rest) =
    (primStatus.wprim pk) `modM_` flip (if i == 0 then clearBit else setBit) bit
    >> return rest

updatePrimStatus fMsg _ _ _ = throwError fMsg

updatePrimPosition pk (pos@(VVal _ _ _):rest) =
    do  getM (primParent.wprim pk) >>= \ mok -> case mok of
                Nothing -> setRootPos pk v
                Just ok -> setChildPos pk v
        return rest
    where v = vVal2Vec pos
updatePrimPosition _ _ = badParms "PRIM_POSITION"

updatePrimRotation pk (rot@(RVal _ _ _ _):rest) =
    do getM (primParent.wprim pk) >>= \ mok -> case mok of
            Nothing -> setRootRotation pk r
            Just rk -> setChildRotation rk pk r
       return rest
    where r = rVal2Rot rot
updatePrimRotation _ _ = badParms "PRIM_ROTATION"
updatePrimScale pk (scale@(VVal _ _ _):rest) = primScale.wprim pk =: (vVal2Vec scale) >> return rest
updatePrimScale _ _ = badParms "PRIM_SIZE"

updatePrimFlexible pk (IVal flex:IVal soft:FVal gravity:FVal friction:FVal wind:FVal tension:VVal fx fy fz:rest) =
    (primFlexibility.wprim pk =:
        (if flex == 0 then Nothing else Just (Flexibility soft gravity friction wind tension (fx,fy,fz)))) >> return rest
updatePrimFlexible _ _ = badParms "PRIM_FLEXIBLE"

updatePrimLight pk (IVal light:VVal r g b:FVal intensity:FVal radius:FVal falloff:rest) = do
    primLight.wprim pk =:
        (if light == 0 then Nothing else Just (LightInfo (r,g,b) intensity radius falloff))
    return rest
updatePrimLight _ _ = badParms "PRIM_POINT_LIGHT"

updatePrimBumpShiny pk params =
    let extract (IVal face:IVal bump:IVal shiny:rest) = return (face, [IVal bump, IVal shiny], rest)
        extract _ = badParms "PRIM_BUMP_SHINY"
        update [IVal bump, IVal shiny] = setI (faceBumpiness.*faceShininess) (bump:*shiny)
    in updatePrimFaceParams pk params extract update
updatePrimColor pk params =
    let extract (IVal face:VVal r g b:FVal alpha:rest) = return (face,[VVal r g b, FVal alpha], rest)
        extract _ = badParms "PRIM_COLOR"
        update [color, FVal alpha] = setI (faceColor.*faceAlpha) (vVal2Vec color:*alpha)
    in updatePrimFaceParams pk params extract update
updatePrimTexture pk params =
    let extract (IVal face:name@(SVal _):repeats@(VVal _ _ _):offsets@(VVal _ _ _):rotation@(FVal _):rest) =
            return (face,[name,repeats,offsets,rotation],rest)
        extract _ = badParms "PRIM_TEXTURE"
        update [SVal name,repeats,offsets,FVal rotation] =
            setI faceTextureInfo (TextureInfo (LSLKey name) (vVal2Vec repeats) (vVal2Vec offsets) rotation)
    in updatePrimFaceParams pk params extract update
updatePrimTexgen pk params =
    let extract (IVal face:IVal mode:rest) = return (face,[IVal mode],rest)
        extract _ = badParms "PRIM_TEXGEN"
        update [IVal mode] = setI faceTextureMode mode
    in updatePrimFaceParams pk params extract update
updatePrimFullbright pk params =
    let extract (IVal face:IVal fullbright:rest) = return (face,[IVal fullbright],rest)
        extract _ = badParms "PRIM_FULLBRIGHT"
        update [IVal fullbright] = setI faceFullbright (fullbright /=0)
    in updatePrimFaceParams pk params extract update

updatePrimFaceParams pk params extract update = do
    (face, faceParams, rest) <- extract params -- this can fail
    faces <- getM $ primFaces.wprim pk
    if face == -1 then primFaces.wprim pk =: (map (update faceParams) faces)
        else case splitAt face faces of
            (_,[]) -> return ()
            (xs,y:ys) -> primFaces.wprim pk =: (xs ++ (update faceParams y:ys))
    return rest

updatePrimTypeBoxCylPrism ptype prim (IVal holeshape:VVal cx cy cz:FVal hollow
        :VVal twx twy twz:VVal tx ty tz:VVal sx sy sz:rest) =
    return (setI lbl (ptype:*holeshape:*(cx,cy,cz):*hollow:*(twx,twy,twz):*
        (tx,ty,tz):*(sx,sy,sz)) prim,rest) where
    lbl = (primTypeCode.*primHoleshape.*primCut.*primHollow.*primTwist.*
        primTaper.*primTopshear).primTypeInfo
updatePrimTypeBoxCylPrism _ _ _ =
    badParms "PRIM_TYPE (PRIM_TYPE_PRISM, PRIM_TYPE_CYLINDER, or PRIM_TYPE_BOX)"
updatePrimTypeSphere prim (IVal holeshape:VVal cx cy cz:FVal hollow:VVal tx ty tz:VVal ax ay az:rest) =
    return (setI lbl (cPrimTypeSphere:*holeshape:*(cx,cy,cz):*hollow:*(tx,ty,tz):*
        (ax,ay,az)) prim, rest) where
    lbl = (primTypeCode.*primHoleshape.*primCut.*primHollow.*primTwist.*
        primAdvancedCut).primTypeInfo
updatePrimTypeSphere _ _ = badParms "PRIM_TYPE (PRIM_TYPE_SPHERE)"
updatePrimTypeRingTorusTube ptype prim (IVal holeshape:VVal cx cy cz:FVal hollow:VVal twx twy twz:VVal hx hy hz:VVal sx sy sz:
                                        VVal ax ay az:VVal tx ty tz:FVal revs:FVal roffset:FVal skew:rest) =
    return (setI lbl (ptype:*holeshape:*(cx,cy,cz):*hollow:*(twx,twy,twz):*
        (hx,hy,hz):*(sx,sy,sz):*(ax,ay,az):*(tx,ty,tz):*revs:*roffset:*skew) prim,
        rest) where
    lbl = (primTypeCode.*primHoleshape.*primCut.*primHollow.*primTwist.*
       primHolesize.*primTopshear.*primAdvancedCut.*primTaper.*
       primRevolutions.*primRadiusOffset.*primSkew).primTypeInfo
updatePrimTypeRingTorusTube _ _ _ =
    badParms "PRIM_TYPE (PRIM_TYPE_RING, PRIM_TYPE_TORUS, or PRIM_TYPE_TUBE)"
updatePrimTypeSculpt prim (SVal name:IVal sculptType:rest) =
    return (setI lbl (cPrimTypeSculpt:*Just name:*sculptType) prim,rest) where
    lbl = (primTypeCode.*primSculptTexture.*primSculptType).primTypeInfo
updatePrimTypeSculpt _ _ = badParms "PRIM_TYPE (PRIM_TYPE_SCULPT)"

updatePrimTypeBoxCylPrismOld ptype prim (VVal cx cy cz:FVal hollow:FVal twisty:VVal tx ty tz:VVal sx sy sz:rest) =
    return (setI lbl (ptype:*(cx,cy,cz):*hollow:*twisty:*(tx,ty,tz):*(sx,sy,sz)) prim,
        rest) where
    lbl = (primTypeCode.*primCut.*primHollow.*(lsnd3.primTwist).*primTaper.*
        primTopshear).primTypeInfo
updatePrimTypeBoxCylPrismOld _ _ _ =
    badParms "deprecated prim type (PRIM_TYPE_BOX, PRIM_TYPE_CYLINDER, or PRIM_TYPE_PRISM)"

updatePrimTypeSphereOld prim (VVal cx cy cz:FVal hollow:VVal ax ay az:rest) =
    return (setI lbl (cPrimTypeSphere:*(cx,cy,cz):*hollow:*(ax,ay,az)) prim,rest)
    where lbl = (primTypeCode.*primCut.*primHollow.*primAdvancedCut)
              .primTypeInfo
updatePrimTypeSphereOld _ _ = badParms "deprecated prim type (PRIM_TYPE_SPHERE)"

updatePrimTypeTorusOld prim (VVal cx cy cz:FVal hollow:FVal twisty:FVal tapery:VVal sx sy sz:VVal ax ay az:rest) =
    return (setI lbl (cPrimTypeTorus:*(cx,cy,cz):*hollow:*twisty:*tapery:*
        (sx,sy,sz):*(ax,ay,az)) prim,rest)
    where lbl = (primTypeCode.*primCut.*primHollow.*(lsnd3.primTwist).*
              (lsnd3.primTaper).*primTopshear.*primAdvancedCut).primTypeInfo
updatePrimTypeTorusOld _ _ = badParms "deprecated prim type (PRIM_TYPE_TORUS)"

updatePrimTypeTubeOld prim (VVal cx cy cz:FVal hollow:FVal twisty:FVal shearx:rest) =
    return (setI (proj.primTypeInfo) (cPrimTypeTube:*(cx,cy,cz):*hollow:*twisty:*shearx) prim,rest)
    where proj = primTypeCode.*primCut.*primHollow.*(lsnd3.primTwist).*(lfst3.primTopshear)
updatePrimTypeTubeOld _ _ = badParms "deprecated prim type (PRIM_TYPE_TUBE)"

llGetObjectDetails _ [KVal k, LVal params] =
     -- TODO: this doesn't take into account multiple regions
     (getAvatarDetails k params <||> getPrimDetails k params) >>= continueL
     where getAvatarDetails k params =
               (map . avq) <$> (getM $ wav k) <*> pure params
               where avq av i | i == llcObjectName = SVal $ getI avatarName av
                              | i == llcObjectDesc = SVal ""
                              | i == llcObjectPos = vec2VVal $ getI avatarPosition av
                              | i == llcObjectRot = rot2RVal $ getI avatarRotation av
                              | i == llcObjectVelocity = VVal 0.0 0.0 0.0 -- TODO: avatar velocities
                              | i == llcObjectOwner = KVal k
                              | i == llcObjectGroup = KVal nullKey
                              | i == llcObjectCreator = KVal nullKey
                              | otherwise = llcObjectUnknownDetail
           getPrimDetails k params =
               do  prim <- getM $ wprim k
                   pos <- getObjectPosition k
                   rot <- getObjectRotation k
                   vel <-  getObjectVelocity k
                   return $ map (primq prim pos rot vel) params
               where primq prim pos rot vel i | i == llcObjectName = SVal $ getI primName prim
                                              | i == llcObjectDesc = SVal $ getI primDescription prim
                                              | i == llcObjectPos = vec2VVal pos
                                              | i == llcObjectRot = rot2RVal rot
                                              | i == llcObjectVelocity = vec2VVal vel
                                              | i == llcObjectOwner = KVal $ getI primOwner prim
                                              | i == llcObjectGroup = KVal nullKey            -- TODO: prim groups
                                              | i == llcObjectCreator = KVal $ getI primOwner prim -- TODO: prim creators
                                              | otherwise = llcObjectUnknownDetail

--------------------------------------------------------------------------------------------------------------
llAllowInventoryDrop info@(ScriptInfo _ _ _ k _) [IVal add] = do
    primAllowInventoryDrop.wprim k =: (add /= 0)
    slog info ("drop is now " ++ (if add == 0 then "not " else "") ++ "allowed")
    continueV

llAdjustSoundVolume info [FVal f] = do
    slog info ("llAdjustSoundVolume: volume " ++
        if f < 0.0 || f > 1.0 then show f ++ " out of range." else "adjusted to " ++ show f)
    continueV

-----------------------------------------------------------------------------------------------------------------
-- Parcel related functions

llSetParcelMusicURL info@(ScriptInfo _ _ _ pk _) [SVal url] =
     -- parcel music URL is a write-only value, so we won't worry about setting anything....
     do  -- make sure object owner == parcel owner
        (_,_,parcel) <- getPrimParcel pk
        owner <- getM $ primOwner.wprim pk
        when (parcelOwner parcel /= owner) $ throwError "prim doesn't have permission to modify parcel"
        slog info ("setting music url for parcel " ++ parcelName parcel ++ " to " ++ url)
        continueV

llGetParcelFlags info@(ScriptInfo _ _ _ pk _) [VVal x y z] =
    (continueI . parcelFlags . snd) =<<
        (getParcelByPosition <<= getPrimRegion pk <<= return (x,y,z))

llGetParcelDetails info@(ScriptInfo _ _ _ pk _) [VVal x y z, LVal details] =
     do  parcel <- snd <$> (getParcelByPosition =<< getPrimRegion pk) (x,y,z)
         detailList <- mapM (getDetail parcel) details
         continueL [ d | Just d <- detailList]
     where getDetail parcel (IVal detail)
               | detail == cParcelDetailsName = return $ Just $ SVal $ parcelName parcel
               | detail == cParcelDetailsDesc =  return $ Just $ SVal $ parcelDescription parcel
               | detail == cParcelDetailsOwner = return $ Just $ KVal $ parcelOwner parcel
               | detail == cParcelDetailsGroup = return $ Just $ KVal nullKey
               | detail == cParcelDetailsArea = return $ Just $ FVal $ fromIntegral $
                   let (bot,top,left,right) = parcelBoundaries parcel in (top - bot) * (right - left)
               | otherwise = slog info ("llGetParcelDetails: invalid detail flag: " ++ show detail) >> return Nothing
           getDetail _ v = slog info ("llGetParcelDetails: invalid detail: " ++ lslValString v) >> return Nothing

whenParcelPermitted info pk action= do
    (regionIndex,parcelIndex,parcel) <- getPrimParcel pk
    owner <- getM $ primOwner.wprim pk
    if parcelOwner parcel /= owner then slog info "prim not permitted to change parcel"
        else action regionIndex parcelIndex parcel >>= putParcel regionIndex parcelIndex

addToLandACLList aclType aclFromParcel aclIntoParcel info@(ScriptInfo _ _ _ pk _) [KVal ak,FVal duration] = do
    whenParcelPermitted info pk $ \ regionIndex parcelIndex parcel ->
        runAndLogIfErr ("attempt to change " ++ aclType ++ " unknown avatar " ++ unLslKey ak) parcel $ do
            getM $ wav ak
            when (duration < 0.0) $ slog info (aclType ++ " change attempted with invalid duration: " ++ show duration)
            t <- getM tick
            let acl = (ak, if duration == 0 then Nothing else Just $ t + durationToTicks duration)
            let acllist = acl : ([ b | b@(k,Just expire) <- aclFromParcel parcel,expire < t, k /= ak] ++
                                 [ b | b@(k,Nothing) <- aclFromParcel parcel, k /= ak])
            let parcel' = aclIntoParcel parcel acllist
            slog info ("added " ++ unLslKey ak ++ " to " ++ aclType ++ " list for parcel in " ++ show regionIndex)
            return parcel'
    continueV

removeFromLandACLList aclFromParcel aclIntoParcel info@(ScriptInfo _ _ _ pk _) ak =
    whenParcelPermitted info pk $ \ regionIndex parcelIndex parcel -> do
        getM $ wav ak
        let acl = aclFromParcel parcel
        return $ aclIntoParcel parcel [ ac | ac@(k,_) <- acl, k /= ak ]

llResetLandBanList info@(ScriptInfo _ _ _ pk _) [] =
    do  whenParcelPermitted info pk (\ regionIndex parcelIndex parcel -> return $ parcel { parcelBanList = [] })
        continueV
llResetLandPassList info@(ScriptInfo _ _ _ pk _) [] =
    do  whenParcelPermitted info pk  (\ regionIndex parcelIndex parcel -> return $ parcel { parcelPassList = [] })
        continueV

llRemoveFromLandPassList info [KVal ak] =
    let aclFromParcel = parcelPassList
        aclIntoParcel = (\ parcel list -> parcel { parcelPassList = list})
    in do removeFromLandACLList aclFromParcel aclIntoParcel info ak
          slog info ("llRemovFromLandPassList: removed " ++ unLslKey ak)
          continueV

llRemoveFromLandBanList info [KVal ak] =
    let aclFromParcel = parcelBanList
        aclIntoParcel = (\ parcel list -> parcel { parcelBanList = list})
    in do removeFromLandACLList aclFromParcel aclIntoParcel info ak
          slog info ("llRemovFromLandBanList: removed " ++ unLslKey ak)
          continueV

llAddToLandPassList info args =
    let aclFromParcel = parcelPassList
        aclIntoParcel = (\ parcel list -> parcel { parcelPassList = list })
    in addToLandACLList "pass" aclFromParcel aclIntoParcel info args
llAddToLandBanList info args =
    let aclFromParcel = parcelBanList
        aclIntoParcel = (\ parcel list -> parcel { parcelBanList = list })
    in addToLandACLList "ban" aclFromParcel aclIntoParcel info args

llGetLandOwnerAt info@(ScriptInfo _ _ _ pk _) [VVal x y z] =
    do  regionIndex <- getPrimRegion pk
        getParcelByPosition regionIndex (x,y,z) >>= continueK . parcelOwner . snd

llOverMyLand info@(ScriptInfo _ _ _ pk _) [KVal ak] =
    do  regionIndex <- getPrimRegion pk
        owner <- getM $ primOwner.wprim pk
        (do av <- getM $ wav ak
            if getI avatarRegion av /= regionIndex
                then slog info "llOverMyLand: avatar not in sim" >> continueI 0
                else do
                    parcel <- getParcelByPosition regionIndex (getI avatarPosition av)
                    continueI $ lslBool (owner == (parcelOwner . snd) parcel)
         ) <||> (slog info "llOverMyLand: no such avatar" >> continueI 0)

-------------------------------------------------------------------------------
-- EMAIL
llGetNextEmail info@(ScriptInfo _ _ sn pk _) [SVal addr, SVal subj] =
   do  emails <- getM $ primPendingEmails.wprim pk
       case find match emails of
           Nothing -> return ()
           Just email -> do
                pushDeferredScriptEvent (Event "email" params M.empty) pk sn 0
                (primPendingEmails.wprim pk) `modM_` (filter (not . match))
                where params = [SVal $ show $ emailTime email,
                                SVal $ emailAddress email,
                                SVal $ emailSubject email,
                                SVal $ emailMessage email,
                                IVal (length emails -1)]
       continueV
   where match email = (addr == "" || addr == emailAddress email) &&
                       (subj == "" || subj == emailSubject email)

llEmail info@(ScriptInfo _ _ _ pk _) [SVal address, SVal subject, SVal message] =
    do  let suffix = "@lsl.secondlife.com"
        let logit = slog info ("llEmail: sending email to " ++ address)
        if suffix `isSuffixOf` address
           then let potentialKey = take (length address - length suffix) address in
               do time <- getUnixTime
                  (primPendingEmails.wprim (LSLKey potentialKey)) `modM`
                      (++[Email subject address message time])
                  logit
           else logit
        continueV -- should do a yield til... (20 second delay!)
-------------------------------------------------------------------------------
soundExists pk sound = do
    sounds <- getPrimSounds pk
    case findByInvName sound sounds of
        Nothing -> do
            result <- findAsset sound
            case result of
                Nothing -> return False
                Just v -> return $ isSoundAsset v
        Just _ -> return True

llTriggerSound info@(ScriptInfo _ _ _ pk _) [SVal sound, FVal volume] =
    do  found <- soundExists pk sound
        let message =
                (if found then "llTriggerSound: triggering sound "
                         else "llTriggerSound: sound not found - ") ++ sound
        slog info message
        continueV

onSound info@(ScriptInfo _ _ _ pk _) fn msg sound = do
    found <- soundExists pk sound
    slog info (fn ++ ":" ++ (if found then msg else "sound not found -") ++
        " " ++ sound)
    continueV

llTriggerSoundLimited info [SVal sound, FVal volume, VVal t n e, VVal b s w] =
    do  when (t <= b || n <= s || e <= w) $ throwError
            "llTriggerSoundLimited: bounding box has zero or negative volume"
        onSound info "llTriggerSoundLimited" "triggering sound" sound

llSound info [SVal sound, FVal vol, IVal q, IVal loop] =
    onSound info "llSound (deprecated):" "playing sound " sound

llPlaySound info [SVal sound, FVal vol] =
    onSound info "llPlaySound" "playing sound"  sound

llCollisionSound info [SVal sound, FVal vol] =
    onSound info "llCollisionSound" "setting collision sound to" sound

llPreloadSound info [SVal sound] =
    onSound info "llPreloadSound" "loading sound" sound

llSoundPreload info [SVal sound] =
    onSound info "llSoundPreload" "loading sound" sound

llPlaySoundSlave info [SVal sound, FVal vol] =
    onSound info "llPlaySoundSlave" "playing sound" sound

llLoopSoundSlave info [SVal sound, FVal vol] =
    onSound info "llLoopSoundSlave" "playing sound" sound

llLoopSound info [SVal sound, FVal vol] =
    onSound info "llLoopSound" "playing sound" sound

llLoopSoundMaster info [SVal sound, FVal vol] =
    onSound info "llLoopSoundMaster" "playing sound" sound

llStopSound info [] = slog info "llStopSound: stopping sound" >> continueV
llSetSoundRadius info [FVal radius] =
    slog info "llSetSoundRadius: called. This LL function has no effect." >>
        continueV
llSetSoundQueueing info [IVal bool] =
    slog info ("llSetSoundQueuiing: set to " ++
        (if bool /= 0 then "TRUE" else "FALSE")) >> continueV

------------------------------------------------------------------------------
llCollisionSprite info@(ScriptInfo _ _ _ pk _) [SVal impactSprite] =
    do  tk <- findTexture pk impactSprite
        slog info ("llCollisionSprite: set sprite to " ++ unLslKey tk)
        continueV

llGetKey (ScriptInfo _ _ _ pk _) [] = continueK pk

llGetOwnerKey info@(ScriptInfo _ _ _ pk _) [KVal k] =
   (do  regionIndex <- getPrimRegion pk
        mRegionIndex <- Just <$> (getPrimRegion k) <||> return Nothing
        key <- case mRegionIndex of
            Nothing ->
                slog info "llGetOwnerKey: object key not found" >> return k
            Just regionIndex' | regionIndex /= regionIndex' ->
                slog info "llGetOwnerKey: object in different simulator" >> return k
                              | otherwise -> getM $ primOwner.wprim k
        continueK key) <||>
   (do (getM $ wav k) <||> throwError "no such key"
       continueK k)

llGetLinkNumber (ScriptInfo oid pid _ pk _) [] =
     if pid /= 0 then continueI (pid + 1) else
         do  links <- getM $ primKeys.lm oid.wobjects
             continueI (if length links == 1 then 0 else 1)

llGetUnixTime (ScriptInfo _ _ _ _ _) [] = getUnixTime >>= continueI

-- UTC timestamp
llGetTimestamp (ScriptInfo _ _ _ _ _) [] =
    getUTCTimeOfDay >>=
    continueS . formatTime defaultTimeLocale "%04d-%02d-%02dT%02d:%02d:%02.6f"

-- UTC date
llGetDate (ScriptInfo _ _ _ _ _) [] = getUTCDate >>= continueS

-- seconds since midnight GMT / UTC
llGetGMTclock (ScriptInfo _ _ _ _ _) [] =
    getUTCTimeOfDay >>=
    continueI . floor . utctDayTime

-- seconds since midnight Pacific
llGetWallclock (ScriptInfo _ _ _ _ _) [] =
    getUTCTimeOfDay >>=
    continueI . floor . timeOfDayToTime . localTimeOfDay . utcToLocalTime llabTZ
  where
    llabTZ = hoursToTimeZone (-8)

llGetTimeOfDay (ScriptInfo _ _ _ _ _) [] =
    continueF . ticksToDuration . flip mod (durationToTicks 4.0) =<< getM tick

getUnixTime :: Monad m => WorldE m Int
getUnixTime = fromIntegral <$> (liftM2 (+) (getM worldZeroTime) (liftM (floor . ticksToDuration) $ getM tick))

getUTCTimeOfDay :: Monad m => WorldE m UTCTime
getUTCTimeOfDay = posixSecondsToUTCTime . fromIntegral <$> getUnixTime

getUTCDate :: Monad a => WorldE a String
getUTCDate = formatTime defaultTimeLocale "%04d-%02d-%02d" <$> getUTCTimeOfDay

llGetRegionFPS _ _ = continueF 45.0
llGetRegionTimeDilation _ _ = continueF 1.0

llGetTime (ScriptInfo _ _ sid pk _) [] =
     do  t <- getM tick
         reset <- getM $ scriptLastResetTick.lscript pk sid
         continueF $ ticksToDuration (t - reset)

getAndResetTick pk sid = do
    t:*t' <- getM $ (scriptLastResetTick.lscript pk sid.*tick)
    scriptLastResetTick.lscript pk sid =: t'
    return $ t' - t

llGetAndResetTime (ScriptInfo _ _ sid pk _) [] =
    getAndResetTick pk sid >>= continueF . ticksToDuration

llResetTime (ScriptInfo _ _ sid pk _) [] =
    getAndResetTick pk sid >> continueV

llSetText info [SVal text, VVal r g b, FVal alpha] = setText "llSetText" 254 info text
llSetSitText info [SVal text] = setText "llSetSitText" 9 info text
llSetTouchText info [SVal text] = setText "llSetTouchText" 9 info text

setText func lim info text =
    do slog info (func ++ ": setting text to " ++ text)
       when (length text > lim) $ slog info (func ++ ": text exceeds " ++ show lim ++ " character limit")
       continueV

llGetScriptName (ScriptInfo _ _ sid _ _) [] = continueS sid

llGetNumberOfNotecardLines (ScriptInfo _ _ sn pk _) [SVal name] =
    do  notecards <- getPrimNotecards pk
        case find ((name==) . inventoryItemName) notecards of
            Nothing -> putChat sayRange pk 0 ("Couldn't find notecard " ++ name) >> continueK nullKey
            Just notecard -> do
                key <- newKey
                pushDataserverEvent pk sn key (show $ length $ invNotecardLines $ inventoryItemData notecard)
                continueK key

llGetNotecardLine info@(ScriptInfo _ _ sn pk _) [SVal name, IVal lineNumber] =
    do  notecards <- getPrimNotecards pk
        case find ((name==) . inventoryItemName) notecards of
            Nothing -> sayErr info ("Couldn't find notecard " ++ name) >> continueK nullKey
            Just notecard -> do
                key <- newKey
                pushDataserverEvent pk sn key $ maybe cEOF (take 255) $
                    lookupByIndex lineNumber $ invNotecardLines $ inventoryItemData notecard
                continueK key

llRequestInventoryData info@(ScriptInfo _ _ sn pk _) [SVal name] =
    do landmarks <- getPrimLandmarks pk
       landmark <- maybe (throwError ("no landmark named " ++ name)) return (findByInvName name landmarks)
       let (_,v) = invLandmarkLocation $ inventoryItemData landmark
       t <- getM tick
       key <- newKey
       pushDataserverEvent pk sn key (lslValString (vec2VVal v))
       yieldWith (t + durationToTicks 1) (KVal key)

llGetLinkName (ScriptInfo oid _ _ _ _) [IVal linkNumber] =
    do  links <- getM $ primKeys.lm oid.wobjects
        name <- if null links
           then if linkNumber == 0
               then getM $ primName.wprim oid
               else return $ unLslKey nullKey
           else case lookupByIndex (linkNumber - 1) links of
                Nothing -> return $ unLslKey nullKey
                Just k -> getM $ primName.wprim k
        continueS name

llGetStatus (ScriptInfo _ _ _ pk _) [IVal check] =
    do  status <- getM $ primStatus.wprim pk
        continueI $ case mssb check of
            Nothing -> 0
            Just b -> lslBool (bit b .&. status /= 0)
    where mssb i = foldl' (\ x y -> x `mplus` (if testBit i y then Just y else Nothing)) Nothing [31,30..0]

llSetStatus info@(ScriptInfo _ _ _ pk _) [IVal mask, IVal val] =
    do  status <- getM $ primStatus.wprim pk
        let status' = if val == 0
                          then status .&. complement mask
                          else status .|. mask
        primStatus.wprim pk =: status'
        continueV

llPassTouches info@(ScriptInfo _ _ _ pk _) [IVal val] =
   primPassTouches.wprim pk =: (val /= 0) >> continueV

llPassCollisions info@(ScriptInfo _ _ _ pk _) [IVal val] =
   primPassCollisions.wprim pk =: (val /= 0) >> continueV

llGetRegionCorner (ScriptInfo _ _ _ pk _) [] =
    do (x,y) <- getPrimRegion pk
       continueVec (256 * fromIntegral x, 256 * fromIntegral y, 0)

llGetRegionFlags info@(ScriptInfo _ _ _ pk _) [] =
    regionFlags <$> (getPrimRegion pk >>= \ i -> getM $ lm i.worldRegions) >>= continueI

llGetSimulatorHostname (ScriptInfo _ _ _ pk _) [] =
    do (i,j) <- getPrimRegion pk
       continueS (show i ++ "x" ++ show j ++ ".example.com")

llGetRegionName (ScriptInfo _ _ _ pk _) [] =
    continueS =<< (liftM regionName $
        mlookup <<= getPrimRegion pk <<= getM worldRegions)

llGetAgentSize info@(ScriptInfo _ _ _ pk _) [KVal ak] =
    withAvatar ak notFound =<< found <$> getPrimRegion pk
    where notFound = slog info ("llGetAgentSize: no such agent - " ++ unLslKey ak) >> continueVec (0,0,0)
          found region av =
              if getI avatarRegion av == region
                   then continueVec (0.45,0.6,getI avatarHeight av)
                   else slog info "llGetAgentSize: agent not in sim" >> continueVec (0,0,0)

llGetAgentInfo info@(ScriptInfo _ _ _ pk _) [KVal ak] =
    withAvatar ak notFound =<< found <$> getPrimRegion pk
    where found region av =
              if getI avatarRegion av == region
                  then continueI $ getI avatarState av
                  else slog info "llGetAgentInfo: agent not in sim" >> continueI 0
          notFound = slog info ("llGetAgentInfo: no such agent - " ++ unLslKey ak) >> continueI 0

llGetAgentLanguage info@(ScriptInfo _ _ _ pk _) [KVal ak] = do
    withAvatar ak notFound =<< found <$> getPrimRegion pk
    where found region av = continueS $
              if getI avatarRegion av == region then "hu" else ""
          notFound = slog info ("llGetAgentLanguage: no such agent - " ++ unLslKey ak)
              >> continueI 0

-- llDetected* functions ------------------------------------------------------
-- TODO: only setup for 1 detected thing at a time...
-- This is a problem especially for sensor testing

klook i = mlookup ("key_" ++ show i) . eventInfo
klookm i = maybe (throwError "nothing detected") (klook i)
ilook i = mlookup ("integer_" ++ show i) . eventInfo
ilookm i = maybe (throwError "nothing detected") (ilook i)

look s i = mlookup (s ++ "_" ++ show i) . eventInfo
lookm s i = maybe (throwError "nothing detected") (look s i)

llDetectedKey (ScriptInfo _ _ _ pk mevent) [IVal i] =
    continueWith =<< (klookm i mevent <||> return (KVal nullKey))

llDetectedLinkNumber (ScriptInfo _ _ _ pk mevent) [IVal i] =
    continueWith =<< (ilookm i mevent <||> return (IVal 0))

llDetectedGrab (ScriptInfo _ _ _ _ mevent) [IVal i] =
    continueWith $ fromMaybe (VVal 0 0 0) (mevent >>= M.lookup ("vector_" ++ show i) . eventInfo)

llDetectedPos (ScriptInfo _ _ _ _ mevent) [IVal i] =
    do  KVal key <- klookm i mevent
        withAvatar key (getRootPrim key >>= getObjectPosition >>= continueVec)
                (continueVec . getI avatarPosition)
llDetectedRot (ScriptInfo _ _ _ _ mevent) [IVal i] =
    do  KVal key <- klookm i mevent
        withAvatar key (getRootPrim key >>= getObjectRotation >>= continueRot)
            (continueRot . getI avatarRotation)
llDetectedName (ScriptInfo _ _ _ _ mevent) [IVal i] =
    do  KVal key <- klookm i mevent
        continueS =<< withAvatar key (getM $ primName.wprim key) (return . getI avatarName)

llDetectedOwner (ScriptInfo _ _ _ _ mevent) [IVal i] =
    do  KVal key <- klookm i mevent
        continueK =<< withAvatar key (getM $ primOwner.wprim key) (return . const key)

llDetectedGroup info@(ScriptInfo _ _ _ _ mevent) [IVal i] =
    do  KVal key <- klookm i mevent
        avatars <- getM worldAvatars
        prims <- getM wprims
        continueK =<< ((fromMaybe nullKey . getI avatarActiveGroup) <$> (mlookup key avatars) <||>
             (fromMaybe nullKey . getI primGroup) <$> (mlookup key prims) <||>
             throwError "error: key not an avatar or an object!")

llDetectedVel _ [IVal i] = continueVec (0,0,0) -- TODO: physics!!!

llDetectedType (ScriptInfo _ _ _ _ mevent) [IVal i] =
    do  KVal key <- klookm i mevent
        continueI =<< withAvatar key (determineActivePassiveScripted key)
            (return . const cAgent)

llDetectedTouchFace (ScriptInfo _ _ _ _ mevent) [IVal i] =
   continueWith =<< lookm "face" i mevent

llDetectedTouchST (ScriptInfo _ _ _ _ mevent) [IVal i] =
    continueWith =<< lookm "st" i mevent

-- hack! TODO (maybe)! compute a real UV
-- I think the following is true iff the texture rotation is
-- zero, the texture repeats are (1.0,1.0) and the offsets
-- are (0.0,0.0).  I'm sure the calculation is something
-- like (cos(rot) * s * rx * ox , sin(rot) * t * ry * oy)
-- but would need to test...
llDetectedTouchUV = llDetectedTouchST

llDetectedTouchPos (ScriptInfo oid _ _ _ mevent) [IVal i] =
    do IVal link <- ilookm i mevent
       IVal face <- lookm "face" i mevent <||> return (IVal (-1))
       -- hack: need to, based on the face and the position, orientation
       -- and various prim parameters, calculate the touch position.
       -- but that's hard.  So we'll just return the position of the link.
       continueVec =<< if face < 0 then return (0,0,0)
                                   else getGlobalPos =<< getLinkKey oid link

-- TODO: make this correct!
llDetectedTouchNormal (ScriptInfo _ _ _ _ mevent) [IVal i] =
    do lookm "face" i mevent -- make sure the index is valid
       continueVec (0,0,1) -- return an arbitrary vector

-- TODO: make this correct!
llDetectedTouchBinormal (ScriptInfo _ _ _ _ mevent) [IVal i] =
    do lookm "face" i mevent -- make sure the index is valid
       continueVec (0,1,0) -- return an arbitrary vector

determineActivePassiveScripted key = do
    status <- getM $ primStatus.wprim key
    case status of
       st | st .&. cStatusPhysics /= 0 -> return cActive
          | otherwise -> do
              scripts <- getPrimScripts key
              return (if length scripts > 0 then cScripted else cPassive)

-------------------------------------------------------------------------------
-- animation functions

llGetAnimationList info@(ScriptInfo _ _ _ pk _) [KVal ak] =
    withAvatar ak notFound =<< found <$> getPrimRegion pk
    where found rp av | getI avatarRegion av /= rp =
                          slog info "llGetAnimationList: avatar not in sim" >> continueL []
                      | otherwise = do
                          now <- getM tick
                          continueL [ KVal anim | (t,anim) <- getI avatarActiveAnimations av, maybe True (<now) t]
          notFound = slog info ("llGetAnimationList: no such avatar: " ++ unLslKey ak) >> continueL []

llGetAnimation info@(ScriptInfo _ _ _ pk _) [KVal ak] =
    withAvatar ak notFound =<< found <$> getPrimRegion pk
    where found rp av | getI avatarRegion av /= rp =
            slog info "llGetAnimationList: avatar no in sim" >> continueS ""
                      | otherwise = continueS "Standing" -- TODO: real animation state
          notFound = slog info ("llGetAnimation: no such avatar: " ++ unLslKey ak) >> continueS ""

llStartAnimation info@(ScriptInfo _ _ sid pk _) [SVal anim] = void $ do
    avKey <- getPermittedAvKey info "llStartAnimation" cPermissionTriggerAnimation
    case avKey of
        Nothing -> return ()
        Just ak -> do
            anims:*now <- getM $ avatarActiveAnimations.wav ak.*tick
            result <- findAnimation pk anim
            case result of
                Just (key,mduration) -> avatarActiveAnimations.wav ak =: anims'
                    where anims' = updateAnims anims now mduration key
                Nothing -> slog info ("llStartAnimation: animation not found - " ++ anim)
            where updateAnims anims now mduration key =
                      (expire,key):[(t,k) | (t,k) <- anims, maybe True (<now) t && key /= k]
                          where expire = fmap ((now+) . durationToTicks) mduration

findAnimation pk anim = do
    primAnims <- getPrimAnimations pk
    case find (\ item -> inventoryItemName item == anim) primAnims of
        Just item -> return $ Just
            (snd $ inventoryItemNameKey $ inventoryItemIdentification item,
                invAnimationDuration $ inventoryItemData item)
        Nothing ->
          case find (\ (name,_,_) -> name == anim) builtInAnimations of
              Just (_,key,mduration) -> return $ Just (LSLKey key,mduration)
              Nothing -> return Nothing

llStopAnimation info@(ScriptInfo _ _ sid pk _) [SVal anim] = void $ do
    avKey <- getPermittedAvKey info "llStartAnimation" cPermissionTriggerAnimation
    case avKey of
        Nothing -> return ()
        Just ak -> do
            anims <- getM $ avatarActiveAnimations.wav ak
            result <- findAnimation pk anim
            case result of
                Just (key,_) ->
                   case find (\ (_,k) -> k == key) anims of
                       Nothing -> slog info "llStopAnimation: animation not active"
                       Just _ -> avatarActiveAnimations.wav ak =:  anims'
                           where anims' = [(expire,k) | (expire,k) <- anims, k /= key]
                Nothing -> slog info ("llStopAnimation: animation not found - " ++ anim)

llSetClickAction info [IVal action] = void $
   (if action `elem` cClickActions
       then slog info "llSetClickAction: setting click action"
       else slog info "llSetClickAction: invalid click action")
-------------------------------------------------------------------------------
-- XML RPC related functions
llCloseRemoteDataChannel info@(ScriptInfo _ _ sn pk _) [KVal key] =
   do  -- according to LSL wiki this function has no effect,
       -- so we'll just log a message indicating whether or not the passed in key is valid
       lookupScriptFromChan key <||> throwError "key not an open channel"
       continueV

llOpenRemoteDataChannel info@(ScriptInfo _ _ sn pk _) [] =
    do key <- lookupDataChannel (pk,sn) <||> newKey
       insertScriptChannelPair (pk,sn) key
       slog info "pushing remote data channel to script"
       pushDeferredScriptEvent (Event "remote_data" [llcRemoteDataChannel,
           KVal key, KVal nullKey, SVal "", IVal 0, SVal ""] M.empty) pk sn 0
       continueV

llSendRemoteData info [KVal channel, SVal dest, IVal idata, SVal sdata] =
    do  -- according to LSL Wiki, this function doesn't do anything... but we'll check
        -- for a valid channel and log a message.
        lookupScriptFromChan channel <||> throwError "key not an open channel"
        newKey >>= continueK

llRemoteDataReply info@(ScriptInfo _ _ _ _ mevent)
    [KVal channel, KVal messageId, SVal sdata, IVal idata] = void $ do
    event <- maybe (throwError "no data event") return mevent
    unless (eventName event == "remote_data") $
        throwError "no effect unless called within remote_data handler"
    let m = eventInfo event
    (KVal k) <- mlookup "requestKey" m <||>
        throwError "problem: invalid or mistyped request key in event"
    putWorldEvent 0 (XMLReplyEvent k channel messageId sdata idata)
    slog info $
        concat ["llRemoteDataReply: (",
            show channel,",",show messageId,",",show sdata,",",show idata,")"]

llRemoteDataSetRegion info [] = void $
    slog info "llRemoteDataSetRegion: this function has no effect"
-------------------------------------------------------------------------------

llDialog info@(ScriptInfo _ _ _ pk _) [KVal ak, SVal message, LVal buttons, IVal channel] =
    do  when (length message > 512) $
            doErr "message too long, must be less than 512 characters"
        when (null message) $
            doErr "must supply a message"
        unless (all (==LLString) $ map typeOfLSLValue buttons) $
            doErr "button list must contain only strings"
        let buttons' = take 12 $ map ( \ (SVal s) -> s) buttons
        when (any ((24 <) . length) buttons') $
            doErr "Button Labels cannot have more that 24 characters"
        when (any null buttons') $ doErr "all buttons must have label strings"
        (getM $ wav ak) <||> throwError ("no such agent/avatar - " ++ unLslKey ak)
        putWorldEvent 0 (DialogEvent ak message buttons' channel pk) -- deprecated!
        putWorldEvent 0
            (AvatarInputEvent ak (AvatarDialog message buttons' channel pk))
        continueV
    where doErr msg = sayErr info ("llDialog: " ++ msg) >> throwError msg
-------------------------------------------------------------------------------
-- old (deprecated) functions for visual effects

llMakeExplosion info _ = void $ slog info "llMakeExplosion: deprecated"
llMakeSmoke info _ = void $ slog info "llMakeSmoke: deprecated"
llMakeFire info _ = void $ slog info "llMakeFire: deprecated"
llMakeFountain info _ = void $ slog info "llMakeFountain: deprecated"

--------------------------------------------------------------------------------
llTakeControls info@(ScriptInfo _ _ sn pk _)
    [IVal controls, IVal accept, IVal pass] = void $ do
    lastPerm :* scperms <- getM $ (scriptLastPerm .* scriptPermissions).lscript pk sn
    case lastPerm of
        Nothing -> slog info "no persmission to take controls from any agent"
        Just ak -> do
            av <- getM $ wav ak
            maybe (noperm av)
                (\ perms -> if perms .&. cPermissionTakeControls /= 0
                    then when (accept /= 0) $
                        (avatarControlListener.wav ak =: Just acl )
                    else noperm av)
                (M.lookup ak $ scperms)
            where acl = AvatarControlListener {
                      avatarControlListenerMask = controls,
                      avatarControlListenerScript = (pk,sn) }
                  noperm = throwError .
                      ("no permission to take controls from " ++) .
                      getI avatarName

llReleaseControls (ScriptInfo _ _ sn pk _) [] = void $ do
    ctrls <- getM $ scriptControls.lscript pk sn
    forM_ ctrls $ \ ak -> do
        av <- getM $ wav ak
        whenJust (getI avatarControlListener av) $ \ acl ->
            when (avatarControlListenerScript acl == (pk,sn)) $
                (avatarControlListener.wav ak =: Nothing)
    (scriptPermissions.lscript pk sn) `modM_` M.map resetPerm
    where resetPerm i = complement cPermissionTakeControls .&. i
--------------------------------------------------------------------------------

llVolumeDetect info@(ScriptInfo { scriptInfoObjectKey = oid }) [IVal i] =
    void $ objectVolumeDetect.dyn oid =: (i /= 0)

llCollisionFilter (ScriptInfo _ _ sn pk _) [SVal name, KVal k, IVal i] =
    void $ scriptCollisionFilter.lscript pk sn =: (name,k, i /= 0)

llTarget (ScriptInfo _ _ sn pk _) [VVal x y z, FVal range] = do
    index <- (scriptTargetIndex.lscript pk sn) `modM` (+1)
    lmi index.scriptPositionTargets.lscript pk sn =: ((x,y,z),range)
    continueI index

llTargetRemove (ScriptInfo _ _ sn pk _) [IVal tnumber] =
    void $ (scriptPositionTargets.lscript pk sn) `modM_` IM.delete tnumber

llRotTarget (ScriptInfo _ _ sn pk _) [RVal x y z s, FVal err] = void $ do
    index <- (scriptTargetIndex.lscript pk sn) `modM` (+1)
    lmi index.scriptRotationTargets.lscript pk sn =: ((x,y,z,s),err)

llRotTargetRemove (ScriptInfo _ _ sn pk _) [IVal tnumber] = void $
    (scriptRotationTargets.lscript pk sn) `modM_` IM.delete tnumber

--------------------------------------------------------------------------------
llGround info [VVal _ _ _] = continueF 0 -- the world is flat!
llGroundContour info [VVal _ _ _] = continueVec (1,0,0) -- could be any vector
llGroundNormal info [VVal _ _ _] = continueVec (0,0,1) -- straight up!
llGroundSlope info [VVal _ _ _] = continueVec (0,1,0)
--------------------------------------------------------------------------------
llGetSunDirection info [] = do
    az <- ((*(pi/7200)) . ticksToDuration) <$> getM tick
    let el = 80 * pi
    continueVec (sin az * cos el, sin el, cos az * cos el)

--------------------------------------------------------------------------------
llLoadURL info [KVal ak, SVal msg, SVal url] = do
    getM $ wav ak
    putWorldEvent 0 (AvatarInputEvent ak (AvatarLoadURL msg url))
    continueV

llMapDestination info [SVal simName, (VVal x y z), (VVal _ _ _)] = do
    case mevent of
       Nothing -> do
           ak <- getM (attachmentKey.primAttachment'.wprim oid) <??>
               "prim neither attached nor touched, so call to llMapDestination is not valid"
           putIt ak
       Just (Event "touch" _ m) ->
           case M.lookup "key_0" m of
               Just (SVal ak) -> putIt $ LSLKey ak
               _ -> throwError "invalid touch event!"
       Just (Event nm _ _) -> throwError $ "invalid event for llMapDestination: " ++ nm
    continueV
    where oid = scriptInfoObjectKey info
          mevent = scriptInfoCurrentEvent info
          putIt ak = putWorldEvent 0 (AvatarInputEvent ak (AvatarMapDestination simName (x,y,z)))
--------------------------------------------------------------------------------
llTargetOmega info [VVal x y z,FVal spinrate, FVal gain] = void $
    slog info $ "spinrate set to: " ++ show spinrate ++ " with gain: " ++ show gain ++
        " around axis: " ++ show (x,y,z)
--------------------------------------------------------------------------------
continueWith val = return (EvalIncomplete,val)
continueVec = continueWith . vec2VVal
continueF = continueWith . FVal
continueI = continueWith . IVal
continueRot = continueWith . rot2RVal
continueS = continueWith . SVal
continueK = continueWith . KVal
continueL = continueWith . LVal
continueV = continueWith VoidVal
void = (>> continueV)

yieldWith t v = return (YieldTil t,v)
yieldV t = yieldWith t VoidVal

doneWith v = return (EvalComplete Nothing,v)
doneV = doneWith VoidVal

defaultPredefs :: Monad m => M.Map String (PredefFunc m)
defaultPredefs = M.fromList $ map (\(x,y) -> (x, defaultPredef x y)) [
        ("llAddToLandBanList",llAddToLandBanList),
        ("llAddToLandPassList",llAddToLandPassList),
        ("llAdjustSoundVolume",llAdjustSoundVolume),
        ("llAllowInventoryDrop",llAllowInventoryDrop),
        ("llApplyImpulse",llApplyImpulse),
        ("llApplyRotationalImpulse", llApplyRotationalImpulse),
        ("llAttachToAvatar",llAttachToAvatar),
        ("llAvatarOnSitTarget",llAvatarOnSitTarget),
        ("llBreakAllLinks",llBreakAllLinks),
        ("llBreakLink",llBreakLink),
        ("llClearCameraParams",llClearCameraParams),
        ("llCloseRemoteDataChannel",llCloseRemoteDataChannel),
        ("llCloud",llCloud),
        ("llCollisionFilter", llCollisionFilter),
        ("llCollisionSound",llCollisionSound),
        ("llCollisionSprite",llCollisionSprite),
        ("llCreateLink",llCreateLink),
        ("llDetachFromAvatar", llDetachFromAvatar),
        ("llDetectedTouchBinormal",llDetectedTouchBinormal),
        ("llDetectedTouchFace",llDetectedTouchFace),
        ("llDetectedTouchNormal",llDetectedTouchNormal),
        ("llDetectedTouchPos",llDetectedTouchPos),
        ("llDetectedTouchST",llDetectedTouchST),
        ("llDetectedTouchUV",llDetectedTouchUV),
        ("llDetectedGrab",llDetectedGrab),
        ("llDetectedGroup",llDetectedGroup),
        ("llDetectedKey", llDetectedKey),
        ("llDetectedLinkNumber", llDetectedLinkNumber),
        ("llDetectedName", llDetectedName),
        ("llDetectedOwner", llDetectedOwner),
        ("llDetectedPos", llDetectedPos),
        ("llDetectedRot", llDetectedRot),
        ("llDetectedType", llDetectedType),
        ("llDetectedVel", llDetectedVel),
        ("llDialog", llDialog),
        ("llDie",llDie),
        ("llEdgeOfWorld", const . const $ continueI 0),
        ("llEjectFromLand",llEjectFromLand),
        ("llEmail",llEmail),
        ("llFrand",llFrand),
        ("llGetAccel",llGetAccel),
        ("llGetAgentInfo",llGetAgentInfo),
        ("llGetAgentLanguage",llGetAgentLanguage),
        ("llGetAgentSize",llGetAgentSize),
        ("llGetAlpha",llGetAlpha),
        ("llGetAndResetTime", llGetAndResetTime),
        ("llGetAnimation", llGetAnimation),
        ("llGetAnimationList",llGetAnimationList),
        ("llGetAttached",llGetAttached),
        ("llGetBoundingBox",llGetBoundingBox),
        ("llGetCameraPos",llGetCameraPos),
        ("llGetCameraRot",llGetCameraRot),
        ("llGetColor",llGetColor),
        ("llGetCreator",llGetCreator),
        ("llGetDate",llGetDate),
        ("llGetForce", llGetForce),
        ("llGetFreeURLs", llGetFreeURLs),
        ("llGetGeometricCenter",llGetGeometricCenter),
        ("llGetGMTclock",llGetGMTclock),
        ("llGetHTTPHeader",llGetHTTPHeader),
        ("llGetInventoryCreator",llGetInventoryCreator),
        ("llGetInventoryKey",llGetInventoryKey),
        ("llGetInventoryName",llGetInventoryName),
        ("llGetInventoryNumber",llGetInventoryNumber),
        ("llGetInventoryPermMask", llGetInventoryPermMask),
        ("llGetInventoryType",llGetInventoryType),
        ("llGetKey",llGetKey),
        ("llGetLocalPos",llGetLocalPos),
        ("llGetNextEmail",llGetNextEmail),
        ("llGetNumberOfPrims",llGetNumberOfPrims),
        ("llGetNumberOfSides",llGetNumberOfSides),
        ("llGetLinkKey",llGetLinkKey),
        ("llGetLinkName",llGetLinkName),
        ("llGetLinkNumber", llGetLinkNumber),
        ("llGetLocalRot", llGetLocalRot),
        ("llGetLandOwnerAt", llGetLandOwnerAt),
        ("llGetMass",llGetMass),
        ("llGetNotecardLine", llGetNotecardLine),
        ("llGetNumberOfNotecardLines",llGetNumberOfNotecardLines),
        ("llGetObjectDesc", llGetObjectDesc),
        ("llGetObjectDetails", llGetObjectDetails),
        ("llGetObjectMass", llGetObjectMass),
        ("llGetObjectName", llGetObjectName),
        ("llGetObjectPermMask", llGetObjectPermMask),
        ("llGetObjectPrimCount",llGetObjectPrimCount),
        ("llGetOmega",llGetOmega),
        ("llGetOwner", llGetOwner),
        ("llGetOwnerKey",llGetOwnerKey),
        ("llGetParcelFlags", llGetParcelFlags),
        ("llGetParcelDetails", llGetParcelDetails),
        ("llGetPermissions",llGetPermissions),
        ("llGetPermissionsKey",llGetPermissionsKey),
        ("llGetPos", llGetPos),
        ("llGetPrimitiveParams", llGetPrimitiveParams),
        ("llGetRegionAgentCount", llGetRegionAgentCount),
        ("llGetRegionCorner", llGetRegionCorner),
        ("llGetRegionFlags", llGetRegionFlags),
        ("llGetRegionFPS", llGetRegionFPS),
        ("llGetRegionName", llGetRegionName),
        ("llGetRegionTimeDilation", llGetRegionTimeDilation),
        ("llGetRot",llGetRot),
        ("llGetRootPosition",llGetRootPosition),
        ("llGetRootRotation",llGetRootRotation),
        ("llGetScale", llGetScale),
        ("llGetScriptName", llGetScriptName),
        ("llGetScriptState", llGetScriptState),
        ("llGetSimulatorHostname", llGetSimulatorHostname),
        ("llGetStartParameter",llGetStartParameter),
        ("llGetStatus",llGetStatus),
        ("llGetSunDirection",llGetSunDirection),
        ("llGetTexture", llGetTexture),
        ("llGetTextureOffset", llGetTextureOffset),
        ("llGetTextureRot", llGetTextureRot),
        ("llGetTextureScale", llGetTextureScale),
        ("llGetTime",llGetTime),
        ("llGetTimeOfDay",llGetTimeOfDay),
        ("llGetTimestamp",llGetTimestamp),
        ("llGetTorque",llGetTorque),
        ("llGetUnixTime",llGetUnixTime),
        ("llGetWallclock",llGetWallclock),
        ("llGetVel",llGetVel),
        ("llGiveInventory", llGiveInventory),
        ("llGiveInventoryList", llGiveInventoryList),
        ("llGiveMoney",llGiveMoney),
        ("llGround", llGround),
        ("llGroundContour", llGroundContour),
        ("llGroundNormal", llGroundNormal),
        ("llGroundRepel",llGroundRepel),
        ("llGroundSlope",llGroundSlope),
        ("llHTTPRequest",llHTTPRequest),
        ("llHTTPResponse",llHTTPResponse),
        ("llInstantMessage",llInstantMessage),
        ("llKey2Name", llKey2Name),
        ("llListRandomize",llListRandomize),
        ("llListen", llListen),
        ("llListenControl",llListenControl),
        ("llListenRemove",llListenRemove),
        ("llLoadURL",llLoadURL),
        ("llLookAt", llLookAt),
        ("llLoopSound",llLoopSound),
        ("llLoopSoundSlave",llLoopSoundSlave),
        ("llLoopSoundMaster", llLoopSoundMaster),
        ("llMakeExplosion",llMakeExplosion),
        ("llMakeFire",llMakeFire),
        ("llMakeFountain",llMakeFountain),
        ("llMakeSmoke",llMakeSmoke),
        ("llMapDestination", llMapDestination),
        ("llMessageLinked", llMessageLinked),
        ("llMoveToTarget", llMoveToTarget),
        ("llOffsetTexture", llOffsetTexture),
        ("llOpenRemoteDataChannel", llOpenRemoteDataChannel),
        ("llOverMyLand", llOverMyLand),
        ("llOwnerSay", llOwnerSay),
        ("llPassCollisions", llPassCollisions),
        ("llPassTouches", llPassTouches),
        ("llPlaySound", llPlaySound),
        ("llPlaySoundSlave", llPlaySoundSlave),
        ("llPointAt",llPointAt),
        ("llPreloadSound", llPreloadSound),
        ("llRefreshPrimURL", llRefreshPrimURL),
        ("llRegionSay", llRegionSay),
        ("llReleaseCamera", llReleaseCamera),
        ("llReleaseControls", llReleaseControls),
        ("llReleaseURL",llReleaseURL),
        ("llRemoteDataReply", llRemoteDataReply),
        ("llRemoteDataSetRegion", llRemoteDataSetRegion),
        ("llRemoteLoadScript", llRemoteLoadScript),
        ("llRemoveFromLandPassList",llRemoveFromLandPassList),
        ("llRemoveFromLandBanList",llRemoveFromLandBanList),
        ("llRemoveInventory",llRemoveInventory),
        ("llRemoveVehicleFlags",llRemoveVehicleFlags),
        ("llRequestAgentData", llRequestAgentData),
        ("llRequestInventoryData",llRequestInventoryData),
        ("llRequestPermissions",llRequestPermissions),
        ("llRequestSecureURL",reqURL True),
        ("llRequestSimulatorData",llRequestSimulatorData),
        ("llRequestURL",reqURL False),
        ("llResetLandPassList",llResetLandPassList),
        ("llResetLandBanList",llResetLandBanList),
        ("llResetOtherScript",llResetOtherScript),
        ("llResetScript",llResetScript),
        ("llResetTime",llResetTime),
        ("llRezAtRoot",llRezAtRoot),
        ("llRezObject",llRezObject),
        ("llRotLookAt",llRotLookAt),
        ("llRotTarget",llRotTarget),
        ("llRotTargetRemove",llRotTargetRemove),
        ("llRotateTexture", llRotateTexture),
        ("llSameGroup", llSameGroup),
        ("llSay",llSay),
        ("llScaleTexture", llScaleTexture),
        ("llSensor",llSensor),
        ("llSensorRemove",llSensorRemove),
        ("llSensorRepeat",llSensorRepeat),
        ("llSendRemoteData",llSendRemoteData),
        ("llSetAlpha", llSetAlpha),
        ("llSetBuoyancy", llSetBuoyancy),
        ("llSetClickAction", llSetClickAction),
        ("llSetColor", llSetColor),
        ("llSetForce", llSetForce),
        ("llSetForceAndTorque", llSetForceAndTorque),
        ("llSetHoverHeight", llSetHoverHeight),
        ("llSetLinkAlpha",llSetLinkAlpha),
        ("llSetLinkColor",llSetLinkColor),
        ("llSetLinkPrimitiveParams",llSetLinkPrimitiveParams),
        ("llSetLinkPrimitiveParamsList",llSetLinkPrimitiveParams),
        ("llSetLinkTexture",llSetLinkTexture),
        ("llSetLocalRot",llSetLocalRot),
        ("llSetObjectName",llSetObjectName),
        ("llSetObjectDesc",llSetObjectDesc),
        ("llSetParcelMusicURL",llSetParcelMusicURL),
        ("llSetPayPrice",llSetPayPrice),
        ("llSetPos",llSetPos),
        ("llSetPrimitiveParams",llSetPrimitiveParams),
        ("llSetPrimURL",llSetPrimURL),
        ("llSetRemoteScriptAccessPin", llSetRemoteScriptAccessPin),
        ("llSetRot",llSetRot),
        ("llSetScale",llSetScale),
        ("llSetScriptState",llSetScriptState),
        ("llSetSoundQueueing",llSetSoundQueueing),
        ("llSetSoundRadius",llSetSoundRadius),
        ("llSetSitText",llSetSitText),
        ("llSetStatus",llSetStatus),
        ("llSetText",llSetText),
        ("llSetTexture",llSetTexture),
        ("llSetTorque",llSetTorque),
        ("llSetTouchText",llSetTouchText),
        ("llSetVehicleFlags",llSetVehicleFlags),
        ("llSitTarget",llSitTarget),
        ("llShout",llShout),
        ("llSleep", llSleep),
        ("llSetTimerEvent",llSetTimerEvent),
        ("llSound",llSound),
        ("llSoundPreload", llSoundPreload),
        ("llStartAnimation", llStartAnimation),
        ("llStopAnimation", llStopAnimation),
        ("llStopHover",llStopHover),
        ("llStopLookAt",llStopLookAt),
        ("llStopMoveToTarget", llStopMoveToTarget),
        ("llStopPointAt", llStopPointAt),
        ("llStopSound",llStopSound),
        ("llTakeCamera",llTakeCamera),
        ("llTakeControls",llTakeControls),
        ("llTarget",llTarget),
        ("llTargetOmega",llTargetOmega),
        ("llTargetRemove",llTargetRemove),
        ("llTeleportAgentHome",llTeleportAgentHome),
        ("llTriggerSound",llTriggerSound),
        ("llTriggerSoundLimited",llTriggerSoundLimited),
        ("llUnSit",llUnSit),
        ("llVolumeDetect", llVolumeDetect),
        ("llWhisper",llWhisper),
        ("llWind",llWind),
        ("llWater",llWater)
    ] ++ map (\ (n,f) -> (n, defaultPredef n (\ i -> f i))) internalLLFuncs

allFuncs = map (\ (name,_,_) -> name) funcSigs
implementedFuncs = map fst $ M.toList (defaultPredefs::(M.Map String (PredefFunc Maybe)))
unimplementedFuncs = S.toList (S.difference (S.fromList allFuncs) (S.fromList implementedFuncs))
