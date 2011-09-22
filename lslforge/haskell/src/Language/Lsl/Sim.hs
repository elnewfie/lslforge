{-# LANGUAGE FlexibleContexts, NoMonomorphismRestriction #-}
{-# OPTIONS_GHC -fwarn-unused-binds -fwarn-unused-imports #-}
module Language.Lsl.Sim(
    SimStatus(..), 
    SimStateInfo(..),
    SimCommand(..),
    SimInputEventDefinition(..),
    SimParam(..),
    SimParamType(..),
    -- imported types re=exported
    LogMessage(..),
    LogLevel(..),
    Breakpoint,
    BreakpointManager,
    EvalResult,
    ScriptInfo(..),
    AvatarOutputEvent(..),
    AvatarInputEvent(..),
    SimEvent(..),
    SimEventArg(..),
    World,
    --
    eventDescriptors,
    simStep,
    unimplementedFuncs
    ) where

import Prelude hiding(id,(.))

import Control.Applicative
import Control.Arrow((&&&))
import Control.Category
import Control.Monad((>=>),filterM,foldM,forM_,liftM,liftM2,unless,when)
import Control.Monad.State(StateT(..))
import Control.Monad.Identity(Identity(..))
import Control.Monad.Error(MonadError(..))
import Data.List(elemIndex,find,foldl',nub)
import Data.Bits((.&.),(.|.),complement,xor,testBit)
import Data.Int()
import Data.Map(Map)
import Data.Maybe(fromMaybe,isJust,isNothing)
import qualified Data.Map as M
import Data.Record.LabelExtras
import qualified Data.Set as S
import qualified Data.IntMap as IM

import Language.Lsl.Internal.AvEvents(AvatarOutputEvent(..),
    AvatarInputEvent(..))
import Language.Lsl.Internal.Breakpoint(Breakpoint,BreakpointManager,
    emptyBreakpointManager,setStepOverBreakpoint,setStepOutBreakpoint,
    replaceBreakpoints,setStepBreakpoint,breakpointFile,breakpointLine,
    checkBreakpoint)
import Language.Lsl.Internal.Constants
import Language.Lsl.Internal.Evaluation(ScriptInfo(..),Event(..),
    EvalResult(..))
import Language.Lsl.Internal.EventSigs(EventAdditionalData(..),
    EventDelivery(..),lslEventDescriptors)
import Language.Lsl.Internal.Exec(ExecutionState(..),
    ExecutionInfo(ExecutionInfo),ScriptImage(..),executeLsl,frameInfo,
    hasActiveHandler,initLSLScript)
import Language.Lsl.Internal.ExpressionHandler(evaluateExpression)
import Language.Lsl.Internal.Key(nullKey,LSLKey(..))
import Language.Lsl.Internal.Log(LogLevel(..),LogMessage(..))
import Language.Lsl.Internal.Physics(checkIntersections,dampForce,dampTorque,
    dampZForce,gravC,kin,rotDyn,totalTorque)
import Language.Lsl.Internal.Type(LSLValue(..),LSLType(..),isIVal,isLVal,
    isSVal,lslShowVal,lslTypeString,rot2RVal,vec2VVal)
import Language.Lsl.UnitTestEnv(simFunc,hasFunc)
import Language.Lsl.Internal.SimLL
import Language.Lsl.Internal.Util(add3d,angleBetween,diff3d,dist3d2,mag3d,
    mlookup,neg3d,norm3d,quaternionToMatrix,rot3d,scale3d,(<||>),filtMapM)
import Language.Lsl.WorldDef(Attachment(..),Avatar(..),
    AvatarControlListener(..),InventoryInfo(..),InventoryItem(..),
    InventoryItemData(..),InventoryItemIdentification(..),LSLObject(..),
    ObjectDynamics(..),Prim(..),PositionTarget(..),RotationTarget(..),
    Script(..),WebHandling(..),defaultDynamics,emptyPrim,inventoryItemName,
    mkScript,isInvObjectItem,isInvScriptItem,primPhysicsBit,
    scriptInventoryItem,worldFromFullWorldDef,ItemPermissions(..))
import Language.Lsl.Internal.WorldState

import System.Random(mkStdGen)
import Text.ParserCombinators.Parsec hiding (State,(<|>),optional,many)

-- this is extremely approximate....
objRadius oid = do
        --LSLObject { primKeys = keys } <- getObject oid
        foldM f 0 =<< getM (primKeys.lm oid.wobjects) --keys
    where f r k = do
              (x,y,z):*(sx,sy,sz) <- getM $ (primPosition.*primScale).wprim k
              let (rx,ry,rz) = (sx / 2 + abs x, sy / 2 + abs y, sz / 2 + abs z)
              return $ maximum [r,rx,ry,rz]
-------------------------------------------------------------------------------
getPrimInfo pkey =
    do mprim <- optional $ getM $ wprim pkey
       maybe (return Nothing) inf mprim
    where inf p = case getI primParent p of
              Nothing -> return $ Just (pkey,0,getI primName p)
              Just p1Key -> do
                  mkeys <- optional $ getM $ primKeys.lm p1Key.wobjects
                  return $ do
                      plist <- mkeys
                      i <- elemIndex pkey plist
                      return (p1Key, i, getI primName p)
                          
pushEvent e key sid =
    ((scriptEventQueue.lm (key,sid).worldScripts) `modM_` (++[e])) <||>
        logAMessage LogWarn "sim" ("no such script: " ++ show (key, sid) ++ ", event: " ++ show e)
     
pushEventToPrim e key =
    runErrPrim key () (getPrimScripts key >>= mapM_ (pushEvent e key . inventoryItemName))
           
pushEventToObject e key =
    do objects <- getM wobjects
       case M.lookup key objects of
           Nothing -> logAMessage LogWarn "sim" ("no such object: " ++ unLslKey key)
           Just o ->  mapM_ (pushEventToPrim e) (getI primKeys o)

newWorld slice maxt iq lib scripts avatars objs prims activeScripts regions
        keyIndex webHandling eventHandler log = 
    World {
        _sliceSize = slice,
        _maxTick = maxt,
        _nextPause = slice,
        _wqueue = iq,
        _wlisteners = IM.empty,
        _nextListenerId = 0,
        _wobjects = objs,
        _wprims = prims,
        _worldScripts = activeScripts,
        _inventory = [],
        _tick = 0,
        _msglog = [LogMessage 0 LogWarn "init" s | s <- log ],
        _predefs = defaultPredefs,
        _randGen = mkStdGen 1,
        _wlibrary = lib,
        _wscripts = scripts,
        _worldEventHandler = fmap (flip (,) []) eventHandler,
        _worldAvatars = avatars,
        _worldBreakpointManager = emptyBreakpointManager,
        _worldSuspended = Nothing,
        _worldRegions = regions,
        _worldZeroTime = 1000000000,
        _worldKeyIndex = keyIndex,
        _worldWebHandling = webHandling,
        _worldOutputQueue = [],
        _worldPendingHTTPRequests = [],
        _worldOpenDataChannels = (M.empty,M.empty),
        _worldXMLRequestRegistry = M.empty,
        _worldPhysicsTime = 0,
        _worldTargetCheckTime = 0,
        _worldLastPositions = M.empty,
        _worldCollisions = S.empty,
        _worldLandCollisions = S.empty,
        _worldTouches = M.empty,
        _worldTouchCheckTime = 0,
        _worldURLRegistry = M.empty,
        _worldPendingHTTPResponses = M.empty }
           
checkBp bp sm =
    do  bpm <- getM worldBreakpointManager
        let (result,bpm',sm') = checkBreakpoint bp bpm sm
        worldBreakpointManager =: bpm'
        return (result,sm')
       
updatePrim f key =
    do  prims <- getM wprims
        case M.lookup key prims of
            Nothing -> logAMessage LogWarn "sim" ("prim " ++ unLslKey key ++ " not found")
            Just p -> do p' <- f p
                         wprims =: (M.insert key p' prims)

              
processEvents :: (Monad m) => WorldE m ()
processEvents =
    do suspenseInfo <- getM worldSuspended
       when (isNothing suspenseInfo) $ do
           (tick:*weq) <- getM (tick.*wqueue)
           case takeWQ tick weq of
               (Nothing,_) -> return ()
               (Just we,weq') -> 
                   do wqueue =: weq'
                      processEvent we
                      processEvents
    
processEvent :: (Monad m) => WorldEventType -> WorldE m ()
processEvent (DeferredScriptEvent event (DeferredScriptEventScriptTarget (pk,sn))) = pushEvent event pk sn
processEvent (DeferredScriptEvent event (DeferredScriptEventPrimTarget pk)) = pushEventToPrim event pk
processEvent (DeferredScriptEvent event (DeferredScriptEventObjectTarget oid)) = pushEventToObject event oid
processEvent (CreatePrim name key) =
    let prim = emptyPrim name key in do
        lm key.wobjects =: LSLObject [key] defaultDynamics
        wprim key =: prim
processEvent (AddScript (name,script) key active) =
       do (t:*scripts) <- getM (tick.*wscripts)
          case lookup script scripts of
              Nothing -> logAMessage LogWarn "sim" ("no such script: " ++ script)
              Just (Left s) -> do
                  scriptKey <- newKey
                  updatePrim (\ p -> return $ addScript p (scriptInventoryItem name scriptKey script)) key
              Just (Right code) -> do
                  scriptKey <- newKey
                  updatePrim (\ p -> return $ addScript p (scriptInventoryItem name scriptKey script)) key
                  loadScript key name script active
--                   let sstate = initLSLScript code
--                   lm (key,name).worldScripts =:
--                       setI (scriptStartTick.*scriptLastResetTick) (t:*t) (mkScript sstate)
       where addScript prim invScript = setI primInventory (invScript:getI primInventory prim) prim
processEvent chat@(Chat chan name key msg location range) =
    do listeners <- getM wlisteners 
       locatedListeners <- mapM locateListener (IM.elems listeners)
       let listeners'= [ l | (l,_,_) <- filter (matchListener chat) locatedListeners]
       -- event goes to all UNIQUE addresses in list
       let addresses = nub $ map listenAddress listeners'
       mapM_ (uncurry (pushEvent (Event "listen" [IVal chan, SVal name, KVal key, SVal msg] M.empty))) addresses
       when (chan == 0) $ case range of
           Nothing -> return ()
           Just dist -> pushChatToAvatars location dist
    where locateListener (listener,active) = do
              (VVal x y z) <- getPos (listenerPrimKey listener)
              region <- getPrimRegion (listenerPrimKey listener)
              return (listener,active,(region,(x,y,z)))
          pushChatToAvatars location range =
              liftM M.elems (getM worldAvatars) >>= mapM_ (pushChatToAvatar location range)
          pushChatToAvatar (region,pos) range avatar =
              when (region == getI avatarRegion avatar && dist3d2 pos (getI avatarPosition avatar) <= range^2) $
                  putWorldEvent 0 $ AvatarInputEvent (getI avatarKey avatar) (AvatarHearsChat name key msg)
processEvent (WorldSimEvent name args) = 
    case M.lookup name eventDescriptors of
        Nothing -> logAMessage LogWarn "sim" ("event " ++ name ++ " not understood")
        Just def -> handleSimInputEvent def args
processEvent (PermissionRequestEvent pk sname ak mask) = 
    runAndLogIfErr ("can't find prim/script: " ++ unLslKey pk ++ "/" ++ sname) () $ do
        scriptLastPerm.lscript pk sname =: Just ak
        lm ak.scriptPermissions.lscript pk sname =: mask
        pushEvent (Event "run_time_permissions" [IVal mask] M.empty) pk sname
processEvent evt@(TimerEvent interval (primKey,scriptName)) =
    do pushEvent (Event "timer" [] M.empty) primKey scriptName
       putWorldEvent interval evt
processEvent evt@(SensorEvent (pk,sn) name key stype range arc rpt) =
    runAndLogIfErr "problem processing sensor event" () $ do 
       t <- getM tick
       maybe (return ()) (\ interval -> putWorldEvent interval evt) rpt
       sensedObjects <- senseObjects
       sensedAvatars <- senseAvatars
       let list = take 16 (sensedObjects ++ sensedAvatars)
       if null list
           then pushEvent (Event "no_sensor" [] M.empty) pk sn
           else pushEvent (Event "sensor" [(IVal $ length list)] (M.fromList (zipWith (\ i k -> ("key_" ++ show i,KVal k)) [0..] list))) pk sn       
    where senseObjects = do
              pos <- getGlobalPos pk
              rot <- getGlobalRot pk
              root <- getRootPrim pk
              let fwd = fwdFromRot rot
              prims <- rootPrims root
              poss <- mapM (getGlobalPos . getI primKey) prims
              let pprims = zip poss prims
              statuses <- mapM (objectStatus . getI primKey) prims
              return [ getI primKey p | ((pos,p),stat) <- zip pprims statuses, withinSensorArea pos fwd range arc pos, stat .&. stype /= 0]
          senseAvatars = 
              if stype .&. cAgent /= 0 then do
                      pos <- getGlobalPos pk
                      rot <- getGlobalRot pk
                      attachKey <- (fmap $ getI attachmentKey) <$> getM (primAttachment.wprim pk)
                      let fwd = fwdFromRot rot
                      avs <- allAvs attachKey
                      return [ getI avatarKey av | av <- avs, withinSensorArea pos fwd range arc (getI avatarPosition av)]
                  else return []
          fwdFromRot rot = let ((x,_,_),(y,_,_),(z,_,_)) = quaternionToMatrix rot in (x,y,z)
          angleBetweenV (x0,y0,z0) (x1,y1,z1) = acos (x0*x1 + y0*y1 + z0*z1)
          withinSensorArea origin direction range arc pos =
              let v = diff3d pos origin
                  m = mag3d v 
                  v' = norm3d v
              in (m == 0 ) || (m <= range && angleBetweenV v' direction <= arc)
          rootPrims exclude = do
              objs <- getM wobjects
              mapM (getM . wprim) [ k | LSLObject { _primKeys = (k:_) } <- M.elems objs, exclude /= k]
          objectStatus root = do
              pks <- getM $ primKeys.lm root.wobjects
              stat <- foldM (\ x -> liftM (x .|.) . determineActivePassiveScripted) 0 pks
              if stat .&. cActive /= 0 && stat .&. cPassive /= 0
                 then return (stat .&. complement cPassive)
                 else return stat
          allAvs exclude = do
              avs <- getM worldAvatars
              return [ av | av <- M.elems avs, exclude /= Just (getI avatarKey av)]
processEvent (HTTPRequestEvent (pk,sn) key url method mimetype maxlength verify body) = do
        logAMessage LogDebug "sim" "processing an HTTP request event"
        handling <- getM worldWebHandling
        case handling of
            WebHandlingByDoingNothing -> putHTTPTimeoutEvent pk sn key 60 
            WebHandlingByFunction -> runAndLogIfErr "problem with handler" () $ do
                (moduleName, state) <- getM worldEventHandler >>= maybe (throwError "no handler defined") return
                lib <- getM wlibrary
                -- invoke the function to get the results
                case simFunc lib (moduleName,"httpRequest") state [SVal url, SVal method, SVal mimetype, IVal maxlength, IVal verify, SVal body] of
                    Left s -> throwError s
                    Right (SVal msg,results) | not (null msg) -> do
                        worldEventHandler =: (Just (moduleName, results))
                        throwError msg
                                             | otherwise ->
                        let mval p v = if p v then Just v else Nothing 
                            IVal status = fromMaybe (IVal 200) 
                                (lookup "outHttpStatus" results >>= mval isIVal)
                            LVal metadata = fromMaybe (LVal []) 
                                (lookup "outHttpMetadata" results >>= mval isLVal)
                            body = fromMaybe (SVal "")
                                (lookup "outHttpBody" results >>= mval isSVal)
                            events = fromMaybe (LVal []) (lookup "outEvents" results)
                        in do putHTTPResponseEvent pk sn key status metadata body 2
                              processEventsList events
                              worldEventHandler =: (Just (moduleName, results))
                    Right _ -> throwError "invalid web handling function, must return a string"
            WebHandlingByInternet timeout -> do
                let event = SimEvent "http_request" [
                                SimEventArg "url" url,
                                SimEventArg "prim" $ unLslKey pk,
                                SimEventArg "script" sn,
                                SimEventArg "requestKey" $ unLslKey key,
                                SimEventArg "HTTP_METHOD" method,
                                SimEventArg "HTTP_MIMETYPE" mimetype,
                                SimEventArg "HTTP_BODY_MAXLENGTH" (show maxlength),
                                SimEventArg "HTTP_VERIFY_CERT" (show verify),
                                SimEventArg "body" body] 0
                worldOutputQueue `modM_` (event:)
                worldPendingHTTPRequests `modM_` (key:)
                -- put a timeout event on the queue.  'real' time and sim time are much different, so the 
                -- 'real' world timeout is much different
                putHTTPTimeoutEvent pk sn key timeout
processEvent (XMLRequestEvent source channel idata sdata) = 
    runAndLogIfErr "invalid xml request" () $ do
        logAMessage LogInfo "sim" "processing XMLRequestEvent"
        (pk,sn) <- lookupScriptFromChan channel <||> throwError ("no such channel" ++ unLslKey channel)
        key <- newKey
        lm key.worldXMLRequestRegistry =: source
        pushEvent (Event "remote_data" [llcRemoteDataRequest, KVal channel, KVal nullKey, SVal "", IVal idata, SVal sdata]
                                              (M.fromList [("requestKey",KVal key)])) pk sn
processEvent (XMLReplyEvent key channel messageId sdata idata) =
    runAndLogIfErr "invalid xml reply" () $ do
        source <- getM (lm key.worldXMLRequestRegistry) <||> throwError "missing source for XML request"
        worldXMLRequestRegistry `modM_` M.delete key
        case source of
            XMLRequestInternal tag -> do
                (moduleName, state) <- getM worldEventHandler >>= maybe (throwError "no handler defined") return
                lib <- getM wlibrary
                case simFunc lib (moduleName, "xmlReply") state [SVal tag, SVal $ unLslKey channel, SVal sdata, IVal idata] of
                    Left s -> throwError s
                    Right (SVal msg, results) | not (null msg) -> do
                        worldEventHandler =: (Just (moduleName, results))
                        throwError msg
                                              | otherwise ->
                        let events = fromMaybe (LVal []) (lookup "outEvents" results) in do
                            processEventsList events
                            worldEventHandler =: Just (moduleName, results)
            XMLRequestExternal tag -> do
                let event = SimEvent "xml_reply" [
                        SimEventArg "tag" tag,
                        SimEventArg "channel" $ unLslKey channel,
                        SimEventArg "sdata" sdata,
                        SimEventArg "idata" (show idata)] 0
                worldOutputQueue `modM_`  (event:)
processEvent (DialogEvent agent message buttons channel source) =
    runAndLogIfErr "invalid dialog event" () $
        getM worldEventHandler >>= (\ mhandler -> case mhandler of
            Nothing -> return ()
            Just (moduleName,state) -> do
                lib <- getM wlibrary
                avName <- liftM (getI avatarName) (getM $ wav agent) <||> throwError "problem finding avatar"
                objName <- getM (primName.wprim source) <||> throwError "problem finding object"
                case hasFunc lib (moduleName,"dialog") of
                    Left s -> throwError s
                    Right False -> return ()
                    Right True -> 
                        case simFunc lib (moduleName, "dialog") state 
                               [SVal avName, SVal message, LVal (map SVal buttons), IVal channel, SVal objName] of
                            Left s -> throwError s
                            Right (SVal msg, results) | not (null msg) -> do
                                warn
                                worldEventHandler =: Just (moduleName, results)
                                throwError msg
                                                      | otherwise ->
                                let (IVal selection) = fromMaybe (IVal (-1)) (lookup "outDialogButtonSelected" results)
                                    events = fromMaybe (LVal []) (lookup "outEvents" results)
                                in do
                                    warn
                                    when (selection >= 0 && selection < length buttons) $ 
                                        putChat sayRange agent channel (buttons !! selection)
                                    processEventsList events
                                    worldEventHandler =: Just (moduleName, results))
                            where warn = logAMessage LogWarn "sim" 
                                    "the 'dialog' entry point for the world event handler is deprecated, use the avatar event handler instead"

processEvent (RezObjectEvent links pos vel rot start pk copy atRoot) =
    runAndLogIfErr "invalid rez object event" () $ do
        when (null links) $ throwError "empty link set!"
        -- reset positions relative to root
        let geomCenter = scale3d ( 1 / fromIntegral (length links)) 
                                 (foldl' add3d (0,0,0) (map (flip diff3d (0,0,0) . getI primPosition) links))
        let pos' = if atRoot then pos else pos `add3d` neg3d geomCenter
        let update = if copy then updateKeys else return
        -- TODO: rotation and velocity
        links' <- mapM update links
        mapM_ activateScripts links'
        let rootKey = getI primKey (head links')
        lm rootKey.wobjects =: LSLObject (map (getI primKey) links') defaultDynamics { _objectPosition = pos' }
        wprims `modM_` (\ m -> (foldl' (\ m l -> M.insert (getI primKey l) l m) m links'))
        pushEventToObject (Event "on_rez" [IVal start] M.empty) rootKey
        pushEventToPrim (Event "object_rez" [KVal rootKey] M.empty) pk
    where activateScripts prim = do
              let scripts = [ item | item <- getI primInventory prim, isInvScriptItem item ]
              let activateScriptItem item = 
                      let InvScript id state = inventoryItemData item
                          name = inventoryItemName item in
                              activateScript (getI primKey prim) name id state start
              mapM activateScriptItem scripts
          updateKeys prim = do
              key <- newKey
              newInventory <- mapM updateItemKeys (getI primInventory prim)
              return $ setI (primKey.*primInventory) (key:*newInventory) prim
          updateItemKeys item = do
              key <- newKey
              let name = inventoryItemName item
              newData <- if isInvObjectItem item
                  then liftM InvObject (mapM updateKeys (invObjectPrims $ inventoryItemData item))
                  else return (inventoryItemData item)
              return $ item { inventoryItemIdentification = InventoryItemIdentification (name,key), inventoryItemData = newData }
processEvent (ResetScriptEvent pk sn) =
     runAndLogIfErr "invalid reset script event" () (resetScript pk sn)
processEvent (DetachCompleteEvent oid ak) =
     runAndLogIfErr "invalid detach complete event" () $ do
         -- need to remove the object and it's prims and its active scripts from the live lists, but
         -- save the script states
         links <- getM $ primKeys.lm oid.wobjects
         prims <- mapM passivatePrim links
         when (null prims) $ throwError "object has no prims!"
         let root = head prims
         let object = InventoryItem (InventoryItemIdentification (getI primName root,getI primKey root)) 
                                    (InventoryInfo (getI primCreator root) 
                                        (ItemPermissions 0x0008e000 0x0008e000 0x0008e000 0x0008e000 0x0008e000))
                                    (InvObject prims)
         (avatarInventory.wav ak) `modM` (object:)
         modM_ wobjects (M.delete oid)
     where passivatePrim pk = do
               scriptItems <- getPrimScripts pk
               mapM_ (moveScriptStateToPrim pk . inventoryItemName) scriptItems
               prim <- getM $ wprim pk
               modM wprims (M.delete pk)
               return prim
           moveScriptStateToPrim pk sn = do
               img <- getM $ scriptImage.lscript pk sn
               primInv <- getPrimInventory pk
               let (xs,ys) = break (\ item -> sn == inventoryItemName item) primInv
               when (null ys) $ throwError "script inexplicably not in inventory"
               (y:ys') <- return ys
               let scriptData = inventoryItemData y
               setPrimInventory pk (((y { inventoryItemData = scriptData { invScriptState = Just img } }) : xs) ++ ys)
               worldScripts `modM` M.delete (pk,sn)
processEvent (GiveAvatarInventoryEvent ak folder items) = 
    logAMessage LogInfo "sim" "giving avatar inventory: not implemented"
processEvent (AvatarOutputEvent ak avEv) = processAvatarOutputEvent ak avEv
processEvent (AvatarInputEvent ak avEv) = processAvatarInputEvent ak avEv
processEvent _ = error "not implemented"

processEventsList (LVal []) = return ()
processEventsList (LVal ((IVal i):l)) = do
    processEventList (take i l)
    processEventsList (LVal (drop i l))
processEventsList _ = logAMessage LogWarn "sim" "user supplied event handler has invalid type for 'outEvents' variable"
    
processEventList [SVal "xml_request", SVal sourceId, KVal channel, IVal idata, SVal sdata, FVal delay] = 
    putWorldEvent delay (XMLRequestEvent (XMLRequestInternal sourceId) channel idata sdata)
processEventList l = logAMessage LogWarn "sim" ("Invalid event from event handler: " ++ lslShowVal (LVal l))

processAvatarOutputEvent k (AvatarTouch {avatarTouchPrimKey = pk, avatarTouchDuration = secs}) = do
        now <- getM tick
        worldTouches `modM_` M.alter (alt now) pk
    where alt start Nothing = Just [newTouch start]
          alt start (Just ts) = Just (newTouch start:ts)
          newTouch start = Touch {touchAvatarKey = k, touchPrimKey = pk,
              touchFace = 0, touchST = (0.5, 0.5), touchStartTick = start, 
              touchEndTick = start + durationToTicks secs}
processAvatarOutputEvent k (AvatarWhisper {avatarChatChannel = chan, avatarChatMessage = msg}) = avChat 10 msg k chan
processAvatarOutputEvent k (AvatarSay {avatarChatChannel = chan, avatarChatMessage = msg}) = avChat 20 msg k chan
processAvatarOutputEvent k (AvatarShout {avatarChatChannel = chan, avatarChatMessage = msg}) = avChat 100 msg k chan
processAvatarOutputEvent k (AvatarPay {avatarPayPrimKey = pk, avatarPayAmount = val}) = runAndLogIfErr "problem handling avatar payment" () $
    primHasActiveHandler k "money" >>= flip when (pushEventToPrim (Event "money" [KVal pk, IVal val] M.empty) pk)
processAvatarOutputEvent k (AvatarControl { avatarNewControlBits = newBits }) = runAndLogIfErr "problem processing control change" () $ do
    av <- getM $ wav k
    prev <- getM $ avatarControls.wav k
    avatarControls.wav k =: newBits
    flip (maybe (return ())) (getI avatarControlListener av) $ 
        \ (AvatarControlListener {avatarControlListenerMask = mask, avatarControlListenerScript = (pk,sn)}) ->
            pushEvent (Event "control" [KVal k, IVal newBits, IVal (newBits `xor` prev)] M.empty) pk sn
processAvatarOutputEvent k (AvatarFaceTouch pk secs face st) = do
    now <- getM tick
    worldTouches `modM_` M.alter (alt now) pk
    where alt start Nothing = Just [newTouch start]
          alt start (Just ts) = Just (newTouch start:ts)
          newTouch start = Touch {touchAvatarKey = k, touchPrimKey = pk,
              touchFace = face, touchST = st, touchStartTick = start, 
              touchEndTick = start + durationToTicks secs}
processAvatarOutputEvent k (AvatarHTTPRequest url method body ip ua) =
    case parseURL url of
        Nothing -> logAMessage LogWarn "sim" ("URL " ++ url ++ " unrecognized")
            >> putWorldEvent 0 (AvatarInputEvent k (AvatarHTTPBadRequest))
        Just (base,path,query) -> do
            dest <- optional $ getM (lm base.worldURLRegistry)
            case dest of
                Just (_,(pk,sn)) -> do
                    expire <- (+) <$> getM tick <*> pure (durationToTicks 25.0)
                    rk <- newKey
                    putWorldEvent 0 (AvatarInputEvent k (AvatarHTTPRequestKey rk))
                    lm rk.worldPendingHTTPResponses =: (PendingHTTPResponse k (pk,sn) base path query ip ua expire)
                    pushEvent (Event "http_request" [KVal rk, SVal method, SVal body] M.empty) pk sn
                Nothing -> logAMessage LogWarn "sim" ("URL " ++ url ++ " not registered")
                    >> putWorldEvent 1 (AvatarInputEvent k (AvatarHTTPBadRequest))
    
avChat range msg key chan = runAndLogIfErr "problem processing chat" () $ do
    logAMessage LogInfo ("av:" ++ unLslKey key) ("chat! chan: " ++ show chan ++ ", range: " ++ show range ++ ", message: " ++ show msg)
    av <- getM $ wav key
    putWorldEvent 0 $ Chat chan (getI avatarName av) key msg (getI avatarRegion av, getI avatarPosition av) (Just range)

processAvatarInputEvent k inputEvent = runAndLogIfErr "problem processing event for avatar" () $ do
        av <- getM $ wav k
        let avInfo = [ IVal lslForgeAvatarKey, KVal (getI avatarKey av), 
                       IVal lslForgeAvatarPos, ((vec2VVal . getI avatarPosition) av),
                       IVal lslForgeAvatarRot, ((rot2RVal . getI avatarRotation) av),
                       IVal lslForgeAvatarName, SVal (getI avatarName av) ]
        flip (maybe (return ())) (getI avatarEventHandler av) (\ (moduleName,state) -> do
            lib <- getM wlibrary
            case callAvatarEventProcessor k moduleName inputEvent lib state avInfo of
                Nothing -> return ()
                Just (Left s) -> throwError s
                Just (Right (LVal events,result)) -> do
                    mapM_ (putAvatarOutputEvent k) events
                    avatarEventHandler.wav k =: Just (moduleName,result))

avEventProcCallInfo (AvatarOwnerSay key msg) avInfo = ("onOwnerSay",[SVal $ unLslKey key, SVal msg, LVal avInfo])
avEventProcCallInfo (AvatarHearsChat name key msg) avInfo = ("onChat",[SVal name, SVal $ unLslKey key, SVal msg, LVal avInfo])
avEventProcCallInfo (AvatarDialog msg buttons chan source) avInfo = 
    ("onDialog",[SVal msg, LVal $ map SVal buttons, IVal chan, SVal $ unLslKey source, LVal avInfo])
avEventProcCallInfo (AvatarLoadURL msg url) avInfo = ("onLoadURL",[SVal msg, SVal url, LVal avInfo])
avEventProcCallInfo (AvatarMapDestination simName position) avInfo = ("onMapDestination",[SVal simName, vec2VVal position, LVal avInfo])
avEventProcCallInfo (AvatarHTTPResponse k status body) avInfo = ("onHTTPResponse",[SVal $ unLslKey k, IVal status, SVal body, LVal avInfo])
avEventProcCallInfo (AvatarHTTPRequestKey k) avInfo = ("onHTTPRequestKey",[SVal $ unLslKey k, LVal avInfo])
avEventProcCallInfo (AvatarHTTPBadRequest) avInfo = ("onHTTPBadRequest",[LVal avInfo])

callAvatarEventProcessor k moduleName avEvent lib state avInfo = 
    case hasFunc lib (moduleName,funcName) of
        Left s -> Just (Left s)
        Right True -> Just $ simFunc lib (moduleName, funcName) state args
        Right False -> Nothing
    where (funcName,args) = avEventProcCallInfo avEvent avInfo
    
putAvatarOutputEvent k (SVal s) =
    case reads s of
       ((ev,_):_) -> putWorldEvent 0 (AvatarOutputEvent k ev)
       [] -> logAMessage LogWarn "sim" ("avatar event handler for " ++ unLslKey k ++ " returned invalid event: " ++ show s)
putAvatarOutputEvent k v = logAMessage LogWarn "sim" ("avatar event handler for " ++ unLslKey k ++ " returned invalid event: " ++ lslShowVal v)

parseURL :: String -> Maybe (String,String,String)
parseURL input = case runParser p () "" input of
    Left _ -> Nothing
    Right pair -> Just pair
    where p = do
              scheme <- ((++) <$> string "http" <*> (string "s://" <|> string "://"))
              subdomain <- many (alphaNum <|> char '-')
              fixedPortion <- string ".example.com:12043/cap/"
              key <- many (alphaNum <|> char '-')
              path <- ((:) <$> char '/' <*> many (noneOf "?")) <|> return ""
              query <- (char '?' >> many anyChar) <|> return ""
              return (scheme ++ subdomain ++ fixedPortion ++ key, path, query)
              
activateScript pk sn scriptId Nothing startParam =
    do wscripts <- getM wscripts
       case lookup scriptId wscripts of
           Nothing -> logAMessage LogWarn "sim" ("script " ++ scriptId ++ " not found")
           Just (Left s) -> logAMessage LogWarn "sim" ("script " ++ scriptId ++ " not valid")
           Just (Right code) ->
               lscript pk sn =: (mkScript (initLSLScript code))
activateScript pk sn _ (Just image) startParam =
    lscript pk sn =: setI scriptStartParameter startParam (mkScript image)
        
matchListener (Chat chan' sender' key' msg' (region,position) range) ((Listener pkey sid chan sender key msg),active,(region',position')) =
    active &&
    region == region' &&
    chan == chan' &&
    key' /= pkey &&
    (sender == "" || sender == sender') &&
    (null (unLslKey key) || key == nullKey || key == key') &&
    (msg == "" || msg == msg') &&
    (case range of
        Nothing -> True
        Just dist -> dist3d2 position position' <= dist^2)

listenAddress l = (listenerPrimKey l, listenerScriptName l)

nextActivity :: (Monad a) => WorldE a (Maybe Int)
nextActivity = 
    do  scripts <- getM worldScripts
        t <- (+1) <$> getM tick
        let ns = foldl' (calcNxt t) Nothing $ M.elems scripts
        wq <- getM wqueue
        let ne = case wq of
                     [] -> Nothing
                     ((i,x):_) -> Just $ max t i
        return $ mmin ne ns
    where calcNxt t n script
              | getI scriptActive script = 
                  case executionState img of
                      Executing -> Just t
                      Suspended _ -> Just t
                      Waiting -> if null q then n else Just t
                      WaitingTil i -> if isNothing n then Just (max t i) else fmap (max t . min i) n 
                      SleepingTil i -> if isNothing n then Just (max t i) else fmap (max t . min i) n
                      _ -> n
              | otherwise = n
              where img:*q = getI (scriptImage.*scriptEventQueue) script
          mmin Nothing Nothing = Nothing
          mmin Nothing (Just i) = Just i
          mmin (Just i) Nothing = Just i
          mmin (Just i) (Just j) = Just (min i j)
                  
runScripts :: Monad m => WorldE m ()
runScripts =
    do scripts <- getM worldScripts
       mapM_ findAndRunScript (M.keys scripts)
       
findAndRunScript scriptKey =
    do scripts <- getM worldScripts
       case M.lookup scriptKey scripts of
          Nothing -> return ()
          Just script -> checkAndRunScript scriptKey script

checkAndRunScript k@(pkey,sname) script
    | getI scriptActive script = do
        pInfo <- getPrimInfo pkey
        suspenseInfo <- getM worldSuspended
        case suspenseInfo of
            Nothing -> run pInfo
            Just (suspKey,suspName) | pkey == suspKey && suspName == sname -> run pInfo
                                    | otherwise -> return ()
    | otherwise = return ()
    where img = getI scriptImage script
          q = getI scriptEventQueue script
          run pInfo =
              case pInfo of
                  Nothing -> logAMessage LogWarn "sim" ("script " ++ show k ++ ": prim not found!")
                  Just (parent, index, primName) -> runScript parent index pkey sname primName img q
runScript parent index pkey sname primName img q =
    do (slice:*t) <- getM (sliceSize.*tick)
       let img' = img { scriptImageName = sname ++ "/" ++ primName ++ "/" ++ unLslKey pkey }
       let log = logTrace (unLslKey pkey ++ ":" ++ sname)
       --let checkBp _ sm = return (False,sm)
       result <- executeLsl img' parent index sname pkey doPredef log (getM tick) (setM tick) checkBp q (t + slice)
       case result of
           Left s -> logAMessage LogWarn "sim" ("execution error in script " ++ "(" ++ unLslKey pkey ++ "," ++ sname ++ ") ->" ++ s)
           Right (img'',q') -> do
               result <- optional $ getM $ lm (pkey,sname).worldScripts
               case result of
                   Nothing -> logAMessage LogInfo "sim" "script seems to have disappeared while executing (result of llDie?)"
                   Just script -> do
                       scriptImage.lscript pkey sname =: img''
                       scriptEventQueue.lscript pkey sname =: q'
                       case executionState img'' of
                           Suspended bp -> worldSuspended =: (Just (pkey,sname))
                           _ -> worldSuspended =: Nothing


handleTouches :: Monad m => WorldE m ()
handleTouches = do
    now:*before <- getM (tick.*worldTouchCheckTime)
    when (now - before > durationToTicks 0.05) $ do
        touchLists <- M.toList <$> getM worldTouches
        runAndLogIfErr "problem handling touches" () $ do
            starts <- collectTouches "touch_start" M.empty $ 
                         concatMap (\ (_,tl) -> [ touch | touch@(Touch { touchStartTick = t }) <- tl, t > before]) touchLists
            ongoings <- collectTouches "touch" M.empty $ 
                concatMap 
                (\ (_,tl) -> [ touch | touch@(Touch { touchStartTick = ts, touchEndTick = te }) <- tl, te >= now && ts <= before]) touchLists
            ends <- collectTouches "touch_end" M.empty $ 
                       concatMap (\ (_,tl) -> [ touch | touch@(Touch { touchEndTick = te }) <- tl, te <= now]) touchLists
            mapM_ (uncurry (sendTouch "touch_start")) $ M.toList starts
            mapM_ (uncurry (sendTouch "touch")) $ M.toList ongoings
            mapM_ (uncurry (sendTouch "touch_end")) $ M.toList ends
        let nonends = [ (pk,[ t | t@(Touch { touchEndTick = te }) <- list, te > now]) | (pk,list) <- touchLists ]
        worldTouchCheckTime =: now
        worldTouches =: (M.fromList nonends)
        return ()

sendTouch ttype k tl =
     pushEventToPrim (Event ttype [IVal $ length tl] payload) k
     where payload = foldl' ins M.empty (zip [0..] tl)
           ins m (i,(Touch { touchAvatarKey = ak, touchFace = face, touchST = (s,t) },link)) = 
               M.insert ("vector_" ++ show i) (VVal 0 0 0) .
               M.insert ("key_" ++ show i) (KVal ak) .
               M.insert ("integer_" ++ show i) (IVal link) .
               M.insert ("face_" ++ show i) (IVal face) .
               M.insert ("st_" ++ show i) (VVal s t 0) $ m
               
collectTouches touchType acc [] = return acc
collectTouches touchType acc (t@(Touch { touchPrimKey = pk }):ts) = do
    lneeds <- primHasActiveHandler pk touchType
    linkNum <- getPrimLinkNum pk
    rk <- getRootPrim pk
    if pk == rk && lneeds then collectTouches touchType (M.alter (alt (t,linkNum)) pk acc) ts
              else do
                  pass <- getM $ primPassTouches.wprim pk
                  rneeds <- primHasActiveHandler rk touchType
                  let r = [(rk,(t { touchPrimKey = rk }, linkNum)) | rneeds && pass]
                  let p = [(pk,(t, linkNum)) | lneeds]
                  collectTouches touchType (foldl' ( \ m (k,d) -> M.alter (alt d) k m) acc (r ++ p)) ts
    where alt i Nothing = Just [i]
          alt i (Just l)  = Just (i:l)
runPhysics :: Monad m => WorldE m ()
runPhysics = do
        t0:*t1:*os <- getM (worldPhysicsTime.*tick.*wobjects)
        let tlist = [t0,(t0+10)..t1] ++ [t1 | (t1 - t0) `mod` 10 > 0]
        let intervals = zip tlist $ tail tlist
        forM_ intervals $ \ (t0,t1) ->
            forM_ (M.toList os) $ \ (pk,obj) -> runErrPrim pk () $ do
                let dynl = dyn pk
                prim:*dynm <- getM (wprim pk.*dynl)
                when (flip testBit primPhysicsBit $ getI primStatus prim) $ do
                    mass <- objectMass pk
                    let pos0@(_,_,z0) = getI objectPosition dynm
                    let vel0@(_,_,vz0) = getI objectVelocity dynm
                    let force = (fst (getI objectForce dynm) `rot3d` rotation) `add3d` (0,0,mass * getI objectBuoyancy dynm * (-gravC))
                            where rotation = if snd $ getI objectForce dynm then getI objectRotation dynm else (0,0,0,1)
                    let impulse = ((fst . fst . getI objectImpulse) dynm `rot3d` rotation, (snd . getI objectImpulse) dynm)
                            where rotation = if (snd . fst . getI objectImpulse) dynm then getI objectRotation dynm else (0,0,0,1)
                    let impulseFNow = if snd impulse >= t1 then fst impulse else (0,0,0)
                    let heightDampF tau z = (0,0, dampZForce tau z z0 mass vz0 fz0)
                            where (_,_,fz0) = force `add3d` impulseFNow
                    let dampF = case getI objectPositionTarget dynm of
                                     Nothing -> (0,0,0)
                                     Just (PositionTarget { positionTargetTau = tau, positionTargetLocation = l }) ->
                                         dampForce tau l pos0 mass vel0 (force `add3d` impulseFNow)
                                     Just (Repel { positionTargetTau = tau, positionTargetHeight = z }) ->
                                             if z0 <= 0.5 * z then heightDampF tau z else (0,0,0)
                                     Just (Hover { positionTargetTau = tau, positionTargetHeight = z}) -> heightDampF tau z
                    let (pos,vel) = kin t0 t1 ticksToDuration 0 mass pos0 vel0 (force `add3d` dampF) impulse
                    -- TODO: local vs global rotations in torque
                    -- TODO: rotational impulses
                    radius <- objRadius pk
                    let torque = totalTorque t0 t1 ticksToDuration
                                     [(fst $ getI objectTorque dynm,t1),
                                      (fst $ fst $ getI objectRotationalImpulse dynm, 
                                       snd $ getI objectRotationalImpulse dynm)]
                    let dampingTorque = case getI objectRotationTarget dynm of
                                            Nothing -> (0,0,0)
                                            Just rt@(RotationTarget {rotationTargetTau = tau, 
                                                                     rotationTargetStrength = strength,
                                                                     rotationTarget = target}) ->
                                                dampTorque tau strength target
                                                           (getI objectRotation dynm) mass radius (getI objectOmega dynm) torque
                    let (rot,omega) = rotDyn (ticksToDuration (t1 - t0)) radius mass (getI objectRotation dynm) 
                                          (getI objectOmega dynm)
                                             (torque `add3d` dampingTorque)
                    objectVelocity.dynl =: vel
                    objectPosition.dynl =: pos
                    objectRotation.dynl =: rot
                    objectOmega.dynl =: omega
                    return ()
        handleCollisions
        handleMovers
        handleTargets
        worldPhysicsTime =: t1

handleMovers :: Monad m => WorldE m ()
handleMovers = runAndLogIfErr "problem determining which objects moved" () $
    do lastPositions <- getM worldLastPositions
       let lastKeys = S.fromList $ M.keys lastPositions
       curPositions <- M.keys <$> (getM wobjects) >>= mapM (\ k -> liftM ((,)k) (getObjectPosition k))
       let curKeys = S.fromList $ map fst curPositions
       let newKeys = curKeys `S.difference` lastKeys
       let keysFromBefore = curKeys `S.difference` newKeys
       m <- return . M.fromList =<< mapM ( \ (k,pos) -> if k `S.member` keysFromBefore
                                                        then do (moving,lpos) <- mlookup k lastPositions
                                                                let moving' = lpos /= pos
                                                                when (moving' && not moving) $ 
                                                                    pushEventToPrim (Event "moving_start" [] M.empty) k
                                                                when (moving && not moving') $ 
                                                                    pushEventToPrim (Event "moving_end" [] M.empty) k
                                                                return (k,(moving', pos))
                                                        else return (k,(False,pos))) curPositions
       worldLastPositions =: m

handleObjectCollisions prims = do
        -- look at all non-phantom prims
        primBoxes <- mapM (\ (k,p) -> liftM ((,) k . (,) p) (getPrimBox k)) prims
        --let voldtct = map fst $ filter (primVolumeDetect . snd) prims
        voldtct <- liftM (map fst) $ flip filterM prims (\ (_,prim) -> 
            getM $ objectVolumeDetect.dyn (fromMaybe (getI primKey prim) (getI primParent prim)))
        let cmp x y = compare (fst x) (fst y)
        let curCollisions = S.fromList [ (k0,k1) | ((k0,(p0,_)),(k1,(p1,_))) <- checkIntersections (snd . snd) cmp primBoxes, 
                                                   getI primParent p0 /= Just k1 &&
                                                   getI primParent p1 /= Just k0 &&
                                                   not (getI primParent p0 == getI primParent p1 && isJust (getI primParent p0))]
        oldCollisions <- getM worldCollisions
        let formerCollisions = oldCollisions `S.difference` curCollisions
        let newCollisions = curCollisions `S.difference` oldCollisions
        toObjCollisions newCollisions "collision_start" >>= mapM_ (send "collision_start")
        (do
            objCollisions <- toObjCollisions curCollisions "collision"
            -- filter out volume detect objects, except for those objects that are attatched
            let attachment k = do
                    parent <- getM $ primParent.wprim k
                    case parent of
                        Nothing -> getM $ primAttachment.wprim k
                        Just ok -> getM $ primAttachment.wprim ok
            objCollisions' <- liftM (map fst . filter (\ ((k,_),mv) -> (k `notElem` voldtct) || isJust mv)) 
                    (mapM (\ v@(k,_) -> (liftM ((,) v) (attachment k))) objCollisions)
            mapM_ (send "collision") objCollisions')
        toObjCollisions formerCollisions "collision_end" >>= mapM_ (send "collision_end")
        worldCollisions =: curCollisions
    where send :: Monad m => String -> (LSLKey,[(Int,LSLKey)]) -> WorldE m ()
          send hn (pk,oinfo) =  collisionScripts >>= mapM_ (sendScript hn pk oinfo)
              where collisionScripts = filtMapM validScript =<< (getPrimScripts pk)
--                          liftM (map inventoryItemName) (getPrimScripts pk) >>= 
--                              filterM (\ sn -> scriptHasActiveHandler pk sn hn)
                    validScript itm@(InventoryItem { inventoryItemData = InvScript { invScriptLibId = lnm }}) = do
                        lib <- getM wscripts
                        case lookup lnm lib of
                           Just (Right _) -> do
                               let sn = inventoryItemName itm
                               p <- scriptHasActiveHandler pk sn hn
                               return $ if p then Just sn else Nothing
                           _ -> return Nothing
                    validScript _ = return Nothing
          sendScript :: Monad m => String -> LSLKey -> [(Int,LSLKey)] -> String -> WorldE m ()
          sendScript hn pk oinfo sn = do
              filt <- getM $ scriptCollisionFilter.lscript pk sn
              case filt of
                  (name,key,accept)
                          | null name && (null (unLslKey key) || nullKey == key) && accept -> pushit oinfo
                          | null name && (null (unLslKey key) || nullKey == key) && not accept -> return ()
                          | accept -> filterM (matchesIn name key) oinfo >>= pushit
                          | otherwise -> filterM (matchesOut name key) oinfo >>= pushit
              where matchesIn name key (_,k) = if k == key 
                                                   then return True 
                                                   else (if null (unLslKey key) || key == nullKey 
                                                             then (name==) <$> (getM $ primName.wprim k)
                                                             else return False)
                    matchesOut name key (_,k) = if k == key 
                                                    then return False 
                                                    else (if null (unLslKey key) || key == nullKey
                                                              then (not . (name==)) <$> (getM $ primName.wprim k)
                                                              else return True)
                    pushit oinfo = unless (null oinfo) $ pushEvent ev pk sn
                        where ev = Event hn [IVal $ length oinfo] 
                                      (M.fromList $
                                          zipWith (\ i (_,k) -> ("key_" ++ show i, KVal k)) [0..] oinfo ++
                                          zipWith (\ i (n,_) -> ("integer_" ++ show i, IVal n)) [0..] oinfo)
          toObjCollisions collisionsS hn = do
                  passed <- passes primColliders
                  colliders' <- mapM (\ (k,cs) -> (getPrimLinkNum k >>= \ i -> return (k,i,cs))) primColliders
                  colliders'' <- mapM pk2ok (colliders' ++ passed)
                  colliders''' <-  mapM (\ (k,i,cs) -> liftM ((,,) k i) (filterM (objectHasVolDetect >=> (return . not)) cs)) colliders''
                  return $ M.toList (foldl' combine M.empty (map dist colliders'''))
              where collisions = S.toList collisionsS
                    dist (pk,i,cs) = (pk, zip (repeat i) cs)
                    combine m (k,lv) = case M.lookup k m of
                            Nothing -> M.insert k lv m
                            Just lv' -> M.insert k (lv ++ lv') m
                    --prims = fromListOfPairs collisions
                    primColliders = map (\ k -> (k, collectColliders collisions k)) (S.toList $ fromListOfPairs collisions)
                    passes [] = return []
                    passes ((pk,cs):pcs) = do
                        passed <- pass hn pk
                        if passed
                            then do
                                parent <- getM (primParent.wprim pk) >>= maybe (throwError "can't get parent") return
                                rest <- passes pcs
                                num <- getPrimLinkNum pk
                                return ((parent,num,cs):rest)
                            else passes pcs
                    pk2ok (pk,i,cs) = liftM ((,,) pk i) (primsToObjects cs)
                    primsToObjects ks = liftM (nub . zipWith fromMaybe ks) 
                        (mapM (\ k -> getM $ primParent.wprim k) ks)
                    pass hn k = do
                        getM (primParent.wprim k) >>= maybe (return False) (pass')
                        where pass' pk = liftM2 (||) (getM $ primPassTouches.wprim k)
                                 (liftM not (k `primHasActiveHandler` hn))
                    fromListOfPairs [] = S.empty
                    fromListOfPairs ((x,y):ps) = (S.insert x . S.insert y) (fromListOfPairs ps)
                    collectColliders intsns pk = [ j | (Just j) <- map (other pk) intsns]
                    objectHasVolDetect ok = do
                         getM (primAttachment.wprim ok) >>=
                             maybe (getM $ objectVolumeDetect.dyn ok) 
                                 (const $ return False)
                    other k (x,y) | k == x = Just y
                                  | k == y = Just x
                                  | otherwise = Nothing

handleLandCollisions prims = do
        primBoxes <- mapM (\ (k,p) -> liftM ((,) k ) (getPrimBox k)) prims
        let curCollisions = S.fromList [ k | (k,((_,_,z),_)) <- primBoxes, z <= 0]
        oldCollisions <- getM worldLandCollisions
        let formerCollisions = oldCollisions `S.difference` curCollisions
        let newCollisions = curCollisions `S.difference` oldCollisions
        mapM_ (send "land_collision_end") $ S.toList formerCollisions
        mapM_ (send "land_collision") $ S.toList curCollisions
        mapM_ (send "land_collision_start") $ S.toList newCollisions
        worldLandCollisions =: curCollisions
    where send hn pk = do
                  pass <- getM $ primPassCollisions.wprim pk
                  pos <- getGlobalPos pk >>= ( \ (x,y,_) -> return (x,y,0))
                  liveScripts <- getActualPrimScripts pk >>= 
                      (\ ss -> return [ s | s@(_,script) <- ss, getI scriptActive script == True])
                  unless (null liveScripts) $ send' pos pk
                  when (null liveScripts || pass) $ getRootPrim pk >>= send' pos
              where send' pos k = do
                        scripts <- liftM (map inventoryItemName) (getPrimScripts k) >>=
                                   filterM (\ sn -> scriptHasActiveHandler k sn hn)
                        forM_ scripts (pushEvent (Event hn [vec2VVal pos] M.empty) k)
                     
handleCollisions :: (Monad m) => WorldE m ()
handleCollisions = runAndLogIfErr "can't handle collisions" () $ do
    prims <- getNonPhantomPrims
    handleObjectCollisions prims
    handleLandCollisions prims
    
getNonPhantomPrims :: (Monad m) => WorldE m [(LSLKey,Prim)]
getNonPhantomPrims = liftM (filter ((==0) . (.&. cStatusPhantom) . getI primStatus . snd) . M.assocs) (getM wprims)
   
handleTargets :: Monad m => WorldE m ()
handleTargets = do
    t:*t0 <- getM (tick.*worldTargetCheckTime)
    when (t - t0 > durationToTicks 0.1) $
        liftM M.keys (getM wobjects) >>= mapM_ (\ k ->
            runErrPrim k () $ do
                worldTargetCheckTime =: t
                pos <- getGlobalPos k
                rot <- getGlobalRot k
                scripts <- getActualPrimScripts k
                forM_ scripts (\ ((_,sn),script) -> do
                        forM_ (IM.toList $ getI scriptPositionTargets script) (\ (i,(target,range)) -> 
                            let image = getI scriptImage script
                                inRange = (dist3d2 target pos <= range^2) in
                            if hasActiveHandler image "at_target" && inRange
                                then pushEvent (Event "at_target" [IVal i, vec2VVal target, vec2VVal pos] M.empty) k sn
                                else when (hasActiveHandler image "not_at_target" && not inRange) $
                                         pushEvent (Event "not_at_target" [] M.empty) k sn
                            )
                        forM_ (IM.toList $ getI scriptRotationTargets script) (\ (i,(target,range)) ->
                            let image = getI scriptImage script
                                inRange = (angleBetween target rot <= range) in
                            if hasActiveHandler image "at_rot_target" && inRange
                                then pushEvent (Event "at_rot_target" [IVal i, rot2RVal target, rot2RVal rot] M.empty) k sn
                                else when (hasActiveHandler image "not_at_rot_target" && not inRange) $
                                         pushEvent (Event "not_at_rot_target" [] M.empty) k sn
                            )
                    )                        
                    
            )

simulate :: Monad m => WorldE m ()
simulate =
    do processEvents
       runScripts
       runPhysics
       handleTouches
       mn <- nextActivity
       t <- getM tick
       let t' = case mn of
                    Nothing -> t + 10
                    Just n -> min (t + 10) n
       tick =: t'
       pauseTime:*suspendInfo <- getM (nextPause.*worldSuspended)
       unless (t' >= pauseTime || isSuspended suspendInfo) simulate
    where isSuspended Nothing = False
          isSuspended _ = True

-- --------------------------------------------------------------------------------------------

data SimCommand = SimContinue { simCmdBreakpoints :: [Breakpoint], simCmdEvents :: [SimEvent] }
                | SimStep { simCmdBreakpoints :: [Breakpoint], simCmdEvents :: [SimEvent] }
                | SimStepOver { simCmdBreakpoints :: [Breakpoint], simCmdEvents :: [SimEvent] }
                | SimStepOut { simCmdBreakpoints :: [Breakpoint], simCmdEvents :: [SimEvent] } deriving (Show)

data SimStatus = SimEnded { simStatusMessage :: String, simStatusLog :: [LogMessage], simStatusState :: SimStateInfo } | 
                 SimInfo { simStatusEvents :: [SimEvent], simStatusLog :: [LogMessage], simStatusState :: SimStateInfo } |
                 SimSuspended { simStatusEvents :: [SimEvent], 
                                simStatusSuspendInfo :: ExecutionInfo Float,
                                simStatusLog :: [LogMessage],
                                simStatusState :: SimStateInfo } deriving (Show)

data SimStateInfo = SimStateInfo {
        simStateInfoTime :: Int,
        simStateInfoPrims :: [(LSLKey,String)],
        simStateInfoAvatars :: [(LSLKey,String)],
        simStateInfoScripts :: [(LSLKey,String)]
    } deriving (Show)
    
nullSimState = SimStateInfo 0 [] [] []

stateInfoFromWorld world =
    SimStateInfo {
        simStateInfoTime = getI tick world,
        simStateInfoPrims = map (getI primKey &&& getI primName) (M.elems $ getI wprims world),
        simStateInfoAvatars = map (\ (_,a) -> (getI avatarKey a, getI avatarName a)) (M.toList $ getI worldAvatars world),
        simStateInfoScripts = M.keys (getI worldScripts world)
    }
    
data SimInputEventDefinition m = SimInputEventDefinition { 
    simInputEventName :: String,
    simInputEventDescription :: String,
    simInputEventParameters :: [SimParam],
    simInputEventHandler :: String -> [LSLValue Float] -> WorldE m () }

data SimParam = SimParam { simParamName :: String, simParamDescription :: String,
                           simParamType :: SimParamType }
     deriving (Show)
data SimParamType = SimParamPrim | SimParamAvatar | SimParamLSLValue LSLType | SimParamRootPrim | SimParamKey |
                    SimParamScript
     deriving (Show)
     
initWorld def scripts lib = worldFromFullWorldDef newWorld def lib scripts

simStep init@(Left (worldDef, scripts, lib)) command =
--     let world = initWorld worldDef scripts lib in simStep (Right world) command
    case initWorld worldDef scripts lib of
        Left s -> -- initialization failed
            (SimEnded ("Error initializing: " ++ s) [LogMessage 0 LogError "sim" ("Error initializing: " ++ s)] nullSimState, init)
        Right world -> simStep (Right world) command
simStep (Right world) command =
    let t = _tick world
        events = simCmdEvents command
        newBreakpointManager = replaceBreakpoints (simCmdBreakpoints command) (_worldBreakpointManager world)
        slice = _sliceSize world
        newScripts = case getI worldSuspended world of
             Nothing -> _worldScripts world
             Just (k,s) ->
                 let Just script = M.lookup (k,s) (_worldScripts world) 
                     img = getI scriptImage script
                     stepMgr = stepManager img
                     img' = case command of
                                SimStep _ _ -> img { stepManager = setStepBreakpoint stepMgr }
                                SimStepOver _ _ -> img { stepManager = setStepOverBreakpoint stepMgr }
                                SimStepOut _ _ -> img { stepManager = setStepOutBreakpoint stepMgr }
                                _ -> img
                 in M.insert (k,s) (setI scriptImage img' script) (_worldScripts world)
        simEventToWorldEvent (SimEvent name args delay) = (t + delay, WorldSimEvent name args)
        wq' = putManyWQ (map simEventToWorldEvent events) (_wqueue world)
        world' = world { _wqueue = wq', 
                         _nextPause = t + slice,
                         _worldBreakpointManager = newBreakpointManager,
                         _worldScripts = newScripts }
        (_,world'') = runIdentity $ runStateT (evalWorldE simulate) world'
        log = _msglog world''
        outputEvents = reverse $ getI worldOutputQueue world''
        world''' = setI msglog [] . setI worldOutputQueue [] $ world'' in
        if _tick world''' < _maxTick world'''
            then case getI worldSuspended world''' of
                Nothing ->
                     (SimInfo { simStatusEvents = outputEvents, 
                                simStatusLog = log,
                                simStatusState = stateInfoFromWorld world''' }, Right world''')
                Just (k,s) ->
                     let Just script = M.lookup (k,s) (_worldScripts world''') -- TODO: non-exhaustive but MUST succeed
                         img = getI scriptImage script
                         (Suspended bp) = executionState img -- TODO: non-exhaustive but MUST succeed
                         executionInfo = ExecutionInfo (breakpointFile bp) (breakpointLine bp) (frameInfo img)
                     in
                     (SimSuspended { simStatusEvents = outputEvents, 
                                     simStatusSuspendInfo = executionInfo,
                                     simStatusLog = reverse log,
                                     simStatusState = stateInfoFromWorld world''' }, Right world''')
            else (SimEnded { simStatusMessage = "ended", 
                             simStatusLog = reverse log,
                             simStatusState = stateInfoFromWorld world''' }, Right world''')
        
-- Event Descriptions and Handlers -------------------------------------------
------------------------------------------------------------------------------
--checkArgs def args = do
checkEventArgs def args = 
    do
        when (length params /= length args) $ throwError "wrong number of parameters"
        mapM (uncurry checkEventArg) argList
    where params = simInputEventParameters def
          argList = map (\ p -> ( p , find (\ a -> simParamName p == simEventArgName a) args)) params
          
checkEventArg (SimParam _ _ SimParamPrim) (Just arg) = return $ KVal $ LSLKey (simEventArgValue arg)
checkEventArg (SimParam _ _ SimParamRootPrim) (Just arg) = return $ KVal $ LSLKey (simEventArgValue arg)
checkEventArg (SimParam _ _ SimParamScript) (Just arg) = return $ SVal (simEventArgValue arg)
checkEventArg (SimParam name _ SimParamAvatar) (Just arg) = return $ KVal $ LSLKey (simEventArgValue arg)
checkEventArg (SimParam name _ SimParamKey) (Just arg) = return $ KVal $ LSLKey (simEventArgValue arg)
checkEventArg (SimParam name _ (SimParamLSLValue t)) (Just arg) = 
    case evaluateExpression t (simEventArgValue arg) of
        Nothing -> throwError ("invalid " ++ lslTypeString t ++ " expression" )
        Just v -> return v
checkEventArg (SimParam name _ _) Nothing = throwError ("event argument " ++ name ++ " not found")

handleSimInputEvent def args = 
  case checkEventArgs def args of
      Left s -> logAMessage LogWarn "sim" s
      Right argValues -> simInputEventHandler def (simInputEventName def) argValues

mkTouchStartEvent (LSLKey pk) nd (LSLKey ak) ln = 
    WorldSimEvent "touch_start" [SimEventArg "Prim Key" pk, SimEventArg "num_detected" nd,
                                 SimEventArg "Avatar key" ak, SimEventArg "Link Number" (show ln)]
mkTouchEvent (LSLKey pk) nd (LSLKey ak) ln = 
    WorldSimEvent "touch" [SimEventArg "Prim Key" pk, SimEventArg "num_detected" nd, 
                           SimEventArg "Avatar key" ak, SimEventArg "Grab vector" "<0.0,0.0,0.0>",
                           SimEventArg "Link Number" (show ln)]
mkTouchEndEvent (LSLKey pk) nd (LSLKey ak) ln = 
    WorldSimEvent "touch_end" [SimEventArg "Prim Key" pk, SimEventArg "num_detected" nd,
                               SimEventArg "Avatar key" ak, SimEventArg "Link Number" (show ln)]
                                 
userTouchEventDef :: Monad m => SimInputEventDefinition m
userTouchEventDef =
    SimInputEventDefinition {
        simInputEventName = "Touch Prim",
        simInputEventDescription = "Avatar touches a prim for a duration",
        simInputEventParameters = [
            SimParam "Prim" "The prim the avatar should touch" SimParamPrim,
            SimParam "Avatar" "The avatar that should do the touching" SimParamAvatar,
            SimParam "Duration" "The duration of the touch (in seconds)" (SimParamLSLValue LLFloat)],
        simInputEventHandler = 
            let f _ [KVal pk, KVal ak, FVal duration] = runAndLogIfErr "can't find prim? " () $ do
                        -- should we pass touches?
                        prim <- getM $ wprim pk
                        linkNum <- getPrimLinkNum pk                        
                        needstouches <- liftM or $ mapM (primHasActiveHandler pk) ["touch","touch_start","touch_end"]
                        let touchList = [pk | needstouches] ++
                                        (if getI primPassTouches prim || not needstouches then maybe [] return (getI primParent prim) else [])
                        mapM_ (doTouch linkNum) touchList
                    where doTouch linkNum k = do
                            putWorldEvent 0 (mkTouchStartEvent k "1" ak linkNum)
                            let tdur = floor (1000 * duration)
                            let tTimes = map ((/1000.0) . fromIntegral) [1,(1 + 500) .. tdur]
                            mapM_ (flip putWorldEvent (mkTouchEvent pk "1" ak linkNum)) tTimes 
                            putWorldEvent duration (mkTouchEndEvent pk "1" ak linkNum)                             
                f name _ = logAMessage LogWarn "sim" ("invalid event activation: " ++ name)
            in f
    }
        
userChatEventDef :: Monad m => SimInputEventDefinition m
userChatEventDef =
    SimInputEventDefinition {
        simInputEventName = "Chat",
        simInputEventDescription = "Avatar chats",
        simInputEventParameters = [
            SimParam "Avatar" "The avatar that should do the chatting" SimParamAvatar,
            SimParam "Message" "What the avatar should say" (SimParamLSLValue LLString),
            SimParam "Channel" "The channel to chat on" (SimParamLSLValue LLInteger)],
        simInputEventHandler = 
            let f _ [KVal ak, SVal message, IVal chan] = do
                    mav <- optional $ getM $ wav ak
                    case mav of
                        Nothing -> logAMessage LogWarn "sim" ("avatar with key " ++ unLslKey ak ++ " not found")
                        Just av ->
                            putWorldEvent 0 $ Chat chan (getI avatarName av) ak message (getI avatarRegion av,getI avatarPosition av) sayRange
                f name _ = logAMessage LogWarn "sim" ("invalid event activation: " ++ name)
            in f
    }
        
eventDescriptors :: Monad m => Map String (SimInputEventDefinition m)
eventDescriptors = M.fromList $ mkEventDefList ([userTouchEventDef,userChatEventDef] ++ rawLslEventDescriptors)

mkEventDefList = map (\ e -> (simInputEventName e, e))

rawLslEventDescriptors :: Monad m => [SimInputEventDefinition m]
rawLslEventDescriptors = map lslEventDescriptorToSimInputDef lslEventDescriptors

lslEventDescriptorToSimInputDef (name, params, delivery, additionalData, description) =
    SimInputEventDefinition {
        simInputEventName = name,
        simInputEventDescription = "This is a raw LSL event: " ++ description,
        simInputEventParameters =
            (case delivery of
               EventDeliveryScript -> [SimParam "Script" "name of script to deliver to" SimParamScript,
                                       SimParam "Prim Key" "key of prim to deliver to" SimParamPrim]
               EventDeliveryObject -> [SimParam "Object Key" "key of object to deliver to" SimParamRootPrim]
               EventDeliveryRoot -> [SimParam "Object Key" "key of object to deliver to" SimParamRootPrim]
               EventDeliveryPrim -> [SimParam "Prim Key" "key of prim to deliver to" SimParamPrim]
               ) ++
            flip map additionalData (\ d -> case d of
                EventAdditionalInts name description -> SimParam name description (SimParamLSLValue LLInteger)
                EventAdditionalKeys name description -> SimParam name description SimParamKey
                EventAdditionalAvatarKeys name description -> SimParam name description SimParamAvatar
                EventAdditionalVectors name description -> SimParam name description (SimParamLSLValue LLVector)) ++
            map ( \ (t,name) -> SimParam name "" (SimParamLSLValue t)) params,
        simInputEventHandler =
            let f _ list = 
                    let lenAddl = length additionalData
                        (df,rest) = case (delivery,list) of
                               (EventDeliveryScript,SVal sn:KVal pk:vals) -> (\ e -> pushEvent e pk sn,vals)
                               (EventDeliveryObject,KVal key:vals) -> (\ e -> pushEventToObject e key,vals)
                               (EventDeliveryRoot, KVal key:vals) -> ((\ e -> pushEventToPrim e key), vals)
                               (EventDeliveryPrim, KVal key:vals) -> ((\ e -> pushEventToPrim e key), vals)
                               _ -> (\ _ -> logAMessage LogWarn "sim" "invalid user event - bad script address",[])
                    in df (Event name (drop lenAddl rest) (M.fromList (zipWith mkInfo additionalData (take lenAddl rest))) )
                       where mkInfo (EventAdditionalVectors _ _) val = ("vector_0",val)
                             mkInfo (EventAdditionalInts _ _)    val = ("integer_0",val)
                             mkInfo _                            val = ("key_0",val)
            in f
    }
