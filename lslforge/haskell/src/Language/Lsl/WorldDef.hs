{-# LANGUAGE FlexibleContexts, FlexibleInstances, TypeSynonymInstances,
             MultiParamTypeClasses, GeneralizedNewtypeDeriving,
             NoMonomorphismRestriction #-}
{-# OPTIONS_GHC -fwarn-unused-binds -fwarn-unused-imports #-}
module Language.Lsl.WorldDef(
    Avatar(..),
    AvatarControlListener(..),
    Prim(..),
    PrimFace(..),
    TextureInfo(..),
    Flexibility(..),
    LightInfo(..),
    LSLObject(..),
    ObjectDynamics(..),
    PositionTarget(..),
    RotationTarget(..),
    PrimType(..),
    FullWorldDef(..),
    Region(..),
    Parcel(..),
    InventoryItemIdentification(..),
    InventoryItem(..),
    InventoryInfo(..),
    InventoryItemData(..),
    Script(..),
    ScriptId,
    Attachment(..),
    CameraParams(..),
    Email(..),
    WebHandling(..),
    ItemPermissions(..),
    -- imported types
    LSLValue(..),
    Event(..),
    ScriptImage,
    defaultAvatar,
    defaultCamera,
    defaultDynamics,
    isInvScriptItem,
    isInvBodyPartItem,
    isInvGestureItem,
    isInvClothingItem,
    isInvTextureItem,
    isInvSoundItem,
    isInvAnimationItem,
    isInvLandmarkItem,
    isInvNotecardItem,
    isInvObjectItem,
    inventoryInfoPermValue,
    defaultInventoryPermissions,
    inventoryItemName,
    inventoryItemNames,
    scriptInventoryItem,
    findByInvName,
    findByInvKey,
    emptyPrim,
    primPhantomBit,
    primPhysicsBit,
    sortByInvName,
    worldFromFullWorldDef,
    defaultRegions,
    mkScript,
    world,
    worldXMLAccept
    ) where

import Control.Monad(when,foldM)
import Control.Monad.Except(MonadError(..))
import Control.Monad.State(evalState)
import Control.Monad.Writer(tell,lift,runWriterT)

import qualified Control.Monad.State as SM(get,put,State)
import Data.List(find,sortBy)
import qualified Data.Map as M
import qualified Data.IntMap as IM
import Data.Maybe(isNothing)

import Language.Lsl.Syntax(CodeErrs(CodeErrs))
import Language.Lsl.Internal.DOMProcessing(req,opt,def,val,text,elist,
    liste,choicet,MonadXMLAccept(..),AcceptT(..),
    xmlAcceptT)
import Language.Lsl.Internal.Evaluation(Event(..))
import Language.Lsl.Internal.Exec(ScriptImage,initLSLScript)
import Language.Lsl.Internal.Key(mkKey,nullKey,LSLKey(..))
import Language.Lsl.Internal.Type(LSLValue(..))
import Language.Lsl.Internal.Util(
    mlookup,Permutation3(..),rotationsToQuaternion,LSLInteger)

type ScriptId = (LSLKey,String)

data FullWorldDef = FullWorldDef {
    fullWorldDefMaxTime :: LSLInteger,
    fullWorldDefSliceSize :: LSLInteger,
    fullWorldDefWebHandling :: WebHandling,
    fullWorldDefEventHandler :: Maybe String,
    fullWorldDefObjects :: [LSLObject],
    fullWorldDefPrims :: [Prim],
    fullWorldDefAvatars :: [Avatar],
    fullWorldDefRegions :: [((LSLInteger,LSLInteger),Region)],
    fullWorldDefInitialKeyIndex :: Integer } deriving (Show)

data WebHandling =
     WebHandlingByFunction
   | WebHandlingByDoingNothing
   | WebHandlingByInternet { webHandlingTimeout :: Float } deriving (Show)

data LSLObject = LSLObject {
    _primKeys :: [LSLKey],
    _dynamics :: !ObjectDynamics } deriving (Show)

data ObjectDynamics = ObjectDynamics {
    _objectPosition :: (Float,Float,Float),
    _objectRotation :: (Float,Float,Float,Float),
    _objectVelocity :: (Float,Float,Float),
    _objectForce :: ((Float,Float,Float),Bool),
    _objectBuoyancy :: Float,
    _objectImpulse :: (((Float,Float,Float),Bool),LSLInteger),
    _objectTorque :: ((Float,Float,Float),Bool),
    _objectRotationalImpulse :: (((Float,Float,Float),Bool),LSLInteger),
    _objectOmega :: (Float,Float,Float),
    _objectPositionTarget :: !(Maybe PositionTarget),
    _objectRotationTarget :: !(Maybe RotationTarget),
    _objectVolumeDetect :: Bool
    } deriving (Show)

defaultDynamics = ObjectDynamics {
    _objectPosition = (0,0,0),
    _objectRotation = (0,0,0,1),
    _objectVelocity = (0,0,0),
    _objectForce = ((0,0,0),False),
    _objectBuoyancy = 0,
    _objectImpulse = (((0,0,0),False),0),
    _objectTorque = ((0,0,0),False),
    _objectRotationalImpulse = (((0,0,0),False),0),
    _objectOmega = (0,0,0),
    _objectPositionTarget = Nothing,
    _objectRotationTarget = Nothing,
    _objectVolumeDetect = False }
data PositionTarget =
      Repel {
        positionTargetTau :: Float,
        positionTargetOverWater :: Bool,
        positionTargetHeight :: Float }
    | Hover {
        positionTargetTau :: Float,
        positionTargetOverWater :: Bool,
        positionTargetHeight :: Float }
    | PositionTarget {
        positionTargetTau :: Float,
        positionTargetLocation :: (Float,Float,Float),
        positionTargetSetBy :: ScriptId }
    deriving (Show)

data RotationTarget = RotationTarget {
    rotationTarget :: (Float,Float,Float,Float),
    rotationTargetStrength :: Float,
    rotationTargetTau :: Float } deriving (Show)

data Avatar = Avatar {
    _avatarKey :: LSLKey,
    _avatarName :: String,
    _avatarActiveGroup :: Maybe LSLKey,
    _avatarRegion :: (LSLInteger,LSLInteger),
    _avatarPosition :: (Float,Float,Float),
    _avatarRotation :: (Float,Float,Float,Float),
    _avatarHeight :: Float,
    _avatarState :: LSLInteger,
    _avatarInventory :: [InventoryItem],
    _avatarCameraPosition :: (Float,Float,Float),
    _avatarCameraRotation :: (Float,Float,Float,Float),
    _avatarCameraControlParams :: CameraParams,
    _avatarActiveAnimations :: [(Maybe LSLInteger,LSLKey)],
    _avatarAttachments :: IM.IntMap LSLKey,
    _avatarEventHandler :: !(Maybe (String,[(String,LSLValue Float)])),
    _avatarControls :: !LSLInteger,
    _avatarControlListener :: !(Maybe AvatarControlListener) } deriving (Show)

data AvatarControlListener = AvatarControlListener {
    avatarControlListenerMask :: !LSLInteger,
    avatarControlListenerScript :: !(LSLKey,String) } deriving (Show)

data CameraParams = CameraParams {
    cameraActive :: Bool,
    cameraBehindednessAngle :: Float,
    cameraBehindednessLag :: Float,
    cameraDistance :: Float,
    cameraFocus :: Maybe (Float,Float,Float),
    cameraFocusLag :: Float,
    cameraFocusLocked :: Bool,
    cameraFocusOffset :: (Float,Float,Float),
    cameraFocusThreshold :: Float,
    cameraPitch :: Float,
    cameraPosition :: Maybe (Float,Float,Float),
    cameraPositionLag :: Float,
    cameraPositionLocked :: Bool,
    cameraPositionThreshold :: Float } deriving Show
defaultCamera = CameraParams {
    cameraActive = False,
    cameraBehindednessAngle = 10.0,
    cameraBehindednessLag = 0,
    cameraDistance = 3.0,
    cameraFocus = Nothing,
    cameraFocusLag = 0,
    cameraFocusLocked = False,
    cameraFocusOffset = (0,0,0),
    cameraFocusThreshold = 1.0,
    cameraPitch = 0.0,
    cameraPosition = Nothing,
    cameraPositionLag = 0.1,
    cameraPositionLocked = False,
    cameraPositionThreshold = 1 }

defaultAvatar key = Avatar {
        _avatarKey = key,
        _avatarName = "Default Avatar",
        _avatarActiveGroup = Nothing,
        _avatarRegion = (0,0),
        _avatarPosition = (128.0,128.0,0.0),
        _avatarRotation = (0.0, 0.0, 0.0, 1.0),
        _avatarHeight = 2,
        _avatarState = 0,
        _avatarInventory = [],
        _avatarCameraPosition = (128.0,128.0,0.0),
        _avatarCameraRotation = (0,0,0,1),
        _avatarCameraControlParams = defaultCamera,
        _avatarActiveAnimations = [],
        _avatarAttachments = IM.empty,
        _avatarEventHandler = Nothing,
        _avatarControls = 0,
        _avatarControlListener = Nothing }

-- these are bit INDEXES not MASKS (0 == least significant bit)
primPhantomBit :: LSLInteger
primPhantomBit = 4
primPhysicsBit :: LSLInteger
primPhysicsBit = 0

data InventoryItemData =
      InvScript { invScriptLibId :: String, invScriptState :: Maybe (ScriptImage Float) }
    | InvBodyPart
    | InvGesture
    | InvClothing
    | InvTexture
    | InvSound { invSoundDuration :: Float }
    | InvAnimation { invAnimationDuration :: Maybe Float }
    | InvLandmark { invLandmarkLocation :: ((LSLInteger,LSLInteger),(Float,Float,Float)) }
    | InvNotecard { invNotecardLines :: [String] }
    | InvObject { invObjectPrims :: [Prim] } deriving (Show)

isInvScript (InvScript _ _) = True
isInvScript _ = False
isInvBodyPart InvBodyPart = True
isInvBodyPart _ = False
isInvGesture InvGesture = True
isInvGesture _ = False
isInvClothing InvClothing = True
isInvClothing _ = False
isInvTexture InvTexture = True
isInvTexture _ = False
isInvSound (InvSound _) = True
isInvSound _ = False
isInvAnimation (InvAnimation _) = True
isInvAnimation _ = False
isInvLandmark (InvLandmark _) = True
isInvLandmark _ = False
isInvNotecard (InvNotecard _) = True
isInvNotecard _ = False
isInvObject (InvObject _) = True
isInvObject _ = False

isInvScriptItem = isInvScript . inventoryItemData
isInvBodyPartItem = isInvBodyPart . inventoryItemData
isInvGestureItem = isInvGesture . inventoryItemData
isInvClothingItem = isInvClothing . inventoryItemData
isInvTextureItem = isInvTexture . inventoryItemData
isInvSoundItem = isInvSound . inventoryItemData
isInvAnimationItem = isInvAnimation . inventoryItemData
isInvLandmarkItem = isInvLandmark . inventoryItemData
isInvNotecardItem = isInvNotecard . inventoryItemData
isInvObjectItem = isInvObject . inventoryItemData

newtype InventoryItemIdentification = InventoryItemIdentification {
    inventoryItemNameKey :: (String,LSLKey) } deriving (Show)
data InventoryInfo  = InventoryInfo {
    inventoryInfoCreator :: LSLKey,
    inventoryInfoPerms :: ItemPermissions } deriving (Show)
data InventoryItem = InventoryItem {
    inventoryItemIdentification :: InventoryItemIdentification,
    inventoryItemInfo :: InventoryInfo,
    inventoryItemData :: InventoryItemData } deriving (Show)

data ItemPermissions = ItemPermissions {
        permMaskBase :: LSLInteger,
        permMaskOwner :: LSLInteger,
        permMaskGroup :: LSLInteger,
        permMaskEveryone :: LSLInteger,
        permMaskNext :: LSLInteger
    } deriving Show

inventoryItemName = fst . inventoryItemNameKey . inventoryItemIdentification
invnetoryItemKey = snd . inventoryItemNameKey . inventoryItemIdentification
inventoryItemNames = map inventoryItemName
scriptInventoryItem s k id =
    InventoryItem (InventoryItemIdentification (s,k))
        (InventoryInfo nullKey defaultInventoryPermissions) (InvScript id Nothing)
findByInvName name = find ((== name) . inventoryItemName)
findByInvKey key = find ((== key) . invnetoryItemKey)

sortByInvName = sortBy (\ i i' -> compare (inventoryItemName i) (inventoryItemName i'))

inventoryInfoPermValue 0 = return . permMaskBase
inventoryInfoPermValue 1 = return . permMaskOwner
inventoryInfoPermValue 2 = return . permMaskGroup
inventoryInfoPermValue 3 = return . permMaskEveryone
inventoryInfoPermValue 4 = return . permMaskNext
inventoryInfoPermValue i =
    const (throwError ("no such perm mask - " ++ show i))

defaultInventoryPermissions =
    ItemPermissions (-1) (-1) (-1) (-1) (-1)

data Prim = Prim {
    _primName :: String,
    _primKey :: LSLKey,
    _primParent :: Maybe LSLKey,
    _primDescription :: String,
    _primInventory :: [InventoryItem],
    _primOwner :: LSLKey,
    _primGroup :: Maybe LSLKey,
    _primCreator :: LSLKey,
    _primPosition :: (Float, Float, Float),
    _primRotation :: (Float, Float, Float, Float),
    _primScale :: (Float, Float, Float),
    _primFaces :: [PrimFace],
    _primFlexibility :: Maybe Flexibility,
    _primMaterial :: LSLInteger,
    _primStatus :: LSLInteger,
    _primVehicleFlags :: LSLInteger,
    _primLight :: Maybe LightInfo,
    _primTempOnRez :: Bool,
    _primTypeInfo :: PrimType,
    _primPermissions :: [LSLInteger],
    _primAllowInventoryDrop :: Bool,
    _primSitTarget :: Maybe ((Float,Float,Float),(Float,Float,Float,Float)),
    _primSittingAvatar :: Maybe LSLKey,
    _primPendingEmails :: [Email],
    _primPassTouches :: Bool,
    _primPassCollisions :: Bool,
    _primPayInfo :: (LSLInteger,LSLInteger,LSLInteger,LSLInteger,LSLInteger),
    _primAttachment :: Maybe Attachment,
    _primRemoteScriptAccessPin :: LSLInteger } deriving (Show)

data PrimType =
    PrimType {
       _primVersion :: LSLInteger, -- 1 or 9
       _primTypeCode :: LSLInteger,
       _primHoleshape :: LSLInteger,
       _primCut :: (Float,Float,Float),
       _primTwist :: (Float,Float,Float),
       _primHolesize :: (Float,Float,Float),
       _primTopshear :: (Float, Float, Float),
       _primHollow :: Float,
       _primTaper :: (Float,Float,Float),
       _primAdvancedCut :: (Float,Float,Float),
       _primRadiusOffset :: Float,
       _primRevolutions :: Float,
       _primSkew :: Float,
       _primSculptTexture :: Maybe String,
       _primSculptType :: LSLInteger
    } deriving (Show)

basicBox = PrimType 9 0 0 (0,1,0) (0,0,0) (0,0,0) (0,0,0) 0 (0,0,0) (0,1,0) 0 0 0 Nothing 0

data Attachment = Attachment {
    _attachmentKey :: LSLKey,
    _attachmentPoint :: LSLInteger } deriving (Show)

data LightInfo = LightInfo {
    _lightColor :: (Float,Float,Float),
    _lightIntensity :: Float,
    _lightRadius :: Float,
    _lightFalloff :: Float
    } deriving (Show)

data Flexibility = Flexibility {
    _flexSoftness :: LSLInteger,
    _flexGravity :: Float,
    _flexFriction :: Float,
    _flexWind :: Float,
    _flexTension :: Float,
    _flexForce :: (Float,Float,Float)
    } deriving (Show)

data PrimFace = PrimFace {
    _faceAlpha :: Float,
    _faceColor :: (Float,Float,Float),
    _faceShininess :: LSLInteger,
    _faceBumpiness :: LSLInteger,
    _faceFullbright :: Bool,
    _faceTextureMode :: LSLInteger,
    _faceTextureInfo :: TextureInfo
    } deriving (Show)

defaultFace = PrimFace 1.0 (1.0,1.0,1.0) 0 0 False 0 defaultTextureInfo

data TextureInfo = TextureInfo {
    _textureKey :: LSLKey,
    _textureRepeats :: (Float,Float,Float),
    _textureOffsets :: (Float,Float,Float),
    _textureRotation :: Float
    } deriving (Show)

defaultTextureInfo = TextureInfo nullKey (1.0,1.0,0.0) (0.0,0.0,0.0) 0.0

data Email = Email {
    emailSubject :: String,
    emailAddress :: String, -- sender address
    emailMessage :: String,
    emailTime :: LSLInteger } deriving (Show)

emptyPrim name key = Prim {
    _primName = name,
    _primKey = key,
    _primParent = Nothing,
    _primDescription = "",
    _primInventory = [],
    _primOwner = nullKey,
    _primGroup = Nothing,
    _primCreator = nullKey,
    _primPosition = (0.0,0.0,0.0),
    _primRotation = (0.0,0.0,0.0,1.0),
    _primScale = (1.0,1.0,1.0),
    _primFaces = replicate 6 defaultFace,
    _primFlexibility = Nothing,
    _primMaterial = 0,
    _primStatus = 0x0e,
    _primVehicleFlags = 0,
    _primLight = Nothing,
    _primTempOnRez = False,
    _primTypeInfo = basicBox,
    _primPermissions = [0],
    _primAllowInventoryDrop = False,
    _primSitTarget = Nothing,
    _primSittingAvatar = Nothing,
    _primPendingEmails = [],
    _primPassTouches = False,
    _primPassCollisions = False,
    _primPayInfo = ( -2, -2, -2, -2, -2),
    _primAttachment = Nothing,
    _primRemoteScriptAccessPin = 0 }

data Region = Region {
    regionName :: String,
    regionFlags :: LSLInteger,
    regionParcels :: [Parcel]
    } deriving (Show)

data Parcel = Parcel {
    parcelName :: String,
    parcelDescription :: String,
    parcelBoundaries :: (LSLInteger,LSLInteger,LSLInteger,LSLInteger), -- bottom, top, left, right aka south,north,west,east
    parcelOwner :: LSLKey,
    parcelFlags :: LSLInteger,
    parcelBanList :: [(LSLKey,Maybe LSLInteger)],
    parcelPassList :: [(LSLKey,Maybe LSLInteger)]
    } deriving (Show)

defaultRegions :: LSLKey -> [((LSLInteger,LSLInteger),Region)]
defaultRegions owner =
    [(
        (0,0),
        Region {
            regionName = "Region_0_0", regionFlags = 0,
            regionParcels =
                [Parcel "parcel_0" "default parcel" (0,256,0,256) owner
                    (-1) [] []] }
    )]

data Script = Script {
    _scriptImage :: !(ScriptImage Float),
    _scriptActive :: Bool,
    _scriptPermissions :: M.Map LSLKey LSLInteger,
    _scriptLastPerm :: Maybe LSLKey,
    _scriptStartTick :: LSLInteger,
    _scriptLastResetTick :: LSLInteger,
    _scriptEventQueue :: [Event Float],
    _scriptStartParameter :: LSLInteger,
    _scriptCollisionFilter :: !(String,LSLKey,Bool),
    _scriptTargetIndex :: !LSLInteger,
    _scriptPositionTargets :: !(IM.IntMap ((Float,Float,Float), Float)),
    _scriptRotationTargets :: !(IM.IntMap ((Float,Float,Float,Float), Float)),
    _scriptControls :: ![LSLKey] } deriving (Show)

mkScript img = Script {
    _scriptImage = img,
    _scriptActive = True,
    _scriptPermissions = M.empty,
    _scriptLastPerm = Nothing,
    _scriptStartTick = 0,
    _scriptLastResetTick = 0,
    _scriptEventQueue = [Event "state_entry" [] M.empty],
    _scriptStartParameter = 0,
    _scriptCollisionFilter = ("",nullKey,True),
    _scriptTargetIndex = 0,
    _scriptPositionTargets = IM.empty,
    _scriptRotationTargets = IM.empty,
    _scriptControls = [] }

worldFromFullWorldDef worldBuilder fwd lib scripts = do
    let primMap = mkPrimMap (fullWorldDefPrims fwd)
    -- prove all the prims in all the objects exists
    primMap' <- checkObjects primMap (fullWorldDefObjects fwd)
    let filt prim = filter isInvScriptItem $ _primInventory prim
    let item2Script prim item = ((_primKey prim, inventoryItemName item),
            invScriptLibId $ inventoryItemData item)
    let activeScripts = concatMap
            (\ prim -> map (item2Script prim) $ filt prim) $ M.elems primMap'
    (activatedScripts,log) <-
        runWriterT (activateScripts activeScripts scripts primMap')
    return $ worldBuilder
        (fullWorldDefSliceSize fwd)
        (fullWorldDefMaxTime fwd)
        [] lib scripts
        (M.fromList $ mkAvatarLookup (fullWorldDefAvatars fwd))
        (mkObjectMap (fullWorldDefObjects fwd))
        primMap'
        (M.fromList activatedScripts)
        (M.fromList (fullWorldDefRegions fwd))
        (fullWorldDefInitialKeyIndex fwd)
        (fullWorldDefWebHandling fwd)
        (fullWorldDefEventHandler fwd)
        log

fctx :: MonadError String m => String -> Either String a -> m a
fctx s (Left s') = throwError s
fctx _ (Right v) = return v

mkPrimMap prims = M.fromList [(_primKey p, p) | p <- prims]

mkObjectMap objects =
    M.fromList [ (p, o) | o@(LSLObject { _primKeys = (p:_) }) <- objects ]
mkAvatarLookup avatars = [ (_avatarKey a,a) | a <- avatars]

checkObject primMap o =
    foldM checkPrim primMap (_primKeys o)
    where
        root = head (_primKeys o)
        checkPrim m k =
            case M.lookup k m of
                Nothing -> fail ("prim " ++ show k ++ " not found in definition")
                Just prim ->
                    return (if (k == root)
                        then m
                        else M.insert k (prim { _primParent = Just root }) m)

checkObjects primMap os = foldM checkObject primMap os

activateScripts scriptIdInfo compiledScripts primMap =
    mapM (activateScript compiledScripts primMap) scriptIdInfo >>=
        (\ ms -> return [ s | Just s <- ms ])

activateScript scripts primMap (k@(primKey,invName),(scriptID)) = do
    let script = case lookup scriptID scripts of
             Nothing -> fail "script not found"
             Just v -> v
    prim <- (lift . fctx ("looking up prim " ++ unLslKey primKey ++ " failed"))
        (mlookup primKey primMap)
    when (isNothing (findByInvName invName (_primInventory prim))) $
        fail (invName ++ " doesn't exist in prim " ++ unLslKey primKey)
    case script of
        Left (CodeErrs ((_,s):_)) ->
            tell [("script \"" ++ invName ++ "\" in prim " ++ unLslKey primKey ++
                  " failed to activate because of error: " ++ s)]
            >> return Nothing
        Right code -> return $ Just (k,mkScript $ initLSLScript code)

world = do
    handler <- opt "simEventHandler" text
    avs <- req "avatars" avatars
    ps <- req "prims" prims
    let webHandling = maybe WebHandlingByDoingNothing
            (const WebHandlingByFunction) handler
    FullWorldDef <$> req "maxTime" val <*> req "sliceSize" val
        <*> pure webHandling <*> pure handler <*> req "objects" objects
        <*> pure ps <*> pure avs
        <*> pure (defaultRegions nullKey) <*> (snd <$> get')

objects = liste "object" object

object = do
    keys <- mapM findRealKey =<< req "primKeys" (liste "string" text)
    position <- dvec0 "position"
    LSLObject <$> pure keys <*> pure defaultDynamics { _objectPosition = position }

vec = (,,) <$> req "x" val <*> req "y" val <*> req "z" val
region = (,) <$> req "x" val <*> req "y" val

prims = liste "prim" prim

prim = do
    key <- req "key" text
    scripts <- req "scripts" (liste "script" script)
    inventory <- def "inventory" [] (elist invItem)
    owner <- findRealKey =<< def "owner" "" text
    rotation <- rotationsToQuaternion P123 <$> dvec0 "rotation"
    Prim <$> req "name" text <*> newKey (Just key) <*> pure Nothing
        <*> def "description" "" text <*> pure (scripts ++ inventory)
        <*> pure owner <*> pure Nothing <*> pure owner
        <*> def "position" (128,128,0) vec <*> pure rotation <*> dvec1 "scale"
        <*> def "faces" (replicate 6 defaultFace) (liste "face" primFace)
        <*> opt "flexibility" flexibility <*> dval "material" 0
        <*> dval "status" 0x0e <*> pure 0 <*> opt "light" lightInfo
        <*> dval "tempOnRez" False <*> def "typeInfo" basicBox primType
        <*> def "permissions"
            [0x0008e000, 0x0008e000, 0x0008c000, 0x00080000, 0x00082000]
                 (liste "int" val)
        <*> dval "dropAllowed" False <*> pure Nothing <*> pure Nothing
        <*> pure [] <*> pure False <*> pure False <*> pure (-2,-2,-2,-2,-2)
        <*> pure Nothing <*> pure 0



invItem = choicet $ map (fmap inventoryItem)
     [("notecardItem",invNotecard),("textureItem", invTexture),
      ("bodyPartItem", invBodyPart), ("clothingItem", invClothing),
      ("gestureItem", invGesture), ("soundItem", invSound),
      ("animationItem", invAnimation), ("inventoryObjectItem", invObject),
      ("landmarkItem", invLandmark)]

invNotecard = InvNotecard <$> req "lines" (liste "string" text)

invTexture = return InvTexture
invBodyPart = return InvBodyPart
invClothing = return InvClothing
invGesture = return InvGesture
invSound = InvSound <$> req "duration" val
invAnimation = InvAnimation <$> req "duration"
    (val >>= \ v -> return $ if v == 0 then Nothing else Just v)

invObject = InvObject <$> req "prims" prims
invLandmark = curry InvLandmark <$> req "region" region <*> req "position" vec

inventoryItem f = do
    id <- curry InventoryItemIdentification <$> req "name" text
        <*> newKey Nothing
    info <- InventoryInfo <$> (LSLKey <$> req "creator" text)
        <*> def "perms" defaultInventoryPermissions itemPermissions -- pure defaultInventoryPermissions
    findRealKey $ unLslKey $ inventoryInfoCreator info
    InventoryItem id info <$> f

itemPermissions = ItemPermissions <$> req "base" val
    <*> req "owner" val <*> req "group" val <*> req "everyone" val
    <*> req "next" val

script = scriptInventoryItem <$> req "scriptName" text <*> newKey Nothing
    <*> req "scriptId" text

primFace = PrimFace <$> dval "alpha" 0 <*> dvec1 "color"
    <*> dval "shininess" 0 <*> dval "bumpiness" 0 <*> dval "fullbright" False
    <*> dval "textureMode" 0
    <*> def "textureInfo" defaultTextureInfo textureInfo

textureInfo = TextureInfo <$> (LSLKey <$> def "name" "" text) <*> dvec1 "repeats"
    <*> dvec0 "offsets" <*> dval "rotation" 0

flexibility = Flexibility <$> dval "softness" 0 <*> dval "gravity" 1
    <*> dval "friction" 0 <*> dval "wind" 0 <*> dval "tension" 1.0
    <*> dvec0 "force"

lightInfo = LightInfo <$> dvec1 "color" <*> dval "intensity" 1.0
    <*> dval "radius" 10.0 <*> dval "falloff" 1.0

primType =
    PrimType <$> dval "version" 9 <*> dval "typeCode" 0
        <*> def "holeshape" 0 val <*> dvec0 "cut" <*> dvec0 "twist"
        <*> dvec0 "holesize" <*> dvec0 "topshear" <*> dval "hollow" 0
        <*> dvec0 "taper" <*> dvec0 "advancedCut" <*> dval "radiusOffset" 0
        <*> dval "revolutions" 0 <*> dval "skew" 0 <*> opt "sculptTexture" text
        <*> dval "scupltType" 0

dvec0 s = def s (0,0,0) vec
dvec1 s = def s (1,1,1) vec
dval s v = def s v val

avatars = liste "avatar" avatar

avatar = do
    (name,x,y,z,handlerName) <- (,,,,) <$> req "name" text
        <*> req "xPos" val <*> req "yPos" val <*> req "zPos" val
        <*> opt "avatarEventHandler" text
    key <- newKey (Just name)
    return $ (defaultAvatar key) {
        _avatarName = name,
        _avatarPosition = (x,y,z),
        _avatarCameraPosition = (x,y,z),
        _avatarEventHandler = fmap (flip (,) []) handlerName }

findRealKey k = fst <$> get' >>= mlookup k
newKey xref = do
    (m,i) <- get'
    let k = mkKey i
    let m' = maybe m (\ v -> M.insert v k m) xref
    put' (m',i + 1)
    return k

worldXMLAccept s a = evalState (((xmlAcceptT . unWorldXMLAccept) a) s) (M.empty,1)

newtype WorldXMLAccept a = WorldXMLAccept { unWorldXMLAccept :: AcceptT (SM.State (M.Map String LSLKey, Integer)) a }
    deriving (Monad,Applicative,Functor,MonadXMLAccept)

get' = WorldXMLAccept $ lift $ SM.get
put' v = WorldXMLAccept $ lift $ SM.put v

instance MonadError String WorldXMLAccept where
    throwError e = WorldXMLAccept { unWorldXMLAccept = throwError e }
    catchError v f = WorldXMLAccept { unWorldXMLAccept = catchError (unWorldXMLAccept v) (unWorldXMLAccept . f) }
