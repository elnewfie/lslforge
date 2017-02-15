{-# OPTIONS_GHC -XQuasiQuotes -fwarn-unused-binds -fwarn-unused-matches #-}
module Lsl.SimTests where

import Data.Bits
import Data.List
import Data.Maybe

import Control.Monad.Identity
import Debug.Trace
import Language.Lsl.Internal.BuiltInModules(avEventGen)
import Language.Lsl.Internal.Constants
import Language.Lsl.Internal.Key
import Language.Lsl.Internal.Log
import Language.Lsl.Syntax
import Language.Lsl.Parse(parseScriptFromString,parseModuleFromString)
import Language.Lsl.QQ(lsl,lslm)
import Language.Lsl.Internal.Type
import Language.Lsl.Sim
import Language.Lsl.WorldDef

import Language.Lsl.Internal.Optimize

import System.FilePath

import Test.HUnit hiding (State,Label)

pk1 = unLslKey $ mkKey 1
av = unLslKey $ mkKey 2
pk2 = unLslKey $ mkKey 3
s1 = unLslKey $ mkKey 5
s2 = unLslKey $ mkKey 6
n1 = unLslKey $ mkKey 7

initialKey = mkKey 10

prim1 = (emptyPrim "prim" $ LSLKey pk1) {
    _primOwner = LSLKey av,
    _primInventory = [
        scriptInventoryItem "script" (LSLKey s1) "script",
        InventoryItem {
            inventoryItemIdentification = InventoryItemIdentification ("notecard", LSLKey n1),
            inventoryItemInfo = InventoryInfo (LSLKey av) defaultInventoryPermissions,
            inventoryItemData = InvNotecard ["hello world", "how are you?"]}],
    _primPosition = (0,0,0),
    _primName = "test name", _primDescription = "test description",
    _primPermissions = [0x0008e000, 0x0008e000, 0x0008c000, 0x00080000, 0x00082000] }

prim1A = (emptyPrim "prim" $ LSLKey pk1)
    { _primOwner = LSLKey av,
      _primInventory = [
          scriptInventoryItem "script" (LSLKey s1) "script",
          InventoryItem {
              inventoryItemIdentification = InventoryItemIdentification ("notecard", LSLKey n1),
              inventoryItemInfo =
                  InventoryInfo (LSLKey av) defaultInventoryPermissions { permMaskOwner = complement cPermCopy },
              inventoryItemData = InvNotecard ["hello world", "how are you?"]}],
      _primPosition = (0,0,0),
      _primName = "test name", _primDescription = "test description",
      _primPermissions = [0x0008e000, 0x0008e000, 0x0008c000, 0x00080000, 0x00082000] }

prim2 = (emptyPrim "prim2" $ LSLKey pk2)
    { _primOwner = LSLKey av,
      _primInventory = [scriptInventoryItem "script" (LSLKey s2) "script2"],
      _primPosition = (0.5, 0.5, 0),
      _primName = "test name 2",
      _primDescription = "test description 2",
      _primPermissions = [0x0008e000, 0x0008e000, 0x0008c000, 0x00080000, 0x00082000] }

primHigh = prim1 { _primStatus = 0x1 .|. (_primStatus prim1) }

emptyPrim2 = prim2 { _primInventory = [] }

rezTestPrim0 = (emptyPrim "prim" $ LSLKey pk1)
    { _primOwner = LSLKey av,
      _primInventory = [scriptInventoryItem "script" (LSLKey s1) "script",
                        InventoryItem {
                            inventoryItemIdentification = InventoryItemIdentification ("Object", LSLKey pk2),
                            inventoryItemInfo = InventoryInfo (LSLKey av) defaultInventoryPermissions,
                            inventoryItemData = InvObject [rezTestPrim1]}],
      _primPosition = (0,0,0),
      _primName = "test name", _primDescription = "test description",
      _primPermissions = [0x0008e000, 0x0008e000, 0x0008c000, 0x00080000, 0x00082000] }

rezTestPrim1 = (emptyPrim "invPim" $ LSLKey pk2)
    { _primOwner = LSLKey av, _primInventory = [scriptInventoryItem "script" (LSLKey s2) "script2"], _primPosition = (0,0,0),
      _primPermissions = [0x0008e000, 0x0008e000, 0x0008c000, 0x00080000, 0x00082000] }

webhandler = [lslm|
    $module
    integer outHttpStatus = 200;
    string outHttpBody = "hello: ";
    list outHttpMetadata = [];
    string httpRequest(string url, string method, string mimetype, integer maxlength, integer verify, string bodyIn) {
        outHttpBody = outHttpBody + bodyIn;
        return "";
    }|]

library = libFromAugLib $ compileLibrary
    [avEventGen,("webhandler",webhandler),("avEventHandler",avEventHandlerModule),("handler2",avEventHandlerModule2)]

abstractWorld = FullWorldDef {
    fullWorldDefMaxTime = 2000,
    fullWorldDefSliceSize = 2000,
    fullWorldDefWebHandling = WebHandlingByDoingNothing,
    fullWorldDefObjects = undefined,
    fullWorldDefPrims = undefined,
    fullWorldDefAvatars = [(defaultAvatar $ LSLKey av) { _avatarEventHandler = Just ("avEventHandler",[])}],
    fullWorldDefRegions = defaultRegions $ LSLKey av,
    fullWorldDefInitialKeyIndex = 10,
    fullWorldDefEventHandler = Nothing
    }

simpleWorld = abstractWorld {
    fullWorldDefObjects = [LSLObject [LSLKey pk1] defaultDynamics { _objectPosition = (128,128,0) }],
    fullWorldDefPrims = [prim1] }
httpWorld = abstractWorld {
    fullWorldDefAvatars = [(defaultAvatar $ LSLKey av) { _avatarEventHandler = Just ("handler2",[])}],
    fullWorldDefObjects = [LSLObject [LSLKey pk1] defaultDynamics { _objectPosition = (128,128,0) }],
    fullWorldDefPrims = [prim1] }

lessSimpleWorld = abstractWorld {
    fullWorldDefObjects = [LSLObject [LSLKey pk1,LSLKey pk2] defaultDynamics { _objectPosition = (128,128,0) }],
    fullWorldDefPrims = [prim1,prim2] }

physicsWorld1 = abstractWorld {
    fullWorldDefObjects = [LSLObject [LSLKey pk1] defaultDynamics { _objectPosition = (128,128,0) }],
    fullWorldDefPrims = [primHigh] }
physicsWorld2 = abstractWorld {
    fullWorldDefObjects = [LSLObject [LSLKey pk1] defaultDynamics { _objectPosition = (128,128,5) }],
    fullWorldDefPrims = [primHigh] }
collisionWorld1 = abstractWorld {
    fullWorldDefObjects = [
        LSLObject [LSLKey pk1] defaultDynamics { _objectPosition = (128,128,0) },
        LSLObject [LSLKey pk2] defaultDynamics { _objectPosition = (128.5,128.5,0) }],
    fullWorldDefPrims = [primHigh,prim2 { _primPosition = (0,0,0)}] }
anotherWorld = abstractWorld {
    fullWorldDefObjects = [
        LSLObject [LSLKey pk1] defaultDynamics { _objectPosition = (128,128,0) },
        LSLObject [LSLKey pk2] defaultDynamics { _objectPosition = (128.5,128.5,0) }],
    fullWorldDefPrims = [prim1A,prim2] }

rezTestWorld = abstractWorld {
   fullWorldDefMaxTime = 300, fullWorldDefSliceSize = 300,
   fullWorldDefObjects = [LSLObject [LSLKey pk1] defaultDynamics],
   fullWorldDefPrims = [rezTestPrim0] }

sensorTestWorld = abstractWorld {
    fullWorldDefObjects = map (flip LSLObject defaultDynamics) [[LSLKey pk1],[LSLKey pk2]],
    fullWorldDefPrims = [prim1,emptyPrim2] }

runRezTestWorld s0 s1 = return $ simStep (Left(rezTestWorld,[("script",compileLSLScript' [] s0),
                                                             ("script2",compileLSLScript' [] s1)],[])) (SimContinue [] [])

data TestRun = TestRun { tWorld :: FullWorldDef,
                         tScripts :: [LSLScript],
                         tLib :: [(String,Validity LModule)],
                         tAssertion :: [LogMessage] -> IO () }

chatRun :: [LSLScript] -> [String] -> TestRun
chatRun scripts msgs = TestRun simpleWorld scripts [] (assertAllChatInLog msgs)
logHTTPRun scripts msgs = TestRun httpWorld scripts [] (\ log -> mapM_ (flip assertInLog log) msgs)
logRun scripts msgs = TestRun simpleWorld scripts [] (\ log -> mapM_ (flip assertInLog log) msgs)
logRunStrict scripts msgs = TestRun simpleWorld scripts [] (assertLogIs msgs)
logRun2 scripts msgs = TestRun lessSimpleWorld scripts [] (\ log -> mapM_ (flip assertInLog log) msgs)
logRun3 scripts msgs = TestRun anotherWorld scripts [] (\ log -> mapM_ (flip assertInLog log) msgs)
mkTest :: String -> TestRun -> Test
mkTest tname trun = TestLabel tname $ TestCase $ do
        (SimEnded "ended" log _, _)<- return $ simStep (Left (tWorld trun,labelScripts $ map (compileLSLScript' []) (tScripts trun), tLib trun))
                                                        (SimContinue [] [])
        tAssertion trun log
   where labelScripts scripts = let labels = "script":(map (("script"++) . show) [2..]) in zip labels scripts

existsInLog msg = isJust . (find (\ logMsg -> logMessageText logMsg == msg))

assertInLog msg log = assertBool ("expected message \"" ++ msg ++ "\" not found in log. Log = " ++ show log) (existsInLog msg log)
assertChatInLog msg log = assertInLog ("chan = 0, message = " ++ msg) log
assertAllChatInLog msgs log = mapM_ (flip assertChatInLog log) msgs
assertLogIs msgs log = assertBool ("expected messages " ++ show msgs ++ " not found. Log = " ++ show lmsgs) (lmsgs == msgs)
   where lmsgs = map logMessageText log

----------------------------------------------------------------------------------------------------------------------
-- TESTS ---

helloWorldScript1 = [lsl|
   default {
       state_entry() {
           llSay(0,"Hello World");
       }
   }|]

helloWorldTest = mkTest "hello world" $ chatRun [helloWorldScript1] ["Hello World"]

forLoopScript = [lsl|
    integer i;
    func() { i++; }
    default{
        state_entry() {
            integer j;
            for(j=0,i=0;j<2 && i<2;j++,func()) {
                llSay(0,(string)i + "," + (string)j);
            }
        }
    }|]

forLoopTest = mkTest "For Loop Test" $ chatRun [forLoopScript] ["0,0","1,1"]

keyScript = [lsl|
    key foo() { return "1"; }
    default {
        state_entry() {
            llSay(0,(string)((integer)((string)foo())));
        }
    }|]

keyTest = mkTest "key test" $ chatRun [keyScript] ["1"]

trimScript =  [lsl|
    default {
        state_entry() {
            llSay(0,llStringTrim(" x  ",STRING_TRIM_TAIL) + llStringTrim("  y ",STRING_TRIM) +
                                                            llStringTrim("   z ",STRING_TRIM_HEAD));
        }
    }|]

trimTest = mkTest "Trim Test" $ chatRun [trimScript] [" xyz "]

getPosScript = [lsl|
    default {
        state_entry() {
            llSay(0,(string)llGetPos());
        }
    }|]

getPosTest = mkTest "getPos Test" $ chatRun [getPosScript] ["<128.00000,128.00000,0.00000>"]

setPosGetPosScript = [lsl|
    default {
        state_entry() {
            llSetPos(<125.0,125.0,3.0>);
            llSay(0,(string)llGetPos());
        }
    }|]

setPosGetPosTest = mkTest "setPosGetPos Test" $ chatRun [setPosGetPosScript] ["<125.00000,125.00000,3.00000>"]

getOwnerScript = [lsl|
    default {
        state_entry() {
            llSay(0,llGetOwner());
        }
    }|]

getOwnerTest = mkTest "getOwner Test" $ chatRun [getOwnerScript] [av]

setGetAlphaScript = [lsl|
    default {
        state_entry() {
            llSetAlpha(0.5,ALL_SIDES);
            llSetAlpha(1.0,0);
            llSetAlpha(1.0,1);
            llSetAlpha(1.0,2);
            llSay(0,(string)llGetAlpha(ALL_SIDES));
        }
     }|]

setGetAlphaTest = mkTest "setGetAlpha Test" $ chatRun [setGetAlphaScript] ["0.750000"]

setGetColorScript = [lsl|
    default {
        state_entry() {
            llSetColor(<0.5,0.5,0.5>,ALL_SIDES);
            llSetColor(<1.0,1.0,1.0>,0);
            llSetColor(<1.0,1.0,1.0>,1);
            llSetColor(<1.0,1.0,1.0>,2);
            llSay(0,(string)llGetColor(ALL_SIDES));
        }
    }|]

setGetColorTest = mkTest "setGetColor Test" $ chatRun [setGetColorScript] ["<0.75000,0.75000,0.75000>"]

getNumberOfPrimsScript = [lsl|
    default {
        state_entry() {
            llSay(0,(string)llGetNumberOfPrims());
        }
    }|]

getNumberOfPrimsTest = mkTest "getNumberOfPrims Test" $ chatRun [getNumberOfPrimsScript] ["1"]

getNumberOfSidesScript = [lsl|
    default {
        state_entry() {
            llSay(0,(string)llGetNumberOfSides());
        }
    }|]

getNumberOfSidesTest = mkTest "getNumberOfSides Test" $ chatRun [getNumberOfSidesScript] ["6"]

setGetNameAndDescriptionScript = [lsl|
    default {
        state_entry() {
            llSetObjectName("this is the name");
            llSetObjectDesc("this is the description");
            llSay(0,"name=" + (string)llGetObjectName());
            llSay(0,"description=" + (string)llGetObjectDesc());
        }
    }|]

setGetNameAndDescriptionTest = mkTest "getNameAndDescription Test" $ chatRun [setGetNameAndDescriptionScript]
                                                                             ["name=this is the name", "description=this is the description"]

getPermsScript = [lsl|
    default {
        state_entry() {
            llSay(0,"0=" + (string)llGetObjectPermMask(0));
            llSay(0,"1=" + (string)llGetObjectPermMask(1));
            llSay(0,"2=" + (string)llGetObjectPermMask(2));
            llSay(0,"3=" + (string)llGetObjectPermMask(3));
            llSay(0,"4=" + (string)llGetObjectPermMask(4));
        }
    }|]

getPermsTest = mkTest "getPerms Test" $ chatRun [getPermsScript] ["0=581632","1=581632","2=573440","3=524288","4=532480"]

getLocalPosScript = [lsl|
    default {
        state_entry() {
            llSay(0,(string)llGetLocalPos());
        }
    }|]

getLocalPosTest = mkTest "getLocalPos Test" $
    (chatRun [nullScript1,getLocalPosScript] ["<0.50000,0.50000,0.00000>"]) { tWorld = lessSimpleWorld }

setGetScaleScript = [lsl|
    default{
         state_entry(){
             llSetScale(<0.5,0.4,0.3>);
             llSay(0,(string)llGetScale());
             llSetScale(<11.0,11.0,11.0>);
             llSay(0,(string)llGetScale());
             llSetScale(<0.001,1.0,1.0>);
             llSay(0,(string)llGetScale());
        }
    }|]

setGetScaleTest = mkTest "setGetScale Test" $ chatRun [setGetScaleScript] ["<0.50000,0.40000,0.30000>",
                                                                           "<10.00000,10.00000,10.00000>",
                                                                           "<10.00000,10.00000,10.00000>"]

getBoundingBoxScript = [lsl|
    default{
        state_entry() {
            llSay(0,(string)llGetBoundingBox($string:pk1));
        }
    }|]

getBoundingBoxTest = mkTest "getBoundingBox Test" $ chatRun [getBoundingBoxScript]
                                                              ["<127.50000,127.50000,-0.50000><128.50000,128.50000,0.50000>"]

getObjectPrimCountScript = [lsl|
    default {
        state_entry() {
            llSay(0,"1 -> " + (string)llGetObjectPrimCount($string:pk1));
            llSay(0,"2 -> " + (string)llGetObjectPrimCount($string:pk2));
            llSay(0,"3 -> " + (string)llGetObjectPrimCount($string:av));
        }
    }|]

getObjectPrimCountTest = mkTest "getObjectPrimCount Test" $
    (chatRun [nullScript1,getObjectPrimCountScript] ["1 -> 2", "2 -> 2", "3 -> 0"]) { tWorld = lessSimpleWorld }

getPrimitiveParamsScript = [lsl|
    default{
        state_entry(){
            llSetPrimitiveParams([PRIM_COLOR,ALL_SIDES,<0.5,0.5,0.5>,0.5]);
            llSay(0,llList2CSV(llGetPrimitiveParams(
                [PRIM_BUMP_SHINY,ALL_SIDES,PRIM_COLOR,ALL_SIDES,PRIM_FLEXIBLE,PRIM_TYPE])));
        }
    }|]

getPrimitiveParamsTest = mkTest "getPrimitiveParams Test" $ chatRun [getPrimitiveParamsScript]
    [("0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, " ++
    "<0.5,0.5,0.5>, 0.5, <0.5,0.5,0.5>, 0.5, <0.5,0.5,0.5>, 0.5, " ++
    "<0.5,0.5,0.5>, 0.5, <0.5,0.5,0.5>, 0.5, <0.5,0.5,0.5>, 0.5, " ++
    "0, 0, 0.0, 0.0, 0.0, 0.0, <0.0,0.0,0.0>, 0, 0, <0.0,1.0,0.0>, 0.0, <0.0,0.0,0.0>, <0.0,0.0,0.0>, <0.0,0.0,0.0>")]


timerScript = [lsl|
    integer count = 0;
    default{
        state_entry() {
            llSetTimerEvent(0.1);
        }
        timer() {
            if (count > 10) llSetTimerEvent(0.0);
            llSay(0,(string)count++);
        }
    }|]

timerTest = mkTest "timer Test" $ chatRun [timerScript] (map show [0..11])

linkMessageReceiverScript = [lsl|
    default{
        link_message(integer sender, integer num, string msg, key k)
            {llSay(0,"got " + msg);
        }
    }|]

linkMessageSenderScript = [lsl|
    default{
        state_entry(){
            llMessageLinked(1,0,"hi ",NULL_KEY);
        }
    }|]

linkMessageTest = mkTest "linkMessage Test" $
    (chatRun [linkMessageReceiverScript,linkMessageSenderScript] ["got hi "]) { tWorld = lessSimpleWorld }

linkMessageReceiverScript2 = [lsl|
    default{
        link_message(integer sender, integer num, string msg, key k) {
            llMessageLinked(sender,0,"Present",NULL_KEY);
        }
    }|]

linkMessageSenderScript2 = [lsl|
    default{
        state_entry(){
            llMessageLinked(1,0,"",NULL_KEY);
        }

        link_message(integer sender, integer num, string msg, key k) {
            llSay(0,"got " + msg);
        }
    }|]

linkMessageTest2 = mkTest "linkMessage Test" $
    (chatRun [linkMessageReceiverScript2,linkMessageSenderScript2] ["got Present"]) { tWorld = lessSimpleWorld }

getObjectDetailsScript = [lsl|
    default{
        state_entry() {
            llSay(0,llList2CSV(llGetObjectDetails($string:pk1,
                [OBJECT_OWNER,OBJECT_CREATOR,OBJECT_POS,OBJECT_VELOCITY,OBJECT_ROT,OBJECT_NAME])));
        }
    }|]

getObjectDetailsTest = mkTest "getObjectDetails Test" $ chatRun [getObjectDetailsScript]
    ["00000000-0000-0000-0000-000000000002, 00000000-0000-0000-0000-000000000002, <128.0,128.0,0.0>, <0.0,0.0,0.0>, <0.0,0.0,0.0,1.0>, test name"]

getObjectDetailsAvScript = [lsl|
    default{
        state_entry() {
            llSay(0,llList2CSV(llGetObjectDetails($string:av,
                [OBJECT_OWNER,OBJECT_CREATOR,OBJECT_POS,OBJECT_VELOCITY,OBJECT_ROT,OBJECT_NAME])));
        }
    }|]

getObjectDetailsAvTest = mkTest "getObjectDetailsAv Test" $ chatRun [getObjectDetailsAvScript]
    ["00000000-0000-0000-0000-000000000002, 00000000-0000-0000-0000-000000000000, <128.0,128.0,0.0>, <0.0,0.0,0.0>, <0.0,0.0,0.0,1.0>, Default Avatar"]

requestPermissionsScript = [lsl|
    default {
        state_entry() {
            llRequestPermissions($string:av,PERMISSION_DEBIT);
        }
        run_time_permissions(integer perm){
            llSay(0,(string)perm);
        }
    }|]

requestPermissionsTest = mkTest "requestPermissions Test" $ chatRun [requestPermissionsScript] ["2"]

addToBanListScript = [lsl|
    default{
        state_entry(){
            llAddToLandBanList($string:av,0.0);
        }
    }|]

addToBanListTest = mkTest "addToBanList Test" $ logRun [addToBanListScript]
    ["added 00000000-0000-0000-0000-000000000002 to ban list for parcel in (0,0)"]

addToPassListScript = [lsl|
    default {
        state_entry() {
            llAddToLandPassList($string:av,0.0);
        }
    }|]

addToPassListTest = mkTest "addToPassList Test" $ logRun [addToPassListScript]
    ["added 00000000-0000-0000-0000-000000000002 to pass list for parcel in (0,0)"]

adjustSoundVolumeScript = [lsl|
    default {
        state_entry(){
            llAdjustSoundVolume(0.5);
        }
    }|]

adjustSoundVolumeTest = mkTest "adjustSoundVolume Test" $ logRun [adjustSoundVolumeScript] ["llAdjustSoundVolume: volume adjusted to 0.5" ]

allowInventoryDropScript = [lsl|
    default {
        state_entry() {
            llAllowInventoryDrop(1);
            llAllowInventoryDrop(0);
        }
    }|]

allowInventoryDropTest = mkTest "allowInventoryDrop Test" $ logRun [allowInventoryDropScript] ["drop is now allowed","drop is now not allowed"]
dieScript = [lsl|
    default {
        state_entry() {
            llDie();
        }
    }|]

dieTest = mkTest "die Test" $ logRun [dieScript] ["object, and therefore this script, is dying"]

numberOfNotecardLinesScript = [lsl|
    key ds;
    default {
        state_entry() {
            ds=llGetNumberOfNotecardLines("notecard");
        }
        dataserver(key queryid, string data) {
            llSay(0,"ok = " + (string)(queryid == ds));
            llSay(0,"n = " + data);
        }
    }|]

numberOfNotecardLinesTest = mkTest "numberOfNotecardLines Test" $ chatRun [numberOfNotecardLinesScript] ["ok = 1","n = 2"]

notecardLinesScript = [lsl|
    key ds;
    integer i = 0;
    default{
        state_entry() {
            ds=llGetNotecardLine("notecard",i++);
        }
        dataserver(key queryid,string data){
            llSay(0,((string)i) + " - ok = " + (string)(queryid == ds));
            if (data != EOF) {
                llSay(0,data);ds = llGetNotecardLine("notecard",i++);
            } else llSay(0,"EOF");
        }
    }|]

notecardLinesTest = mkTest "notecardLines Test" $ chatRun [notecardLinesScript] ["1 - ok = 1","2 - ok = 1","3 - ok = 1", "hello world","how are you?"]

webTest1 = mkTest "Web Test 1" $ TestRun {
    tWorld = simpleWorld { fullWorldDefMaxTime = 10000, fullWorldDefSliceSize = 10000,
        fullWorldDefWebHandling = WebHandlingByFunction,
        fullWorldDefEventHandler = Just "webhandler" },
    tScripts = [[lsl|key k = "";
                      default{
                          state_entry() {
                              k = llHTTPRequest("http://example.com",[HTTP_METHOD,"GET"],"this is the body");
                          }
                          http_response(key id, integer status, list metadata, string body) {
                              llSay(0,body);
                              llSay(0,(string)status);
                              llSay(0,"Ok " + (string)(k == id));
                          }
                      }|]],
    tLib = library,
    tAssertion = assertAllChatInLog ["hello: this is the body","200","Ok 1"] }

stateChangeScript = [lsl|
    default{
        state_entry() {
            state foo;
        }
        state_exit() {
            llSay(0,"exiting default");
        }
    }
    state foo {
        state_entry() {
            llSay(0,"entering state foo");
        }
    }|]

stateChangeTest = mkTest "State Change Test" $ chatRun [stateChangeScript] ["exiting default","entering state foo"]

rezzerScript = [lsl|
    default {
        state_entry(){
            llRezObject("Object",<128,128,0>,<0,0,0>,<0,0,0,1>,1);
        }
        object_rez(key k) {
            llSay(0,"it wuz rezzed");
        }
    }|]

rezzeeScript = [lsl|
    default{
        state_entry(){
            llSay(0,"i wuz rezzed");
        }
        on_rez(integer param) {
            llSay(0,"param = " + (string) param);
        }
    }|]

rezTest = TestLabel "Rez Test" $ TestCase $ do
    (SimEnded "ended" log _,_) <- runRezTestWorld rezzerScript rezzeeScript
    mapM_ (flip assertChatInLog log) ["i wuz rezzed","it wuz rezzed","param = 1"]

resetScriptScript = [lsl|
    default{
        state_entry(){
            if (llGetObjectName() == "reset") {
                llSay(0,"reset!");
            } else {
                llSetObjectName("reset");
                llResetScript();
            }
        }
    }|]

resetTest = mkTest "Reset Test" $ chatRun [resetScriptScript] ["reset!"]

sensorScript = [lsl|
    default{
        state_entry() {
            llSensor("Default Avatar", NULL_KEY, AGENT, 96.0, PI);
        }
        sensor(integer num_detected){
            llSay(0,(string)num_detected + "," + (string)llDetectedKey(0));
        }
    }|]

sensorTest = mkTest "Sensor Test" $ chatRun [sensorScript] [("1," ++ av)]

sensorScript1 = [lsl|
    default{
        state_entry() {
            llSensor("", NULL_KEY, ACTIVE|PASSIVE|SCRIPTED, 96.0, PI);
        }
        no_sensor() {
            llSay(0,"nothing");
        }
    }|]

sensorTest1 = mkTest "Sensor Test 1" $ (chatRun [sensorScript1] ["nothing"])

sensorScript2 = [lsl|
    default{
        state_entry(){
            llSensor("", NULL_KEY, ACTIVE|PASSIVE|SCRIPTED, 96.0, PI);
        }
        sensor(integer num_detected) {
            llSay(0,(string)num_detected + "," + (string)llDetectedKey(0));
        }
    }|]

sensorTest2 = mkTest "Sensor Test 2" $ (chatRun [sensorScript2] ["1," ++ pk2]) { tWorld = sensorTestWorld }
sensorTests = TestList [sensorTest,sensorTest1,sensorTest2]

attachDetachScript = [lsl|
    default{
        state_entry() {
            llRequestPermissions($string:av,PERMISSION_ATTACH);
        }
        run_time_permissions(integer perm) {
            llSay(0,"perm = " + (string)perm);
            llAttachToAvatar(ATTACH_CHEST);
        }
        attach(key id) {
            llSay(0,"id = " + (string)id);
            if (id != NULL_KEY) {
                llSay(0,"attached = " + (string)llGetAttached());
                llDetachFromAvatar();
            } else {
                llSay(0,"detached");
            }
        }
    }|]

attachDetachTest = mkTest "Attach/Detach Test" $ chatRun [attachDetachScript]
    ["perm = 32", "id = " ++ av, "attached = 1", "id = " ++ unLslKey nullKey, "detached"]

massScript = [lsl|
    default{
        state_entry() {
            llSay(0,(string)llGetMass());
        }
    }|]

massTest = mkTest "Mass Test" $ chatRun [massScript] ["10.000000"]

massScript1 = [lsl|
    default{
        state_entry() {
            llSay(0,(string)llGetObjectMass($string:pk1));
        }
    }|]

massTest1 = mkTest "Mass Test1" $ chatRun [massScript1] ["10.000000"]

massScript2 = [lsl|
    default{
        state_entry() {
            llSay(0,(string)llGetObjectMass($string:av));
        }
    }|]

massTest2 = mkTest "Mass Test2" $ chatRun [massScript2] ["80.000000"]

condKeyScript k = [lsl|
    key v = $string:k;
    default {
        state_entry() {
            if(v) llSay(0,"yes");
            else llSay(0,"no");
        }
    }|]
condStringScript s = [lsl|
    string v = $string:s;
    default {
        state_entry() {
            if(v) llSay(0,"yes");
            else llSay(0,"no");
        }
    }|]

condFloatScript f = [lsl|
    float v = $float:f;
    default {
        state_entry() {
            if(v) llSay(0,"yes");
            else llSay(0,"no");
        }
    }|]

condVectorScript1 = [lsl|
    vector v = <1,1,1>;
    default {
        state_entry() {
            if(v) llSay(0,"yes");
            else llSay(0,"no");
        }
    }|]

condVectorScript2 = [lsl|
    vector v = ZERO_VECTOR;
    default {
        state_entry() {
            if(v) llSay(0,"yes");
            else llSay(0,"no");
        }
    }|]

condRotScript1 = [lsl|
    rotation v = <1,1,1,1>;
    default {
        state_entry() {
            if(v) llSay(0,"yes");
            else llSay(0,"no");
        }
    }|]

condRotScript2 = [lsl|
    rotation v = ZERO_ROTATION;
        default {
        state_entry() {
            if(v) llSay(0,"yes");
            else llSay(0,"no");
        }
    }|]

condTests = TestList [
        mkTest "Condition Test 1" $ chatRun [condKeyScript av] ["yes"],
        mkTest "Condition Test 2" $ chatRun [condKeyScript $ unLslKey nullKey] ["no"],
        mkTest "Condition Test 3" $ chatRun [condKeyScript "12312312-23123"] ["no"],
        mkTest "Condition Test 4" $ chatRun [condStringScript "hi"] ["yes"],
        mkTest "Condition Test 5" $ chatRun [condStringScript ""] ["no"],
        mkTest "Condition Test 6" $ chatRun [condFloatScript 1] ["yes"],
        mkTest "Condition Test 7" $ chatRun [condFloatScript 0] ["no"],
        mkTest "Condition Test 8" $ chatRun [condVectorScript1] ["yes"],
        mkTest "Condition Test 9" $ chatRun [condVectorScript2] ["no"],
        mkTest "Condition Test 10" $ chatRun [condRotScript1] ["yes"],
        mkTest "Condition Test 11" $ chatRun [condRotScript2] ["no"]
    ]

physicsScript1 = [lsl|
    default {
        timer() {
            float mag = llVecMag(llGetVel());
            integer x = mag > 0.45 && mag < 0.55; llSay(0,"x = " + (string)x);
            llSetTimerEvent(0.0);
        }
        state_entry() {
            llSetForce(<10.0,0,0>,1);
            llSetTimerEvent(0.5);
            llSay(0,"mass =" + (string)llGetMass());
        }
    }|]

physTest1 = mkTest "Physics Test 1" $ (chatRun [physicsScript1] ["x = 1"]) { tWorld = physicsWorld1 }

physicsScript2 = [lsl|
    default {
        timer() {
            float mag = llVecMag(llGetVel());
            integer x = mag > 0.95 && mag < 1.05;
            llSay(0,"x = " + (string)x);
            llSetTimerEvent(0.0);
        }
        state_entry() {
            llApplyImpulse(<10.0,0,0>,1);
            llSetTimerEvent(1.5);
            llSay(0,"mass =" + (string)llGetMass());
        }
    }|]

physTest2 = mkTest "Physics Test 1" $ (chatRun [physicsScript2] ["x = 1"]) { tWorld = physicsWorld1 }

physicsScript3 = [lsl|
    default {
        state_entry() {
            llSetForce(<10.0,0,0>,0);
            llSetTimerEvent(0.4);
        }
        timer() {
            llSetTimerEvent(0);
            integer i = llVecMag(llGetAccel() - <1,0,0>) < 0.1;
            llSay(0,(string)i);
        }
    }|]

physTest3 = mkTest "Accel Test" $ (chatRun [physicsScript3] ["1"]) { tWorld = physicsWorld1 }

fallingScript = [lsl|
    default {
        moving_start() {
            llSay(0,"moving started");
        }
        moving_end() {
            llSay(0,"moving ended");
            vector pos = llGetPos();
            llSay(0,(string)(llFabs(pos.z) < 0.01));
        }
    }|]

fallingTest = mkTest "Falling Test" $ (chatRun [fallingScript] ["moving started","moving ended","1"]) { tWorld = physicsWorld2 }

buoyancyScript = [lsl|
    default {
        state_entry() {
            llSetBuoyancy(1.0);
            llSetTimerEvent(1.0);
        }
        timer() {
            vector v = llGetPos();
            llSay(0,(string) (0.05 > (llFabs(v.z - 5.0))));
        }
    }|]

buoyancyTest = mkTest "Buoyancy Test" $ (chatRun [buoyancyScript] ["1"]) { tWorld = physicsWorld2 }

collisionScript1 = [lsl|
    default {
        state_entry() {
            llSetForce(<10.0,0,0>,1);
            llSetTimerEvent(1.5);
        }
        collision_start(integer num_detected) {
            llSay(0,(string)num_detected );
            llSay(0,(string)llDetectedKey(0));
        }
        collision(integer num_detected) {}
        collision_end(integer num_detected) {
            llSay(0,"end");
        }
    }|]

collisionTest1 = mkTest "Collision Test 1" $ (chatRun [collisionScript1,nullScript1] ["1",pk2,"end"]) { tWorld = collisionWorld1 }

moverScript1 = [lsl|
    default {
        state_entry() {
            llSetForce(<10.0,0,0>,1);
            llSetTimerEvent(1.0);
        }
        moving_start() {
            llSay(0,"moving");
        }
    }|]

moverTest1 = mkTest "Mover Test 1" $ (chatRun [moverScript1] ["moving"]) { tWorld = physicsWorld1 }

moverScript2 = [lsl|
    default {
        state_entry() {
            llSetForce(<10.0,0,0>,1);
            llSetTimerEvent(0.1);
        }
        timer(){
            vector vel = llGetVel();
            llSay(0,(string)vel);
            vector force = ZERO_VECTOR - vel;
            llSetForce(force*40,1);
        }
        moving_end() {
            llSay(0, "stopped");
        }
    }|]

moverTest2 = mkTest "Mover Test 2" $ (chatRun [moverScript2] ["stopped"]) { tWorld = physicsWorld1 }

moveToTargetScript1 = [lsl|
    default {
        state_entry() {
            llMoveToTarget(<100,128.0,20.0>,0.2);
            llSetTimerEvent(1.0);
        }
        timer() {
            integer i = llVecMag(<100,128,20> - llGetPos()) < 1.0;
            llSay(0,(string)i);
        }
    }|]

moveToTargetTest1 = mkTest "Move To Target Test 1" $ (chatRun [moveToTargetScript1] ["1"]) { tWorld = physicsWorld1 }

moveToTargetScript2 = [lsl|
    integer not_at = 1;
    integer at = 0;
    default {
        state_entry() {
            llMoveToTarget(<100,128.0,20.0>,0.2);
            llTarget(<100,128.0,20.0>,1.0);
        }
        at_target(integer i, vector t, vector p) {
            if (not_at) {
                not_at = 0;
                at = 1;
                llSay(0,"at_target");
            }
        }
        not_at_target() {
            if (!at) {
                llSay(0,"not_at_target");
            }
        }
    }|]

moveToTargetTest2 = mkTest "Move To Target Test 2" $ (chatRun [moveToTargetScript2] ["not_at_target","at_target"]) { tWorld = physicsWorld1 }

rotScript1'1 = [lsl|
    default {
        state_entry() {
            llSetRot(llEuler2Rot(<0,0,1.5707963267948966>));
        }
    }|]

rotScript1'2 = [lsl|
    default {
        state_entry() {
            llSetTimerEvent(0.2);
        }
        timer() {
            llSetTimerEvent(0);
            integer close = llVecDist(<127.5, 128.5, 0.0>,llGetPos()) < 0.01;
            llSay(0,(string)close);
        }
    }|]

rotTest1 = mkTest "Rot Test 1" $ (chatRun [rotScript1'1,rotScript1'2] ["1"]) { tWorld =lessSimpleWorld }

rotScript2 = [lsl|
    default {
        state_entry() {
            vector v = <DEG_TO_RAD*22.5,DEG_TO_RAD*45.0,DEG_TO_RAD*67.5>;
            rotation r = llEuler2Rot(v);
            vector v1 = llRot2Euler(r);
            integer close = llVecDist(v,v1) < 0.01;
            llSay(0,(string)close);
        }
    }|]

rotTest2 = mkTest "Rot Test 2" $ (chatRun [rotScript2] ["1"])

rotScript3 = [lsl|
    default {
        state_entry() {
            llSetTorque(<0,0,DEG_TO_RAD*10.0>, FALSE);
            llSetTimerEvent(1.0);
        }
        timer() {
            integer close = llVecDist(llGetOmega() * RAD_TO_DEG,<0,0,10>) < 0.5;
            llSay(0,(string)close);
            llSetTimerEvent(0.0);
        }
    }|]

rotTest3 = mkTest "Rot Test 3" $ (chatRun [rotScript3] ["1"]) { tWorld = physicsWorld1 }

rotScript4 = [lsl|
    default {
        state_entry() {
            llLookAt(<120,128,0>,2.0,0.2);
            llSetTimerEvent(1.0);
        }
        timer(){
            llSetTimerEvent(0.0);
            integer close = llFabs(llAngleBetween(llEuler2Rot(<0,DEG_TO_RAD * (-90),0>),llGetRot())) < DEG_TO_RAD * 1.0;
            llSay(0,(string)close);
        }
    }|]

rotTest4 = mkTest "Rot Test 4" $ (chatRun [rotScript4] ["1"]) { tWorld = physicsWorld1 }

rotScript5 = [lsl|
    integer not_at = 0;
    default {
        state_entry() {
            llLookAt(<120,128,0>,2.0,0.2);
            llRotTarget(llEuler2Rot(<0,DEG_TO_RAD * (-90),0>), DEG_TO_RAD * 1.0);
        }
        not_at_rot_target() {
            if (!not_at) {
                not_at = TRUE;
                llSay(0,"not-at");
            }
        }
        at_rot_target(integer i,rotation t, rotation r) {
            integer close = llFabs(llAngleBetween(t,r)) < DEG_TO_RAD * 1.0;
            llSay(0,(string)close);
        }
    }|]

rotTest5 = mkTest "Rot Test 5" $ (chatRun [rotScript5] ["not-at","1"]) { tWorld = physicsWorld1 }

landCollisionScript = [lsl|
    default {
        land_collision_start(vector pos) {
            llSay(0,"start " + (string)pos);
        }
        land_collision(vector pos) {
            llSay(0,"collide " + (string)pos);
            llSetPos(<128,128,10>);
        }
        land_collision_end(vector pos){
            llSay(0,"end");
        }
    }|]

landCollisionTest = mkTest "Land Collision Test" $ (chatRun [landCollisionScript]
                                                            ["start <128.00000,128.00000,0.00000>",
                                                             "collide <128.00000,128.00000,0.00000>",
                                                             "end"]) { tWorld = physicsWorld2 }
scriptedTouchTrigger = [lsl|
    default {
        state_entry() {
            llOwnerSay("touch me");
        }
        touch_start(integer num_detected) {
            llSay(0,"touched");
        }
    }|]

scriptedTouchTriggerTest =mkTest "Scripted Touch Trigger Test" $ ((chatRun [scriptedTouchTrigger] ["touched"]) { tLib = library })

scriptedFaceTouchTrigger = [lsl|
    default {
        state_entry() {
            llOwnerSay("touch me more specifically");
        }

        touch_start(integer num_detected) {
            integer face = llDetectedTouchFace(0);
            llSay(0,"touched " + (string)face);
            vector st = llDetectedTouchST(0);
            if (st.x > 0.599 && st.x < 0.6001) llSay(0,"x ok");
            if (st.y > 0.399 && st.y < 0.4001) llSay(0,"y ok");
        }
    }|]

scriptedFaceTouchTriggerTest =mkTest "Scripted Face Touch Trigger Test" $
   ((chatRun [scriptedFaceTouchTrigger] ["touched 0", "x ok", "y ok"]) { tLib = library })


controlScript = [lsl|
    default {
        state_entry() {
            llRequestPermissions($string:av,PERMISSION_TAKE_CONTROLS);
        }
        run_time_permissions(integer perm) {
            llTakeControls(0xffff,TRUE,FALSE);
            llOwnerSay("took controls");
        }
        control(key id, integer held, integer change) {
            llSay(0,(string)id + " " + (string) held + " " + (string)change);
        }
    }|]

controlTest = mkTest "Control Test" $ ((chatRun [controlScript] [av ++ " " ++ "65535 " ++ "65535"]) { tLib = library })

wildRegressScript1 = [lsl|
    default {
        state_entry() {
            key k1 = (key)$string:av;
            key q1 = llRequestAgentData(k1, DATA_ONLINE);
            llSay(0, "request key: " + (string)q1);
        }
        dataserver(key queryid, string data) {
            llSay(0, "got data for " + (string)queryid);
        }
    }|]

wildRegressTest1 = mkTest "wild regress 1" $ ((chatRun [wildRegressScript1]
    ["request key: " ++ unLslKey initialKey,"got data for " ++ unLslKey initialKey]))
negIndexScript = [lsl|
    default {
        state_entry() {
            llSay(0,llList2String([0,1,2,3],-1));
            llSay(0,(string)llList2Integer([0,1,2,3],-2));
        }
    }
    |]

negIndexTest = mkTest "Neg Test" $ (chatRun [negIndexScript] ["3","2"])

loadURLScript = [lsl|
    default {
        state_entry() {
            llListen(0,"",NULL_KEY,"");
            llLoadURL($key:av,"hi","http://www.google.com");
        }

        listen(integer channel, string nm, key k, string m) {
            llSay(0, m);
        }

    }

    |]

loadURLScriptTest = mkTest "Load URL Test" $ ((chatRun [loadURLScript]
    ["0, 00000000-0000-0000-0000-000000000002, 1, <128.0,128.0,0.0>, 2, <0.0,0.0,0.0,1.0>, 3, Default Avatar"]) { tLib = library })

castTest = mkTest "Cast Test" $ ((chatRun [[lsl|
    default {
        state_entry() {
            list il = (list) 1;
            list fl = (list) 1.1;
            list vl = (list) <1,2,3>;
            list rl = (list) ZERO_ROTATION;
            list kl = (list) ((key) "x");
            llSay(0,(string)il + "," + (string)fl + "," + (string)vl + "," + (string)rl + "," + (string)kl);
        }
    }|]] ["1,1.100000,<1.00000,2.00000,3.00000>,<0.00000,0.00000,0.00000,1.00000>,x"]) { tLib = library })

jumpScript1 = [lsl|
    default {
        state_entry() {
            while (1) jump foo;
            @foo;
            llSay(0,"ok");
        }
    }
    |]

jumpTest1 = mkTest "Jump Test 1" $ ((logRunStrict [jumpScript1] ["chan = 0, message = ok"]) { tLib = library })

jumpScript2 = [lsl|
    default {
        state_entry() {
            while (1) while (1) jump foo;
            @foo;
            llSay(0,"ok");
        }
    }
    |]

jumpTest2 = mkTest "Jump Test 2" $ ((logRunStrict [jumpScript2] ["chan = 0, message = ok"]) { tLib = library })

jumpScript3 = [lsl|
    default {
        state_entry() {
            while (1) while (1) { jump foo; }
            @foo;
            llSay(0,"ok");
        }
    }
    |]

jumpTest3 = mkTest "Jump Test 3" $ ((logRunStrict [jumpScript3] ["chan = 0, message = ok"]) { tLib = library })

jumpScript4 = [lsl|
    default {
        state_entry() {
            integer i = 0;
            @foo;
            if (i) {
                llSay(0,"ok");
                return;
            }
            i = 1;
            while (1) while (1) { jump foo; }
        }
    }
    |]

jumpTest4 = mkTest "Jump Test 4" $ ((logRunStrict [jumpScript4] ["chan = 0, message = ok","return: n/a"]) { tLib = library })

jumpScript5 = [lsl|
    default {
        state_entry() {
            integer i = 0;
            @foo;
            if (i) {
                llSay(0,"ok");
                return;
            }
            i = 1;
            while (1) while (1) { if (0); else jump foo; }
        }
    }
    |]

jumpTest5 = mkTest "Jump Test 5" $ ((logRunStrict [jumpScript5] ["chan = 0, message = ok","return: n/a"]) { tLib = library })

jumpTests = TestList [ jumpTest1, jumpTest2, jumpTest3, jumpTest4, jumpTest5 ]

cryptoTest = mkTest "Crypto Test" $ ((chatRun [[lsl|
    string crypto_key = "";

    default {
        state_entry() {
            string b64 = llXorBase64StringsCorrect( llStringToBase64( "abcdef" ), "");
            llSay( 0, "Enc: "+b64 );
        }
    }|]] ["Enc: YWJjZGVm"]) { tLib = library })

wallclockTest = mkTest "Wallclock Test" $ ((chatRun [[lsl|
    default {
        state_entry() {
           llSay(0,"time is " + (string)llGetWallclock());
        }
    }|]] ["time is 3208"]) { tLib = library })

stringTest1 = mkTest "String Test 1" $ ((chatRun [[lsl|
    default {
        state_entry() {
            integer i = (integer) "-10";
            llSay(0,(string)i);
        }
    }
    |]]["-10"]) { tLib = library })

stringTest2 = mkTest "String Test 2" $ ((chatRun [[lsl|
    default {
        state_entry() {
            integer i = (integer) "+10";
            llSay(0,(string)i);
        }
    }
    |]]["10"]) { tLib = library })
stringTest3 = mkTest "String Test 3" $ ((chatRun [[lsl|
    default {
        state_entry() {
            float i = (float) "+10";
            llSay(0,(string)i);
        }
    }
    |]]["10.000000"]) { tLib = library })
stringTest4 = mkTest "String Test 4" $ ((chatRun [[lsl|
    default {
        state_entry() {
            float i = (float) "-10.5";
            llSay(0,(string)i);
        }
    }
    |]]["-10.500000"]) { tLib = library })

agentCountTest1 = mkTest "Agent Count Test 1" $ ((chatRun [[lsl|
    default {
        state_entry() {
            llSay(0,(string)llGetRegionAgentCount());
        }
    }
    |]]["1"]) { tLib = library })

giveTest1 = mkTest "Give Test 1" ((logRun2 [scr1,scr2] ["Owner Say: notecard"]) { tLib = library })
    where scr1 = [lsl|
              default {
                  state_entry() {
                      llGiveInventoryList($string:pk2,"new",["notecard"]);
                  }
              }
              |]
          scr2 = [lsl|
              default {
                  changed(integer change) {
                      integer num = llGetInventoryNumber(INVENTORY_ALL);
                      integer i = 0;
                      for (;i < num; i++) {
                          llOwnerSay(llGetInventoryName(INVENTORY_ALL,i));
                      }
                  }
              }
              |]

giveTest2 = mkTest "Give Test 2" ((logRun2 [scr1,scr2] ["Owner Say: script 1"]) { tLib = library })
    where scr1 = [lsl|
              default {
                  state_entry() {
                      llGiveInventoryList($string:pk2,"new",["script"]);
                  }
              }
              |]
          scr2 = [lsl|
              default {
                  changed(integer change) {
                      integer num = llGetInventoryNumber(INVENTORY_ALL);
                      integer i = 0;
                      for (;i < num; i++) {
                          llOwnerSay(llGetInventoryName(INVENTORY_ALL,i));
                      }
                  }
              }
              |]

giveTest3 = mkTest "Give Test 3" ((logRun3 [scr1,scr2] ["chan = 2147483647, message = inventory item notecard not copyable"]) { tLib = library })
    where scr1 = [lsl|
              default {
                  state_entry() {
                      llGiveInventoryList($string:pk2,"new",["notecard"]);
                  }
              }
              |]
          scr2 = [lsl|
              default {
                  changed(integer change) {
                      integer num = llGetInventoryNumber(INVENTORY_ALL);
                      integer i = 0;
                      for (;i < num; i++) {
                          llOwnerSay(llGetInventoryName(INVENTORY_ALL,i));
                      }
                  }
              }
              |]

httpRequestHandlerTest1 = mkTest "http request handler test 1"
    (logHTTPRun [httpRequestHandler] [
        "chat! chan: 0, range: 20.0, message: \"status = 200, body = ok\"",
        "chan = 0, message = x-path-info: /hello, x-query-string: x=1, x-remote-ip: 127.0.0.1, user-agent: Mozilla",
        "chan = 0, message = free-urls: 9"]) { tLib = library }
httpRequestHandler = [lsl|
    key k = NULL_KEY;
    default {
        state_entry() {
            k = llRequestURL();
        }

        http_request(key id, string method, string body) {
            if (method == "URL_REQUEST_GRANTED") {
                llOwnerSay(body);
            } else if (method == "GET") {
                llSay(0, "x-path-info: " + llGetHTTPHeader(id,"x-path-info") +
                    ", x-query-string: " + llGetHTTPHeader(id,"x-query-string") +
                    ", x-remote-ip: " + llGetHTTPHeader(id,"x-remote-ip") +
                    ", user-agent: " + llGetHTTPHeader(id,"user-agent"));
                llSay(0,"free-urls: " + (string) llGetFreeURLs());
                llHTTPResponse(id,200,"ok");
            }
        }
    }|]

tests = TestList [
        helloWorldTest,
        forLoopTest,
        keyTest,
        trimTest,
        getPosTest,
        setPosGetPosTest,
        getOwnerTest,
        setGetAlphaTest,
        setGetColorTest,
        getNumberOfPrimsTest,
        getNumberOfSidesTest,
        setGetNameAndDescriptionTest,
        getPermsTest,
        getLocalPosTest,
        setGetScaleTest,
        getBoundingBoxTest,
        getObjectPrimCountTest,
        getPrimitiveParamsTest,
        timerTest,
        linkMessageTest,
        linkMessageTest2,
        getObjectDetailsTest,
        getObjectDetailsAvTest,
        requestPermissionsTest,
        addToBanListTest,
        addToPassListTest,
        adjustSoundVolumeTest,
        allowInventoryDropTest,
        dieTest,
        numberOfNotecardLinesTest,
        notecardLinesTest,
        rezTest,
        stateChangeTest,
        resetTest,
        webTest1,
        sensorTests,
        attachDetachTest,
        massTest,
        massTest1,
        massTest2,
        condTests,
        physTest1,
        physTest2,
        physTest3,
        fallingTest,
        buoyancyTest,
        collisionTest1,
        moverTest1,
        moverTest2,
        moveToTargetTest1,
        moveToTargetTest2,
        rotTest1,
        rotTest2,
        rotTest3,
        rotTest4,
        rotTest5,
        landCollisionTest,
        scriptedTouchTriggerTest,
        scriptedFaceTouchTriggerTest,
        controlTest,
        wildRegressTest1,
        negIndexTest,
        loadURLScriptTest,
        jumpTests,
        castTest,
        cryptoTest,
        wallclockTest,
        stringTest1,
        stringTest2,
        stringTest3,
        stringTest4,
        agentCountTest1,
        giveTest1,
        giveTest2,
        giveTest3,
        httpRequestHandlerTest1
    ]

----------------------------------------------------------------------------------------------------------------------

nullScript1 = [lsl|
    default {
        state_entry() {
        }
    }|]

avEventHandlerModule = [lslm|
    $module
    $import $avEventGen ();
    list onOwnerSay(string k, string msg) {
        if (msg == "touch me") return [mkTouch($string:pk1, 1.0)];
        else if (msg == "took controls") return [mkControl(0xffff)];
        else if (msg == "touch me more specifically") return [mkFaceTouch($string:pk1, 1.0, 0, 0.6, 0.4)];
        else return [];
    }

    list onLoadURL(string msg, string url, list avInfo) {
        string s = llList2CSV(avInfo);
        return [mkSay(0,s)];
    }
    |]

avEventHandlerModule2 = [lslm|
    $module
    $import $avEventGen ();
    list onOwnerSay(string k, string msg) {
        string url = msg;
        return [mkHTTPRequest(url + "/hello?x=1", "GET","", "127.0.0.1","Mozilla")];
    }
    list onHTTPRequestKey(string k, list avInfo) {
        return [mkSay(0,"key = " + k)];
    }

    list onHTTPBadRequest() {
        return [mkSay(0,"badRequest!")];
    }

    list onHTTPResponse(string k, integer status, string body) {
        return [mkSay(0,"status = " + (string) status + ", body = " + body)];
    }
    |]

tmpModule = [lslm|
    $module

    integer foo() {
        return bar() + baz();
    }

    integer bar() {
        return 1;
    }

    integer baz() {
        return 2;
    }|]
