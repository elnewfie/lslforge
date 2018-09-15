{-# OPTIONS_GHC -XScopedTypeVariables #-}
{-# OPTIONS_GHC -XQuasiQuotes #-}
module Lsl.OptTests where

import Control.Monad
import qualified Control.Monad.State as S
import Data.List
import Debug.Trace

import Language.Lsl.Internal.Compiler
import Language.Lsl.Syntax
import Language.Lsl.Parse
import Language.Lsl.Render
import Language.Lsl.Internal.Optimize
import Language.Lsl.Internal.Pragmas
import Language.Lsl.QQ(lsl,lslm)
import Language.Lsl.Sim

import Test.HUnit hiding (State,Label)
import Lsl.SimTests


showResults = False

tscript l s v = TestCase $
        case compileLSLScript' lib s of
            Left e -> assertFailure (show e)
            Right cs -> v lib cs os where os = optimizeScript [OptimizationInlining] cs
    where lib = libFromAugLib $ compileLibrary l

v0 lib cs os = do
    let rs = renderCompiledScript "" os
    when showResults $ putStr ("\n" ++ rs)
    case parseScriptFromString rs of
        Left e -> assertFailure (show e)
        Right cs' -> case compileLSLScript' [] cs' of
             Left e -> assertFailure (show e)
             Right cs'' -> do
                     let result1 = exec cs
                     let result2 = exec cs''
                     let cslog =  filtLog result1
                     let oslog = filtLog result2
                     assertEqual "logs don't match!" cslog oslog
                     when showResults $ print oslog
                 where exec s = simStatusLog $ fst $ simStep (Left (simpleWorld,[("script",Right s)],lib)) (SimContinue [] [])
                       filtLog log = [ m | LogMessage { logMessageLevel = l, logMessageText = m } <- log, l /= LogTrace]

v1 lib cs os = do
    let rs = renderCompiledScript "" os
    when showResults $ putStr ("\n" ++ rs)
    case parseScriptFromString rs of
        Left e -> assertFailure (show e)
        Right cs' -> case compileLSLScript' [] cs' of
             Left e -> assertFailure (show e)
             Right _ -> do
                     let result2 = exec os
                     let oslog = filtLog result2
                     when showResults $ print oslog
                 where exec s = simStatusLog $ fst $ simStep (Left (simpleWorld,[("script",Right s)],lib)) (SimContinue [] [])
                       filtLog log = [ m | LogMessage { logMessageLevel = l, logMessageText = m } <- log, l /= LogTrace]

t1 = tscript [] [lsl|
    integer x;
    // pragma inline
    integer foo(integer x) {
        integer i;
        integer sum = 0;
        for (i = 0; i <= x; i++) {
            sum += i;
        }
        return sum;
    }
    default { state_entry() {
        llSay(foo(5),(string)foo(6)); }
    }
    |] v0


t2 = tscript [] [lsl|
    integer x;
    // pragma inline
    integer bar(integer i) {
       integer x = 5;
       return i * x;
    }

    default {
        state_entry() {
            integer i;
            for (i = 0; i < bar(i); i++) {
                llSay(bar(i),"hello");
            }
        }
    }|] v0

pragmaScript3 = [lsl|
    integer x;
    // pragma inline
    float sqrtR(float val) {
       return llSqrt(DEG_TO_RAD * val);
    }

    default {
       state_entry() {
           llOwnerSay("- trace pt 1 -" + (string)sqrtR(90.0));

           if (sqrtR(45.0) < 2.0) {
              llOwnerSay("- trace pt 2 -");
           }

           float f = 0;
           while (sqrtR(f) < 1.0) {
              llOwnerSay("- trace pt 3 -");
              f += 90.0;
           }

           do {
              llOwnerSay("- trace pt 4 -");
           } while (sqrtR(f) < 1.0);

           for (f = 0.0; sqrtR(f) < 1.0; f += 90.0) {
              llOwnerSay("- trace pt 5 -");
           }
       }
    }|]

t3 = tscript [] pragmaScript3 v0

t4 = tscript [] [lsl|
    // pragma inline
    say(float f) {
        x = 81.0;
        llSay(0,(string)f);
    }

    float x = 100.0;
    default {
        state_entry() {
            say(x);
        }
    }|] v0

t5 = tscript [] [lsl|
    //pragma inline
    foo() {
        llOwnerSay("hello");
    }

    // pragma inline
    bar() {
        foo();
    }

    default {
        state_entry() {
          bar();
        }
    }|] v0

m0 = [lslm|
   $module ()

   // pragma inline
   float f(float v) {
       return llFabs(v);
   }

   //pragma inline
   float g(float v) {
       return v * f(v);
   }
   |]

t6 = tscript [("m0",m0)] [lsl|
    $import m0 ();

    default {
        state_entry() {
            llOwnerSay((string)g(7.0));
        }
    }|] v0


t7 = tscript [] [lsl|
    integer x = 7;

    impure() {
        x--;
    }

    // pragma inline
    float f(vector v) {
        float x = v.x;

        impure();
        return x;
    }

    default {
       state_entry() {

           integer i = 0;

           for (i = 0; i < x; i++) {
               llOwnerSay((string)f(<0,1,0>));
           }
       }
    }|] v0


t8noinlining = tscript [] [lsl|
    // pragma inline
    foo(string s) {
        llOwnerSay(s);
    }

    // pragma noinlining
    bar() {
        foo("bar");
    }

    default {
        state_entry() {
            foo("state_entry:default");
            bar();
            state other;
        }

        // pragma noinlining
        state_exit() {
            foo("state_exit:default");
        }
    }

    // pragma noinlining
    state other {
        state_entry() {
            foo("state_entry:other");
        }
    }
    |] v0

t9 = tscript [] [lsl|
        // pragma inline
        float foo(vector v) {
            impure();
            return v.x;
        }

        impure() {
            llSetPos(<128,128,0>);
        }

        default {
            state_entry() {
                llOwnerSay((string)foo(<1.0,2.0,3.0>));
            }
        }
    |] v0

t10 = tscript [] [lsl|
        integer fib(integer n) {
            integer prev1 = 1;
            integer prev2 = 1;

            if (n < 2) return 1;

            integer i;
            integer val = prev1 + prev2;
            for (i = 2; i < n; i++) {
                prev1 = prev2;
                prev2 = val;
                val = prev1 + prev2;
            }

            return val;
        }

        default {
            state_entry() {
                llOwnerSay((string)fib(llAbs(-5)));
            }
        }
    |] v0

t11 = tscript [] [lsl|
        // pragma inline
        integer sqr(integer i) {
            return i * i;
        }

        integer i = 5;

        default {
            state_entry() {
                vector v = llGetPos();
                integer x = (integer) v.x;

                llOwnerSay((string)sqr(x));
            }
        }
    |] v0

t12 = tscript [] [lsl|
    float x = PI;
    // pragma inline
    float foo(integer i) {
       float v = x;

       float x = 7.0;

       integer j = 0;

       for (j = 0; j < i; j++) {
           x *= v;
       }

       return x;
    }

    default {
        state_entry() {
            llOwnerSay((string)foo(llGetStartParameter()));
        }
    }
    |] v0


t13 = tscript [] [lsl|
    float x = 0;

    // pragma inline
    float foo(integer i) {
        float v = x;

        integer j;
        float r =0;
        for (j = 0; j < i; j++) {
            float x = (float) j*j;
            r += x;
        }

        x = r;
        return r;
    }

    default {
        state_entry() {
             llOwnerSay((string)foo(llGetStartParameter() + 5));
             llOwnerSay((string)x);
        }
    }
    |] v0

t14 = tscript [] [lsl|
    float x = 0;

    // pragma inline
    float foo(integer i) {
        float v = x;

        integer j;
        float r =0;
        for (j = 0; j < i; j++) {
            float x = (float) j*j;
            r += x;
        }

        x = r;
        return r;
    }

    default {
        state_entry() {
             {
                 float x = 0;
                 llOwnerSay((string)foo(llGetStartParameter() + 5));
             }
             llOwnerSay((string)x);
        }
    }
    |] v0

t15 = tscript [] [lsl|
    float x = 4.5;
    float foo(float x) {
        integer i;

        float tot = 0;
        for (i = 0; i < 7; i++) {
            tot += i + x;
        }

        return tot;
    }

    default {
        state_entry() {
             vector x = llGetPos();
             llOwnerSay((string)foo(x.x));
             llOwnerSay((string)foo(7));
        }
    }
    |] v0

t16 = tscript [] [lsl|
    rotation x = <4.5,4.5,7.2,7.2>;
    float foo(float x) {
        integer i;

        float tot = 0;
        for (i = 0; i < 7; i++) {
            tot += i + x;
        }

        return tot;
    }

    default {
        state_entry() {
             vector v = llGetPos();
             llOwnerSay((string)foo(v.x));
             llOwnerSay((string)foo(x.s));
        }
    }
    |] v0

t17 = tscript [] [lsl|
    rotation x = <4.5,4.5,7.2,7.2>;
    float foo(float x) {
        integer i;

        float tot = 0;
        for (i = 0; i < 7; i++) {
            tot += i + x;
        }

        return tot;
    }

    default {
        state_entry() {
             vector v = llGetPos();
             llOwnerSay((string)foo(v.x));
             llOwnerSay((string)foo(x.s));
             llOwnerSay((string)x);
        }
    }
    |] v0

t18 = tscript [] [lsl|
    // pragma inline
    func1( string l )
    {
    	llMessageLinked( LINK_THIS, -1, l + "", "FOO" );
    }

    default
    {
        state_entry()
        {
            string l = "";
            func1 ( l + "42" );
        }
    }
    |] v0

t19 = tscript [] [lsl|
    integer MINIMUM = 30;
    integer current = MINIMUM;
    default
    {
        state_entry()
        {
            current++;
        }
    }
    |] v0

t20 = tscript [] [lsl|
        // pragma inline
        f1( vector v )
        {
            if( v.x < 0.0 )
                llSay( 0, "I am broken." );
            if (1);
        }

        f2()
        {
            f1( ZERO_VECTOR );
        }

        default
        {
            state_entry()
            {
                f2();

                if (1);
            }
        }
    |] v0

t21 = tscript [] [lsl|
    default {
        state_entry() {
            while (1);
            do;
            while (1);
        }
    }
    |] v0

t22 = tscript [] [lsl|
// from http://wiki.secondlife.com/w/index.php?title=LSL_Library_Call_Test_1&oldid=67582

integer gTests = 0;

test(string name)
{
    ++gTests;
    //llSay(0, name);
}

tests()
{
    float floatResult;
    integer integerResult;
    string stringResult;
    key keyResult;
    vector vectorResult;
    rotation rotationResult;
    list listResult;

    test("llSin"); floatResult = llSin(0.5);
    test("llCos"); floatResult = llCos(0.5);
    test("llTan"); floatResult = llTan(0.5);
    test("llAtan2"); floatResult = llAtan2(0.5, 0.5);
    test("llSqrt"); floatResult = llSqrt(0.5);
    test("llPow"); floatResult = llPow(0.5, 0.5);
    test("llAbs"); integerResult = llAbs(42);
    test("llFabs"); floatResult = llFabs(0.5);
    test("llFrand"); floatResult = llFrand(0.5);
    test("llFloor"); integerResult = llFloor(0.5);
    test("llCeil"); integerResult = llCeil(0.5);
    test("llRound"); integerResult = llRound(0.5);
    test("llVecMag"); floatResult = llVecMag(<1.1,2.2,3.3>);
    test("llVecNorm"); vectorResult = llVecNorm(<1.1,2.2,3.3>);
    test("llVecDist"); floatResult = llVecDist(<1.1,2.2,3.3>, <1.1,2.2,3.3>);
    test("llRot2Euler"); vectorResult = llRot2Euler(<1.1,2.2,3.3,4.4>);
    test("llEuler2Rot"); rotationResult = llEuler2Rot(<1.1,2.2,3.3>);
    test("llAxes2Rot"); rotationResult = llAxes2Rot(<1.1,2.2,3.3>, <1.1,2.2,3.3>, <1.1,2.2,3.3>);
    test("llRot2Fwd"); vectorResult = llRot2Fwd(<1.1,2.2,3.3,4.4>);
    test("llRot2Left"); vectorResult = llRot2Left(<1.1,2.2,3.3,4.4>);
    test("llRot2Up"); vectorResult = llRot2Up(<1.1,2.2,3.3,4.4>);
    test("llRotBetween"); rotationResult = llRotBetween(<1.1,2.2,3.3>, <1.1,2.2,3.3>);
    test("llWhisper"); llWhisper(42, "foo");
    test("llSay"); llSay(42, "foo");
    test("llShout"); llShout(42, "foo");
    test("llListen"); integerResult = llListen(42, "foo", NULL_KEY, "foo");
    test("llListenControl"); llListenControl(42, 42);
    test("llListenRemove"); llListenRemove(42);
    test("llSensor"); llSensor("foo", NULL_KEY, 42, 0.5, 0.5);
    test("llSensorRepeat"); llSensorRepeat("foo", NULL_KEY, 42, 0.5, 0.5, 0.5);
    test("llSensorRemove"); llSensorRemove();
    test("llDetectedName"); stringResult = llDetectedName(42);
    test("llDetectedKey"); keyResult = llDetectedKey(42);
    test("llDetectedOwner"); keyResult = llDetectedOwner(42);
    test("llDetectedType"); integerResult = llDetectedType(42);
    test("llDetectedPos"); vectorResult = llDetectedPos(42);
    test("llDetectedVel"); vectorResult = llDetectedVel(42);
    test("llDetectedGrab"); vectorResult = llDetectedGrab(42);
    test("llDetectedRot"); rotationResult = llDetectedRot(42);
    test("llDetectedGroup"); integerResult = llDetectedGroup(42);
    test("llDetectedLinkNumber"); integerResult = llDetectedLinkNumber(42);
    test("llDie"); //llDie();
    test("llGround"); floatResult = llGround(<1.1,2.2,3.3>);
    test("llCloud"); floatResult = llCloud(<1.1,2.2,3.3>);
    test("llWind"); vectorResult = llWind(<1.1,2.2,3.3>);
    test("llSetStatus"); llSetStatus(42, 42);
    test("llGetStatus"); integerResult = llGetStatus(42);
    test("llSetScale"); llSetScale(<1.1,2.2,3.3>);
    test("llGetScale"); vectorResult = llGetScale();
    test("llSetColor"); llSetColor(<1.1,2.2,3.3>, 42);
    test("llGetColor"); vectorResult = llGetColor(0);
    test("llSetAlpha"); llSetAlpha(0.5, 42);
    test("llGetAlpha"); floatResult = llGetAlpha(42);
    test("llSetTexture"); llSetTexture("foo", 42);
    test("llScaleTexture"); llScaleTexture(0.5, 0.5, 42);
    test("llOffsetTexture"); llOffsetTexture(0.5, 0.5, 42);
    test("llRotateTexture"); llRotateTexture(0.5, 42);
    test("llGetTexture"); stringResult = llGetTexture(42);
    test("llSetPos"); llSetPos(<1.1,2.2,3.3>);
    test("llGetPos"); vectorResult = llGetPos();
    test("llGetLocalPos"); vectorResult = llGetLocalPos();
    test("llSetRot"); llSetRot(<1.1,2.2,3.3,4.4>);
    test("llGetRot"); rotationResult = llGetRot();
    test("llGetLocalRot"); rotationResult = llGetLocalRot();
    test("llSetForce"); llSetForce(<1.1,2.2,3.3>, 42);
    test("llGetForce"); vectorResult = llGetForce();
    test("llMoveToTarget"); llMoveToTarget(<1.1,2.2,3.3>, 0.5);
    test("llStopMoveToTarget"); llStopMoveToTarget();
    test("llTarget"); integerResult = llTarget(<1.1,2.2,3.3>, 0.5);
    test("llTargetRemove"); llTargetRemove(42);
    test("llRotTarget"); integerResult = llRotTarget(<1.1,2.2,3.3,4.4>, 0.5);
    test("llRotTargetRemove"); llRotTargetRemove(42);
    test("llApplyImpulse"); llApplyImpulse(<1.1,2.2,3.3>, 42);
    test("llApplyRotationalImpulse"); llApplyRotationalImpulse(<1.1,2.2,3.3>, 42);
    test("llSetTorque"); llSetTorque(<1.1,2.2,3.3>, 42);
    test("llGetTorque"); vectorResult = llGetTorque();
    test("llSetForceAndTorque"); llSetForceAndTorque(<1.1,2.2,3.3>, <1.1,2.2,3.3>, 42);
    test("llGetVel"); vectorResult = llGetVel();
    test("llGetAccel"); vectorResult = llGetAccel();
    test("llGetOmega"); vectorResult = llGetOmega();
    test("llGetTimeOfDay"); floatResult = llGetTimeOfDay();
    test("llGetWallclock"); floatResult = llGetWallclock();
    test("llGetTime"); floatResult = llGetTime();
    test("llResetTime"); //llResetTime();
    test("llGetAndResetTime"); //floatResult = llGetAndResetTime();
    test("llSound"); llSound("foo", 0.5, 42, 42);
    test("llPlaySound"); llPlaySound(NULL_KEY, 0.5);
    test("llLoopSound"); llLoopSound("foo", 0.5);
    test("llLoopSoundMaster"); llLoopSoundMaster("foo", 0.5);
    test("llLoopSoundSlave"); llLoopSoundSlave("foo", 0.5);
    test("llPlaySoundSlave"); llPlaySoundSlave("foo", 0.5);
    test("llTriggerSound"); llTriggerSound(NULL_KEY, 0.5);
    test("llStopSound"); llStopSound();
    test("llPreloadSound"); llPreloadSound("foo");
    test("llGetSubString"); stringResult = llGetSubString("foo", 42, 42);
    test("llDeleteSubString"); stringResult = llDeleteSubString("foo", 42, 42);
    test("llInsertString"); stringResult = llInsertString("foo", 42, "foo");
    test("llToUpper"); stringResult = llToUpper("foo");
    test("llToLower"); stringResult = llToLower("foo");
// TODO #2016190
    test("llGiveMoney"); integerResult = llGiveMoney(NULL_KEY, 42);
    test("llMakeExplosion"); llMakeExplosion(42, 0.5, 0.5, 0.5, 0.5, "foo", <1.1,2.2,3.3>);
    test("llMakeFountain"); llMakeFountain(42, 0.5, 0.5, 0.5, 0.5, 1,"foo", <1.1,2.2,3.3>, 0);
    test("llMakeSmoke"); llMakeSmoke(42, 0.5, 0.5, 0.5, 0.5, "foo", <1.1,2.2,3.3>);
    test("llMakeFire"); llMakeFire(42, 0.5, 0.5, 0.5, 0.5, "foo", <1.1,2.2,3.3>);
    test("llRezObject"); llRezObject("foo", <1.1,2.2,3.3>, <1.1,2.2,3.3>, <1.1,2.2,3.3,4.4>, 42);
    test("llLookAt"); llLookAt(<1.1,2.2,3.3>, 0.5, 0.5);
    test("llStopLookAt"); llStopLookAt();
    test("llSetTimerEvent"); llSetTimerEvent(0.5);
    test("llSleep"); llSleep(0.5);
    test("llGetMass"); floatResult = llGetMass();
    test("llCollisionFilter"); llCollisionFilter("foo", NULL_KEY, 42);
    test("llTakeControls"); llTakeControls(42, 42, 42);
    test("llReleaseControls"); llReleaseControls();
    test("llAttachToAvatar"); llAttachToAvatar(42);
    test("llDetachFromAvatar"); llDetachFromAvatar();
    test("llGetOwner"); keyResult = llGetOwner();
    test("llInstantMessage"); llInstantMessage(NULL_KEY, "foo");
    test("llEmail"); llEmail("foo", "foo", "foo");
    test("llGetNextEmail"); llGetNextEmail("foo", "foo");
    test("llGetKey"); keyResult = llGetKey();
    test("llSetBuoyancy"); llSetBuoyancy(0.5);
    test("llSetHoverHeight"); llSetHoverHeight(0.5, 42, 0.5);
    test("llStopHover"); llStopHover();
    test("llMinEventDelay"); llMinEventDelay(0.5);
    test("llSoundPreload"); llSoundPreload(NULL_KEY);
    test("llRotLookAt"); llRotLookAt(<1.1,2.2,3.3,4.4>, 0.5, 0.5);
    test("llStringLength"); integerResult = llStringLength("foo");
    test("llStartAnimation"); llStartAnimation("foo");
    test("llStopAnimation"); llStopAnimation("foo");
    test("llPointAt"); llPointAt(<1.1,2.2,3.3>);
    test("llStopPointAt"); llStopPointAt();
    test("llTargetOmega"); llTargetOmega(<1.1,2.2,3.3>, 0.5, 0.5);
    test("llGetStartParameter"); integerResult = llGetStartParameter();
    //test("llGodLikeRezObject"); llGodLikeRezObject(NULL_KEY, <1.1,2.2,3.3>);
    test("llRequestPermissions"); llRequestPermissions(NULL_KEY, 42);
    test("llGetPermissionsKey"); keyResult = llGetPermissionsKey();
    test("llGetPermissions"); integerResult = llGetPermissions();
    test("llGetLinkNumber"); integerResult = llGetLinkNumber();
    test("llSetLinkColor"); llSetLinkColor(42, <1.1,2.2,3.3>, 42);
    test("llCreateLink"); llCreateLink(NULL_KEY, 42);
    test("llBreakLink"); llBreakLink(42);
    test("llLinks"); llBreakAllLinks();
    test("llGetLinkKey"); keyResult = llGetLinkKey(42);
    test("llGetLinkName"); stringResult = llGetLinkName(42);
    test("llGetInventoryNumber"); integerResult = llGetInventoryNumber(42);
    test("llGetInventoryName"); stringResult = llGetInventoryName(42, 42);
    test("llSetScriptState"); llSetScriptState("foo", 42);
    test("llGetEnergy"); floatResult = llGetEnergy();
    test("llGiveInventory"); llGiveInventory(NULL_KEY, "foo");
    test("llRemoveInventory"); llRemoveInventory("foo");
    test("llSetText"); llSetText("foo", <1.1,2.2,3.3>, 0.5);
    test("llWater"); floatResult = llWater(<1.1,2.2,3.3>);
    test("llPassTouches"); llPassTouches(42);
    test("llRequestAgentData"); keyResult = llRequestAgentData(NULL_KEY, 42);
    test("llRequestInventoryData"); keyResult = llRequestInventoryData("foo");
    test("llSetDamage"); llSetDamage(0.5);
    test("llTeleportAgentHome"); llTeleportAgentHome(NULL_KEY);
    test("llModifyLand"); llModifyLand(42, 42);
    test("llCollisionSound"); llCollisionSound("foo", 0.5);
    test("llCollisionSprite"); llCollisionSprite("foo");
    test("llGetAnimation"); stringResult = llGetAnimation(NULL_KEY);
    test("llGetAnimationList"); listResult = llGetAnimationList(NULL_KEY);
    //test("llResetScript"); llResetScript();
    test("llMessageLinked"); llMessageLinked(42, 42, "foo", NULL_KEY);
    test("llPushObject"); llPushObject(NULL_KEY, <1.1,2.2,3.3>, <1.1,2.2,3.3>, 42);
    test("llPassCollisions"); llPassCollisions(42);
    test("llGetScriptName"); stringResult = llGetScriptName();
    test("llGetNumberOfSides"); integerResult = llGetNumberOfSides();
    test("llAxisAngle2Rot"); rotationResult = llAxisAngle2Rot(<1.1,2.2,3.3>, 0.5);
    test("llRot2Axis"); vectorResult = llRot2Axis(<1.1,2.2,3.3,4.4>);
    test("llRot2Angle"); floatResult = llRot2Angle(<1.1,2.2,3.3,4.4>);
    test("llAcos"); floatResult = llAcos(0.5);
    test("llAsin"); floatResult = llAsin(0.5);
    test("llAngleBetween"); floatResult = llAngleBetween(<1.1,2.2,3.3,4.4>, <1.1,2.2,3.3,4.4>);
}

default
{
    touch_start(integer total_number)
    {
        llResetTime();
        tests();
        llSay(0, "Ran " + (string) gTests + " tests in " + (string) llGetTime() + " seconds");
        gTests = 0;
    }
}
|] v0

t23 = tscript [] [lsl|
    // pragma inline
    float do_stuff( float input )
    {
    	integer i;
    	for( i=0; i<5; i++ )
    		if( i == 2 )
    			return llFrand(input);
    	return -1.0;
    }

    default
    {
        state_entry()
        {
            if( llGetStartParameter() )
            {
                float foo = do_stuff(1.0);
            }
            else
            {
                float bar = do_stuff(2.0);
            }
        }
    }
|] v0

t24 = tscript [] [lsl|
   integer BASIC = FALSE;

   foo() {
       if (BASIC) {
           llOwnerSay("hello");
       }
   }

   // pragma inline
   bar() {
       if (BASIC) {
           llOwnerSay("hi");
       }
   }

   default {
      state_entry() {
          foo();
          bar();
          if (BASIC) {
              llOwnerSay("goodbye");
          }
      }
   }
   |] v0


t25 = tscript [] [lsl|
    //pragma inline
    string get() {
    	if (llGetStartParameter()) return "a";
    	return "b";
    }

    //pragma inline
    string send(string s) {
    	string k = llGetDate();
    	llOwnerSay(k + get());
    	return k;
    }

    default
    {
        state_entry()
        {
            send("Some message");
        }
    }
    |] v0

t26 = tscript [] [lsl|
    //pragma inline
    do_stuff()
    {
    	if( llGetStartParameter() > 1.0 )
    		llOwnerSay("Foo" );
    }

    default
    {
        state_entry()
        {
            if( llGetStartParameter() < 1.0 )
            	do_stuff();
            else
            	llOwnerSay("Bar" );
        }
    }|] v0

t27 = tscript [] [lsl|
    // pragma inline
    list parse(string src) {
        return llCSV2List(src);
    }

    // pragma inline
    string listStr(list src, integer pos) {
        return llList2String(src, pos);
    }

    default {
        state_entry() {
            string params = llGetOwner();
            llOwnerSay(listStr(parse(params), 0));
        }
    }
    |] v0

m28 = [lslm|
    $module ()

    string unused = "unused";
    |]

s28 = [lsl|
    $import unused ();

    default {
        state_entry() {
            llOwnerSay("hi");
        }
    }
    |]

t28 = tscript [("unused",m28)] s28 v0

t29 = tscript [] [lsl|
    default {
        state_entry() {
            llOwnerSay((string)llFrand(100.0));
            llOwnerSay((string)llFrand(100.0));
            llOwnerSay((string)llFrand(100.0));
            llOwnerSay((string)llFrand(100.0));
            llOwnerSay((string)llFrand(100.0));
            llOwnerSay((string)llFrand(100.0));
        }
    }
    |] v0

t30 = tscript [] [lsl|
    // pragma inline
    call(list lst) {
        llOwnerSay(llList2CSV(lst));
    }


    string frameNotecard = "foo";
    default {
        state_entry() {
            call([]);
            call([frameNotecard]);
        }
    }
    |] v0

t31 = tscript [] [lsl|
   default {
    state_entry()
    {
        integer i = 0;
        llOwnerSay((string)(++i) );
        llOwnerSay((string)(i++) );
    }
}|] v0

t32 = tscript [] [lsl|
    default {
        state_entry() {
            llOwnerSay((string) llAxisAngle2Rot(<1,3,3>,12));
        }
    }
    |] v0

optTests = TestList [ t1, t2, t3, t4, t5, t6, t7, t8noinlining, t9, t10, t11, t12, t13,
                      t14, t15, t16, t17, t18, t19, t20, t21, t22, t23, t24, t25, t26,
                      t27, t28, t29]
