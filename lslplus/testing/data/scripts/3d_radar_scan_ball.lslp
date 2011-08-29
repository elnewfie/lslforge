// ATTRIBUTION_BEGIN
// This work uses content from the Second Life® Wiki article:
// http://wiki.secondlife.com/wiki/3D_Radar
// Copyright © 2008 Linden Research, Inc. 
// Author: Jesse Barnett, others
// Licensed under the Creative Commons Attribution-Share Alike 3.0 License:
// http://creativecommons.org/licenses/by-sa/3.0
// See the complete license terms:
// http://creativecommons.org/licenses/by-sa/3.0/legalcode
// ATTRIBUTION_END

string avName;
string avDistance;
key avKey;
integer avListen;
integer key_chan;
integer die_chan = -9423753;
integer key_rem_chan = -49222879;
vector avPos;
vector rPos;
default {
	state_entry() {
		llSetObjectName("scan ball");
	}
	on_rez(integer start_param) {
		rPos = llGetPos();
		key_chan = start_param;
		llListen(die_chan, "", "", "");
		avListen = llListen(key_chan, "", "", "");
	}
	listen(integer c, string n, key id, string msg) {
		if (c == die_chan)
			llDie();
		else {
			avKey = (key) msg;
			llSensorRepeat("", avKey, AGENT, 96, PI, 1.0);
			llListenRemove(avListen);
		}
	}
	sensor(integer n) {
		avPos = llDetectedPos(0);
		integer name = TRUE;
		if (name) {
			avName = llDetectedName(0);
			name = FALSE;
		}
		vector avDivPos = (avPos - rPos) * 0.010417;//Scan range/Radius of large sphere
		avDistance = (string) llVecDist(rPos, llDetectedPos(0));
		llSetPos(rPos + avDivPos);
		llSetText(avName + "[" + avDistance + "]", <1, 1, 1 >, 1);
	}
	no_sensor() {
		llRegionSay(key_rem_chan, avKey);
		llOwnerSay(avName + " is now out of range.");
		llDie();
	}
}
 