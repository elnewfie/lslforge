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

integer Scan = TRUE;
string avKey;
integer list_pos;
list key_list;
integer key_chan;//Key channel is generated randomly and passed to the scan ball
integer die_chan = -9423753;//Hey pick your own channels and be sure to paste them into 
                            //the scan balls too!
integer key_rem_chan = -49222879;
default {
	state_entry() {
		llSetObjectName("3D Radar");
	}
	touch_start(integer total_number) {
		if (Scan) {
			llSensorRepeat("", "", AGENT, 96, PI, 1);
			key_list =[];
			llListen(key_rem_chan, "", "", "");
			llOwnerSay("on");
			Scan = FALSE;
		}
		else {
			llSensorRemove();
			llRegionSay(die_chan, "die");
			llOwnerSay("off");
			Scan = TRUE;
		}
	}
	sensor(integer iNum) {
		integer p = 0;
		for (p = 0; p < iNum; ++p) {
			avKey = llDetectedKey(p);
			list_pos = llListFindList(key_list,[avKey]);
			if (list_pos == -1) {
				key_list = (key_list =[]) + key_list +[avKey];
				key_chan = (integer)llFrand(-1000000) - 1000000;
				llRezObject("scan ball", llGetPos(), ZERO_VECTOR, ZERO_ROTATION, key_chan);
				llSleep(.25);
				llRegionSay(key_chan, avKey);
			}
		}
	}
	listen(integer c, string name, key id, string msg) {
		key remove_key = msg;
		integer r = llListFindList(key_list,[remove_key]);
		llDeleteSubList(key_list, r, r);
	}
}
 