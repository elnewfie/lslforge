// ATTRIBUTION_BEGIN
// This work uses content from the Second Life® Wiki article:
// http://wiki.secondlife.com/wiki/Describe_Chatter
// Copyright © 2008 Linden Research, Inc. 
// Licensed under the Creative Commons Attribution-Share Alike 3.0 License:
// http://creativecommons.org/licenses/by-sa/3.0
// See the complete license terms:
// http://creativecommons.org/licenses/by-sa/3.0/legalcode
// ATTRIBUTION_END

// Chat to see yourself as others do
// http://wiki.secondlife.com/wiki/Describe_Chatter
 
key theDataNameKey;
key theDataBornKey;
key theDataPayInfoKey;
key theDataOnlineKey;
 
integer sendableKeyings = 4;
integer receivedKeyings;
 
string theDataNameString;
string theDataBornString;
string theDataPayInfoString;
string theDataOnlineString;
 
// Say why chat with this script
 
startup()
{
    scrubKeys();
    llOwnerSay("You chat at the mirror, mirror, on the wall ...");
    list lines = [llGetObjectName(), llGetObjectDesc()];
    string label = llDumpList2String(lines, "\n---\n");
    llSetText(label, <1.0, 1.0, 1.0>, 1.0);
}
 
// Forget old chat
 
scrubKeys()
{
    llSetText("", <0.0, 0.0, 0.0>, 1.0);
 
    theDataNameKey = NULL_KEY;
    theDataBornKey = NULL_KEY;
    theDataPayInfoKey = NULL_KEY;
    theDataOnlineKey = NULL_KEY;
 
    receivedKeyings = 0;    
}
 
// Float an image of the chatter above the object
 
receiveKeys()
{
    string text = ""
        + "---\n" + " \n"
        + theDataNameString + "\n" + " \n"
        + "Born " + theDataBornString + "\n" + " \n"
        + toPayInfoEcho(theDataPayInfoString) + "\n" + " \n"
        + toOnlineEcho(theDataOnlineString) + "\n" + " \n"
        + "---\n" + " \n "
        ;
    vector color = <0.0, 1.0, 1.0>;
    float opacity = 1.0;
    llSetText(text, color, opacity);
}      
 
// Convert to string from PAYMENT_INFO
 
string toPayInfoEcho(string data)
{
    integer payInfo = (integer)data;
    if(payInfo  & ~(PAYMENT_INFO_ON_FILE | PAYMENT_INFO_USED))
        return data;
    integer not_has = !(payInfo & PAYMENT_INFO_ON_FILE);
    integer not_used = !(payInfo & PAYMENT_INFO_USED);
    string out = "Has ";
    if(not_has)
        out += "not ";
    if(not_has == not_used)//use the proper conjunction
        out += "used but does";
    else
        out += "used and does";
    if(not_used)
        out += " not";
    return out + " have payment info on file";
}
 
// Convert to string from DATA_ONLINE
 
string toOnlineEcho(string data)
{
    integer online = (integer) data;
    if (!online)
    {
        return "Not online";
    }
     return "Online";
}
 
// Receive an image of the chatter
 
catchKey(key queryid, string data)
{
        if (queryid == theDataNameKey)
        {
            theDataNameString = data;
        }
        else if (queryid == theDataBornKey)
        {
            theDataBornString = data;
        }        
        else if (queryid == theDataPayInfoKey)
        {
            theDataPayInfoString = data;
        }
        else if (queryid == theDataOnlineKey)
        {
            theDataOnlineString = data;
        }
}
 
// Ask to receive an image of the chatter in pieces
 
sendKeys(key who)
{
    theDataNameKey = llRequestAgentData(who, DATA_NAME);
    theDataBornKey = llRequestAgentData(who, DATA_BORN);
    theDataPayInfoKey = llRequestAgentData(who, DATA_PAYINFO);
    theDataOnlineKey = llRequestAgentData(who, DATA_ONLINE);
}
 
// For each reset ...
 
default
{
 
    // Listen to all ordinary chat
 
    state_entry()
    {
        startup();
        llListen(0, "", NULL_KEY, "");
    }
 
    // Ask to receive an image of the chatter in pieces
 
    listen(integer channel, string name, key id, string message)
    {
        if (llGetAgentSize(id) != ZERO_VECTOR)
        {
            scrubKeys();
            sendKeys(id);
        }
    }
 
    // Receive every piece of an image of the chatter
 
    dataserver(key queryid, string data)
    {        
        catchKey(queryid, data);   
        receivedKeyings += 1;
        if (receivedKeyings == sendableKeyings)
        {
            receiveKeys();
        }        
    }
 
}