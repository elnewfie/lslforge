// LSL script generated: Sun Apr 12 23:52:35 Eastern Daylight Time 2009
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
startup(){
    scrubKeys();
    llOwnerSay("You chat at the mirror, mirror, on the wall ...");
    list lines = [llGetObjectName(),llGetObjectDesc()];
    string label = llDumpList2String(lines,"\n---\n");
    llSetText(label,<1.0,1.0,1.0>,1.0);
}
scrubKeys(){
    llSetText("",<0.0,0.0,0.0>,1.0);
    (theDataNameKey = NULL_KEY);
    (theDataBornKey = NULL_KEY);
    (theDataPayInfoKey = NULL_KEY);
    (theDataOnlineKey = NULL_KEY);
    (receivedKeyings = 0);
}
receiveKeys(){
    string text = ((((((((((((((((("" + "---\n") + " \n") + theDataNameString) + "\n") + " \n") + "Born ") + theDataBornString) + "\n") + " \n") + toPayInfoEcho(theDataPayInfoString)) + "\n") + " \n") + toOnlineEcho(theDataOnlineString)) + "\n") + " \n") + "---\n") + " \n ");
    vector color = <0.0,1.0,1.0>;
    float opacity = 1.0;
    llSetText(text,color,opacity);
}
string toPayInfoEcho(string data){
    integer payInfo = ((integer)data);
    if ((payInfo & (~(PAYMENT_INFO_ON_FILE | PAYMENT_INFO_USED)))) return data;
    integer not_has = (!(payInfo & PAYMENT_INFO_ON_FILE));
    integer not_used = (!(payInfo & PAYMENT_INFO_USED));
    string out = "Has ";
    if (not_has) (out += "not ");
    if ((not_has == not_used)) (out += "used but does");
    else  (out += "used and does");
    if (not_used) (out += " not");
    return (out + " have payment info on file");
}
string toOnlineEcho(string data){
    integer online = ((integer)data);
    if ((!online)) {
        return "Not online";
    }
    return "Online";
}
catchKey(key queryid,string data){
    if ((queryid == theDataNameKey)) {
        (theDataNameString = data);
    }
    else  if ((queryid == theDataBornKey)) {
        (theDataBornString = data);
    }
    else  if ((queryid == theDataPayInfoKey)) {
        (theDataPayInfoString = data);
    }
    else  if ((queryid == theDataOnlineKey)) {
        (theDataOnlineString = data);
    }
}
sendKeys(key who){
    (theDataNameKey = llRequestAgentData(who,DATA_NAME));
    (theDataBornKey = llRequestAgentData(who,DATA_BORN));
    (theDataPayInfoKey = llRequestAgentData(who,DATA_PAYINFO));
    (theDataOnlineKey = llRequestAgentData(who,DATA_ONLINE));
}
default {
    state_entry() {
        startup();
        llListen(0,"",NULL_KEY,"");
    }
    listen(integer channel,string name,key id,string message) {
        if ((llGetAgentSize(id) != ZERO_VECTOR)) {
            scrubKeys();
            sendKeys(id);
        }
    }
    dataserver(key queryid,string data) {
        catchKey(queryid,data);
        (receivedKeyings += 1);
        if ((receivedKeyings == sendableKeyings)) {
            receiveKeys();
        }
    }
}
