
key rpcChannel = NULL_KEY;
default {
    state_entry() {
        llOpenRemoteDataChannel();
        llOwnerSay("opening remote data channel");
    }
    
    remote_data(integer event_type, key channel, key message_id, string sender, integer idata, string sdata){
    	llOwnerSay("got remote data event");
    	if (event_type == REMOTE_DATA_CHANNEL) {
    		rpcChannel = channel;
    		
    		llHTTPRequest("http://www.example.com/post_channel", [HTTP_METHOD, "POST"], (string)channel);
    	} else if (event_type == REMOTE_DATA_REQUEST) {
    		llOwnerSay("recieved request: " + sdata + ", " + (string) idata);
    		llRemoteDataReply(channel, message_id, "", 0);
    	}
    }
    
    http_response(key request_id, integer status, list metadata, string body){
    	llOwnerSay("Registration of channel successful!");
    }
}
