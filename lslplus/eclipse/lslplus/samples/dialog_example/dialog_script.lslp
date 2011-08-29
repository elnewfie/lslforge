

default {
    state_entry() {
        llSensor("Default Avatar", NULL_KEY, AGENT, 96.0, PI);
    }
    
    sensor(integer num_detected){
    	key k = llDetectedKey(0);
    	llListen(-12345, "", NULL_KEY, "");
    	llDialog(k, "hello", ["Not Ok", "Ok"], -12345);
    }
    
    no_sensor(){
    	llOwnerSay("couldn't find Default Avatar to talk to");
    }
    
    listen(integer channel, string name, key id, string message){
    	llOwnerSay("on channel " + (string) channel + ", received message \"" + message +
    	           "\" from " + name);
    }
}
