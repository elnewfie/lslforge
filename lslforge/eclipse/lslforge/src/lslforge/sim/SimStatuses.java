package lslforge.sim;

import lslforge.debug.LSLScriptExecutionState;

import com.thoughtworks.xstream.XStream;
import com.thoughtworks.xstream.io.xml.DomDriver;

public class SimStatuses {
    private static XStream xstream = new XStream(new DomDriver());
    
    public static class Message {
        public static final String INFO_LEVEL = "INFO"; //$NON-NLS-1$
        String time;
        String level;
        String source;
        String text;
        
        public String getText() { return text; }
        public String getTime() { return time; }
        public String getSource() { return source; }
        public String getLevel() { return level; }
    }
    
    public static interface NameKeyType {
        public String getName();
        public String getKey();
        public String getCombinedRepresentation();
    }
    
    public static class SimPrim implements NameKeyType {
        private String key;
        private String name;
        
        public String getKey() { return key; }
        public String getName() { return name; }
        
        @Override
		public String toString() {
            return name + " - " + key; //$NON-NLS-1$
        }
        
        public String getCombinedRepresentation() {
            return toString();
        }
    }
    
    public static class SimAvatar implements NameKeyType {
        private String key;
        private String name;
        
        public String getKey() { return key; }
        public String getName() { return name; }
        public String getCombinedRepresentation() {
            
            return toString();
        }
        
        @Override
		public String toString() {
            return name + " - " + key; //$NON-NLS-1$
        }
    }
    
    public static class SimScript {
        private String primKey;
        private String scriptName;
        public String getPrimKey() { return primKey; }
        public String getScriptName() { return scriptName; }
    }
    
    public static class SimState {
        private int time;
        private SimPrim[] prims;
        private SimAvatar[] avatars;
        private SimScript[] scripts;
        
        public int getTime() { return time; }
        public SimPrim[] getPrims() { return prims; }
        public SimAvatar[] getAvatars() { return avatars; }
        public SimScript[] getScripts() { return scripts; }
    }
    
    public static class SimStatus {
        private Message[] messages;
        private SimState state;
        public Message[] getMessages() {
            return messages;
        }
        
        public SimState getState() {
            return state;
            
        }
    }
    
    public static class SimInfo extends SimStatus {
    }

    public static class SimEnded extends SimStatus {
    }
    
    public static class SimSuspended extends SimStatus {
        LSLScriptExecutionState scriptState;
        public LSLScriptExecutionState getScriptState() { return scriptState; }
    }
    
    public static void configureXStream(XStream xstream) {
        LSLScriptExecutionState.configureXStream(xstream);
        xstream.alias("sim-info", SimInfo.class); //$NON-NLS-1$
        xstream.alias("sim-ended", SimEnded.class); //$NON-NLS-1$
        xstream.alias("sim-suspended", SimSuspended.class); //$NON-NLS-1$
        xstream.alias("message", Message.class);        //$NON-NLS-1$
        xstream.alias("prim", SimPrim.class); //$NON-NLS-1$
        xstream.alias("avatar", SimAvatar.class); //$NON-NLS-1$
        xstream.alias("script", SimScript.class); //$NON-NLS-1$
        xstream.aliasField("script-state", SimSuspended.class, "scriptState");  //$NON-NLS-1$//$NON-NLS-2$
    }
    
    static {
        configureXStream(xstream);
    }
    public static SimStatus fromXML(String xml) {
        return (SimStatus) xstream.fromXML(xml);
    }
}
