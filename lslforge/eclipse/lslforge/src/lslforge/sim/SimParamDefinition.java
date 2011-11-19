package lslforge.sim;

import com.thoughtworks.xstream.XStream;

public class SimParamDefinition {
    public abstract static class SimParamType {
        public abstract String getControlID();
    }
    public static class SimParamPrim extends SimParamType { 
        @Override
		public String getControlID() {
            return "prim"; //$NON-NLS-1$
        }
    }
    public static class SimParamAvatar extends SimParamType { 
        @Override
		public String getControlID() {
            return "avatar"; //$NON-NLS-1$
        }
    }
    public static class SimParamObject extends SimParamType {
        @Override
		public String getControlID() {
            return "object"; //$NON-NLS-1$
        }
    }
    public static class SimParamKey extends SimParamType { 
        @Override
		public String getControlID() {
            return "any-key"; //$NON-NLS-1$
        }
    }
    public static class SimParamScript extends SimParamType { 
        @Override
		public String getControlID() {
            return "script"; //$NON-NLS-1$
        }
    }
    public static class SimParamValue extends SimParamType {
        private String valueType;
        
        public String getValueType() { return valueType; }
        
        @Override
		public String getControlID() {
            return "expression-" + valueType; //$NON-NLS-1$
        }
    }
    
    private String name;
    private String description;
    private SimParamType type;
    public String getName() { return name; }
    public String getDescription() { return description; }
    public SimParamType getType() { return type; }
    public String getControlID() {
        return getType().getControlID();
    }
    
    public static void configureXStream(XStream xstream) {
        xstream.alias("param", SimParamDefinition.class); //$NON-NLS-1$
        xstream.aliasType("prim", SimParamPrim.class); //$NON-NLS-1$
        xstream.aliasType("avatar", SimParamAvatar.class); //$NON-NLS-1$
        xstream.aliasType("object", SimParamObject.class); //$NON-NLS-1$
        xstream.aliasType("script", SimParamScript.class); //$NON-NLS-1$
        xstream.aliasType("any-key", SimParamKey.class); //$NON-NLS-1$
        xstream.aliasType("value", SimParamValue.class); //$NON-NLS-1$
    }
}
