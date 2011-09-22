package lslforge.sim;

import com.thoughtworks.xstream.XStream;

public class SimEventDefinition {
    private String name;
    private String description;
    private SimParamDefinition[] params;
    public static void configureXStream(XStream xstream) {
         xstream.alias("event-def", SimEventDefinition.class); //$NON-NLS-1$
         SimParamDefinition.configureXStream(xstream);
    }
    
    public String getName() { return name; }
    public String getDescription() { return description; }
    public SimParamDefinition[] getParams() { return params; }
}
