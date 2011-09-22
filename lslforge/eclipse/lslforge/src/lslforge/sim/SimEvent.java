package lslforge.sim;

import com.thoughtworks.xstream.XStream;

public class SimEvent {
    private static XStream xstream = new XStream();
    
    public static void configureXStream(XStream xstream) {
        xstream.alias("event", SimEvent.class); //$NON-NLS-1$
        xstream.alias("arg", SimEventArg.class); //$NON-NLS-1$
    }
    
    static {
        configureXStream(xstream);
    }
    
    @SuppressWarnings("unused")
	private String name;
    @SuppressWarnings("unused")
	private int delay;
    @SuppressWarnings("unused")
	private SimEventArg args[];
    
    public SimEvent() { }
    
    public SimEvent(String name, int delay, SimEventArg[] args) {
        this.name = name;
        this.delay = delay;
        this.args = args;
    }
}
