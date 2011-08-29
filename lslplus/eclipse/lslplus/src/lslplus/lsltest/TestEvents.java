package lslplus.lsltest;

import lslplus.debug.LslScriptExecutionState;

import com.thoughtworks.xstream.XStream;
import com.thoughtworks.xstream.io.xml.DomDriver;

public class TestEvents {

    public static class TestEvent { }
    
    public static class TestCompleteEvent extends TestEvent {
        private TestResult result;
        public TestResult getTestResult() { return result; }
    }
    
    public static class AllCompleteEvent extends TestEvent {}
    
    public static class TestSuspendedEvent extends TestEvent {
        LslScriptExecutionState scriptState;
        public LslScriptExecutionState getScriptState() { return scriptState; }
    }
    
    private static XStream xstream;
    
    static {
        xstream = new XStream(new DomDriver());
        LslScriptExecutionState.configureXStream(xstream);
        TestResult.configureXStream(xstream);
        xstream.alias("test-complete", TestCompleteEvent.class); //$NON-NLS-1$
        xstream.alias("all-complete", AllCompleteEvent.class); //$NON-NLS-1$
        xstream.alias("test-suspended", TestSuspendedEvent.class); //$NON-NLS-1$
        xstream.aliasField("test-result", TestCompleteEvent.class, "result"); //$NON-NLS-1$ //$NON-NLS-2$
        xstream.aliasField("script-state", TestSuspendedEvent.class, "scriptState");  //$NON-NLS-1$//$NON-NLS-2$
    }
    
    public static TestEvent fromXML(String xml) {
        //Util.log(xml);
        return (TestEvent) xstream.fromXML(xml);
    }
    
}
