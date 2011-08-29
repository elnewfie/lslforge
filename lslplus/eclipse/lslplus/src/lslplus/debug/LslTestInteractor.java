package lslplus.debug;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.OutputStream;
import java.io.PrintStream;
import java.util.HashSet;
import java.util.Iterator;
import java.util.LinkedList;

import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IMarker;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IPath;
import org.eclipse.debug.core.DebugPlugin;
import org.eclipse.debug.core.IBreakpointManager;
import org.eclipse.debug.core.ILaunchManager;
import org.eclipse.debug.core.model.IBreakpoint;

import com.thoughtworks.xstream.XStream;
import com.thoughtworks.xstream.io.xml.DomDriver;

import lslplus.LslPlusPlugin;
import lslplus.lsltest.TestEvents;
import lslplus.lsltest.TestManager;
import lslplus.lsltest.TestEvents.AllCompleteEvent;
import lslplus.lsltest.TestEvents.TestCompleteEvent;
import lslplus.lsltest.TestEvents.TestEvent;
import lslplus.lsltest.TestEvents.TestSuspendedEvent;
import lslplus.util.Util;

/**
 * Interact with a running test session.
 */
public class LslTestInteractor implements Runnable, Interactor {
    private static class BreakpointData {
        private static XStream xstream = new XStream(new DomDriver());
        public String file;
        public int line;
        public BreakpointData(String file, int line) {
            this.file = file;
            this.line = line;
        }
        
        public static void configureXStream(XStream xstream) {
            xstream.alias("breakpoint", BreakpointData.class); //$NON-NLS-1$
        }
        
        static {
            configureXStream(xstream);
        }
    }

    private abstract static class ExecCommand {
        protected BreakpointData[] breakpoints = null;
        protected ExecCommand(BreakpointData[] breakpoints) {
            this.breakpoints = breakpoints;
        }
    }
    
    private static class ContinueCommand extends ExecCommand {
        private static XStream xstream = new XStream(new DomDriver());
        public ContinueCommand(BreakpointData[] breakpoints) {
            super(breakpoints);
        }
        
        static {
            xstream.alias("exec-continue", ContinueCommand.class); //$NON-NLS-1$
            BreakpointData.configureXStream(xstream);
        }
        
        public static String toXML(ContinueCommand cmd) {
            return xstream.toXML(cmd);
        }
    }
    
    private static class StepCommand extends ExecCommand {
        private static XStream xstream = new XStream(new DomDriver());
        public StepCommand(BreakpointData[] breakpoints) {
            super(breakpoints);
        }
        
        static {
            xstream.alias("exec-step", StepCommand.class); //$NON-NLS-1$
            BreakpointData.configureXStream(xstream);
        }
        
        public static String toXML(StepCommand cmd) {
            return xstream.toXML(cmd);
        }
    }
    
    private static class StepOverCommand extends ExecCommand {
        private static XStream xstream = new XStream(new DomDriver());
        public StepOverCommand(BreakpointData[] breakpoints) {
            super(breakpoints);
        }
        
        static {
            xstream.alias("exec-step-over", StepOverCommand.class); //$NON-NLS-1$
            BreakpointData.configureXStream(xstream);
        }
        
        public static String toXML(StepOverCommand cmd) {
            return xstream.toXML(cmd);
        }
    }
    
    private static class StepOutCommand extends ExecCommand {
        private static XStream xstream = new XStream(new DomDriver());
        public StepOutCommand(BreakpointData[] breakpoints) {
            super(breakpoints);
        }
        
        static {
            xstream.alias("exec-step-out", StepOutCommand.class); //$NON-NLS-1$
            BreakpointData.configureXStream(xstream);
        }
        
        public static String toXML(StepOutCommand cmd) {
            return xstream.toXML(cmd);
        }
    }
    
    
    private HashSet<InteractorListener> listeners = new HashSet<InteractorListener>();
    private BufferedReader reader;
    private PrintStream writer;
    private String testDescriptor;
    private TestManager manager;
    private Thread thread;
    private boolean done = false;
    private boolean debugMode;
    public LslTestInteractor(String launchMode, TestManager manager, String testDescriptor, InputStream in, 
            OutputStream out) {
        reader = new BufferedReader(new InputStreamReader(in));
        writer = new PrintStream(out);
        
        this.testDescriptor = testDescriptor;
        this.manager =  manager;
        this.debugMode = ILaunchManager.DEBUG_MODE.equals(launchMode);
    }
    
    public void start() {
        if (done || thread != null && thread.isAlive()) return;
        writeOut(testDescriptor);
        writeOut(continueText());
        thread = new Thread(this);
        thread.start();
    }
 
    public void stop() {
        
    }
    
    private String continueText() {
        BreakpointData[] bpData = null;
        if (debugMode) {
            bpData = createBreakpointData();
        }
        ContinueCommand cmd = new ContinueCommand(bpData);
        return ContinueCommand.toXML(cmd);
    }
    
    private String stepText() {
        BreakpointData[] bpData = null;
        if (debugMode) {
            bpData = createBreakpointData();
        }
        StepCommand cmd = new StepCommand(bpData);
        return StepCommand.toXML(cmd);
    }

    private String stepOverText() {
        BreakpointData[] bpData = null;
        if (debugMode) {
            bpData = createBreakpointData();
        }
        StepOverCommand cmd = new StepOverCommand(bpData);
        return StepOverCommand.toXML(cmd);
    }

    private String stepOutText() {
        BreakpointData[] bpData = null;
        if (debugMode) {
            bpData = createBreakpointData();
        }
        StepOutCommand cmd = new StepOutCommand(bpData);
        return StepOutCommand.toXML(cmd);
    }

    private BreakpointData[] createBreakpointData() {
        IBreakpointManager bpm = getBreakpointManager();
        IBreakpoint[] breakpoints = bpm.getBreakpoints(LslDebugTarget.LSLPLUS);
        LinkedList<BreakpointData> list = new LinkedList<BreakpointData>();
        for (int i = 0; i < breakpoints.length; i++) {
            try {
                if (breakpoints[i] instanceof LslLineBreakpoint) {
                    LslLineBreakpoint bp = (LslLineBreakpoint) breakpoints[i];
                    int line = bp.getLineNumber();
                    IMarker marker = bp.getMarker();
                    IResource resource = marker.getResource();
                    IFile file = (IFile) resource.getAdapter(IFile.class);
                    if (file == null)
                        continue;
                    if (!marker.getAttribute(IBreakpoint.ENABLED,false)) continue;
                    IPath fullPath = file.getLocation();
                    list.add(new BreakpointData(fullPath.toOSString(), line));
                }
            } catch (CoreException e) {
                Util.error(e, e.getLocalizedMessage());
            }
        }
        return list.toArray(new BreakpointData[list.size()]);
    }
    
    public void continueExecution() {
        sendCommand(continueText());
    }

    private void sendCommand(String commandText) {
        if (done || thread != null && thread.isAlive()) return;
        writeOut(commandText);
        thread = new Thread(this);
        thread.start();
    }
    
    public void step() {
        sendCommand(stepText());
    }
    
    public void stepOver() {
        sendCommand(stepOverText());
    }
    
    public void stepOut() {
        sendCommand(stepOutText());
    }
    
    public void addListener(InteractorListener listener) { listeners.add(listener); }
    public void removeListener(InteractorListener listener) { listeners.remove(listener); }
    
    public void close() {
        writer.close();
    }
    
    private void fireSuspended(LslScriptExecutionState state) {
        for (Iterator<InteractorListener> i = listeners.iterator(); i.hasNext();) {
            i.next().suspended(state);
        }
    }
    
    private void fireComplete() {
        for (Iterator<InteractorListener> i = listeners.iterator(); i.hasNext();) {
            i.next().completed();
        }
    }
    public void run() {
        String line = null;
        
        try {
            while ((line = reader.readLine()) != null) {
                if (LslPlusPlugin.DEBUG) Util.log("read:" + Util.URIDecode(line)); //$NON-NLS-1$
                TestEvent event = TestEvents.fromXML(Util.URIDecode(line));
                
                // kludge for the mo'
                if (event instanceof TestCompleteEvent) {
                    manager.postTestResult(((TestCompleteEvent)event).getTestResult());
                    String cmd = continueText();
                    if (LslPlusPlugin.DEBUG) Util.log("writing: " + cmd); //$NON-NLS-1$
                    writeOut(cmd);
                } else if (event instanceof AllCompleteEvent) {
                    endSession();
                    fireComplete();
                    // TODO: this is a place where a debug event would happen
                    return;
                } else if (event instanceof TestSuspendedEvent) {
                    // TODO: this is where we'd extract the debug info...
                    if (LslPlusPlugin.DEBUG) Util.log("hit a breakpoint... suspending!"); //$NON-NLS-1$
                    fireSuspended(((TestSuspendedEvent)event).getScriptState());
                    return;
                }
            }
        } catch (IOException e) {
            Util.error(e, e.getLocalizedMessage());
        } catch (RuntimeException e) {
            Util.error(e, e.getLocalizedMessage());
            try {
                endSession();
            } catch (Exception e1) {
            }
        }
    }
    
    private DebugPlugin getDebugPlugin() { return DebugPlugin.getDefault(); }
    private IBreakpointManager getBreakpointManager() { return getDebugPlugin().getBreakpointManager(); }

    private void writeOut(String cmd) {
        writer.println(Util.URIEncode(cmd));
        writer.flush();
    }
    
    private void endSession() {
        writer.println("quit"); //$NON-NLS-1$
        writer.flush();
        writer.close();
    }
}
