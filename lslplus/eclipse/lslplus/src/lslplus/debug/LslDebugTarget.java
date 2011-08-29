package lslplus.debug;

import lslplus.LslPlusPlugin;

import org.eclipse.core.resources.IMarkerDelta;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.debug.core.DebugEvent;
import org.eclipse.debug.core.DebugException;
import org.eclipse.debug.core.DebugPlugin;
import org.eclipse.debug.core.ILaunch;
import org.eclipse.debug.core.model.IBreakpoint;
import org.eclipse.debug.core.model.IDebugTarget;
import org.eclipse.debug.core.model.ILineBreakpoint;
import org.eclipse.debug.core.model.IMemoryBlock;
import org.eclipse.debug.core.model.IProcess;
import org.eclipse.debug.core.model.IThread;

public class LslDebugTarget implements IDebugTarget, IProcessListener {
    public static final String LSLPLUS = "lslplus"; //$NON-NLS-1$
    private String name;
    private LslProcess process;
    private LslThread thread;
    private IThread[] threads;
    private ILaunch launch;
    private boolean suspended = false;
    private boolean terminated;
    public LslDebugTarget(String name, ILaunch launch, LslProcess process) {
        this.name = name;
        this.process = process;
        this.launch = launch;
        thread = new LslThread(this);
        threads = new LslThread[] { thread };
        process.setThread(thread);
        process.addListener(this);
    }

    public String getName() throws DebugException {
        return name;
    }

    public IProcess getProcess() {
        return process;
    }

    public IThread[] getThreads() throws DebugException {
        return threads;
    }

    public boolean hasThreads() throws DebugException {
        return true;
    }

    public boolean supportsBreakpoint(IBreakpoint breakpoint) {
        return (breakpoint instanceof ILineBreakpoint);
    }

    public String getModelIdentifier() {
        return LSLPLUS;
    }

    public boolean canTerminate() {
        return false;
    }

    public boolean isTerminated() {
        return terminated;
    }

    public void setTerminated() {
        this.terminated = true;
        DebugPlugin.getDefault().fireDebugEventSet(
                new DebugEvent[] {
                        new DebugEvent(this, DebugEvent.TERMINATE)
                });
    }
    
    public void terminate() throws DebugException {
    }

    public boolean canResume() {
        return suspended;
    }

    public boolean canSuspend() {
        return false;
    }

    public boolean isSuspended() {
        return suspended;
    }

    public void resume() throws DebugException {
    }

    public void suspend() throws DebugException { }
    void setSuspended() {
        suspended = true;
        DebugPlugin.getDefault().fireDebugEventSet(
            new DebugEvent[] {
                    new DebugEvent(this, DebugEvent.SUSPEND, DebugEvent.BREAKPOINT)
            });
    }

    public void breakpointAdded(IBreakpoint breakpoint) {
        // TODO Auto-generated method stub

    }

    public void breakpointChanged(IBreakpoint breakpoint, IMarkerDelta delta) {
        // TODO Auto-generated method stub

    }

    public void breakpointRemoved(IBreakpoint breakpoint, IMarkerDelta delta) {
        // TODO Auto-generated method stub

    }

    public boolean canDisconnect() {
        return false;
    }

    public void disconnect() throws DebugException {
        throw notSupported();
    }

    private DebugException notSupported() throws DebugException {
        return new DebugException(
                new Status(IStatus.ERROR,LslPlusPlugin.PLUGIN_ID,
                        DebugException.NOT_SUPPORTED,"",null)); //$NON-NLS-1$
    }

    public boolean isDisconnected() {
        return false;
    }

    public IMemoryBlock getMemoryBlock(long startAddress, long length) throws DebugException {
        throw notSupported();
    }

    public boolean supportsStorageRetrieval() {
        return false;
    }

    public IDebugTarget getDebugTarget() {
        return this;
    }

    public ILaunch getLaunch() {
        return launch;
    }

    @SuppressWarnings("unchecked")
	public Object getAdapter(Class adapter) {

        if (ILaunch.class.equals(adapter)) {
            return getLaunch();
        }
        return null;
    }

    public void processTerminated(LslProcess p) {
        setTerminated();
        p.removeListener(this);
    }

}
