package lslplus.debug;

import lslplus.debug.LslScriptExecutionState.Binding;
import lslplus.debug.LslScriptExecutionState.Frame;

import org.eclipse.debug.core.DebugEvent;
import org.eclipse.debug.core.DebugException;
import org.eclipse.debug.core.DebugPlugin;
import org.eclipse.debug.core.ILaunch;
import org.eclipse.debug.core.model.IBreakpoint;
import org.eclipse.debug.core.model.IDebugTarget;
import org.eclipse.debug.core.model.IStackFrame;
import org.eclipse.debug.core.model.IThread;

public class LslThread implements IThread, InteractorListener {

    private LslStackFrame[] stackFrames;
    private LslDebugTarget target;
    private boolean active;
    private boolean suspended;
    private boolean stepping;
    private boolean terminated;
    private Interactor interactor;
    private String name = "LslThread"; //$NON-NLS-1$
    
    public LslThread(LslDebugTarget target) {
        this.target = target;
        this.active = true;
        this.suspended = false;
        this.stepping = false;
        stackFrames = new LslStackFrame[0];
    }

    public IBreakpoint[] getBreakpoints() {
        // TODO Auto-generated method stub
        return null;
    }

    public String getName() throws DebugException {
        return name;
    }

    public int getPriority() throws DebugException {
        return 0;
    }

    public IStackFrame[] getStackFrames() throws DebugException {
        return stackFrames;
    }

    public IStackFrame getTopStackFrame() throws DebugException {
        if (stackFrames != null && stackFrames.length > 0) return stackFrames[0];
        return null;
    }

    public boolean hasStackFrames() throws DebugException {
        return (stackFrames != null && stackFrames.length > 0);
    }

    public void setStackFrames(LslStackFrame[] stackFrames) {
        if (stackFrames == null) stackFrames = new LslStackFrame[0];
        this.stackFrames = stackFrames;
    }
    
    public IDebugTarget getDebugTarget() {
        return target;
    }

    public ILaunch getLaunch() {
        return getDebugTarget().getLaunch();
    }

    public String getModelIdentifier() {
        return getDebugTarget().getModelIdentifier();
    }

    @SuppressWarnings("unchecked")
	public Object getAdapter(Class adapter) {
        // TODO Auto-generated method stub
        return null;
    }

    public boolean canResume() {
        return active && suspended;
    }

    public boolean canSuspend() {
        return active && !suspended;
    }

    public boolean isSuspended() {
        return suspended;
    }

    public void resume() throws DebugException {
        doResume();
        interactor.continueExecution();
    }

    public void suspend() throws DebugException {
        // TODO Auto-generated method stub

    }

    public boolean canStepInto() {
        return canResume();
    }

    public boolean canStepOver() {
        return canResume();
    }

    public boolean canStepReturn() {
        return canResume();
    }

    public boolean isStepping() {
        return stepping;
    }

    public void stepInto() throws DebugException {
        doResume();
        interactor.step();
    }

    private void doResume() {
        suspended = false;
        debugPlugin().fireDebugEventSet(new DebugEvent[] {
                new DebugEvent(this, DebugEvent.RESUME, DebugEvent.BREAKPOINT)
        });
    }

    public void stepOver() throws DebugException {
        doResume();
        interactor.stepOver();
    }

    public void stepReturn() throws DebugException {
        doResume();
        interactor.stepOut();
    }

    public boolean canTerminate() {
        return false;
    }

    public boolean isTerminated() {
        return terminated;
    }

    public void terminate() throws DebugException {
    }

    public void setSuspended(boolean b) {
        this.suspended = b;
        stepping = false;
    }

    public void completed() {
        this.terminated = true;
        debugPlugin().fireDebugEventSet(new DebugEvent[] {
                new DebugEvent(this, DebugEvent.TERMINATE)
        });
        target.setTerminated();
    }

    public void suspended(LslScriptExecutionState state) {
        setSuspended(true);
        this.name = state.getThreadInfo().getName();
        
        this.stackFrames = new LslStackFrame[state.getThreadInfo().getFrames().length];
        
        for (int i = 0; i < this.stackFrames.length; i++) {
            Frame frame = state.getThreadInfo().getFrames()[i];
            LslVariable[] variables = new LslVariable[frame.getBindings().length];
            
            int line = 0;
            
            if (i == 0) line = state.getCurrentLine();
            for (int j = 0; j < variables.length; j++) {
                Binding b = frame.getBindings()[j];
                variables[j] = new LslVariable(b.getName(), b.getVal().typeString(),
                        b.getVal().toString(), this.target);
            }
            this.stackFrames[i] = new LslStackFrame(frame.getName(), frame.getFile(),
                    this, this.getDebugTarget(), variables, line, i == 0);
        }
        
        debugPlugin().fireDebugEventSet(new DebugEvent[] {
                new DebugEvent(this, DebugEvent.SUSPEND,
                        DebugEvent.BREAKPOINT)
        });
        
        target.setSuspended();
    }

    private DebugPlugin debugPlugin() {
        return DebugPlugin.getDefault();
    }

    public void setInteractor(Interactor interactor) {
        this.interactor = interactor;
    }
}
