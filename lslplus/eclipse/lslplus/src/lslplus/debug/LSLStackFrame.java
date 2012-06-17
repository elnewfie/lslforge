package lslplus.debug;

import org.eclipse.debug.core.DebugException;
import org.eclipse.debug.core.ILaunch;
import org.eclipse.debug.core.model.IDebugTarget;
import org.eclipse.debug.core.model.IRegisterGroup;
import org.eclipse.debug.core.model.IStackFrame;
import org.eclipse.debug.core.model.IThread;
import org.eclipse.debug.core.model.IVariable;

public class LslStackFrame implements IStackFrame{
    private static final IRegisterGroup[] EMPTY_REGISTER_GROUP =
        new IRegisterGroup[0];

    private boolean top;
    private String name;
    private String file;
    private IThread thread;
    private IVariable[] variables;
    //private boolean stepping;
    private int line;
    public LslStackFrame(
            String name, 
            String file,
            IThread thread,
            IDebugTarget debugTarget,
            IVariable[] variables,
            int line,
            boolean top) {
        this.name = name;
        this.thread = thread;
        this.file = file;
        this.variables = variables;
        this.line = line;
        this.top = top;
    }
    
    public int getCharEnd() throws DebugException {
        // TODO Auto-generated method stub
        return 0;
    }

    public int getCharStart() throws DebugException {
        return -1;
    }

    public int getLineNumber() throws DebugException {
        return line;
    }

    public String getName() throws DebugException {
        return name;
    }

    public IRegisterGroup[] getRegisterGroups() throws DebugException {
        return EMPTY_REGISTER_GROUP;
    }

    public IThread getThread() {
        return thread;
    }

    public IVariable[] getVariables() throws DebugException {
        return variables;
    }

    public boolean hasRegisterGroups() throws DebugException {
        return false;
    }

    public boolean hasVariables() throws DebugException {
        return variables != null && variables.length > 0;
    }

    public IDebugTarget getDebugTarget() {
        return thread.getDebugTarget();
    }

    public ILaunch getLaunch() {
        return thread.getLaunch();
    }

    public String getModelIdentifier() {
        return getDebugTarget().getModelIdentifier();
    }

    @SuppressWarnings("unchecked")
	public Object getAdapter(Class adapter) {
        //Util.log("lslStackFrame - asked to adapt to: " + adapter);
        return null;
    }

    public boolean canStepInto() {
        return top && isSuspended();
    }

    public boolean canStepOver() {
        return top && isSuspended();
    }

    public boolean canStepReturn() {
        return top && isSuspended();
    }

    public boolean isStepping() {
        return !isSuspended();
    }

    public void stepInto() throws DebugException {
        thread.stepInto();
    }

    public void stepOver() throws DebugException {
        thread.stepOver();
    }

    public void stepReturn() throws DebugException {
        thread.stepReturn();
    }

    public boolean canResume() {
        return thread.canResume();
    }

    public boolean canSuspend() {
        return false;
    }

    public boolean isSuspended() {
        return thread.isSuspended();
    }

    public void resume() throws DebugException {
        thread.resume();
    }

    public void suspend() throws DebugException {
    }

    public boolean canTerminate() {
        // TODO Auto-generated method stub
        return false;
    }

    public boolean isTerminated() {
        // TODO Auto-generated method stub
        return false;
    }

    public void terminate() throws DebugException {
        // TODO Auto-generated method stub
        
    }

    public String getFile() {
        return file;
    }

}
