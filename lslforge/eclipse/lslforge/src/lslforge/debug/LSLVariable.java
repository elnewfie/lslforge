package lslforge.debug;

import lslforge.LSLForgePlugin;

import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.debug.core.DebugException;
import org.eclipse.debug.core.ILaunch;
import org.eclipse.debug.core.model.IDebugTarget;
import org.eclipse.debug.core.model.IValue;
import org.eclipse.debug.core.model.IVariable;

public class LSLVariable implements IVariable {

    private String name;
    private String typeName;
    private LSLDebugValue value;
    private LSLDebugTarget target;
    
    public LSLVariable(String name, String typeName, String valueString,
            LSLDebugTarget target) {
        this.name = name;
        this.typeName = typeName;
        this.value = new LSLDebugValue(typeName, valueString, target);
        this.target = target;
    }
    public String getName() throws DebugException {
        return name;
    }

    public String getReferenceTypeName() throws DebugException {
        return typeName;
    }

    public IValue getValue() throws DebugException {
        return value;
    }

    public boolean hasValueChanged() throws DebugException {
        return true;
    }

    public IDebugTarget getDebugTarget() {
        return target;
    }

    public ILaunch getLaunch() {
        return target.getLaunch();
    }

    public String getModelIdentifier() {
        return target.getModelIdentifier();
    }

	public Object getAdapter(@SuppressWarnings("rawtypes") Class adapter) {
        return null;
    }

    public void setValue(String expression) throws DebugException {
        throw new DebugException(new Status(IStatus.ERROR, LSLForgePlugin.PLUGIN_ID, 
                DebugException.NOT_SUPPORTED, "",null)); //$NON-NLS-1$
    }

    public void setValue(IValue value) throws DebugException {
        throw new DebugException(new Status(IStatus.ERROR, LSLForgePlugin.PLUGIN_ID, 
                DebugException.NOT_SUPPORTED, "",null)); //$NON-NLS-1$
    }

    public boolean supportsValueModification() {
        return false;
    }

    public boolean verifyValue(String expression) throws DebugException {
        throw new DebugException(new Status(IStatus.ERROR, LSLForgePlugin.PLUGIN_ID, 
                DebugException.NOT_SUPPORTED, "",null)); //$NON-NLS-1$
    }

    public boolean verifyValue(IValue value) throws DebugException {
        throw new DebugException(new Status(IStatus.ERROR, LSLForgePlugin.PLUGIN_ID, 
                DebugException.NOT_SUPPORTED, "",null)); //$NON-NLS-1$
    }

}
