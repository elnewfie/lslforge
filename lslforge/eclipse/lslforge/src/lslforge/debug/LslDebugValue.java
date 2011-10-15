package lslforge.debug;

import org.eclipse.debug.core.DebugException;
import org.eclipse.debug.core.ILaunch;
import org.eclipse.debug.core.model.IDebugTarget;
import org.eclipse.debug.core.model.IValue;
import org.eclipse.debug.core.model.IVariable;

public class LSLDebugValue implements IValue {

    private String typeName;
    private String valueString;
    private LSLDebugTarget target;
    
    public LSLDebugValue(String typeName, String valueString, LSLDebugTarget target) {
        this.typeName = typeName;
        this.valueString = valueString;
        this.target = target;
    }
    
    public String getReferenceTypeName() throws DebugException {
        return typeName;
    }

    public String getValueString() throws DebugException {
        return valueString;
    }

    public IVariable[] getVariables() throws DebugException {
        return new IVariable[0];
    }

    public boolean hasVariables() throws DebugException {
        return false;
    }

    public boolean isAllocated() throws DebugException {
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

}
