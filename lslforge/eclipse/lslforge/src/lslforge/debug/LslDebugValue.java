package lslforge.debug;

import org.eclipse.debug.core.DebugException;
import org.eclipse.debug.core.ILaunch;
import org.eclipse.debug.core.model.IDebugTarget;
import org.eclipse.debug.core.model.IValue;
import org.eclipse.debug.core.model.IVariable;

public class LslDebugValue implements IValue {

    private String typeName;
    private String valueString;
    private LslDebugTarget target;
    
    public LslDebugValue(String typeName, String valueString, LslDebugTarget target) {
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

    @SuppressWarnings("unchecked")
	public Object getAdapter(Class adapter) {
        return null;
    }

}
