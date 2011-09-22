package lslforge;

import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IAdaptable;

public class PropertyTester extends org.eclipse.core.expressions.PropertyTester {

	public PropertyTester() {
	}

	public boolean test(Object receiver, String property, Object[] args,
			Object expectedValue) {
		if (receiver instanceof IAdaptable) {
			LslForgeElement element = (LslForgeElement) ((IAdaptable)receiver).getAdapter(LslForgeElement.class);
			try {
				return element != null &&
				    element.getResource().getProject().getNature("lslForge") != null; //$NON-NLS-1$
			} catch (CoreException e) {
				return false;
			}
		} else {
		    return false;
		}
	}

}
