package lslplus;

import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IAdaptable;

public class PropertyTester extends org.eclipse.core.expressions.PropertyTester {

	public PropertyTester() {
	}

	public boolean test(Object receiver, String property, Object[] args,
			Object expectedValue) {
		if (receiver instanceof IAdaptable) {
			LslPlusElement element = (LslPlusElement) ((IAdaptable)receiver).getAdapter(LslPlusElement.class);
			try {
				return element != null &&
				    element.getResource().getProject().getNature("lslPlus") != null; //$NON-NLS-1$
			} catch (CoreException e) {
				return false;
			}
		} else {
		    return false;
		}
	}

}
