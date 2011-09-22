package lslforge;

import org.eclipse.core.resources.IResource;
import org.eclipse.core.runtime.IAdapterFactory;

/**
 * An adaptor factory that can adapt LslDerivedScript objects into other
 * objects.
 * 
 * @author rgreayer
 *
 */
public class LslDerivedScriptAdapterFactory implements IAdapterFactory {

	
	@SuppressWarnings("unchecked")
	public Object getAdapter(Object adaptableObject, Class adapterType) {
		if (adaptableObject instanceof LslDerivedScript && adapterType == IResource.class) {
			return ((LslDerivedScript)adaptableObject).getResource();
		}
		return null;
	}

	@SuppressWarnings("unchecked")
	public Class[] getAdapterList() {
		return new Class[] {
				IResource.class
		};
	}

}
