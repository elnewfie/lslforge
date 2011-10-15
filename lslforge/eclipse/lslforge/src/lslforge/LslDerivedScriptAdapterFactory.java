package lslforge;

import org.eclipse.core.resources.IResource;
import org.eclipse.core.runtime.IAdapterFactory;

/**
 * An adaptor factory that can adapt LSLDerivedScript objects into other
 * objects.
 * 
 * @author rgreayer
 *
 */
public class LSLDerivedScriptAdapterFactory implements IAdapterFactory {

	
	public Object getAdapter(Object adaptableObject, @SuppressWarnings("rawtypes") Class adapterType) {
		if (adaptableObject instanceof LSLDerivedScript && adapterType == IResource.class) {
			return ((LSLDerivedScript)adaptableObject).getResource();
		}
		return null;
	}

	@SuppressWarnings("rawtypes")
	public Class[] getAdapterList() {
		return new Class[] {
				IResource.class
		};
	}

}
