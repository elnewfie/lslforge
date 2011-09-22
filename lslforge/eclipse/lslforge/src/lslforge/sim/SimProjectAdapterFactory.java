package lslforge.sim;

import org.eclipse.core.resources.IResource;
import org.eclipse.core.runtime.IAdapterFactory;

public class SimProjectAdapterFactory implements IAdapterFactory {

	@SuppressWarnings("unchecked")
	public Object getAdapter(Object adaptableObject, Class adapterType) {
		if (!(adaptableObject instanceof SimProject.WorldNode) ||
		    !(IResource.class.equals(adapterType))) return null;
		return ((SimProject.WorldNode)adaptableObject).getResource();
	}

	@SuppressWarnings("unchecked")
	public Class[] getAdapterList() {
		return new Class[] { IResource.class };
	}

}
