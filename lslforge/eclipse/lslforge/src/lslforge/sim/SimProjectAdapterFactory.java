package lslforge.sim;

import org.eclipse.core.resources.IResource;
import org.eclipse.core.runtime.IAdapterFactory;

public class SimProjectAdapterFactory implements IAdapterFactory {

	public Object getAdapter(Object adaptableObject, @SuppressWarnings("rawtypes") Class adapterType) {
		if (!(adaptableObject instanceof SimProject.WorldNode) ||
		    !(IResource.class.equals(adapterType))) return null;
		return ((SimProject.WorldNode)adaptableObject).getResource();
	}

	@SuppressWarnings("rawtypes")
	public Class[] getAdapterList() {
		return new Class[] { IResource.class };
	}

}
