package lslforge.lsltest;

import org.eclipse.core.resources.IResource;
import org.eclipse.core.runtime.IAdapterFactory;

public class LslTestSuiteAdapterFactory implements IAdapterFactory {

	@SuppressWarnings("unchecked")
	public Object getAdapter(Object adaptableObject, Class adapterType) {
		if (!(adaptableObject instanceof LslTestSuite) ||
		    !(IResource.class.equals(adapterType))) return null;
		return ((LslTestSuite)adaptableObject).getResource();
	}

	@SuppressWarnings("unchecked")
	public Class[] getAdapterList() {
		return new Class[] { IResource.class };
	}

}
