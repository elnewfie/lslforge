package lslforge;

import lslforge.lsltest.LSLTestSuite;
import lslforge.sim.SimProject;
import lslforge.util.Util;

import org.eclipse.core.resources.IFile;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IAdapterFactory;

/**
 * And adapter factory that can transform IResource objects into various
 * different LSL elements types (LSLElement objects, LSLDerivedScript objects,
 * and LSLTestSuite objects).
 * @author rgreayer
 *
 */
public class ResourceAdapterFactory implements IAdapterFactory {

	public Object getAdapter(Object adaptableObject, @SuppressWarnings("rawtypes") Class adapterType) {
		if (adaptableObject instanceof IFile) {
			IFile f = (IFile) adaptableObject;
			String ext = f.getFileExtension();
			if (LSLForgeElement.class.equals(adapterType) && ("lslp".equals(ext) //$NON-NLS-1$
			                                         || "lslm".equals(ext) //$NON-NLS-1$
			                                         || "lsl".equals(ext))) { //$NON-NLS-1$
				return new LSLForgeElement(f);
			} else if ("lslp".equals(ext) && LSLForgeScript.class.equals(adapterType)) { //$NON-NLS-1$
			    return new LSLForgeScript(f);
			} else if ("lsl".equals(ext) && LSLDerivedScript.class.equals(adapterType)) { //$NON-NLS-1$
				return new LSLDerivedScript(f);
			} else if ("lslt".equals(ext) && LSLTestSuite.class.equals(adapterType)) { //$NON-NLS-1$
				try {
					LSLTestSuite suite = LSLTestSuite.fromXml(f.getContents(), f);
					suite.setIResource(f);
					return suite;
				} catch (CoreException e) {
					Util.error(e, e.getLocalizedMessage());
					return null;
				}
			} else if ("simp".equals(ext) && SimProject.WorldNode.class.equals(adapterType)) { //$NON-NLS-1$
			    try {
			        SimProject.WorldNode node = SimProject.fromXml(f.getContents(), f);
			        return node;
			    } catch (CoreException e) {
			        Util.error(e, e.getLocalizedMessage());
			        return null;
			    }
			}
		}
		return null;
	}

	static private Class<?>[] adapterList = {
		LSLForgeElement.class,
		LSLDerivedScript.class,
		LSLTestSuite.class,
		LSLForgeScript.class,
		SimProject.WorldNode.class
	};
	
	@SuppressWarnings("rawtypes")
	public Class[] getAdapterList() {
		return adapterList;
	}

}
