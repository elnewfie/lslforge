package lslplus;

import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.runtime.PlatformObject;

/**
 * Represents a unit of LSL Plus code (a script or a module).
 * 
 * @author rgreayer
 *
 */
public class LslPlusElement extends PlatformObject {
	private IFile f;
	public LslPlusElement(IFile f) {
		this.f = f;
	}

	public IResource getResource() {
		return f;
	}
	
	public boolean isModule() {
		return "lslm".equals(f.getProjectRelativePath().getFileExtension()); //$NON-NLS-1$
	}
	
	public boolean isScript() {
		return "lslp".equals(f.getProjectRelativePath().getFileExtension()); //$NON-NLS-1$
	}
}
