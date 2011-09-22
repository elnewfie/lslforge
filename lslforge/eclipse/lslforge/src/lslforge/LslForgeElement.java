package lslforge;

import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.runtime.PlatformObject;

/**
 * Represents a unit of LSLForge code (a script or a module).
 * 
 * @author rgreayer
 *
 */
public class LslForgeElement extends PlatformObject {
	private IFile f;
	public LslForgeElement(IFile f) {
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
