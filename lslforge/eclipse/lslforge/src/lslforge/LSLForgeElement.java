package lslforge;

import lslforge.util.Util;
import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.runtime.PlatformObject;

/**
 * Represents a unit of LSLForge code (a script or a module).
 * 
 * @author rgreayer
 *
 */
public class LSLForgeElement extends PlatformObject {
	private final IFile f;
	public LSLForgeElement(IFile f) {
		this.f = f;
	}

	public IResource getResource() {
		return f;
	}
	
	public boolean isModule() {
		return Util.isModule(f);
	}

	public boolean isScript(boolean strict) {
		return Util.isScript(strict, f);
	}

	public boolean isScript() {
		return Util.isScript(false, f);
	}
}
