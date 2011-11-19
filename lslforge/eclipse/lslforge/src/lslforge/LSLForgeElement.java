package lslforge;

import java.net.URI;
import java.net.URISyntaxException;

import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.resources.ResourcesPlugin;
import org.eclipse.core.runtime.IPath;
import org.eclipse.core.runtime.Path;
import org.eclipse.core.runtime.PlatformObject;

/**
 * Represents a unit of LSLForge code (a script or a module).
 * 
 * @author rgreayer
 *
 */
public class LSLForgeElement extends PlatformObject {
	private IFile f;
	public LSLForgeElement(IFile f) {
		this.f = f;
	}

	public IResource getResource() {
		return f;
	}
	
	public boolean isModule() {
		return "lslm".equals(f.getProjectRelativePath().getFileExtension()); //$NON-NLS-1$
	}
	
	public boolean isScript() {
		return isScript(false);
	}
	
	public boolean isScript(boolean strict) {
		if("lslp".equals(f.getProjectRelativePath().getFileExtension())) return true; //$NON-NLS-1$

		
		if(!strict) {
			if("lsl".equals(f.getProjectRelativePath().getFileExtension())) { //$NON-NLS-1$
				//This file may be a script, if there's no matching .lslp file
				String name = f.getLocationURI().toString() + "p"; //$NON-NLS-1$
				try {
					URI pFile = new URI(name);
					IPath pPath = new Path(pFile.getPath());
					if(!ResourcesPlugin.getWorkspace().getRoot().exists(pPath)) {
						return true;
					}
				} catch (URISyntaxException e) {
					//Assume the worst
					return false;
				}
			}
		}
		return false;
	}
}
