package lslforge;

import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.runtime.PlatformObject;

public class LSLForgeScript extends PlatformObject {
    private IFile scriptFile;

    public LSLForgeScript(IFile scriptFile) {
        this.scriptFile = scriptFile;
    }
    
    public IResource getResource() {
        return scriptFile;
    }
    
    
}
