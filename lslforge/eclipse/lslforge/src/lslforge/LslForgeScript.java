package lslforge;

import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.runtime.PlatformObject;

public class LslForgeScript extends PlatformObject {
    private IFile scriptFile;

    public LslForgeScript(IFile scriptFile) {
        this.scriptFile = scriptFile;
    }
    
    public IResource getResource() {
        return scriptFile;
    }
    
    
}
