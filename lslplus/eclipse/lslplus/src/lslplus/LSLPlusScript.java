package lslplus;

import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.runtime.PlatformObject;

public class LslPlusScript extends PlatformObject {
    private IFile scriptFile;

    public LslPlusScript(IFile scriptFile) {
        this.scriptFile = scriptFile;
    }
    
    public IResource getResource() {
        return scriptFile;
    }
    
    
}
