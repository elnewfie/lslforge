package lslforge;

import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.runtime.IAdapterFactory;
import org.eclipse.ui.part.FileEditorInput;

/**
 * An adaptor factory that can adapt LSLDerivedScript objects into other
 * objects.
 * 
 * @author rgreayer
 *
 */
public class LSLDerivedScriptAdapterFactory implements IAdapterFactory {

	
	public Object getAdapter(Object adaptableObject, @SuppressWarnings("rawtypes") Class adapterType) {
		if (adaptableObject instanceof LSLForgeElement && adapterType == IResource.class) {
			LSLForgeElement e = (LSLForgeElement) adaptableObject;
			return e.getResource();
			
		} else if (adaptableObject instanceof LSLForgeElement && LSLForgeScript.class.equals(adapterType)) {
		    LSLForgeElement element = (LSLForgeElement)adaptableObject;
		    if (element.isScript()) {
		        return new LSLDerivedScript((IFile)element.getResource());
		    }
		    
		} else if (adaptableObject instanceof LSLDerivedScript && IResource.class.equals(adapterType)) {
		    return ((LSLDerivedScript)adaptableObject).getResource();
		    
		} else if (adaptableObject instanceof LSLDerivedScript && LSLForgeElement.class.equals(adapterType)) {
		    return new LSLDerivedScript((IFile)((LSLDerivedScript)adaptableObject).getResource());
		    
		} else if (adaptableObject instanceof FileEditorInput &&  LSLForgeScript.class.equals(adapterType)) {
		    FileEditorInput inp = (FileEditorInput) adaptableObject;
		    return new LSLDerivedScript(inp.getFile());
		    
		}
		return null;
	}

	@SuppressWarnings("rawtypes")
	public Class[] getAdapterList() {
		return new Class[] {
				IResource.class,
				LSLForgeElement.class,
				LSLDerivedScript.class
		};
	}

}
