package lslforge;

import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.runtime.IAdapterFactory;
import org.eclipse.ui.part.FileEditorInput;

/**
 * An adapter factory that can create adapters for LSLForgeElement objects.
 * It can adapt LSLForgeElements into their underlying resource objects.
 * @author rgreayer
 *
 */
public class LSLForgeAdapterFactory implements IAdapterFactory {

	public Object getAdapter(Object adaptableObject, @SuppressWarnings("rawtypes") Class adapterType) {
		if (adaptableObject instanceof LSLForgeElement &&
		    adapterType == IResource.class) {
			LSLForgeElement e = (LSLForgeElement) adaptableObject;
			return e.getResource();
		} else if (adaptableObject instanceof LSLForgeElement &&
		        LSLForgeScript.class.equals(adapterType)) {
		    LSLForgeElement element = (LSLForgeElement)adaptableObject;
		    if (element.isScript()) {
		        return new LSLForgeScript((IFile)element.getResource());
		    }
		} else if (adaptableObject instanceof LSLForgeScript &&
		           IResource.class.equals(adapterType)) {
		    
		    return ((LSLForgeScript)adaptableObject).getResource();
		} else if (adaptableObject instanceof LSLForgeScript &&
		           LSLForgeElement.class.equals(adapterType)) {
		    return new LSLForgeScript((IFile)((LSLForgeScript)adaptableObject).getResource());
		} else if (adaptableObject instanceof FileEditorInput && 
		        LSLForgeScript.class.equals(adapterType)) {
		    FileEditorInput inp = (FileEditorInput) adaptableObject;
		    return new LSLForgeScript(inp.getFile());
		}
		return null;
	}

	@SuppressWarnings("rawtypes")
	public Class[] getAdapterList() {
		return new Class[] {
				IResource.class,
				LSLForgeElement.class,
				LSLForgeScript.class
		};
	}

}
