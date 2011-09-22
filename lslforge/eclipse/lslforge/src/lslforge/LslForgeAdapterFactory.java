package lslforge;

import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.runtime.IAdapterFactory;
import org.eclipse.ui.part.FileEditorInput;

/**
 * An adapter factory that can create adapters for LslForgeElement objects.
 * It can adapt LslForgeElements into their underlying resource objects.
 * @author rgreayer
 *
 */
public class LslForgeAdapterFactory implements IAdapterFactory {

	@SuppressWarnings("unchecked")
	public Object getAdapter(Object adaptableObject, Class adapterType) {
		if (adaptableObject instanceof LslForgeElement &&
		    adapterType == IResource.class) {
			LslForgeElement e = (LslForgeElement) adaptableObject;
			return e.getResource();
		} else if (adaptableObject instanceof LslForgeElement &&
		        LslForgeScript.class.equals(adapterType)) {
		    LslForgeElement element = (LslForgeElement)adaptableObject;
		    if (element.isScript()) {
		        return new LslForgeScript((IFile)element.getResource());
		    }
		} else if (adaptableObject instanceof LslForgeScript &&
		           IResource.class.equals(adapterType)) {
		    
		    return ((LslForgeScript)adaptableObject).getResource();
		} else if (adaptableObject instanceof LslForgeScript &&
		           LslForgeElement.class.equals(adapterType)) {
		    return new LslForgeScript((IFile)((LslForgeScript)adaptableObject).getResource());
		} else if (adaptableObject instanceof FileEditorInput && 
		        LslForgeScript.class.equals(adapterType)) {
		    FileEditorInput inp = (FileEditorInput) adaptableObject;
		    return new LslForgeScript(inp.getFile());
		}
		return null;
	}

	@SuppressWarnings("unchecked")
	public Class[] getAdapterList() {
		return new Class[] {
				IResource.class,
				LslForgeElement.class,
				LslForgeScript.class
		};
	}

}
