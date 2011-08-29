package lslplus;

import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.runtime.IAdapterFactory;
import org.eclipse.ui.part.FileEditorInput;

/**
 * An adapter factory that can create adapters for LslPlusElement objects.
 * It can adapt LslPlusElements into their underlying resource objects.
 * @author rgreayer
 *
 */
public class LslPlusAdapterFactory implements IAdapterFactory {

	@SuppressWarnings("unchecked")
	public Object getAdapter(Object adaptableObject, Class adapterType) {
		if (adaptableObject instanceof LslPlusElement &&
		    adapterType == IResource.class) {
			LslPlusElement e = (LslPlusElement) adaptableObject;
			return e.getResource();
		} else if (adaptableObject instanceof LslPlusElement &&
		        LslPlusScript.class.equals(adapterType)) {
		    LslPlusElement element = (LslPlusElement)adaptableObject;
		    if (element.isScript()) {
		        return new LslPlusScript((IFile)element.getResource());
		    }
		} else if (adaptableObject instanceof LslPlusScript &&
		           IResource.class.equals(adapterType)) {
		    
		    return ((LslPlusScript)adaptableObject).getResource();
		} else if (adaptableObject instanceof LslPlusScript &&
		           LslPlusElement.class.equals(adapterType)) {
		    return new LslPlusScript((IFile)((LslPlusScript)adaptableObject).getResource());
		} else if (adaptableObject instanceof FileEditorInput && 
		        LslPlusScript.class.equals(adapterType)) {
		    FileEditorInput inp = (FileEditorInput) adaptableObject;
		    return new LslPlusScript(inp.getFile());
		}
		return null;
	}

	@SuppressWarnings("unchecked")
	public Class[] getAdapterList() {
		return new Class[] {
				IResource.class,
				LslPlusElement.class,
				LslPlusScript.class
		};
	}

}
