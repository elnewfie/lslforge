package lslforge.editor;

import org.eclipse.core.runtime.IPath;
import org.eclipse.ui.IEditorInput;
import org.eclipse.ui.IEditorMatchingStrategy;
import org.eclipse.ui.IEditorReference;
import org.eclipse.ui.IFileEditorInput;
import org.eclipse.ui.PartInitException;

/**
 * Both .lsl and .lslp files use a multi-page editor which combines both files into a single view.
 * Since the editor may be opened via either type of file, this class is needed to ensure
 * that duplicate instances of the editor is not opened.
 */
public class MultiPageMatchingStrategy implements IEditorMatchingStrategy {

	public boolean matches(IEditorReference editorRef, IEditorInput input) {
		if(!(input instanceof IFileEditorInput)) return false;
		
		IFileEditorInput fileEditorRef;
		IFileEditorInput fileInput;
		try {
			if(!(editorRef.getEditorInput() instanceof IFileEditorInput)) return false;
			fileEditorRef = (IFileEditorInput)editorRef.getEditorInput();
			fileInput = (IFileEditorInput)input;
		} catch (PartInitException e) {
			return false;	//Assume bad right now
		}
		
		IPath baseFileA = fileEditorRef.getFile().getFullPath().removeFileExtension();
		IPath baseFileB = fileInput.getFile().getFullPath().removeFileExtension();
		
		return baseFileA.equals(baseFileB);
	}

}
