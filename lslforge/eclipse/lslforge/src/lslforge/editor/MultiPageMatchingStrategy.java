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
		
		IPath baseFileA = fileEditorRef.getFile().getFullPath();
		IPath baseFileB = fileInput.getFile().getFullPath();
		
		//Are we dealing with two module files?
		if(isModuleFile(baseFileA) && isModuleFile(baseFileB)) {
			return baseFileA.equals(baseFileB);
		}
		
		if(isScriptFile(baseFileA) && isScriptFile(baseFileB)) {
			//Remove the extensions and compare
			baseFileA = baseFileA.removeFileExtension();
			baseFileB = baseFileB.removeFileExtension();
			
			if(baseFileA.equals(baseFileB)) {
				//If we end up switching to the multipage editor, flip the tab to the right one
				if(editorRef.getEditor(false) instanceof LSLMultiPageEditor) {
					LSLMultiPageEditor editor = (LSLMultiPageEditor)editorRef.getEditor(false);
					editor.setActiveByFile(fileInput.getFile());
				}
				
				return true;
			}
		}

		return false;
	}
	
	private boolean isScriptFile(IPath file) {
		if("lslp".equals(file.getFileExtension())) return true; //$NON-NLS-1$
		if("lsl".equals(file.getFileExtension())) return true; //$NON-NLS-1$
		return false;
	}
	
	private boolean isModuleFile(IPath file) {
		if("lslm".equals(file.getFileExtension())) return true; //$NON-NLS-1$
		
		return false;
	}
}
