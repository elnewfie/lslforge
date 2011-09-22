package lslforge.debug;

import lslforge.editor.LslForgeEditor;

import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IWorkspace;
import org.eclipse.core.resources.ResourcesPlugin;
import org.eclipse.core.runtime.Path;
import org.eclipse.debug.core.model.ISourceLocator;
import org.eclipse.debug.core.model.IStackFrame;
import org.eclipse.debug.ui.ISourcePresentation;
import org.eclipse.ui.IEditorInput;
import org.eclipse.ui.part.FileEditorInput;

public class LslSourceLocator implements ISourceLocator, ISourcePresentation {

    public Object getSourceElement(IStackFrame stackFrame) {
        if (stackFrame instanceof LslStackFrame) {
            LslStackFrame frame = (LslStackFrame) stackFrame;
            
            if (frame.getFile() == null) return null;
            Path p = new Path(frame.getFile());
            
            IWorkspace w = ResourcesPlugin.getWorkspace();
            IFile[] files = w.getRoot().findFilesForLocation(p);
            
            if (files != null && files.length > 0) return files[0];
        }
        return null;
    }

    public String getEditorId(IEditorInput input, Object element) {
        return LslForgeEditor.ID;
    }

    public IEditorInput getEditorInput(Object element) {
        if (element instanceof IFile) {
            return new FileEditorInput((IFile)element);
        }
        
        return null;
    }

}
