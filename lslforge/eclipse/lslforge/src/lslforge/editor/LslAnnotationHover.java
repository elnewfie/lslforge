package lslforge.editor;

import java.util.Iterator;
import java.util.LinkedList;
import java.util.List;

import org.eclipse.jface.text.BadLocationException;
import org.eclipse.jface.text.IDocument;
import org.eclipse.jface.text.IRegion;
import org.eclipse.jface.text.Position;
import org.eclipse.jface.text.source.Annotation;
import org.eclipse.jface.text.source.IAnnotationHover;
import org.eclipse.jface.text.source.IAnnotationModel;
import org.eclipse.jface.text.source.ISourceViewer;

/** 
 * The LslAnnotationHover provides the hover support for editors.
 */
public class LslAnnotationHover implements IAnnotationHover {
    private static final List<String> ANNOTATIONS_TO_DISPLAY; 

    static {
        ANNOTATIONS_TO_DISPLAY = new LinkedList<String>();
        ANNOTATIONS_TO_DISPLAY.add("org.eclipse.ui.workbench.texteditor.error"); //$NON-NLS-1$
        ANNOTATIONS_TO_DISPLAY.add("org.eclipse.ui.workbench.texteditor.warning");   //$NON-NLS-1$
    }
	/* (non-Javadoc)
	 * Method declared on IAnnotationHover
	 */
	public String getHoverInfo(ISourceViewer sourceViewer, int lineNumber) {
	    IAnnotationModel m = sourceViewer.getAnnotationModel();
        IDocument document= sourceViewer.getDocument();
	    try {
	        IRegion info = document.getLineInformation(lineNumber);
	        for (Iterator<?> i = m.getAnnotationIterator(); i.hasNext(); ) {
	            Annotation a = (Annotation) i.next();
	            if (!ANNOTATIONS_TO_DISPLAY.contains(a.getType())) continue;
	            Position p = m.getPosition(a);
	            if (p.overlapsWith(info.getOffset(), info.getLength())) {
	                return a.getText();
	            }
	        }
	        return null;
	    } catch (BadLocationException e) {
	        return null;
	    }
	}
}
