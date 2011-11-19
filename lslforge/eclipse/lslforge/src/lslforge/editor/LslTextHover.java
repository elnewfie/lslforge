package lslforge.editor;


import java.util.HashMap;
import java.util.Map;

import lslforge.LSLForgePlugin;
import lslforge.language_metadata.LSLFunction;
import lslforge.language_metadata.LSLHandler;
import lslforge.util.LSLWordDetector;

import org.eclipse.jface.text.BadLocationException;
import org.eclipse.jface.text.BadPartitioningException;
import org.eclipse.jface.text.IDocument;
import org.eclipse.jface.text.IDocumentExtension3;
import org.eclipse.jface.text.IRegion;
import org.eclipse.jface.text.ITextHover;
import org.eclipse.jface.text.ITextViewer;
import org.eclipse.jface.text.Region;

/**
 * Example implementation for an <code>ITextHover</code> which hovers over Java code.
 */
public class LSLTextHover implements ITextHover {

	private static LSLWordDetector detector = new LSLWordDetector();
	private static HashMap<String,String> hoverInfo = null;

	/**
     * @param textViewer 
	 * @param hoverRegion 
	 * @return hover info string
	 * @deprecated
     */
	@Deprecated
	public String getHoverInfo(ITextViewer textViewer, IRegion hoverRegion) {
		if (hoverRegion != null) {
			try {
				IDocument doc = textViewer.getDocument();
				IDocumentExtension3 doc3 = (IDocumentExtension3) doc;
				
				if (!IDocument.DEFAULT_CONTENT_TYPE.equals(
						doc3.getContentType(LSLForgePlugin.LSL_PARTITIONING,hoverRegion.getOffset(), false))) {
					return null;
				}
				if (hoverRegion.getLength() > -1) {
					Map<String,String> hinfo = getHoverInfo();
					String s = doc.get(hoverRegion.getOffset(), hoverRegion.getLength());
					
					return hinfo.get(s);
				}
			} catch (BadLocationException x) {
			} catch (BadPartitioningException x) {
			}
		}
		return null;
	}
	
	/* (non-Javadoc)
	 * Method declared on ITextHover
	 */
	public IRegion getHoverRegion(ITextViewer textViewer, int offset) {
		try {
			IDocument doc = textViewer.getDocument();
			int len = doc.getLength() - 1;
			int start = offset;
			while (start >= 1 && detector.isWordPart(doc.getChar(start - 1))) start--;
			int end = offset;
			while (end < len && detector.isWordPart(doc.getChar(end))) end++;
//			Point selection= textViewer.getSelectedRange();
//			if (selection.x <= offset && offset < selection.x + selection.y)
//				return new Region(selection.x, selection.y);
			return new Region(start, end - start);
		} catch (BadLocationException e) {
			return new Region(offset,0);
		}
	}
	
	private static synchronized HashMap<String,String> getHoverInfo() {
		if (hoverInfo == null) {
			hoverInfo = new HashMap<String,String>();
			LSLHandler handlers[] = LSLForgePlugin.getDefault().getLSLMetaData().getHandlers();
			for (int i = 0; i < handlers.length; i++) {
				hoverInfo.put(handlers[i].getName(), handlers[i].fullDescription());
			}
			
			LSLFunction functions[] = LSLForgePlugin.getDefault().getLSLMetaData().getFunctions();
			for (int i = 0; i < functions.length; i++) {
				hoverInfo.put(functions[i].getName(), functions[i].fullDescription());
			}
		}
		return hoverInfo;
	}
}
