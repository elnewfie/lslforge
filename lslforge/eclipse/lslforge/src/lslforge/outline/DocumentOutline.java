package lslforge.outline;

import java.util.List;
import lslforge.editor.LSLForgeEditor;
import lslforge.generated.ErrInfo;
import lslforge.outline.items.Function;
import lslforge.outline.items.OutlineItem;
import lslforge.outline.items.TextPosition;
import org.eclipse.jface.viewers.ITreeContentProvider;
import org.eclipse.jface.viewers.LabelProvider;
import org.eclipse.jface.viewers.Viewer;
import org.eclipse.swt.graphics.Image;

public class DocumentOutline extends LabelProvider implements ITreeContentProvider
{
	private List<ErrInfo> errors = null;
	private List<OutlineItem> items;
	private boolean needRebuild = true;

	public void inputChanged(Viewer viewer, Object oldInput, Object newInput) {
		items = null;
	}

	@Override
	public Image getImage(Object element) {
		if(element instanceof OutlineItem) {
			return ((OutlineItem)element).getImage();
		}

		return super.getImage(element);
	}

	public List<OutlineItem> getOutline() {
		return items;
	}
	
	public Object[] getElements(Object inputElement) {
		//Pass back our cached copy?
		if((items != null) && (!needRebuild)) {
			return items.toArray();
		}
		
		//Try to retrieve the file and source text
	    LSLForgeEditor editor = (LSLForgeEditor) inputElement;
	    
	    
	    OutlineBuilder builder = new OutlineBuilder(editor);
	    builder.generateOutline();
	    items = builder.getOutline();
	    errors = builder.getErrors();
        
	    //Create the folding annotations in the editor
        editor.annotateErrs(errors);
        editor.clearProjections();
		
	    for(OutlineItem item: items) {
	    	if(item instanceof Function) {
	    		TextPosition textPos = item.getTextPosition();
	    		editor.addProjection(textPos.start, textPos.end - textPos.start);
	    		
	    	} else if(item instanceof lslforge.outline.items.State) {
	    		//Add annotation for the state block
	    		TextPosition textPos = item.getTextPosition();
	    		editor.addProjection(textPos.start, textPos.end - textPos.start);
	    		
	    		//Add annotations for the event handlers
	    		if(item.hasChildren()) {
		    		for(OutlineItem stateItem: item.getChildren()) {
		    			textPos = stateItem.getTextPosition();
			    		editor.addProjection(textPos.start, textPos.end - textPos.start);
		    		}
	    		}
	    	}
	    }

	    //Pass back a final list for the tree viewer
	    needRebuild = false;
	    return items.toArray();
	}

	public Object[] getChildren(Object parentElement) {
		if(parentElement instanceof OutlineItem) {
			return ((OutlineItem)parentElement).getChildren().toArray();
		}
		
		return null;
	}

	public Object getParent(Object element) {
		if(element instanceof OutlineItem) {
			return ((OutlineItem)element).getParent();
		}
		
		return null;
	}

	public boolean hasChildren(Object element) {
		if(element instanceof OutlineItem) {
			return ((OutlineItem)element).hasChildren();
		}
		
		return false;
	}

	@Override
	public String getText(Object element) {
		if(element instanceof OutlineItem) {
			return ((OutlineItem)element).getName();
		}
		
		return super.getText(element);
	}
	
	public void markChanged() {
		needRebuild = true;
	}
	
	public enum DocumentType {
		SCRIPT,
		MODULE
	}
}
