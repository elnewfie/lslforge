package lslforge.outline;

import java.util.List;
import lslforge.editor.LSLForgeEditor;
import lslforge.generated.ErrInfo;
import lslforge.outline.items.Function;
import lslforge.outline.items.OutlineItem;
import lslforge.outline.items.TextPosition;
import lslforge.util.Log;

import org.eclipse.core.resources.WorkspaceJob;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.jface.viewers.ITreeContentProvider;
import org.eclipse.jface.viewers.LabelProvider;
import org.eclipse.jface.viewers.Viewer;
import org.eclipse.swt.graphics.Image;
import org.eclipse.swt.widgets.Display;

public class DocumentOutline extends LabelProvider implements ITreeContentProvider
{
	private List<OutlineItem> items;
	private Object[] itemsArray = new Object[0];

	private boolean needRebuild = true;
	private WorkspaceJob rebuildJob = null;

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
		//Do we need a refresh?
		if(needRebuild) {
			rebuild((LSLForgeEditor)inputElement);
		}
		
		//Pass back back the most recent completed build
		return itemsArray;
		
	}

	public Object[] getChildren(Object parentElement) {
		if(parentElement instanceof OutlineItem) {
			List<OutlineItem> children = ((OutlineItem)parentElement).getChildren();
			return (children == null) ? new Object[0] : children.toArray();
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
	
	private void rebuild(final LSLForgeEditor editor) {
		//If there's a pending job, cancel it (we dont need it anymore)
		if(rebuildJob != null) {
			Log.debug("Cancelling current rebuild"); //$NON-NLS-1$
			rebuildJob.cancel();
		}
		
		rebuildJob = new WorkspaceJob("Rebuilding outline") { //$NON-NLS-1$
			private boolean cancelled = false;
			
			@Override
			protected void canceling() {
				this.cancelled = true;
				super.canceling();
			}
			
			@Override
			public IStatus runInWorkspace(IProgressMonitor monitor) throws CoreException {
				//See if we were cancelled before we even started
				if(cancelled) {
					return new Status(IStatus.CANCEL, Log.pluginId, "Rebuild outline cancelled"); //$NON-NLS-1$
				}
				
				//Try to retrieve the file and source text
			    OutlineBuilder builder = new OutlineBuilder(editor);
			    builder.generateOutline();
			    List<OutlineItem> newItems = builder.getOutline();
				List<ErrInfo> newErrors = builder.getErrors();
				
				//Check if this was cancelled  before we go for the UI-changing stuff
				if(cancelled) {
					return new Status(IStatus.CANCEL, Log.pluginId, "Rebuild cancelled"); //$NON-NLS-1$
				}
				
			    //Create the folding annotations in the editor
		        editor.annotateErrs(newErrors);
		        editor.clearProjections();
				
			    for(OutlineItem item: newItems) {
			    	if(item instanceof Function) {
			    		TextPosition textPos = item.getTextPosition();
			    		editor.addProjection(textPos.start, textPos.end - textPos.start);
			    		
			    	} else if(item instanceof lslforge.outline.items.State) {
			    		//Add annotation for the state block
			    		TextPosition textPos = item.getTextPosition();
			    		if(textPos != null) { 
			    			editor.addProjection(textPos.start, textPos.end - textPos.start);
			    		} else {
			    			Log.warning("Could not determine location for marker"); //$NON-NLS-1$
			    			editor.addProjection(0, 0);
			    		}
			    			
			    		
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
			    synchronized(this) {
			    	needRebuild = false;
			    	items = newItems;
			    	itemsArray = items.toArray();
			    	
			    }
			    
			    Display.getDefault().asyncExec(new Runnable() {
					public void run() {
						LSLForgeOutlinePage outlinePage = (LSLForgeOutlinePage)editor.getOutlinePage();
						outlinePage.refresh();
					}
				});

			    rebuildJob = null;		//We are done
			    return new Status(IStatus.OK, Log.pluginId, "Outline rebuild completed"); //$NON-NLS-1$
			}
		};
		
		rebuildJob.schedule();
	}
}
