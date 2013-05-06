package lslforge.outline;

import java.util.List;
import lslforge.LSLForgePlugin;
import lslforge.LSLProjectNature;
import lslforge.editor.LSLForgeEditor;
import lslforge.outline.items.OutlineItem;
import lslforge.outline.items.TextPosition;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.jface.action.Action;
import org.eclipse.jface.action.IAction;
import org.eclipse.jface.action.IToolBarManager;
import org.eclipse.jface.resource.ImageDescriptor;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.jface.viewers.ITreeContentProvider;
import org.eclipse.jface.viewers.LabelProvider;
import org.eclipse.jface.viewers.SelectionChangedEvent;
import org.eclipse.jface.viewers.TreeViewer;
import org.eclipse.jface.viewers.Viewer;
import org.eclipse.jface.viewers.ViewerComparator;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.ui.IActionBars;
import org.eclipse.ui.IFileEditorInput;
import org.eclipse.ui.texteditor.IElementStateListener;
import org.eclipse.ui.views.contentoutline.ContentOutlinePage;

public class LSLForgeOutlinePage extends ContentOutlinePage
{
	public static final ImageDescriptor image = LSLForgePlugin.imageDescriptorFromPlugin("icons/alpha_mode.gif"); //$NON-NLS-1$

	private DocumentOutline docOutline = null;
	private final LSLForgeEditor editor;

	public LSLForgeOutlinePage(LSLForgeEditor e) {
		this.editor = e;
	}
	
	public List<OutlineItem> getOutline() {
		if(docOutline == null) return null;
		return docOutline.getOutline();
	}

	@Override
	public void createControl(Composite parent) {
		if(parent.isDisposed()) return;	//Skip if it is shut down
		
		super.createControl(parent);
		TreeViewer viewer = getTreeViewer();

		//See if this project supports the outline view
		boolean supported = true;
		if(this.editor.getEditorInput() instanceof IFileEditorInput) {
			try {
				IProject project = ((IFileEditorInput)this.editor.getEditorInput()).getFile().getProject();
				if(!LSLProjectNature.hasProjectNature(project)) supported = false;
			} catch (CoreException e) {
				supported = false;
			}
		}
		
		if(supported) {
			docOutline = new DocumentOutline();
			viewer.setContentProvider(docOutline);
			viewer.setLabelProvider(docOutline);
			viewer.setInput(editor);
			
			// Hook up our listener, so we know when to rebuild the outline
			editor.getDocumentProvider().addElementStateListener(new IElementStateListener() {
				public void elementMoved(Object originalElement, Object movedElement) {
					docOutline.markChanged();
				}
				
				public void elementDirtyStateChanged(Object element, boolean isDirty) {
					docOutline.markChanged();
				}
				
				public void elementDeleted(Object element) {
					docOutline.markChanged();
				}
				
				public void elementContentReplaced(Object element) {
					docOutline.markChanged();
				}
				
				public void elementContentAboutToBeReplaced(Object element) {
					docOutline.markChanged();
				}
			});
			
		} else {
			EmptyOutline outline = new EmptyOutline();
			viewer.setContentProvider(outline);
			viewer.setLabelProvider(outline);
			viewer.setInput(editor);
		}
	}

	@Override
	public void setActionBars(IActionBars actionBars) {
		super.setActionBars(actionBars);

		IToolBarManager toolbar = actionBars.getToolBarManager();
		toolbar.add(new SortAction());
		toolbar.update(true);
	}

	@Override
	public void selectionChanged(SelectionChangedEvent event) {
		super.selectionChanged(event);

		if (!event.getSelection().isEmpty()) {
			Object element = ((IStructuredSelection) event.getSelection()).getFirstElement();
			if (element instanceof OutlineItem) {
				TextPosition textPos = ((OutlineItem) element).getTextPosition();
				editor.setHighlightRange(textPos.start, textPos.end - textPos.start, true);
				return;
			}
		}
		editor.resetHighlightRange();
	}

	public void update() {
		if(docOutline != null) docOutline.markChanged();
		refresh();
	}
	
	public void refresh() {
		TreeViewer viewer = getTreeViewer();
		if (viewer != null) viewer.refresh();
	}

	public void sortItems(boolean sorted) {
		TreeViewer viewer = getTreeViewer();
		if(sorted) {
			//Add a ViewComparator which will auto-sort the list
			if(viewer.getComparator() == null) {
				viewer.setComparator(new ViewerComparator() {
					@Override
					public int compare(Viewer viewer, Object e1, Object e2) {
						if((e1 instanceof OutlineItem) && (e2 instanceof OutlineItem))
							return ((OutlineItem)e1).getName().compareToIgnoreCase(((OutlineItem)e2).getName());
						
						return super.compare(viewer, e1, e2);
					}
				});
			}
		} else {
			//Remove the sorter
			viewer.setComparator(null);
		}
	}
	
	private class SortAction extends Action
	{
		private boolean sorted = false;
		
		@Override
		public ImageDescriptor getImageDescriptor() {
			return image;
		}

		@Override
		public int getStyle() {
			return IAction.AS_CHECK_BOX;
		}

		@Override
		public void run() {
			sorted = !sorted;
			sortItems(sorted);
			super.run();
		}

	}
	
	private class EmptyOutline extends LabelProvider implements ITreeContentProvider {
		public void inputChanged(Viewer viewer, Object oldInput, Object newInput) {
		}

		public Object[] getElements(Object inputElement) {
			return new String[] { Messages.LSLForgeOutlinePage_OutlineUnsupported };
		}

		public Object[] getChildren(Object parentElement) {
			return null;
		}

		public Object getParent(Object element) {
			return null;
		}

		public boolean hasChildren(Object element) {
			return false;
		}
		
	}
}