package lslforge.editor;

import java.util.ArrayList;
import java.util.List;

import lslforge.outline.LSLForgeMultiOutlinePage;

import org.eclipse.core.resources.IMarker;
import org.eclipse.core.resources.IResourceChangeEvent;
import org.eclipse.core.resources.IResourceChangeListener;
import org.eclipse.core.resources.IResourceDelta;
import org.eclipse.core.resources.ResourcesPlugin;
import org.eclipse.core.runtime.IPath;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.jface.dialogs.ErrorDialog;
import org.eclipse.swt.widgets.Display;
import org.eclipse.ui.IEditorInput;
import org.eclipse.ui.IEditorPart;
import org.eclipse.ui.IEditorSite;
import org.eclipse.ui.IFileEditorInput;
import org.eclipse.ui.IWorkbenchPage;
import org.eclipse.ui.IWorkbenchPart;
import org.eclipse.ui.PartInitException;
import org.eclipse.ui.ide.IDE;
import org.eclipse.ui.part.FileEditorInput;
import org.eclipse.ui.part.IPage;
import org.eclipse.ui.part.MultiPageEditorPart;
import org.eclipse.ui.part.PageBook;
import org.eclipse.ui.part.PageBookView;
import org.eclipse.ui.views.contentoutline.IContentOutlinePage;

/**
 * An example showing how to create a multi-page editor.
 * This example has 3 pages:
 * <ul>
 * <li>page 0 contains a nested text editor.
 * <li>page 1 allows you to change the font used in page 2
 * </ul>
 */
public class LSLMultiPageEditor extends MultiPageEditorPart implements IResourceChangeListener{

	/** The text editor used in page 0. */
	private LSLForgeEditor sourceEditor;
	private LSLForgeMultiOutlinePage outlinePage = null;

	private IFileEditorInput compiledEditorInput = null;
	private LSLForgeEditor compiledEditor = null;
	private LSLForgeEditor currentEditor = null;
	private int compiledPage = -1;

	/**
	 * Creates a multi-page editor example.
	 */
	public LSLMultiPageEditor() {
		super();
		ResourcesPlugin.getWorkspace().addResourceChangeListener(this);
	}
	/**
	 * Creates page 0 of the multi-page editor,
	 * which contains a text editor.
	 */
	void createSourcePage() {
		try {
			sourceEditor = new LSLForgeEditor();
			currentEditor = sourceEditor;
			sourceEditor.updateOutline();
			
			compiledPage = addPage(sourceEditor, getEditorInput());
			setPageText(compiledPage, sourceEditor.getTitle());
			
		} catch (PartInitException e) {
			ErrorDialog.openError(
				getSite().getShell(),
				"Error creating nested text editor",
				null,
				e.getStatus());
		}
	}

	void createCompiledPage() {
		try {
			if(ResourcesPlugin.getWorkspace().getRoot().exists(compiledEditorInput.getFile().getFullPath())) {
				compiledEditor = new LSLForgeEditor();
				compiledEditor.setReadOnly();
				compiledEditor.updateOutline();
				compiledPage = addPage(compiledEditor, compiledEditorInput);
				setPageText(compiledPage, compiledEditorInput.getName());
//				if(getOutlinePage() != null) {
//					getOutlinePage().addEditor(compiledEditor);
//				}
				
			} else {
				return;
				//TODO Create something here....
//				Composite composite = new Composite(getContainer(), SWT.NONE);
//				GridLayout layout = new GridLayout();
//				composite.setLayout(layout);
//				layout.numColumns = 2;
		//
//				Button fontButton = new Button(composite, SWT.NONE);
//				GridData gd = new GridData(GridData.BEGINNING);
//				gd.horizontalSpan = 2;
//				fontButton.setLayoutData(gd);
//				fontButton.setText("Change Font...");
//				
//				fontButton.addSelectionListener(new SelectionAdapter() {
//					public void widgetSelected(SelectionEvent event) {
////						setFont();
//					}
//				});
		//
//				compiledPage = addPage(composite);
//				setPageText(compiledPage, "<no file>");
			}
			
		} catch (PartInitException e) {
			ErrorDialog.openError(
				getSite().getShell(),
				"Error creating nested text editor",
				null,
				e.getStatus());
		}

	}

	@Override
	protected void pageChange(final int newPageIndex) {
		Display.getDefault().asyncExec(new Runnable(){
			public void run(){
				if(newPageIndex == compiledPage) {
					currentEditor = compiledEditor;
					
				} else {
					currentEditor = sourceEditor;
				}
				
				getOutlinePage().setPageActive(currentEditor.getOutlinePage());
			}
		});

		super.pageChange(newPageIndex);
	}
	
	/**
	 * Creates the pages of the multi-page editor.
	 */
	@Override
	protected void createPages() {
		createSourcePage();
		createCompiledPage();
	}
	
	@Override
	public Object getAdapter(@SuppressWarnings("rawtypes") Class adapter) {
        if (IContentOutlinePage.class.equals(adapter)) {
            return getOutlinePage();
        }

		return super.getAdapter(adapter);
	}
	
	public LSLForgeMultiOutlinePage getOutlinePage() {
		if(outlinePage == null) {
			outlinePage = new LSLForgeMultiOutlinePage();
			if(currentEditor != null) outlinePage.setPageActive(currentEditor.getOutlinePage());
		}
		
		return outlinePage;
	}
	
	/**
	 * The <code>MultiPageEditorPart</code> implementation of this 
	 * <code>IWorkbenchPart</code> method disposes all nested editors.
	 * Subclasses may extend.
	 */
	@Override
	public void dispose() {
		ResourcesPlugin.getWorkspace().removeResourceChangeListener(this);
		super.dispose();
	}
	
	/**
	 * Saves the multi-page editor's document.
	 */
	@Override
	public void doSave(IProgressMonitor monitor) {
		getEditor(0).doSave(monitor);
	}
	
	/**
	 * Saves the multi-page editor's document as another file.
	 * Also updates the text for page 0's tab, and updates this multi-page editor's input
	 * to correspond to the nested editor's.
	 */
	@Override
	public void doSaveAs() {
		IEditorPart editor = getEditor(0);
		editor.doSaveAs();
		setPageText(0, editor.getTitle());
		setInput(editor.getEditorInput());
	}
	
	/* (non-Javadoc)
	 * Method declared on IEditorPart
	 */
	public void gotoMarker(IMarker marker) {
		setActivePage(0);
		IDE.gotoMarker(getEditor(0), marker);
	}
	
	/**
	 * The <code>MultiPageEditorExample</code> implementation of this method
	 * checks that the input is an instance of <code>IFileEditorInput</code>.
	 */
	@Override
	public void init(IEditorSite site, IEditorInput editorInput)
		throws PartInitException {
		if (!(editorInput instanceof IFileEditorInput))
			throw new PartInitException("Invalid Input: Must be IFileEditorInput");
		
		//Try to open the associated .lsl file that goes with this file
		IFileEditorInput ei = (IFileEditorInput)editorInput;
		if(ei.getFile().getFileExtension().equals("lslp")) { //$NON-NLS-1$
			IPath eiPath = ei.getFile().getFullPath().removeFileExtension().addFileExtension("lsl"); //$NON-NLS-1$
			compiledEditorInput = new FileEditorInput(ResourcesPlugin.getWorkspace().getRoot().getFile(eiPath));
		}
		
		super.init(site, editorInput);
	}
	/* (non-Javadoc)
	 * Method declared on IEditorPart.
	 */
	@Override
	public boolean isSaveAsAllowed() {
		return true;
	}

	/**
	 * Closes all project files on project close.
	 * @param event 
	 */
	public void resourceChanged(final IResourceChangeEvent event){
		Display.getDefault().asyncExec(new Runnable(){
			public void run(){
				//Was it our compiled file?
				IResourceDelta[] deltas = getDeltasForPath(compiledEditorInput.getFile().getFullPath(), event.getDelta());
				if(deltas.length > 0) {
					//What kind of change was recorded?
					for(IResourceDelta delta: deltas) {
						switch(delta.getKind()) {
						case IResourceDelta.ADDED:
							//Compiled file added, so add its corresponding tab
							createCompiledPage();
							break;
							
						case IResourceDelta.REMOVED:
							//File removed, so remove the tab if necessary
							
							//Switch back to main tab first
							if(currentEditor.equals(compiledPage)) {
								currentEditor = sourceEditor;
								getOutlinePage().setPageActive(currentEditor.getOutlinePage());
							}
							
							//Now toss the compiled version
							if(compiledEditor != null) {
								removePage(compiledPage);
								compiledEditor = null;
							}
							break;
						}
					}
				}
			}
		});
		
		if(event.getType() == IResourceChangeEvent.PRE_CLOSE){
			Display.getDefault().asyncExec(new Runnable(){
				public void run(){
					IWorkbenchPage[] pages = getSite().getWorkbenchWindow().getPages();
					for (int i = 0; i<pages.length; i++){
						if(((FileEditorInput)sourceEditor.getEditorInput()).getFile().getProject().equals(event.getResource())){
							IEditorPart editorPart = pages[i].findEditor(compiledEditorInput);
							pages[i].closeEditor(editorPart,true);
						}
					}
				}            
			});
		}
	}

	private IResourceDelta[] getDeltasForPath(IPath path, IResourceDelta delta) {
		List<IResourceDelta> matches = new ArrayList<IResourceDelta>();
		matches = getDeltasForPath(path, delta, matches);
		IResourceDelta[] matchesArray = new IResourceDelta[matches.size()];
		matches.toArray(matchesArray);
		return matchesArray;
	}
	
	private List<IResourceDelta> getDeltasForPath(IPath path, IResourceDelta delta, List<IResourceDelta> matches) {
		//Check ourselves
		if(delta.getFullPath().equals(path)) matches.add(delta);
		
		//Now check any children entries
		if(delta.getAffectedChildren().length > 0) {
			for(IResourceDelta childDelta: delta.getAffectedChildren()) {
				matches = getDeltasForPath(path, childDelta, matches);
			}
		}
		
		return matches;
	}
	
	public class LSLForgePageBookView extends PageBookView {
		@Override
		protected IPage createDefaultPage(PageBook book) {
			// TODO Auto-generated method stub
			return null;
		}

		@Override
		protected PageRec doCreatePage(IWorkbenchPart part) {
			// TODO Auto-generated method stub
			return null;
		}

		@Override
		protected void doDestroyPage(IWorkbenchPart part, PageRec pageRecord) {
			// TODO Auto-generated method stub
			
		}

		@Override
		protected IWorkbenchPart getBootstrapPart() {
			// TODO Auto-generated method stub
			return null;
		}

		@Override
		protected boolean isImportant(IWorkbenchPart part) {
			// TODO Auto-generated method stub
			return false;
		}
		
	}
}
