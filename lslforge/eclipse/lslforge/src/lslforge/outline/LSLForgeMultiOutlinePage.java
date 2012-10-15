package lslforge.outline;

import org.eclipse.jface.action.Action;
import org.eclipse.jface.action.IAction;
import org.eclipse.jface.action.IToolBarManager;
import org.eclipse.jface.resource.ImageDescriptor;
import org.eclipse.jface.viewers.ISelection;
import org.eclipse.jface.viewers.ISelectionChangedListener;
import org.eclipse.swt.SWT;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.ui.IActionBars;
import org.eclipse.ui.part.Page;
import org.eclipse.ui.part.PageBook;
import org.eclipse.ui.views.contentoutline.IContentOutlinePage;

public class LSLForgeMultiOutlinePage extends Page implements IContentOutlinePage {
	protected PageBook fPagebook;
	protected IContentOutlinePage fCurrentPage;
	private boolean sorted = false;

	@Override
	public void createControl(Composite parent) {
		fPagebook = new PageBook(parent, SWT.NONE);
		if (fCurrentPage != null)
			setPageActive(fCurrentPage);
	}

	public void setPageActive(IContentOutlinePage page) {
		if (page != null) {
			fCurrentPage = page;
			if (fPagebook == null || fPagebook.isDisposed()) {
				return;
			}

			Control control = page.getControl();

			//if (control == null || control.isDisposed()) {
			if (control == null) {
				page.createControl(fPagebook);
				control = page.getControl();
			}

			fPagebook.showPage(control);
			
			if(page instanceof LSLForgeOutlinePage) {
				((LSLForgeOutlinePage)page).sortItems(sorted);
			}
			
		}
	}

	@Override
	public void setActionBars(IActionBars actionBars) {
		IToolBarManager toolbar = actionBars.getToolBarManager();
		toolbar.add(new SortAction());
		toolbar.update(false);
		actionBars.updateActionBars();
		
		super.setActionBars(actionBars);
	}

	@Override
	public void dispose() {
		if(fCurrentPage != null) fCurrentPage.dispose();
	}

	@Override
	public Control getControl() {
		if(fCurrentPage != null) 
			return fPagebook;
		else
			return null;
	}

	@Override
	public void setFocus() {
		if(fCurrentPage != null) fCurrentPage.setFocus();
	}

	public void addSelectionChangedListener(ISelectionChangedListener listener) {
		if(fCurrentPage != null) fCurrentPage.addSelectionChangedListener(listener);
	}

	public ISelection getSelection() {
		if(fCurrentPage != null) 
			return fCurrentPage.getSelection();
		else
			return null;
	}

	public void removeSelectionChangedListener(ISelectionChangedListener listener) {
		if(fCurrentPage != null) fCurrentPage.removeSelectionChangedListener(listener);
	}

	public void setSelection(ISelection selection) {
		if(fCurrentPage != null) fCurrentPage.setSelection(selection);
	}
	
    private class SortAction extends Action {
    	
		@Override
		public ImageDescriptor getImageDescriptor() {
			return LSLForgeOutlinePage.image;
		}

		@Override
		public int getStyle() {
			return IAction.AS_CHECK_BOX;
		}

		@Override
		public void run() {
			sorted = !sorted;
			if(fCurrentPage instanceof LSLForgeOutlinePage) {
				((LSLForgeOutlinePage)fCurrentPage).sortItems(sorted);
			}
			super.run();
		}
    }
}
