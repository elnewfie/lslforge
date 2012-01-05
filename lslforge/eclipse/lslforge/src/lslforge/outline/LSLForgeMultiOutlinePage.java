package lslforge.outline;

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

	@Override
	public void createControl(Composite parent) {
		fPagebook = new PageBook(parent, SWT.NONE);
		if (fCurrentPage != null)
			setPageActive(fCurrentPage);
	}

	public void setPageActive(IContentOutlinePage page) {
		// clearActionBars();

		if (page != null) {
			fCurrentPage = page;
			if (fPagebook == null) {
				return;
			}

			Control control = page.getControl();

			if (control == null || control.isDisposed()) {
				page.createControl(fPagebook);
				control = page.getControl();
			}

			fPagebook.showPage(control);
		}
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
	public void setActionBars(IActionBars actionBars) {
		// TODO Auto-generated method stub
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
}
