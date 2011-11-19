package lslforge.wizards;

import java.io.ByteArrayInputStream;
import java.io.InputStream;

import lslforge.LSLForgePlugin;
import lslforge.util.Util;

import org.eclipse.core.resources.IFile;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.jface.resource.ImageDescriptor;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.jface.wizard.Wizard;
import org.eclipse.ui.INewWizard;
import org.eclipse.ui.IWorkbench;

public class NewModuleWizard extends Wizard implements INewWizard {
	private LSLModuleWizardPage mainPage;
	private IStructuredSelection selection;
	private class LSLModuleWizardPage extends LSLFileCreationWizardPage {
		public LSLModuleWizardPage(IStructuredSelection selection) {
			super("createModule", selection); //$NON-NLS-1$
			setTitle(Messages.getString("NewModuleWizard.NEW_MODULE")); //$NON-NLS-1$
			setPageComplete(false);
			setFileExtension("lslm"); //$NON-NLS-1$
			setDefaultPageImageDescriptor(image());
			
		}

		@Override
		protected InputStream getInitialContents() {
			return new ByteArrayInputStream(
					"$module ()\n".getBytes()); //$NON-NLS-1$
		}

		@Override
		protected IStatus validateFileName(String fileName) {
			return new Status(IStatus.OK,  "lslforge",""); //$NON-NLS-1$ //$NON-NLS-2$
		}
	}

	public NewModuleWizard() {
		this.setDefaultPageImageDescriptor(image());
	}

	private static ImageDescriptor image() {
		return Util.findDescriptor("$nl$/icons/new_test.png"); //$NON-NLS-1$
	}

	@Override
	public boolean performFinish() {
        IFile f = mainPage.createNewFile();
        LSLForgePlugin.openResource(getShell(), f);
		return true;
	}

	@Override
	public void addPages() {
		super.addPages();
		mainPage = new LSLModuleWizardPage(selection);
		addPage(mainPage);
	}

	public void init(IWorkbench workbench, IStructuredSelection selection) {
		this.selection = selection;
	}
}