package lslforge.wizards;

import java.io.ByteArrayInputStream;
import java.io.InputStream;

import lslforge.LSLForgePlugin;
import lslforge.lsltest.LSLTestSuite;
import lslforge.util.Util;

import org.eclipse.core.resources.IFile;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.jface.resource.ImageDescriptor;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.jface.wizard.Wizard;
import org.eclipse.ui.INewWizard;
import org.eclipse.ui.IWorkbench;

public class NewTestWizard extends Wizard implements INewWizard {
	private LSLTestWizardPage mainPage;
	private IStructuredSelection selection;
	private class LSLTestWizardPage extends LSLFileCreationWizardPage {
		public LSLTestWizardPage(IStructuredSelection selection) {
			super("createTest", selection); //$NON-NLS-1$
			setTitle(Messages.getString("NewTestWizard.Title")); //$NON-NLS-1$
			setPageComplete(false);
			setFileExtension("lslt"); //$NON-NLS-1$
			setDefaultPageImageDescriptor(image());
			
		}

		@Override
		protected InputStream getInitialContents() {
			return new ByteArrayInputStream(
					LSLTestSuite.empty().toXml().getBytes());
		}

		@Override
		protected IStatus validateFileName(String fileName) {
			return new Status(IStatus.OK,  "lslforge",""); //$NON-NLS-1$ //$NON-NLS-2$
		}
	}

	public NewTestWizard() {
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
		mainPage = new LSLTestWizardPage(selection);
		addPage(mainPage);
	}

	public void init(IWorkbench workbench, IStructuredSelection selection) {
		this.selection = selection;
	}
}