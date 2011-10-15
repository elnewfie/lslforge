package lslforge.wizards;

import java.io.ByteArrayInputStream;
import java.io.InputStream;

import lslforge.LSLForgePlugin;
import lslforge.sim.SimProject;
import lslforge.util.Util;

import org.eclipse.core.resources.IFile;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.jface.resource.ImageDescriptor;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.jface.wizard.Wizard;
import org.eclipse.ui.INewWizard;
import org.eclipse.ui.IWorkbench;

public class NewSimProjectWizard extends Wizard implements INewWizard {
	private LSLSimProjectWizardPage mainPage;
	private IStructuredSelection selection;
	private class LSLSimProjectWizardPage extends LSLFileCreationWizardPage {
		public LSLSimProjectWizardPage(IStructuredSelection selection) {
			super("Create Simulation Project", selection); //$NON-NLS-1$
			setTitle("Create Simulation Project"); //$NON-NLS-1$
			setPageComplete(false);
			setFileExtension("simp"); //$NON-NLS-1$
			setDefaultPageImageDescriptor(image());
			
		}

		protected InputStream getInitialContents() {
			return new ByteArrayInputStream(
					SimProject.toXml(new SimProject.WorldNode("world")).getBytes()); //$NON-NLS-1$
		}

		protected IStatus validateFileName(String fileName) {
			return new Status(IStatus.OK,  "lslforge", ""); //$NON-NLS-1$ //$NON-NLS-2$
		}
	}

	public NewSimProjectWizard() {
		this.setDefaultPageImageDescriptor(image());
	}

	private static ImageDescriptor image() {
		return Util.findDescriptor("$nl$/icons/new_test.png"); //$NON-NLS-1$
	}

	public boolean performFinish() {
        IFile f = mainPage.createNewFile();
        LSLForgePlugin.openResource(getShell(), f);
		return true;
	}

	public void addPages() {
		super.addPages();
		mainPage = new LSLSimProjectWizardPage(selection);
		addPage(mainPage);
	}

	public void init(IWorkbench workbench, IStructuredSelection selection) {
		this.selection = selection;
	}
}