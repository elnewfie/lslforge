package lslforge.wizards;

import java.io.ByteArrayInputStream;
import java.io.InputStream;

import lslforge.LslForgePlugin;
import lslforge.util.Util;

import org.eclipse.core.resources.IFile;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.jface.resource.ImageDescriptor;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.jface.wizard.Wizard;
import org.eclipse.ui.INewWizard;
import org.eclipse.ui.IWorkbench;

public class NewScriptWizard extends Wizard implements INewWizard {
	private LslScriptWizardPage mainPage;
	private IStructuredSelection selection;
	private class LslScriptWizardPage extends LslFileCreationWizardPage {
		private static final String DEFAULT_SCRIPT_CONTENTS = 
		    "\n\ndefault {\n    state_entry() {\n        llOwnerSay(\"Hello " + //$NON-NLS-1$
		    "Scripter\");\n    }\n}\n"; //$NON-NLS-1$

        public LslScriptWizardPage(IStructuredSelection selection) {
			super("createModule", selection); //$NON-NLS-1$
			setTitle(Messages.getString("NewScriptWizard.NEW_SCRIPT")); //$NON-NLS-1$
			setPageComplete(false);
			setFileExtension("lslp"); //$NON-NLS-1$
			setDefaultPageImageDescriptor(image());
			
		}

		protected InputStream getInitialContents() {
			return new ByteArrayInputStream(
			    DEFAULT_SCRIPT_CONTENTS.
				getBytes());
		}

		protected IStatus validateFileName(String fileName) {
			return new Status(IStatus.OK,  "lslforge",""); //$NON-NLS-1$ //$NON-NLS-2$
		}
	}

	public NewScriptWizard() {
		this.setDefaultPageImageDescriptor(image());
	}

	private static ImageDescriptor image() {
		return Util.findDescriptor("$nl$/icons/new_test.png"); //$NON-NLS-1$
	}

	public boolean performFinish() {
		IFile f = mainPage.createNewFile();
		LslForgePlugin.openResource(getShell(), f);
		return true;
	}

	public void addPages() {
		super.addPages();
		mainPage = new LslScriptWizardPage(selection);
		addPage(mainPage);
	}

	public void init(IWorkbench workbench, IStructuredSelection selection) {
		this.selection = selection;
	}
}