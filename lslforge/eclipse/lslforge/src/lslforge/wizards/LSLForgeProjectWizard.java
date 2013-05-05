package lslforge.wizards;

import lslforge.LSLForgePerspectiveFactory;
import lslforge.Messages;
import lslforge.util.Log;
import lslforge.util.Util;

import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IProjectDescription;
import org.eclipse.core.resources.ResourcesPlugin;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.NullProgressMonitor;
import org.eclipse.jface.resource.ImageDescriptor;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.jface.wizard.Wizard;
import org.eclipse.ui.INewWizard;
import org.eclipse.ui.IWorkbench;
import org.eclipse.ui.IWorkbenchWindow;

public class LSLForgeProjectWizard extends Wizard implements INewWizard {
	private LSLForgeProjectWizardPage page1;
	private IWorkbench workbench = null;
    private IWorkbenchWindow window;
	public LSLForgeProjectWizard() {
		setDefaultPageImageDescriptor(image());
	}

	private ImageDescriptor image() {
//		IPath path = new Path("$nl$/icons/newlprj_wiz.png"); //$NON-NLS-1$
		return Util.findDescriptor("$nl$/icons/newlprj_wiz.png"); //$NON-NLS-1$
//		URL url = 
//			FileLocator.find(LSLForgePlugin.getDefault().getBundle(), path, null);
//		Log.log("Path = " + url.getPath()); //$NON-NLS-1$
//		if (url != null) {
//			return ImageDescriptor.createFromURL(url);
//		} else {
//		    return ImageDescriptor.getMissingImageDescriptor();
//		}
		
	}

	@Override
	public boolean performFinish() {
		IProject p = ResourcesPlugin.getWorkspace().getRoot().getProject(page1.getProjectName());
		if (p.exists()) return false;
		IProgressMonitor monitor = new NullProgressMonitor();
		try {
			IProjectDescription desc= p.getWorkspace().newProjectDescription(p.getName());

			p.create(desc, monitor);
			p.open(monitor);
            p.setDefaultCharset("UTF-8",monitor); //$NON-NLS-1$
			IProjectDescription description = p.getDescription();
			String[] natures = description.getNatureIds();
			String[] newNatures = (String[]) Util.append(natures, new String[] { "lslforge.LSLForgeNature" }); //$NON-NLS-1$
			description.setNatureIds(newNatures);
			p.setDescription(description, monitor);
			workbench.showPerspective(LSLForgePerspectiveFactory.PERSPECTIVE_ID, window);
		} catch (CoreException e) {
			Log.error(e);
		} finally {
			monitor.done();
		}
		return true;
	}

	@Override
	public void addPages() {
		super.addPages();
		addPage(page1 = new LSLForgeProjectWizardPage(Messages.LSLForgeProjectWizard_CREATE_LSL_PLUS_PROJECT));
	}

	public void init(IWorkbench workbench, IStructuredSelection selection) {
	    this.workbench = workbench;
	    window = workbench.getActiveWorkbenchWindow();
	}

}
