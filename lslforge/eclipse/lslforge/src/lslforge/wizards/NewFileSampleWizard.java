package lslforge.wizards;

import java.io.ByteArrayInputStream;
import java.io.IOException;
import java.io.InputStream;

import lslforge.LSLForgePlugin;
import lslforge.util.Log;
import lslforge.util.Util;

import org.eclipse.core.resources.IFile;
import org.eclipse.core.runtime.FileLocator;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Path;
import org.eclipse.core.runtime.Status;
import org.eclipse.jface.resource.ImageDescriptor;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.jface.wizard.Wizard;
import org.eclipse.ui.IWorkbench;

public abstract class NewFileSampleWizard extends Wizard {

    class LSLModuleWizardPage extends LSLFileCreationWizardPage {
    	public LSLModuleWizardPage(IStructuredSelection selection) {
    		super("createModule", selection); //$NON-NLS-1$
    		setTitle(title);
    		setPageComplete(false);
    		setFileExtension("lslm"); //$NON-NLS-1$
    		setDefaultPageImageDescriptor(image());
    		
    	}
    
    	@Override
		protected InputStream getInitialContents() {
    	    try {
                return FileLocator.openStream(LSLForgePlugin.getDefault().getBundle(),
                        new Path(pathToSample), false);
            } catch (IOException e) {
                Log.error(e);
                return new ByteArrayInputStream("$module ()\n// can't find template!".getBytes()); //$NON-NLS-1$ TODO
            } 
    	}
    
    	@Override
		protected IStatus validateFileName(String fileName) {
    		return new Status(IStatus.OK,  "lslforge",""); //$NON-NLS-1$ //$NON-NLS-2$
    	}
    }

    private LSLModuleWizardPage mainPage;
    private IStructuredSelection selection;
    private String pathToSample;
    private String title;
    
    protected static ImageDescriptor image() {
    	return Util.findDescriptor("$nl$/icons/new_test.png"); //$NON-NLS-1$
    }

    public NewFileSampleWizard(String title, String pathToSample) {
        super();
        this.title = title;
        this.pathToSample = pathToSample;
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