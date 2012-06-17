/*******************************************************************************
 * Copyright (c) 2000, 2007 IBM Corporation and others.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 *     IBM Corporation - initial API and implementation
 *     Rob Greayer - adaptation as part of LSL Plus Plug-In
 *******************************************************************************/
package lslplus.wizards;

import java.io.ByteArrayInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.lang.reflect.InvocationTargetException;
import java.util.Iterator;
import java.util.List;

import lslplus.util.Util;

import org.eclipse.core.commands.ExecutionException;
import org.eclipse.core.commands.operations.IOperationHistory;
import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IFolder;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.resources.IWorkspace;
import org.eclipse.core.resources.ResourcesPlugin;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.FileLocator;
import org.eclipse.core.runtime.IAdaptable;
import org.eclipse.core.runtime.IPath;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.jface.dialogs.ErrorDialog;
import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.jface.operation.IRunnableWithProgress;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.jface.wizard.WizardPage;
import org.eclipse.osgi.util.NLS;
import org.eclipse.swt.SWT;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Event;
import org.eclipse.swt.widgets.Listener;
import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.ide.undo.CreateFileOperation;
import org.eclipse.ui.ide.undo.CreateFolderOperation;
import org.eclipse.ui.ide.undo.WorkspaceUndoUtil;
import org.osgi.framework.Bundle;

abstract public class LslSampleCreationWizardPage extends WizardPage implements Listener {
    public static class Sample {
        private String name;
        private Bundle bundle;
        private IPath path;
        private boolean filter;
        
        public Sample(String name, Bundle bundle, IPath path, boolean filter) {
            this.name = name;
            this.bundle = bundle;
            this.path = path;
            this.filter = filter;
        }
        
        public String getName() {
            return name;
        }
        
        public InputStream getInputStream(IFolder folder) throws IOException {
            // if we are filtering, we'll just assume the files small enough to fit in memory...
            // could be smarter, but...
            if (filter) {
                String id = folder.getProjectRelativePath().toString().replace("/", ".");  //$NON-NLS-1$//$NON-NLS-2$
                InputStream in = FileLocator.openStream(bundle, path, false);
                InputStreamReader reader = new InputStreamReader(in);
                char buf[] = new char[1024];
                StringBuilder str = new StringBuilder();
                int numRead;
                while ((numRead = reader.read(buf)) >= 0) {
                    str.append(buf,0,numRead);
                }
                
                if (!id.endsWith(".")) id = id + ".";  //$NON-NLS-1$//$NON-NLS-2$
                return new ByteArrayInputStream(str.toString().replace("@PREFIX@", id).getBytes()); //$NON-NLS-1$
            } else {
                return FileLocator.openStream(bundle, path, false);
            }
        }
    }
    
	private final class CreateSampleRunnable implements IRunnableWithProgress {
	    private final IFolder folder;
	    private List<Sample> samples;

		private CreateSampleRunnable(IFolder folder,
				List<Sample> samples) {
			this.folder = folder;
			this.samples = samples;
		}

		public void run(IProgressMonitor monitor) {
		    CreateFolderOperation crFolderOp = new CreateFolderOperation(folder, null, "create folder"); //$NON-NLS-1$ TODO
			try {
			    IOperationHistory history = PlatformUI.getWorkbench().getOperationSupport().getOperationHistory();
			    IAdaptable info = WorkspaceUndoUtil.getUIInfoAdapter(getShell());
				history.execute(crFolderOp,	monitor, info);
				
				for (Iterator<Sample> i = samples.iterator(); i.hasNext(); ) {
				    Sample sample = i.next();
				    
				    IFile file = folder.getFile(sample.getName());
				    CreateFileOperation op = new CreateFileOperation(file, null, sample.getInputStream(folder), "create file"); //$NON-NLS-1$ TODO
				    history.execute(op, monitor, info);
				}
			} catch (IOException e) {
			    handleException(e);
			} catch (final ExecutionException e) {
				handleException(e);
			}
		}

		private void handleException(final Exception e) {
			getContainer().getShell().getDisplay().syncExec(
				new Runnable() {
					public void run() {
						if (e.getCause() instanceof CoreException) {
							ErrorDialog.openError(getContainer().getShell(),
											Messages.getString("LslFileCreationWizardPage.FILE_CREATION_PROBLEMS"), //$NON-NLS-1$
											null,((CoreException) e.getCause()).getStatus());
						} else {
							MessageDialog
									.openError(getContainer().getShell(),Messages.getString("LslFileCreationWizardPage.CREATION_PROBLEMS"), //$NON-NLS-1$
											NLS.bind(Messages.getString("LslFileCreationWizardPage.INTERNAL_ERROR"), e.getMessage())); //$NON-NLS-1$
						}
					}
				});
		}
	}

	private static final int SIZING_CONTAINER_GROUP_HEIGHT = 250;

	// the current resource selection
	private IStructuredSelection currentSelection;

	// widgets
	private ResourceAndContainerGroup resourceGroup;

	// initial value stores
	private String initialFileName;
	
	private String initialFileExtension;

	private IPath initialContainerFullPath;

	/**
	 * Creates a new file creation wizard page. If the initial resource
	 * selection contains exactly one container resource then it will be used as
	 * the default container resource.
	 * 
	 * @param pageName
	 *            the name of the page
	 * @param selection
	 *            the current resource selection
	 */
	public LslSampleCreationWizardPage(String pageName,
			IStructuredSelection selection) {
		super(pageName);
		setPageComplete(false);
		this.currentSelection = selection;
	}


	/**
	 * Create the control.
	 * @param parent 
	 */
	public void createControl(Composite parent) {
		initializeDialogUnits(parent);
		// top level group
		Composite topLevel = new Composite(parent, SWT.NONE);
		topLevel.setLayout(new GridLayout());
		topLevel.setLayoutData(new GridData(GridData.VERTICAL_ALIGN_FILL
				| GridData.HORIZONTAL_ALIGN_FILL));
		topLevel.setFont(parent.getFont());

		// resource and container group
		resourceGroup = new ResourceAndContainerGroup(topLevel, this,
				getNewFileLabel(),
				Messages.getString("LslFileCreationWizardPage.FILE"), false, //$NON-NLS-1$
				SIZING_CONTAINER_GROUP_HEIGHT);
		resourceGroup.setAllowExistingResources(false);
		initialPopulateContainerNameField();
		if (initialFileName != null) {
			resourceGroup.setResource(initialFileName);
		}
		if (initialFileExtension != null) {
			resourceGroup.setResourceExtension(initialFileExtension);
		}
		validatePage();
		// Show description on opening
		setErrorMessage(null);
		setMessage(null);
		setControl(topLevel);
	}


	protected IFolder createFolderHandle(IPath filePath) {
		return ResourcesPlugin.getWorkspace().getRoot().getFolder(filePath);
	}

	public void createSample() {
		final IPath containerPath = resourceGroup.getContainerFullPath();
		IPath newFilePath = containerPath.append(resourceGroup.getResource());
		final IFolder newFolderHandle = createFolderHandle(newFilePath);

		IRunnableWithProgress op = new CreateSampleRunnable(newFolderHandle,getSampleItems());
		try {
			getContainer().run(true, true, op);
			return;
		} catch (InterruptedException e) {
		    return;
		} catch (InvocationTargetException e) {
			// Execution Exceptions are handled above but we may still get
			// unexpected runtime errors.
			Util.error(e,e.getLocalizedMessage());
			MessageDialog.openError(
					getContainer().getShell(),
					Messages.getString("LslFileCreationWizardPage.CREATION_PROBLEMS"), //$NON-NLS-1$
					NLS.bind(Messages.getString("LslFileCreationWizardPage.INTERNAL_ERROR"),e.getTargetException().getMessage())); //$NON-NLS-1$

			return;
		}
	}

	/**
	 * Returns the current full path of the containing resource as entered or
	 * selected by the user, or its anticipated initial value.
	 * 
	 * @return the container's full path, anticipated initial value, or
	 *         <code>null</code> if no path is known
	 */
	public IPath getContainerFullPath() {
		return resourceGroup.getContainerFullPath();
	}

	/**
	 * Returns the current file name as entered by the user, or its anticipated
	 * initial value.
	 * <br><br>
	 * The current file name will include the file extension if 
	 * the preconditions are met.
	 * @see LslSampleCreationWizardPage#setFileExtension(String)
	 * 
	 * @return the file name, its anticipated initial value, or
	 *         <code>null</code> if no file name is known
	 */
	public String getFileName() {
		if (resourceGroup == null) {
			return initialFileName;
		}

		return resourceGroup.getResource();
	}
	
	/**
	 * Returns the file extension to use when creating the new file.
	 * 
	 * @return the file extension or <code>null</code>.
	 * @see LslSampleCreationWizardPage#setFileExtension(String)
	 * @since 3.3 
	 */
	public String getFileExtension() {
		if (resourceGroup == null) {
			return initialFileExtension;
		}
		return resourceGroup.getResourceExtension();		
	}

	/**
	 * Returns a stream containing the initial contents to be given to new file
	 * resource instances. <b>Subclasses</b> may wish to override. This default
	 * implementation provides no initial contents.
	 * 
	 * @return initial contents to be given to new file resource instances
	 */
	protected List<Sample> getSampleItems() {
		return null;
	}

	/**
	 * Returns the label to display in the file name specification visual
	 * component group.
	 * <p>
	 * Subclasses may reimplement.
	 * </p>
	 * 
	 * @return the label to display in the file name specification visual
	 *         component group
	 */
	protected String getNewFileLabel() {
		return Messages.getString("LslFileCreationWizardPage.FILE_NAME"); //$NON-NLS-1$
	}

	/**
	 * The <code>WizardNewFileCreationPage</code> implementation of this
	 * <code>Listener</code> method handles all events and enablements for
	 * controls on this page. Subclasses may extend.
	 * @param event 
	 */
	public void handleEvent(Event event) {
		setPageComplete(validatePage());
	}

	/**
	 * Sets the initial contents of the container name entry field, based upon
	 * either a previously-specified initial value or the ability to determine
	 * such a value.
	 */
	protected void initialPopulateContainerNameField() {
		if (initialContainerFullPath != null) {
			resourceGroup.setContainerFullPath(initialContainerFullPath);
		} else {
			Iterator<?> it = currentSelection.iterator();
			if (it.hasNext()) {
				Object object = it.next();
				IResource selectedResource = null;
				if (object instanceof IResource) {
					selectedResource = (IResource) object;
				} else if (object instanceof IAdaptable) {
					selectedResource = (IResource) ((IAdaptable) object)
							.getAdapter(IResource.class);
				}
				if (selectedResource != null) {
					if (selectedResource.getType() == IResource.FILE) {
						selectedResource = selectedResource.getParent();
					}
					if (selectedResource.isAccessible()) {
						resourceGroup.setContainerFullPath(selectedResource
								.getFullPath());
					}
				}
			}
		}
	}

	/**
	 * Sets the value of this page's container name field, or stores it for
	 * future use if this page's controls do not exist yet.
	 * 
	 * @param path
	 *            the full path to the container
	 */
	public void setContainerFullPath(IPath path) {
		if (resourceGroup == null) {
			initialContainerFullPath = path;
		} else {
			resourceGroup.setContainerFullPath(path);
		}
	}

	/**
	 * Sets the value of this page's file name field, or stores it for future
	 * use if this page's controls do not exist yet.
	 * 
	 * @param value
	 *            new file name
	 */
	public void setFileName(String value) {
		if (resourceGroup == null) {
			initialFileName = value;
		} else {
			resourceGroup.setResource(value);
		}
	}

	/**
	 * Set the only file extension allowed for this page's file name field.
	 * 
	 * @param value
	 *             The file extension without the '.' prefix 
	 *             (e.g. 'java', 'xml') 
	 */
	public void setFileExtension(String value) {
		if (resourceGroup == null) {
			initialFileExtension = value;
		} else {
			resourceGroup.setResourceExtension(value);
		}
	}
	
	protected abstract IStatus validateFileName(String fileName);
	
	/**
	 * Returns whether this page's controls currently all contain valid values.
	 * 
	 * @return <code>true</code> if all controls are valid, and
	 *         <code>false</code> if at least one is invalid
	 */
	protected boolean validatePage() {
		boolean valid = true;

		if (!resourceGroup.areAllValuesValid()) {
			// if blank name then fail silently
			if (resourceGroup.getProblemType() == ResourceAndContainerGroup.PROBLEM_RESOURCE_EMPTY
					|| resourceGroup.getProblemType() == ResourceAndContainerGroup.PROBLEM_CONTAINER_EMPTY) {
				setMessage(resourceGroup.getProblemMessage());
				setErrorMessage(null);
			} else {
				setErrorMessage(resourceGroup.getProblemMessage());
			}
			valid = false;
		}

		String resourceName = resourceGroup.getResource();
		IWorkspace workspace = ResourcesPlugin.getWorkspace();
		IStatus result = workspace.validateName(resourceName, IResource.FILE);
		if (!result.isOK()) {
			setErrorMessage(result.getMessage());
			return false;
		}

		result = validateFileName(resourceName);
		if (!result.isOK()) {
			setErrorMessage(result.getMessage());
		}
		// validateLinkedResource sets messages itself
		if (valid) {
			setMessage(null);
			setErrorMessage(null);
		}
		return valid;
	}

	/*
	 * @see DialogPage.setVisible(boolean)
	 */
	public void setVisible(boolean visible) {
		super.setVisible(visible);
		if (visible) {
			resourceGroup.setFocus();
		}
	}
}
