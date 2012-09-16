package lslforge.project;

import lslforge.LSLProjectNature;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IAdaptable;
import org.eclipse.jface.action.IContributionItem;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.swt.SWT;
import org.eclipse.ui.IWorkbenchWindow;
import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.actions.CompoundContributionItem;
import org.eclipse.ui.menus.CommandContributionItem;
import org.eclipse.ui.menus.CommandContributionItemParameter;

public class ProjectContributionItem extends CompoundContributionItem
{

	public ProjectContributionItem() {
		// TODO Auto-generated constructor stub
	}

	public ProjectContributionItem(String id) {
		super(id);
		// TODO Auto-generated constructor stub
	}

	@Override
	protected IContributionItem[] getContributionItems() {
		IContributionItem[] items = new CommandContributionItem[] { };
		
		IProject project = getSelectedProject();
		if(project != null) {
			try {
				if(LSLProjectNature.hasProjectNature(project)) {
					//Use the 'Remove LSLForge Support' menu
					final CommandContributionItemParameter itemParam = new CommandContributionItemParameter(PlatformUI.getWorkbench().getActiveWorkbenchWindow(),
							"lslforge.project.projectContributionItem", "lslforge.project.removeSupport", SWT.NONE); //$NON-NLS-1$ //$NON-NLS-2$
					itemParam.label = Messages.ProjectContributionItem_removeSupport;
					items = new IContributionItem[] { new CommandContributionItem(itemParam) };
					
				} else {
					//Use the 'Add LSLForge Support' menu
					final CommandContributionItemParameter itemParam = new CommandContributionItemParameter(PlatformUI.getWorkbench().getActiveWorkbenchWindow(),
							"lslforge.project.projectContributionItem", "lslforge.project.addSupport", SWT.NONE); //$NON-NLS-1$ //$NON-NLS-2$
					itemParam.label = Messages.ProjectContributionItem_addSupport;
					items = new IContributionItem[] { new CommandContributionItem(itemParam) };
				}
			} catch (CoreException e) {
				//Nothing to do
			}
		}
		
		return items;
	}
	
	public static IProject getSelectedProject() {
		IWorkbenchWindow window = PlatformUI.getWorkbench().getActiveWorkbenchWindow();
		if (window != null) {
			IStructuredSelection selection = (IStructuredSelection) window.getSelectionService().getSelection();
			Object firstElement = selection.getFirstElement();
			if (firstElement instanceof IAdaptable) {
				return (IProject) ((IAdaptable) firstElement).getAdapter(IProject.class);
			}
		}

		return null;
	}
}
