package lslforge.project;

import lslforge.LSLProjectNature;
import lslforge.util.Log;
import org.eclipse.core.commands.AbstractHandler;
import org.eclipse.core.commands.ExecutionEvent;
import org.eclipse.core.commands.ExecutionException;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.ui.handlers.HandlerUtil;

public class AddSupportHandler extends AbstractHandler
{

	public Object execute(ExecutionEvent event) throws ExecutionException {
		IProject project = ProjectContributionItem.getSelectedProject();
		if(project != null) {
			try {
				LSLProjectNature.addProjectNature(project);
				MessageDialog.openInformation(HandlerUtil.getActiveShellChecked(event), Messages.AddSupportHandler_AddSupport, Messages.AddSupportHandler_AddSupportReply);
			} catch (CoreException e) {
				Log.error(e);
				MessageDialog.openInformation(
						HandlerUtil.getActiveShellChecked(event), 
						Messages.AddSupportHandler_AddSupport, 
						Messages.AddSupportHandler_AddSupportErr + e.getMessage()
				);
			}
		}
		return null;
	}

}
