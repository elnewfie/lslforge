package lslforge.launching;

import lslforge.LSLForgeScript;
import lslforge.sim.SimProject;
import lslforge.util.Util;

import org.eclipse.core.resources.IResource;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IAdaptable;
import org.eclipse.debug.core.DebugPlugin;
import org.eclipse.debug.core.ILaunchConfiguration;
import org.eclipse.debug.core.ILaunchConfigurationType;
import org.eclipse.debug.core.ILaunchConfigurationWorkingCopy;
import org.eclipse.debug.ui.DebugUITools;
import org.eclipse.debug.ui.ILaunchShortcut;
import org.eclipse.jface.viewers.ISelection;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.ui.IEditorPart;

public class LaunchLSLSimShortcut implements ILaunchShortcut {

	public static final String LC_RESOURCE_NAME = "resource_name"; //$NON-NLS-1$

	public void launch(ISelection selection, String mode) {
		if (selection instanceof IStructuredSelection &&
		    ((IStructuredSelection)selection).size() == 1) {
		    Object o = ((IStructuredSelection)selection).getFirstElement();
		    adaptAndLaunch(mode, o);
		}
	}

    private void adaptAndLaunch(String mode, Object o) {
        if (o instanceof IAdaptable) {
            LSLForgeScript s = null;
            SimProject.WorldNode w = null;
            IResource r = null;
            if ((s = (LSLForgeScript)((IAdaptable)o).getAdapter(LSLForgeScript.class)) != null) {
                r = s.getResource();
            } else if ((w = (SimProject.WorldNode)((IAdaptable)o).getAdapter(SimProject.WorldNode.class)) != null) {
                r = w.getResource();
            }
            if (r != null) {
                try {
                    ILaunchConfiguration config = findConfig(r);
                    DebugUITools.launch(config, mode);
                } catch (CoreException e) {
                    Util.error(e, e.getLocalizedMessage());
                }
            }
        }
    }

    public void launch(IEditorPart editor, String mode) {
        if (editor.getEditorInput() != null) {
            adaptAndLaunch(mode, editor.getEditorInput());
        }
    }
    
	private ILaunchConfiguration findConfig(IResource r) throws CoreException {
		ILaunchConfiguration[] configs = debugPlugin().getLaunchManager().getLaunchConfigurations(getConfigurationType());
		
		String name = r.getFullPath().toPortableString();
		ILaunchConfiguration config = null;
		
		for (int i = 0; i < configs.length; i++) {
			if (configs[i].getAttribute(LC_RESOURCE_NAME, "").equals(name)) { //$NON-NLS-1$
				config = configs[i];
				break;
			}
		}
	
		if (config == null) {
			ILaunchConfigurationWorkingCopy wc = getConfigurationType().newInstance(null, debugPlugin().getLaunchManager().generateUniqueLaunchConfigurationNameFrom(r.getName()));
			wc.setAttribute(LC_RESOURCE_NAME, name);
			config = wc.doSave();
		}
		return config;
	}
	
	private ILaunchConfigurationType getConfigurationType() {
		return DebugPlugin.getDefault().getLaunchManager().getLaunchConfigurationType("lslforge.simLaunch");		 //$NON-NLS-1$
	}

	private DebugPlugin debugPlugin() { return DebugPlugin.getDefault(); }
}
