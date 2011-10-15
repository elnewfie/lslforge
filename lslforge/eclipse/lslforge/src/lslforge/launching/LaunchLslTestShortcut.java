package lslforge.launching;

import lslforge.lsltest.LSLTestSuite;
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

public class LaunchLSLTestShortcut implements ILaunchShortcut {

	public static final String LC_RESOURCE_NAME = "resource_name"; //$NON-NLS-1$

	public void launch(ISelection selection, String mode) {
		Object o = null;
		LSLTestSuite s = null;
		if (selection instanceof IStructuredSelection &&
		    ((IStructuredSelection)selection).size() == 1 &&
			(o = ((IStructuredSelection)selection).getFirstElement()) instanceof IAdaptable &&
			(s = (LSLTestSuite)((IAdaptable)o).getAdapter(LSLTestSuite.class)) != null) {
			
			try {
				ILaunchConfiguration config = findConfig(s);
				DebugUITools.launch(config, mode);
			} catch (CoreException e) {
				Util.error(e, e.getLocalizedMessage());
			}
		}
	}

	public ILaunchConfiguration findConfig(LSLTestSuite suite) throws CoreException {
		ILaunchConfiguration[] configs = debugPlugin().getLaunchManager().getLaunchConfigurations(getConfigurationType());
		IResource r = suite.getResource();
		
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
	
	public void launch(IEditorPart editor, String mode) {
	}
	private ILaunchConfigurationType getConfigurationType() {
		return DebugPlugin.getDefault().getLaunchManager().getLaunchConfigurationType("lslforge.testLaunch");		 //$NON-NLS-1$
	}

	private DebugPlugin debugPlugin() { return DebugPlugin.getDefault(); }
}
