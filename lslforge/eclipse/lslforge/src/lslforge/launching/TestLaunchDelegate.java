package lslforge.launching;


import lslforge.LslForgePlugin;
import lslforge.LslProjectNature;
import lslforge.debug.LslDebugTarget;
import lslforge.debug.LslSourceLocator;
import lslforge.debug.LslTestProcess;
import lslforge.lsltest.LslTestSuite;
import lslforge.lsltest.TestManager;
import lslforge.util.Util;

import org.eclipse.core.resources.IResource;
import org.eclipse.core.resources.ResourcesPlugin;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.Path;
import org.eclipse.debug.core.ILaunch;
import org.eclipse.debug.core.ILaunchConfiguration;
import org.eclipse.debug.core.model.LaunchConfigurationDelegate;
import org.eclipse.jface.dialogs.MessageDialog;

public class TestLaunchDelegate extends LaunchConfigurationDelegate {

	static final String BLANK = ""; //$NON-NLS-1$

    public void launch(ILaunchConfiguration configuration, String mode,
			ILaunch launch, IProgressMonitor monitor) throws CoreException {
		if (LslForgePlugin.DEBUG) Util.log("launch!!!"); //$NON-NLS-1$

		String fullPath = configuration.getAttribute(LaunchLslTestShortcut.LC_RESOURCE_NAME, BLANK);
		Path path = new Path(fullPath);
		IResource resource = ResourcesPlugin.getWorkspace().getRoot().findMember(path);
		
		if (resource == null) {
		    MessageDialog.openError(null, Messages.getString("TestLaunchDelegate.TEST_NO_LONGER_EXISTS"),  //$NON-NLS-1$
		            Messages.getString("TestLaunchDelegate.REFERENCED_TEST_NO_LONGER_EXISTS")); //$NON-NLS-1$
		    return;
		}
		LslTestSuite suite = (LslTestSuite) resource.getAdapter(LslTestSuite.class);
		LslProjectNature nature = (LslProjectNature) resource.getProject().getNature(LslProjectNature.ID);
		String sourceDescriptor = nature.projectSourceList();
		String suiteDescriptor = suite.toXml();
		String testDescriptor = "<test-descriptor>" + sourceDescriptor + suiteDescriptor + "</test-descriptor>";  //$NON-NLS-1$//$NON-NLS-2$
		if (LslForgePlugin.DEBUG) Util.log(testDescriptor);
		TestManager testManager = LslForgePlugin.getDefault().getTestManager();
		testManager.testLaunched(configuration, launch, suite.getTests().length);
		LslTestProcess p = new LslTestProcess(testDescriptor, launch);
		LslDebugTarget target = new LslDebugTarget("lslforge-test", launch, p); //$NON-NLS-1$
        p.go();
		launch.addDebugTarget(target);
        launch.addProcess(p);
        launch.setSourceLocator(new LslSourceLocator());
	}
}
