package lslforge.launching;


import lslforge.LSLForgePlugin;
import lslforge.LSLProjectNature;
import lslforge.debug.LSLDebugTarget;
import lslforge.debug.LSLSourceLocator;
import lslforge.debug.LSLTestProcess;
import lslforge.lsltest.LSLTestSuite;
import lslforge.lsltest.TestManager;
import lslforge.util.Log;

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
		Log.debug("launch!!!"); //$NON-NLS-1$

		String fullPath = configuration.getAttribute(LaunchLSLTestShortcut.LC_RESOURCE_NAME, BLANK);
		Path path = new Path(fullPath);
		IResource resource = ResourcesPlugin.getWorkspace().getRoot().findMember(path);
		
		if (resource == null) {
		    MessageDialog.openError(null, Messages.getString("TestLaunchDelegate.TEST_NO_LONGER_EXISTS"),  //$NON-NLS-1$
		            Messages.getString("TestLaunchDelegate.REFERENCED_TEST_NO_LONGER_EXISTS")); //$NON-NLS-1$
		    return;
		}
		LSLTestSuite suite = (LSLTestSuite) resource.getAdapter(LSLTestSuite.class);
		LSLProjectNature nature = (LSLProjectNature) resource.getProject().getNature(LSLProjectNature.ID);
		String sourceDescriptor = nature.projectSourceList();
		String suiteDescriptor = suite.toXml();
		String testDescriptor = "<test-descriptor>" + sourceDescriptor + suiteDescriptor + "</test-descriptor>";  //$NON-NLS-1$//$NON-NLS-2$
		Log.debug(testDescriptor);
		TestManager testManager = LSLForgePlugin.getDefault().getTestManager();
		testManager.testLaunched(configuration, launch, suite.getTests().length);
		LSLTestProcess p = new LSLTestProcess(testDescriptor, launch);
		LSLDebugTarget target = new LSLDebugTarget("lslforge-test", launch, p); //$NON-NLS-1$
        p.go();
		launch.addDebugTarget(target);
        launch.addProcess(p);
        launch.setSourceLocator(new LSLSourceLocator());
	}
}
