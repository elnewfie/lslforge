package lslforge.launching;

import lslforge.LSLForgePlugin;
import lslforge.LSLProjectNature;
import lslforge.SimManager;
import lslforge.debug.LSLDebugTarget;
import lslforge.debug.LSLSimProcess;
import lslforge.debug.LSLSourceLocator;
import lslforge.sim.SimKeyManager;
import lslforge.sim.SimProject;
import lslforge.sim.SimWorldDef;
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

public class SimLaunchDelegate extends LaunchConfigurationDelegate {
    private static final String BLANK = ""; //$NON-NLS-1$
    public void launch(ILaunchConfiguration configuration, String mode, ILaunch launch,
            IProgressMonitor monitor) throws CoreException {
        Util.log("launch!!!"); //$NON-NLS-1$

        String fullPath = configuration.getAttribute(LaunchLSLTestShortcut.LC_RESOURCE_NAME, BLANK);
        Path path = new Path(fullPath);
        IResource resource = ResourcesPlugin.getWorkspace().getRoot().findMember(path);
        if (resource == null) {
            MessageDialog.openError(null, "Resource no longer exists", //$NON-NLS-1$ TODO NLS
                    "Referenced resource no longer exists!"); //$NON-NLS-1$ TODO NLS
            return;
        }
        
        SimWorldDef def = null;
        SimProject.WorldNode world = (SimProject.WorldNode) resource.getAdapter(SimProject.WorldNode.class);
        
        if (world == null) {
            String name = LSLProjectNature.resourceToLSLForgeName(resource);
            def =  SimWorldDef.mkSimpleWorld(new SimKeyManager(), name);
        } else {
            def = SimProject.toSimWorldDef(world);
        }
        LSLProjectNature nature = (LSLProjectNature) resource.getProject().getNature(LSLProjectNature.ID);
        String sourceDescriptor = nature.projectSourceList();
        //SimWorldDef def =  SimWorldDef.mkSimpleWorld(simManager().getKeyManager(), name);
        String testDescriptor = "<sim-descriptor>" + sourceDescriptor + //$NON-NLS-1$
                                SimWorldDef.toXML(def) + 
                                "</sim-descriptor>";  //$NON-NLS-1$
        if (LSLForgePlugin.DEBUG) Util.log(testDescriptor);
        LSLSimProcess p = new LSLSimProcess(testDescriptor, launch);
        LSLDebugTarget target = new LSLDebugTarget("lslforge-test", launch, p); //$NON-NLS-1$
        p.go();
        simManager().simLaunched(p);
        launch.addDebugTarget(target);
        launch.addProcess(p);
        launch.setSourceLocator(new LSLSourceLocator());
    }

    private SimManager simManager() {
        return LSLForgePlugin.getDefault().getSimManager();
    }
}
