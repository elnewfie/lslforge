package lslforge.launching;

import lslforge.LslForgePlugin;
import lslforge.LslProjectNature;
import lslforge.SimManager;
import lslforge.debug.LslDebugTarget;
import lslforge.debug.LslSimProcess;
import lslforge.debug.LslSourceLocator;
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

        String fullPath = configuration.getAttribute(LaunchLslTestShortcut.LC_RESOURCE_NAME, BLANK);
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
            String name = LslProjectNature.resourceToLslForgeName(resource);
            def =  SimWorldDef.mkSimpleWorld(new SimKeyManager(), name);
        } else {
            def = SimProject.toSimWorldDef(world);
        }
        LslProjectNature nature = (LslProjectNature) resource.getProject().getNature(LslProjectNature.ID);
        String sourceDescriptor = nature.projectSourceList();
        //SimWorldDef def =  SimWorldDef.mkSimpleWorld(simManager().getKeyManager(), name);
        String testDescriptor = "<sim-descriptor>" + sourceDescriptor + //$NON-NLS-1$
                                SimWorldDef.toXML(def) + 
                                "</sim-descriptor>";  //$NON-NLS-1$
        if (LslForgePlugin.DEBUG) Util.log(testDescriptor);
        LslSimProcess p = new LslSimProcess(testDescriptor, launch);
        LslDebugTarget target = new LslDebugTarget("lslforge-test", launch, p); //$NON-NLS-1$
        p.go();
        simManager().simLaunched(p);
        launch.addDebugTarget(target);
        launch.addProcess(p);
        launch.setSourceLocator(new LslSourceLocator());
    }

    private SimManager simManager() {
        return LslForgePlugin.getDefault().getSimManager();
    }
}
