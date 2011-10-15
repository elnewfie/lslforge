/**
 * 
 */
package lslforge.debug;

import lslforge.LSLForgePlugin;
import lslforge.SimManager;

import org.eclipse.debug.core.ILaunch;

public class LSLSimProcess extends LSLProcess {
    private static final String SYSTEM_TESTER = "SystemTester"; //$NON-NLS-1$
    private String simDescription;
	public LSLSimProcess(String descriptor, ILaunch launch) {
	    super(launch);
	    this.simDescription = descriptor;
	}

    protected Interactor createInteractor(Process p) {
        return new LSLSimInteractor(launch.getLaunchMode(),simDescription, 
                p.getInputStream(), p.getOutputStream());
    }

    protected Process launchExecutable() {
        return LSLForgePlugin.launchCoreCommand(SYSTEM_TESTER, false);
    }
	
	public String getLabel() {
		return "LSL Simulator"; //$NON-NLS-1$
	}

	protected void onTerminate() {
	    super.onTerminate();
	    simManager().simStopped();
	}
	
    private SimManager simManager() {
        return LSLForgePlugin.getDefault().getSimManager();
    }
}