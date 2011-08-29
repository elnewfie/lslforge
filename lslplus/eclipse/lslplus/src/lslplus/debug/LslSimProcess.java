/**
 * 
 */
package lslplus.debug;

import lslplus.LslPlusPlugin;
import lslplus.SimManager;

import org.eclipse.debug.core.ILaunch;

public class LslSimProcess extends LslProcess {
    private static final String SYSTEM_TESTER = "SystemTester"; //$NON-NLS-1$
    private String simDescription;
	public LslSimProcess(String descriptor, ILaunch launch) {
	    super(launch);
	    this.simDescription = descriptor;
	}

    protected Interactor createInteractor(Process p) {
        return new LslSimInteractor(launch.getLaunchMode(),simDescription, 
                p.getInputStream(), p.getOutputStream());
    }

    protected Process launchExecutable() {
        return LslPlusPlugin.launchCoreCommand(SYSTEM_TESTER, false);
    }
	
	public String getLabel() {
		return "LSL Simulator"; //$NON-NLS-1$
	}

	protected void onTerminate() {
	    super.onTerminate();
	    simManager().simStopped();
	}
	
    private SimManager simManager() {
        return LslPlusPlugin.getDefault().getSimManager();
    }
}