package lslforge.debug;

import lslforge.LslForgePlugin;
import lslforge.launching.Messages;

import org.eclipse.debug.core.ILaunch;

public class LslTestProcess extends LslProcess {
    private static final String UNIT_TESTER = "UnitTester"; //$NON-NLS-1$
    private String descriptor;
    public LslTestProcess(String descriptor, ILaunch launch) {
        super(launch);
        this.descriptor = descriptor;
    }
    
    protected Interactor createInteractor(Process p) {
        return new LslTestInteractor(launch.getLaunchMode(),LslForgePlugin.getDefault().getTestManager(),
                descriptor, p.getInputStream(), p.getOutputStream());
    }
    
    protected Process launchExecutable() {
        return LslForgePlugin.launchCoreCommand(UNIT_TESTER, false);
    }
    
    public String getLabel() {
        return Messages.getString("TestLaunchDelegate.TEST"); //$NON-NLS-1$
    }

}
