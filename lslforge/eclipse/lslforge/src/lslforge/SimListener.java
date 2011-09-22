package lslforge;

import lslforge.sim.SimStatuses;

public interface SimListener {

    //public void newLogMessages(SimStatuses.Message[] messages);
    
    public void newSimState(SimStatuses.SimState state, SimStatuses.Message[] messages);
    
    public void simLaunched();
    
    public void simEnded();
}
