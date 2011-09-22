package lslforge.debug;

public interface Interactor {
    public void start();
    public void continueExecution();
    public void step();
    public void stepOut();
    public void stepOver();
    public void addListener(InteractorListener listener);
    public void removeListener(InteractorListener listener);
    public void stop();
}
