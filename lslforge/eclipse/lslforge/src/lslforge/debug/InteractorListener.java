/**
 * 
 */
package lslforge.debug;

public interface InteractorListener {
    public void suspended(LSLScriptExecutionState state);
    public void completed();
}