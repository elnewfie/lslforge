/**
 * 
 */
package lslplus.debug;

public interface InteractorListener {
    public void suspended(LslScriptExecutionState state);
    public void completed();
}