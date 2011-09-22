/**
 * 
 */
package lslforge.gentree;

public class NodeStatus {
    private boolean ok;
    private String error;
    public static final NodeStatus OK = new NodeStatus(true,""); //$NON-NLS-1$

    public NodeStatus(boolean ok, String error) {
        this.ok = ok;
        this.error = error;
    }
    
    public String toString() { return error; }
    
    public boolean isOk() { return ok; }
}