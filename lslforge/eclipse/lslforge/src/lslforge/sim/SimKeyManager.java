package lslforge.sim;

public class SimKeyManager {
    private int counter = 0;
    private static final String ZEROES = "0000"; //$NON-NLS-1$
    public SimKeyManager() {
        
    }
    
    public synchronized String getNextKey() {
        counter++;
        String hex = Integer.toHexString(counter);
        return "00000000-0000-0000-" + //$NON-NLS-1$
        ZEROES.substring(0, ZEROES.length() - hex.length()) +
        hex + "-000000000000"; //$NON-NLS-1$
        
    }
}
