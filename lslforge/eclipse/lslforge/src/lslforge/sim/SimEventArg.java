package lslforge.sim;

public class SimEventArg {
    private String name;
    private String value;
    
    public String getName() { return name; }
    public String getValue() { return value; }
    
    public SimEventArg(String name, String value) {
        this.name = name;
        this.value = value;
    }
}
