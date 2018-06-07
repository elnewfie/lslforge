package lslforge.generated;
import com.thoughtworks.xstream.XStream;
import java.util.LinkedList;
public class Var_Var extends Var{
    public String varName;
    public LSLType varType;
    public static void init(XStream xstream) {
        xstream.alias("Var_Var",Var_Var.class); //$NON-NLS-1$
    }
}
