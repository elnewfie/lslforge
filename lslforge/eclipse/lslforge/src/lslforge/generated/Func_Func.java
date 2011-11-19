package lslforge.generated;
import com.thoughtworks.xstream.XStream;
import java.util.LinkedList;
public class Func_Func extends Func{
    public FuncDec el1;
    public LinkedList<Ctx<Statement>> el2;
    public static void init(XStream xstream) {
        xstream.alias("Func_Func",Func_Func.class); //$NON-NLS-1$
    }
}
