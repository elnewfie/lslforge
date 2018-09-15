package lslforge.generated;
import com.thoughtworks.xstream.XStream;
import java.util.LinkedList;
public class Statement_Decl extends Statement{
    public Var el1;
    public Maybe<Ctx<Expr>> el2;
    public static void init(XStream xstream) {
        xstream.alias("Statement_Decl",Statement_Decl.class); //$NON-NLS-1$
    }
}
