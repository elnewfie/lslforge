package lslforge.generated;
import com.thoughtworks.xstream.XStream;
import java.util.LinkedList;
public class Statement_While extends Statement{
    public Ctx<Expr> el1;
    public Ctx<Statement> el2;
    public static void init(XStream xstream) {
        xstream.alias("Statement_While",Statement_While.class); //$NON-NLS-1$
    }
}
