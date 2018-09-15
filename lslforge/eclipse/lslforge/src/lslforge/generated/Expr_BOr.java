package lslforge.generated;
import com.thoughtworks.xstream.XStream;
import java.util.LinkedList;
public class Expr_BOr extends Expr{
    public Ctx<Expr> el1;
    public Ctx<Expr> el2;
    public static void init(XStream xstream) {
        xstream.alias("Expr_BOr",Expr_BOr.class); //$NON-NLS-1$
    }
}
