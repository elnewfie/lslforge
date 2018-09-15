package lslforge.generated;
import com.thoughtworks.xstream.XStream;
import java.util.LinkedList;
public class Expr_NotEqual extends Expr{
    public Ctx<Expr> el1;
    public Ctx<Expr> el2;
    public static void init(XStream xstream) {
        xstream.alias("Expr_NotEqual",Expr_NotEqual.class); //$NON-NLS-1$
    }
}
