package lslforge.generated;
import com.thoughtworks.xstream.XStream;
import java.util.LinkedList;
public class Expr_VecExpr extends Expr{
    public Ctx<Expr> el1;
    public Ctx<Expr> el2;
    public Ctx<Expr> el3;
    public static void init(XStream xstream) {
        xstream.alias("Expr_VecExpr",Expr_VecExpr.class); //$NON-NLS-1$
    }
}
