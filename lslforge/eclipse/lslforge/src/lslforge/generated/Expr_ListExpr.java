package lslforge.generated;
import com.thoughtworks.xstream.XStream;
import java.util.LinkedList;
public class Expr_ListExpr extends Expr{
    public LinkedList<Ctx<Expr>> el1;
    public static void init(XStream xstream) {
        xstream.alias("Expr_ListExpr",Expr_ListExpr.class); //$NON-NLS-1$
    }
}
