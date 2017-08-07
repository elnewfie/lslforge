package lslforge.generated;
import com.thoughtworks.xstream.XStream;
public class Expr_Set extends Expr{
    public Tuple2Ctx<String>,Component> el1;
    public Ctx<Expr> el2;
    public static void init(XStream xstream) {
        xstream.alias("Expr_Set",Expr_Set.class); //$NON-NLS-1$
    }
}
