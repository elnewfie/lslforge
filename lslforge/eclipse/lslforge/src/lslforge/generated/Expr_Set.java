package lslforge.generated;
import com.thoughtworks.xstream.XStream;
import java.util.LinkedList;
public class Expr_Set extends Expr{
    public Tuple2<Ctx<String>,Component> el1;
    public Ctx<Expr> el2;
    public static void init(XStream xstream) {
        xstream.alias("Expr_Set",Expr_Set.class); //$NON-NLS-1$
    }
}
