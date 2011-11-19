package lslforge.generated;
import com.thoughtworks.xstream.XStream;
public class Expr_ModBy extends Expr{
    public Tuple2<Ctx<String>,Component> el1;
    public Ctx<Expr> el2;
    public static void init(XStream xstream) {
        xstream.alias("Expr_ModBy",Expr_ModBy.class); //$NON-NLS-1$
    }
}
