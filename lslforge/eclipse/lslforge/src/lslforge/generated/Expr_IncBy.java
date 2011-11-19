package lslforge.generated;
import com.thoughtworks.xstream.XStream;
public class Expr_IncBy extends Expr{
    public Tuple2<Ctx<String>,Component> el1;
    public Ctx<Expr> el2;
    public static void init(XStream xstream) {
        xstream.alias("Expr_IncBy",Expr_IncBy.class); //$NON-NLS-1$
    }
}
