package lslforge.generated;
import com.thoughtworks.xstream.XStream;
public class Expr_Add extends Expr{
    public Ctx<Expr> el1;
    public Ctx<Expr> el2;
    public static void init(XStream xstream) {
        xstream.alias("Expr_Add",Expr_Add.class); //$NON-NLS-1$
    }
}
