package lslforge.generated;
import com.thoughtworks.xstream.XStream;
import java.util.LinkedList;
public class Expr_Cast extends Expr{
    public LSLType el1;
    public Ctx<Expr> el2;
    public static void init(XStream xstream) {
        xstream.alias("Expr_Cast",Expr_Cast.class); //$NON-NLS-1$
    }
}
