package lslforge.generated;
import com.thoughtworks.xstream.XStream;
import java.util.LinkedList;
public class Expr_Call extends Expr{
    public Ctx<String> el1;
    public LinkedList<Ctx<Expr>> el2;
    public static void init(XStream xstream) {
        xstream.alias("Expr_Call",Expr_Call.class); //$NON-NLS-1$
    }
}
