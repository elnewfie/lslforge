package lslforge.generated;
import com.thoughtworks.xstream.XStream;
import java.util.LinkedList;
public class Expr_PostDec extends Expr{
    public Tuple2<Ctx<String>,Component> el1;
    public static void init(XStream xstream) {
        xstream.alias("Expr_PostDec",Expr_PostDec.class); //$NON-NLS-1$
    }
}
