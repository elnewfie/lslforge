package lslforge.generated;
import com.thoughtworks.xstream.XStream;
import java.util.LinkedList;
public class Statement_For extends Statement{
    public LinkedList<Ctx<Expr>> el1;
    public Maybe<Ctx<Expr>> el2;
    public LinkedList<Ctx<Expr>> el3;
    public Ctx<Statement> el4;
    public static void init(XStream xstream) {
        xstream.alias("Statement_For",Statement_For.class); //$NON-NLS-1$
    }
}
