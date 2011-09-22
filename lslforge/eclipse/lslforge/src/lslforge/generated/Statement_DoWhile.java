package lslforge.generated;
import com.thoughtworks.xstream.XStream;
public class Statement_DoWhile extends Statement{
    public Ctx<Statement> el1;
    public Ctx<Expr> el2;
    public static void init(XStream xstream) {
        xstream.alias("Statement_DoWhile",Statement_DoWhile.class); //$NON-NLS-1$
    }
}
