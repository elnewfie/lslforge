package lslforge.generated;
import com.thoughtworks.xstream.XStream;
public class Statement_If extends Statement{
    public Ctx<Expr> el1;
    public Ctx<Statement> el2;
    public Ctx<Statement> el3;
    public static void init(XStream xstream) {
        xstream.alias("Statement_If",Statement_If.class); //$NON-NLS-1$
    }
}
