package lslforge.generated;
import com.thoughtworks.xstream.XStream;
import java.util.LinkedList;
public class Statement_Return extends Statement{
    public Maybe<Ctx<Expr>> el1;
    public static void init(XStream xstream) {
        xstream.alias("Statement_Return",Statement_Return.class); //$NON-NLS-1$
    }
}
