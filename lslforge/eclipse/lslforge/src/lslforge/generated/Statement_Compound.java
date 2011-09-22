package lslforge.generated;
import com.thoughtworks.xstream.XStream;
import java.util.LinkedList;
public class Statement_Compound extends Statement{
    public LinkedList<Ctx<Statement>> el1;
    public static void init(XStream xstream) {
        xstream.alias("Statement_Compound",Statement_Compound.class); //$NON-NLS-1$
    }
}
