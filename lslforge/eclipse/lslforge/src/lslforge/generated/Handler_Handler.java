package lslforge.generated;
import com.thoughtworks.xstream.XStream;
import java.util.LinkedList;
public class Handler_Handler extends Handler{
    public Ctx<String> handlerName;
    public LinkedList<Ctx<Var>> handlerParams;
    public LinkedList<Ctx<Statement>> handlerStatements;
    public static void init(XStream xstream) {
        xstream.alias("Handler_Handler",Handler_Handler.class); //$NON-NLS-1$
    }
}
