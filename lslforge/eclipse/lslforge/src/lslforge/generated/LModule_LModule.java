package lslforge.generated;
import com.thoughtworks.xstream.XStream;
import java.util.LinkedList;
public class LModule_LModule extends LModule{
    public LinkedList<GlobDef> el1;
    public LinkedList<Ctx<Var>> el2;
    public static void init(XStream xstream) {
        xstream.alias("LModule_LModule",LModule_LModule.class); //$NON-NLS-1$
    }
}
