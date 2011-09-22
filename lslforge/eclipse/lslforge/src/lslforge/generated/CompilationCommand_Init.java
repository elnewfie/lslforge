package lslforge.generated;
import com.thoughtworks.xstream.XStream;
import java.util.LinkedList;
public class CompilationCommand_Init extends CompilationCommand{
    public Tuple3<Boolean,LinkedList<Tuple2<String,String>>,LinkedList<Tuple2<String,String>>> el1;
    public static void init(XStream xstream) {
        xstream.alias("CompilationCommand_Init",CompilationCommand_Init.class); //$NON-NLS-1$
    }
}
