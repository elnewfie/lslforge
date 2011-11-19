package lslforge.generated;
import com.thoughtworks.xstream.XStream;
import java.util.LinkedList;
public class CompilationResponse_ScriptResponse extends CompilationResponse{
    public Tuple2<LSLScript,LinkedList<ErrInfo>> el1;
    public static void init(XStream xstream) {
        xstream.alias("CompilationResponse_ScriptResponse",CompilationResponse_ScriptResponse.class); //$NON-NLS-1$
    }
}
