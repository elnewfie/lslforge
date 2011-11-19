package lslforge.generated;
import com.thoughtworks.xstream.XStream;
import java.util.LinkedList;
public class CompilationResponse_ModuleResponse extends CompilationResponse{
    public Tuple2<LModule,LinkedList<ErrInfo>> el1;
    public static void init(XStream xstream) {
        xstream.alias("CompilationResponse_ModuleResponse",CompilationResponse_ModuleResponse.class); //$NON-NLS-1$
    }
}
