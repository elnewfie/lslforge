package lslforge.generated;
import com.thoughtworks.xstream.XStream;
import java.util.LinkedList;
public class CompilationResponse_FullSourceValidation extends CompilationResponse{
    public Tuple2<LinkedList<CompilationStatus>,LinkedList<CompilationStatus>> el1;
    public static void init(XStream xstream) {
        xstream.alias("CompilationResponse_FullSourceValidation",CompilationResponse_FullSourceValidation.class); //$NON-NLS-1$
    }
}
