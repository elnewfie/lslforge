package lslforge.generated;
import com.thoughtworks.xstream.XStream;
public class CompilationStatus_CompilationStatus extends CompilationStatus{
    public String csName;
    public Either<LinkedList<ErrInfo>>,Tuple2LinkedList<GlobalSummary>>,LinkedList<EPSummary>>>> csInfo;
    public static void init(XStream xstream) {
        xstream.alias("CompilationStatus_CompilationStatus",CompilationStatus_CompilationStatus.class); //$NON-NLS-1$
    }
}
