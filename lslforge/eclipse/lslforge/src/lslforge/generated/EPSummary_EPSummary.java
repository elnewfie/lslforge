package lslforge.generated;
import com.thoughtworks.xstream.XStream;
import java.util.LinkedList;
public class EPSummary_EPSummary extends EPSummary{
    public EPKind epKind;
    public String epName;
    public LSLType epType;
    public LinkedList<Tuple2<String,LSLType>> epParams;
    public static void init(XStream xstream) {
        xstream.alias("EPSummary_EPSummary",EPSummary_EPSummary.class); //$NON-NLS-1$
    }
}
