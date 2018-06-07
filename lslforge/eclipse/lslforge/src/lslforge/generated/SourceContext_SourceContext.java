package lslforge.generated;
import com.thoughtworks.xstream.XStream;
import java.util.LinkedList;
public class SourceContext_SourceContext extends SourceContext{
    public TextLocation srcTextLocation;
    public String srcPreText;
    public String srcPostTxt;
    public LinkedList<Pragma> srcPragmas;
    public static void init(XStream xstream) {
        xstream.alias("SourceContext_SourceContext",SourceContext_SourceContext.class); //$NON-NLS-1$
    }
}
