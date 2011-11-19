package lslforge.generated;
import com.thoughtworks.xstream.XStream;
public class TextLocation_TextLocation extends TextLocation{
    public Integer textLine0;
    public Integer textColumn0;
    public Integer textLine1;
    public Integer textColumn1;
    public String textName;
    public static void init(XStream xstream) {
        xstream.alias("TextLocation_TextLocation",TextLocation_TextLocation.class); //$NON-NLS-1$
    }
}
