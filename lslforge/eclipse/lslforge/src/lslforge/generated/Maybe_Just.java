package lslforge.generated;
import com.thoughtworks.xstream.XStream;
import java.util.LinkedList;
public class Maybe_Just<E1> extends Maybe<E1>{
    public E1 el1;
    public static void init(XStream xstream) {
        xstream.alias("Maybe_Just",Maybe_Just.class); //$NON-NLS-1$
    }
}
