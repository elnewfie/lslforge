package lslforge.generated;
import com.thoughtworks.xstream.XStream;
import java.util.LinkedList;
public class Either_Right<E1,E2> extends Either<E1,E2>{
    public E2 el1;
    public static void init(XStream xstream) {
        xstream.alias("Either_Right",Either_Right.class); //$NON-NLS-1$
    }
}
