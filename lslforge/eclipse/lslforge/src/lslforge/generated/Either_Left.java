package lslforge.generated;
import com.thoughtworks.xstream.XStream;
public class Either_Left<E1,E2> extends Either<E1,E2>{
    public E1 el1;
    public static void init(XStream xstream) {
        xstream.alias("Either_Left",Either_Left.class); //$NON-NLS-1$
    }
}
