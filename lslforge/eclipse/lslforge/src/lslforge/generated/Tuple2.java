package lslforge.generated;
import com.thoughtworks.xstream.XStream;
public class Tuple2<E1,E2> {
    public E1 el1;
    public E2 el2;
    public static void init(XStream xstream) {
         xstream.alias("Tuple2",Tuple2.class); //$NON-NLS-1$
    }
}
