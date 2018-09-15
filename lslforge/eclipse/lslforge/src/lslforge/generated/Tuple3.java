package lslforge.generated;
import com.thoughtworks.xstream.XStream;
public class Tuple3<E1,E2,E3> {
    public E1 el1;
    public E2 el2;
    public E3 el3;
    public static void init(XStream xstream) {
         xstream.alias("Tuple3",Tuple3.class); //$NON-NLS-1$
    }
}
