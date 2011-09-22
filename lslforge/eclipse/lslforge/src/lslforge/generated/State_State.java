package lslforge.generated;
import com.thoughtworks.xstream.XStream;
import java.util.LinkedList;
public class State_State extends State{
    public Ctx<String> el1;
    public LinkedList<Ctx<Handler>> el2;
    public static void init(XStream xstream) {
        xstream.alias("State_State",State_State.class); //$NON-NLS-1$
    }
}
