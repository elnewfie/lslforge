package lslforge.generated;
import com.thoughtworks.xstream.XStream;
public class Expr_Get extends Expr{
    public Tuple2<Ctx<String>,Component> el1;
    public static void init(XStream xstream) {
        xstream.alias("Expr_Get",Expr_Get.class); //$NON-NLS-1$
    }
}
