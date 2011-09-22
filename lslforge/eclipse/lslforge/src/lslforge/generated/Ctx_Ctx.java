package lslforge.generated;
import com.thoughtworks.xstream.XStream;
public class Ctx_Ctx<E1> extends Ctx<E1>{
    public Maybe<SourceContext> srcCtx;
    public E1 ctxItem;
    public static void init(XStream xstream) {
        xstream.alias("Ctx_Ctx",Ctx_Ctx.class); //$NON-NLS-1$
    }
}
