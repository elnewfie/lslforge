package lslforge.util;

import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;

import org.eclipse.jface.preference.IPreferenceStore;
import org.eclipse.jface.preference.PreferenceConverter;
import org.eclipse.jface.util.IPropertyChangeListener;
import org.eclipse.jface.util.PropertyChangeEvent;
import org.eclipse.swt.graphics.Color;
import org.eclipse.swt.graphics.RGB;
import org.eclipse.swt.widgets.Display;

/**
 * Manager for colors used in the LSLForge Editor.  This provider creates colors lazily as needed
 * and then caches them.
 */
public class LSLColorProvider implements IPropertyChangeListener {

    public static final String DEFAULT_COLOR             = "default.color"; //$NON-NLS-1$
    public static final String HANDLER_COLOR             = "handler.color"; //$NON-NLS-1$
    public static final String KEYWORD_COLOR             = "keyword.color"; //$NON-NLS-1$
    public static final String MULTI_LINE_COMMENT_COLOR  = "multi_line_comment.color"; //$NON-NLS-1$
    public static final String PREDEF_CONST_COLOR        = "predef_const.color"; //$NON-NLS-1$
    public static final String PREDEF_FUNC_COLOR         = "predef_func.color"; //$NON-NLS-1$
    public static final String SINGLE_LINE_COMMENT_COLOR = "single_line_comment.color"; //$NON-NLS-1$
    public static final String STRING_COLOR              = "string.color"; //$NON-NLS-1$
    public static final String TYPE_COLOR                = "type.color"; //$NON-NLS-1$
    
    private static final RGB DEFAULT             = new RGB(0, 0, 0);
    private static final RGB HANDLER             = new RGB(0, 0, 128);
    private static final RGB KEYWORD             = new RGB(100, 25, 50);
    private static final RGB MULTI_LINE_COMMENT  = new RGB(64, 64, 128);
    private static final RGB PREDEF_CONST        = new RGB(0, 128, 0);
    private static final RGB PREDEF_FUNC         = new RGB(128, 0, 0);
    private static final RGB SINGLE_LINE_COMMENT = new RGB(64, 64, 128);
    private static final RGB STRING              = new RGB(0, 128, 128);
    private static final RGB TYPE                = new RGB(0, 0, 128);

    protected Map<RGB, Color> colorTable = new HashMap<RGB,Color>(10);
    private HashSet<ColorProviderListener> listeners = new HashSet<ColorProviderListener>();
    private IPreferenceStore store;

    public LSLColorProvider(IPreferenceStore store) {
        PreferenceConverter.setDefault(store, DEFAULT_COLOR, DEFAULT);
        PreferenceConverter.setDefault(store, HANDLER_COLOR, HANDLER);
        PreferenceConverter.setDefault(store, KEYWORD_COLOR, KEYWORD);
        PreferenceConverter.setDefault(store, MULTI_LINE_COMMENT_COLOR, MULTI_LINE_COMMENT);
        PreferenceConverter.setDefault(store, PREDEF_CONST_COLOR, PREDEF_CONST);
        PreferenceConverter.setDefault(store, PREDEF_FUNC_COLOR, PREDEF_FUNC);
        PreferenceConverter.setDefault(store, SINGLE_LINE_COMMENT_COLOR, SINGLE_LINE_COMMENT);
        PreferenceConverter.setDefault(store, STRING_COLOR, STRING);
        PreferenceConverter.setDefault(store, TYPE_COLOR, TYPE);
        this.store = store;
        store.addPropertyChangeListener(this);
        
    }
    /**
     * Release all of the color resources held onto by the receiver.
     */
    public void dispose() {
    	for (Color c : colorTable.values()) c.dispose();
    }

    /**
     * Return the color that is stored in the color table under the given RGB
     * value.
     * 
     * @param rgb the RGB value
     * @return the color stored in the color table for the given RGB value
     */
    public Color getColor(RGB rgb) {
        Color color = colorTable.get(rgb);
        if (color == null) {
            color = new Color(Display.getCurrent(), rgb);
            colorTable.put(rgb, color);
        }
        return color;
    }
    
    public Color getColor(String colorId) {
        RGB rgb = PreferenceConverter.getColor(store, colorId);
        if (rgb == null) rgb = DEFAULT;
        Color color = colorTable.get(rgb);
        if (color == null)  {
            color = new Color(Display.getCurrent(), rgb);
            colorTable.put(rgb, color);
        }
        return color;
    }
    
    public synchronized void addListener(ColorProviderListener listener) {
        listeners.add(listener);
    }
    
    public synchronized void removeListener(ColorProviderListener listener) {
        listeners.remove(listener);
    }
    
    public void propertyChange(PropertyChangeEvent event) {
        for (ColorProviderListener p : listeners) p.onColorChange();
    }
}
