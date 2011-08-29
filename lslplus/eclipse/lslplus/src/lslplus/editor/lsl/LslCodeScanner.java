package lslplus.editor.lsl;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;

import lslplus.util.ColorProviderListener;
import lslplus.util.LslColorProvider;
import lslplus.util.LslWhitespaceDetector;
import lslplus.util.LslWordDetector;

import org.eclipse.jface.preference.IPreferenceStore;
import org.eclipse.jface.text.TextAttribute;
import org.eclipse.jface.text.rules.EndOfLineRule;
import org.eclipse.jface.text.rules.IRule;
import org.eclipse.jface.text.rules.IToken;
import org.eclipse.jface.text.rules.RuleBasedScanner;
import org.eclipse.jface.text.rules.SingleLineRule;
import org.eclipse.jface.text.rules.Token;
import org.eclipse.jface.text.rules.WhitespaceRule;
import org.eclipse.jface.text.rules.WordRule;
import org.eclipse.jface.util.IPropertyChangeListener;
import org.eclipse.jface.util.PropertyChangeEvent;
import org.eclipse.swt.SWT;

/**
 *  An LSL Plus rule-based code scanner.
 */
public class LslCodeScanner extends RuleBasedScanner implements ColorProviderListener, IPropertyChangeListener {
    public static final int DEFAULT_STYLE_INDEX = 0;
    public static final int HANDLER_STYLE_INDEX = 1;
    public static final int KEYWORD_STYLE_INDEX = 2;
    public static final int MULTILINE_COMMENT_STYLE_INDEX = 3;
    public static final int SINGLE_LINE_COMMENT_STYLE_INDEX = 4;
    public static final int PREDEF_CONST_STYLE_INDEX = 5;
    public static final int PREDEF_FUNC_STYLE_INDEX = 6;
    public static final int STRING_STYLE_INDEX = 7;
    public static final int TYPE_STYLE_INDEX = 8;
    
    public static final String DEFAULT_STYLE = "default"; //$NON-NLS-1$
    public static final String HANDLER_STYLE = "handler"; //$NON-NLS-1$
    public static final String KEYWORD_STYLE = "keyword"; //$NON-NLS-1$
    public static final String MULTILINE_COMMENT_STYLE = "multi_line_comment"; //$NON-NLS-1$
    public static final String SINGLE_LINE_COMMENT_STYLE = "single_line_comment"; //$NON-NLS-1$
    public static final String PREDEF_CONST_STYLE = "predef_cont"; //$NON-NLS-1$
    public static final String PREDEF_FUNC_STYLE = "predef_func"; //$NON-NLS-1$
    public static final String STRING_STYLE = "string"; //$NON-NLS-1$
    public static final String TYPE_STYLE = "type"; //$NON-NLS-1$
    
    public static final String[] STYLE_KINDS = 
    { DEFAULT_STYLE, HANDLER_STYLE, KEYWORD_STYLE, MULTILINE_COMMENT_STYLE, 
      SINGLE_LINE_COMMENT_STYLE, PREDEF_CONST_STYLE, PREDEF_FUNC_STYLE,
      STRING_STYLE, TYPE_STYLE
    };
    
    public static final String[] KIND_NAMES = {
        "Default", //$NON-NLS-1$ TODO
        "Handler", //$NON-NLS-1$ TODO
        "Keyword", //$NON-NLS-1$ TODO
        "Multi-line comment", //$NON-NLS-1$ TODO
        "Single line comment", //$NON-NLS-1$ TODO
        "Predefined constant", //$NON-NLS-1$ TODO
        "Predefined function", //$NON-NLS-1$ TODO
        "String", //$NON-NLS-1$ TODO
        "Type" //$NON-NLS-1$ TODO
    };
    public static final String BOLD_STYLE = "bold"; //$NON-NLS-1$
    public static final String ITALIC_STYLE = "italic"; //$NON-NLS-1$
    public static final String UL_STYLE = "underline"; //$NON-NLS-1$
    
    public static final String[] STYLES = { BOLD_STYLE, ITALIC_STYLE, UL_STYLE };
    
    private static final HashMap<String,Integer> STYLE_TO_CONST = new HashMap<String,Integer>();
    
    private static String[] lslPlusKeywords = {
            "$module", "jump", "default", "do", "else", "for", "if", "$import", "state", "return", "while" }; //$NON-NLS-11$ //$NON-NLS-10$ //$NON-NLS-9$ //$NON-NLS-8$ //$NON-NLS-7$ //$NON-NLS-6$ //$NON-NLS-5$ //$NON-NLS-4$ //$NON-NLS-3$ //$NON-NLS-2$ //$NON-NLS-1$

    private static String[] lslPlusTypes = {
            "integer", "string", "float", "list", "vector", "rotation", "key", "quaternion" }; //$NON-NLS-1$ //$NON-NLS-5$ //$NON-NLS-7$ //$NON-NLS-8$ //$NON-NLS-6$ //$NON-NLS-3$ //$NON-NLS-4$ //$NON-NLS-2$

    static {
        STYLE_TO_CONST.put(BOLD_STYLE, new Integer(SWT.BOLD));
        STYLE_TO_CONST.put(ITALIC_STYLE, new Integer(SWT.ITALIC));
        STYLE_TO_CONST.put(UL_STYLE, new Integer(SWT.UNDERLINE_SINGLE));
    }
    
    private String[] handlerNames;
    private String[] predefFuncNames;
    private String[] predefConstNames;

    private HashSet<ScannerChangeListener> listeners = new HashSet<ScannerChangeListener>();

    private IPreferenceStore store;

    private LslColorProvider provider;
    
    private static int computeBit(boolean pred, int bitConst) {
        return pred ? bitConst : 0;
    }

    private static String mkId(String s0, String s1) {
        return s0 + '.' + s1;
    }
    
    /**
     * Creates an LslPlus code scanner.
     * 
     * @param provider the color provider
     * @param handlerNames
     * @param predefFuncNames
     * @param predefConstNames
     * @param store 
     */
    public LslCodeScanner(LslColorProvider provider, String[] handlerNames,
            String[] predefFuncNames, String[] predefConstNames, IPreferenceStore store) {
        this.provider = provider;
        this.handlerNames = handlerNames;
        this.predefFuncNames = predefFuncNames;
        this.predefConstNames = predefConstNames;
        this.store = store;
        provider.addListener(this);
        store.addPropertyChangeListener(this);
        
        for (int i = 0; i < STYLE_KINDS.length; i++) {
            for (int j = 0; j < STYLES.length; j++) {
                store.setDefault(STYLE_KINDS[i] + '.' + STYLES[j], false);
            }
        }
        
        store.setDefault(KEYWORD_STYLE + '.' + BOLD_STYLE, true);
        store.setDefault(HANDLER_STYLE + '.' + BOLD_STYLE, true);
        store.setDefault(TYPE_STYLE + '.' + BOLD_STYLE, true);
        initRules(provider, handlerNames, predefFuncNames, predefConstNames);
    }

    private int computeBits(String s0) {
        int x = 0;
        for (int i = 0; i < STYLES.length; i++) {
            x |= computeBit(store.getBoolean(mkId(s0,STYLES[i])), STYLE_TO_CONST.get(STYLES[i]).intValue());
        }
        return x;
    }
    
    private void initRules(LslColorProvider provider, String[] handlerNames,
            String[] predefFuncNames, String[] predefConstNames) {
        IToken keyword = new Token(new TextAttribute(provider.getColor(LslColorProvider.KEYWORD_COLOR),
                null, computeBits(KEYWORD_STYLE)));
        IToken type = new Token(new TextAttribute(provider.getColor(LslColorProvider.TYPE_COLOR), null,
                computeBits(TYPE_STYLE)));
        IToken string = new Token(new TextAttribute(provider.getColor(LslColorProvider.STRING_COLOR), null,
                computeBits(STRING_STYLE)));
        IToken comment = new Token(new TextAttribute(provider
                .getColor(LslColorProvider.SINGLE_LINE_COMMENT_COLOR), null,
                computeBits(SINGLE_LINE_COMMENT_STYLE)));
        IToken other = new Token(new TextAttribute(provider.getColor(LslColorProvider.DEFAULT_COLOR), null,
                computeBits(DEFAULT_STYLE)));
        IToken handler = new Token(new TextAttribute(provider.getColor(LslColorProvider.HANDLER_COLOR),
                null, computeBits(HANDLER_STYLE)));
        IToken predefFunc = new Token(new TextAttribute(provider
                .getColor(LslColorProvider.PREDEF_FUNC_COLOR), null, computeBits(PREDEF_FUNC_STYLE)));
        IToken predefConst = new Token(new TextAttribute(provider
                .getColor(LslColorProvider.PREDEF_CONST_COLOR), null, computeBits(PREDEF_CONST_STYLE)));
        List<IRule> rules = new ArrayList<IRule>();

        // Add rule for single line comments.
        rules.add(new EndOfLineRule("//", comment)); //$NON-NLS-1$

        // Add rule for strings and character constants.
        rules.add(new SingleLineRule("\"", "\"", string, '\\')); //$NON-NLS-2$ //$NON-NLS-1$
        rules.add(new SingleLineRule("'", "'", string, '\\')); //$NON-NLS-2$ //$NON-NLS-1$

        // Add generic whitespace rule.
        rules.add(new WhitespaceRule(new LslWhitespaceDetector()));

        // Add word rule for keywords, types, handlers, constants and functions.
        WordRule wordRule = new WordRule(new LslWordDetector(), other);
        addWordsToRule(wordRule, lslPlusKeywords, keyword);
        addWordsToRule(wordRule, lslPlusTypes, type);
        addWordsToRule(wordRule, handlerNames, handler);
        addWordsToRule(wordRule, predefConstNames, predefConst);
        addWordsToRule(wordRule, predefFuncNames, predefFunc);
        rules.add(wordRule);

        IRule[] result = new IRule[rules.size()];
        rules.toArray(result);
        setRules(result);
    }

    private static void addWordsToRule(WordRule rule, String[] words, IToken t) {
        for (int i = 0; i < words.length; i++) {
            rule.addWord(words[i], t);
        }
    }

    public synchronized void onColorChange() {
        onChange();
    }

    private void onChange() {
        initRules(provider, handlerNames, predefFuncNames, predefConstNames);

        Iterator<ScannerChangeListener> i = listeners.iterator();
        
        while (i.hasNext()) {
            i.next().scannerChanged();
        }
    }
    
    public synchronized void addListener(ScannerChangeListener listener) {
        this.listeners.add(listener);
        
    }
    
    public synchronized void removeListener(ScannerChangeListener listener) {
        this.listeners.remove(listener);
    }

    public void propertyChange(PropertyChangeEvent event) {
        onChange();
    }
}
