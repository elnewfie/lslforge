package lslplus.preferences;

import java.io.IOException;
import java.text.MessageFormat;

import lslplus.LslPlusPlugin;
import lslplus.editor.lsl.LslCodeScanner;

import org.eclipse.jface.preference.BooleanFieldEditor;
import org.eclipse.jface.preference.ColorFieldEditor;
import org.eclipse.jface.preference.FieldEditorPreferencePage;
import org.eclipse.jface.preference.IPreferenceStore;
import org.eclipse.ui.IWorkbench;
import org.eclipse.ui.IWorkbenchPreferencePage;

public class LslPlusEditorPreferencePage extends FieldEditorPreferencePage implements
        IWorkbenchPreferencePage {

    public LslPlusEditorPreferencePage() throws IOException {
        super(FieldEditorPreferencePage.GRID);
        setPreferenceStore(LslPlusPlugin.getDefault().getPreferenceStore());
    }

    public void init(IWorkbench workbench) {
    }

    protected IPreferenceStore doGetPreferenceStore() {
        return LslPlusPlugin.getDefault().getPreferenceStore();
    }

    protected void createFieldEditors() {
//        addField(new ColorFieldEditor(LslColorProvider.DEFAULT_COLOR, "Default color", getFieldEditorParent()));
//        addField(new ColorFieldEditor(LslColorProvider.HANDLER_COLOR, "Handler color", getFieldEditorParent()));
//        addField(new ColorFieldEditor(LslColorProvider.KEYWORD_COLOR, "Keyword color", getFieldEditorParent()));
//        addField(new ColorFieldEditor(LslColorProvider.MULTI_LINE_COMMENT_COLOR, "Multi-line comment color", getFieldEditorParent()));
//        addField(new ColorFieldEditor(LslColorProvider.PREDEF_CONST_COLOR, "Predefined constant color", getFieldEditorParent()));
//        addField(new ColorFieldEditor(LslColorProvider.PREDEF_FUNC_COLOR, "Predefined (ll) function color", getFieldEditorParent()));
//        addField(new ColorFieldEditor(LslColorProvider.SINGLE_LINE_COMMENT_COLOR, "Single line comment color", getFieldEditorParent()));
//        addField(new ColorFieldEditor(LslColorProvider.STRING_COLOR, "String color", getFieldEditorParent()));
//        addField(new ColorFieldEditor(LslColorProvider.TYPE_COLOR, "Type color", getFieldEditorParent()));

        for (int i = 0; i < LslCodeScanner.STYLE_KINDS.length; i++) {
            createFieldEditors(i);
        }
    }

    private static String fmt(String pattern, String arg) {
        return MessageFormat.format(pattern,new Object[] { arg });
    }
    private void createFieldEditors(int kind) {
        String kname = LslCodeScanner.KIND_NAMES[kind];
        String kindId = LslCodeScanner.STYLE_KINDS[kind];
        addField(new ColorFieldEditor(kindId + '.' + "color",  //$NON-NLS-1$
                fmt("{0} color", kname),getFieldEditorParent())); //$NON-NLS-1$
        addField(new BooleanFieldEditor(kindId + '.' + "bold",//$NON-NLS-1$
                fmt("{0} bold?", kname), getFieldEditorParent()));//$NON-NLS-1$
        addField(new BooleanFieldEditor(kindId + '.' + "italic",//$NON-NLS-1$
                fmt("{0} italic?", kname), getFieldEditorParent()));//$NON-NLS-1$
        addField(new BooleanFieldEditor(kindId + '.' + "underline",//$NON-NLS-1$
                fmt("{0} underline?", kname), getFieldEditorParent()));//$NON-NLS-1$
    }
}
