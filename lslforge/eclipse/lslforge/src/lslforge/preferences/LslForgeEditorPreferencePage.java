package lslforge.preferences;

import java.io.IOException;
import java.text.MessageFormat;

import lslforge.LSLForgePlugin;
import lslforge.editor.lsl.LSLCodeScanner;

import org.eclipse.jface.preference.BooleanFieldEditor;
import org.eclipse.jface.preference.ColorFieldEditor;
import org.eclipse.jface.preference.FieldEditorPreferencePage;
import org.eclipse.jface.preference.IPreferenceStore;
import org.eclipse.ui.IWorkbench;
import org.eclipse.ui.IWorkbenchPreferencePage;

public class LSLForgeEditorPreferencePage extends FieldEditorPreferencePage implements
        IWorkbenchPreferencePage {

    public LSLForgeEditorPreferencePage() throws IOException {
        super(FieldEditorPreferencePage.GRID);
        setPreferenceStore(LSLForgePlugin.getDefault().getPreferenceStore());
    }

    public void init(IWorkbench workbench) {
    }

    @Override
	protected IPreferenceStore doGetPreferenceStore() {
        return LSLForgePlugin.getDefault().getPreferenceStore();
    }

    @Override
	protected void createFieldEditors() {
//        addField(new ColorFieldEditor(LSLColorProvider.DEFAULT_COLOR, "Default color", getFieldEditorParent()));
//        addField(new ColorFieldEditor(LSLColorProvider.HANDLER_COLOR, "Handler color", getFieldEditorParent()));
//        addField(new ColorFieldEditor(LSLColorProvider.KEYWORD_COLOR, "Keyword color", getFieldEditorParent()));
//        addField(new ColorFieldEditor(LSLColorProvider.MULTI_LINE_COMMENT_COLOR, "Multi-line comment color", getFieldEditorParent()));
//        addField(new ColorFieldEditor(LSLColorProvider.PREDEF_CONST_COLOR, "Predefined constant color", getFieldEditorParent()));
//        addField(new ColorFieldEditor(LSLColorProvider.PREDEF_FUNC_COLOR, "Predefined (ll) function color", getFieldEditorParent()));
//        addField(new ColorFieldEditor(LSLColorProvider.SINGLE_LINE_COMMENT_COLOR, "Single line comment color", getFieldEditorParent()));
//        addField(new ColorFieldEditor(LSLColorProvider.STRING_COLOR, "String color", getFieldEditorParent()));
//        addField(new ColorFieldEditor(LSLColorProvider.TYPE_COLOR, "Type color", getFieldEditorParent()));

        for (int i = 0; i < LSLCodeScanner.STYLE_KINDS.length; i++) {
            createFieldEditors(i);
        }
    }

    private static String fmt(String pattern, String arg) {
        return MessageFormat.format(pattern,new Object[] { arg });
    }
    private void createFieldEditors(int kind) {
        String kname = LSLCodeScanner.KIND_NAMES[kind];
        String kindId = LSLCodeScanner.STYLE_KINDS[kind];
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
