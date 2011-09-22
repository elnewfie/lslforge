package lslforge.preferences;

import java.io.IOException;

import lslforge.LslForgePlugin;
import lslforge.LslProjectNature;
import lslforge.util.Util;

import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.ResourcesPlugin;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.jface.preference.BooleanFieldEditor;
import org.eclipse.jface.preference.FieldEditorPreferencePage;
import org.eclipse.jface.preference.FileFieldEditor;
import org.eclipse.jface.preference.IPreferenceStore;
import org.eclipse.ui.IWorkbench;
import org.eclipse.ui.IWorkbenchPreferencePage;

public class LslForgePreferencePage extends FieldEditorPreferencePage implements
        IWorkbenchPreferencePage {

    private static final String LSLFORGE_PREFERENCE_PAGE_ENABLE_OPTIMIZATIONS = "LslForgePreferencePage.ENABLE_OPTIMIZATIONS"; //$NON-NLS-1$
    private static final String LSLFORGE_EXECUTABLE_PATH = "LslForgePreferencePage.LSLForgeExecutablePath"; //$NON-NLS-1$

    public LslForgePreferencePage() throws IOException {
        setPreferenceStore(LslForgePlugin.getDefault().getPreferenceStore());
    }

    public void init(IWorkbench workbench) {
    }

    protected IPreferenceStore doGetPreferenceStore() {
        return LslForgePlugin.getDefault().getPreferenceStore();
    }

    protected void createFieldEditors() {
        addField(new FileFieldEditor(LslForgePlugin.LSLFORGE_NATIVE_PATH,
                Messages.getString(LSLFORGE_EXECUTABLE_PATH), getFieldEditorParent())); 
        addField(new BooleanFieldEditor(LslProjectNature.OPTIMIZE,
                Messages.getString(LSLFORGE_PREFERENCE_PAGE_ENABLE_OPTIMIZATIONS), getFieldEditorParent()));
    }

    public boolean performOk() {
        if (super.performOk()) {
            IProject[] p = ResourcesPlugin.getWorkspace().getRoot().getProjects();
            for (int i = 0; i < p.length; i++) {
                try {
                    LslProjectNature nature = (LslProjectNature) p[i].getNature(LslProjectNature.ID);
                    if (nature != null) nature.scheduleBuild(true,null,null);
                } catch (CoreException e) {
                    Util.error(e, "problem determining project nature"); //$NON-NLS-1$
                }
            }
            return true;
        } else return false;
    }
}
