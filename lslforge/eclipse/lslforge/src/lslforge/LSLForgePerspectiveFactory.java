package lslforge;

import org.eclipse.ui.IFolderLayout;
import org.eclipse.ui.IPageLayout;
import org.eclipse.ui.IPerspectiveFactory;

/**
 * A factory for setting up the LSLForge eclipse perspective.  The LSLForge
 * eclipse perspective includes editors for editing LSL code and tests, views
 * for running LSL tests, and shortcuts for launching tests and wizards for 
 * creating LSL related files.
 * 
 * @author rgreayer
 *
 */
public class LSLForgePerspectiveFactory implements IPerspectiveFactory {
    public static final String PERSPECTIVE_ID = "lslforge.LSLForgePerspective"; //$NON-NLS-1$
	public void createInitialLayout(IPageLayout layout) {
		String editorArea = layout.getEditorArea();
		
		IFolderLayout left = layout.createFolder("left", IPageLayout.LEFT, 0.25f, editorArea); //$NON-NLS-1$
		left.addView(IPageLayout.ID_PROJECT_EXPLORER);
		
		IFolderLayout bottom = layout.createFolder("bottom", IPageLayout.BOTTOM, 0.75f, editorArea); //$NON-NLS-1$
		bottom.addView(IPageLayout.ID_PROBLEM_VIEW);
		
		IFolderLayout right = layout.createFolder("right", IPageLayout.RIGHT, 0.80f, editorArea); //$NON-NLS-1$
		right.addView(IPageLayout.ID_OUTLINE);
		
		layout.addActionSet("org.eclipse.debug.ui.launchActionSet"); //$NON-NLS-1$
		layout.addNewWizardShortcut("lslforge.LSLForgeProjectWizard"); //$NON-NLS-1$
		layout.addNewWizardShortcut("lslforge.newTestWizard"); //$NON-NLS-1$
        layout.addNewWizardShortcut("lslforge.module_wizard"); //$NON-NLS-1$
        layout.addNewWizardShortcut("lslforge.script_wizard"); //$NON-NLS-1$
        layout.addNewWizardShortcut("lslforge.newSimProjectWizard"); //$NON-NLS-1$
        layout.addNewWizardShortcut("lslforge.newSimEventHandlerWizard"); //$NON-NLS-1$
        layout.addNewWizardShortcut("lslforge.newAvEventHandlerWizard"); //$NON-NLS-1$
        layout.addNewWizardShortcut("lslforge.HttpRpcExample"); //$NON-NLS-1$
        layout.addNewWizardShortcut("lslforge.DialogExample"); //$NON-NLS-1$
        layout.addNewWizardShortcut("lslforge.HttpRequestExample"); //$NON-NLS-1$
	}

}
