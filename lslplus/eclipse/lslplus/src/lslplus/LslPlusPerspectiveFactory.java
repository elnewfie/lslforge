package lslplus;

import org.eclipse.ui.IFolderLayout;
import org.eclipse.ui.IPageLayout;
import org.eclipse.ui.IPerspectiveFactory;

/**
 * A factory for setting up the LSL Plus eclipse perspective.  The LSL Plus
 * eclipse perspective includes editors for editing LSL code and tests, views
 * for running LSL tests, and shortcuts for launching tests and wizards for 
 * creating LSL related files.
 * 
 * @author rgreayer
 *
 */
public class LslPlusPerspectiveFactory implements IPerspectiveFactory {
    public static final String PERSPECTIVE_ID = "lslplus.LslPlusPerspective"; //$NON-NLS-1$
	public void createInitialLayout(IPageLayout layout) {
		String editorArea = layout.getEditorArea();
		
		IFolderLayout left = layout.createFolder("left", IPageLayout.LEFT, 0.25f, editorArea); //$NON-NLS-1$
		left.addView(IPageLayout.ID_RES_NAV);
		
		IFolderLayout bottom = layout.createFolder("bottom", IPageLayout.BOTTOM, 0.75f, editorArea); //$NON-NLS-1$
		bottom.addView(IPageLayout.ID_PROBLEM_VIEW);
		
		IFolderLayout right = layout.createFolder("right", IPageLayout.RIGHT, 0.80f, editorArea); //$NON-NLS-1$
		right.addView(IPageLayout.ID_OUTLINE);
		
		layout.addActionSet("org.eclipse.debug.ui.launchActionSet"); //$NON-NLS-1$
		layout.addNewWizardShortcut("lslplus.LslPlusProjectWizard"); //$NON-NLS-1$
		layout.addNewWizardShortcut("lslplus.newTestWizard"); //$NON-NLS-1$
        layout.addNewWizardShortcut("lslplus.module_wizard"); //$NON-NLS-1$
        layout.addNewWizardShortcut("lslplus.script_wizard"); //$NON-NLS-1$
        layout.addNewWizardShortcut("lslplus.newSimProjectWizard"); //$NON-NLS-1$
        layout.addNewWizardShortcut("lslplus.newSimEventHandlerWizard"); //$NON-NLS-1$
        layout.addNewWizardShortcut("lslplus.newAvEventHandlerWizard"); //$NON-NLS-1$
        layout.addNewWizardShortcut("lslplus.HttpRpcExample"); //$NON-NLS-1$
        layout.addNewWizardShortcut("lslplus.DialogExample"); //$NON-NLS-1$
        layout.addNewWizardShortcut("lslplus.HttpRequestExample"); //$NON-NLS-1$
	}

}
