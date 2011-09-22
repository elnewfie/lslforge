package lslforge;

import org.eclipse.osgi.util.NLS;

public class Messages extends NLS {
	private static final String BUNDLE_NAME = "lslforge.messages"; //$NON-NLS-1$
	public static String LslForgePlugin_COULD_NOT_DESERIALIZE_META_DATA;
    public static String LslForgePlugin_NO_META_DATA;
    public static String LslForgeProjectWizard_CREATE_LSL_PLUS_PROJECT;
	public static String LslForgeProjectWizardPage_CreateAnLSLForgeProject;
    public static String LslForgeProjectWizardPage_ENTER_PROJECT_NAME;
    public static String LslForgeProjectWizardPage_FOLDER_EXISTS_IN_WORKSPACE;
    public static String LslForgeProjectWizardPage_PROJECT_ALREADY_EXISTS;
	public static String LslForgeProjectWizardPage_ProjectName;
    public static String ProjectNature_MARK_DERIVED_COMPLETE;
    public static String ProjectNature_OK;
    public static String ProjectNature_REFRESH;
    public static String ProjectNature_REFRESHED_OK;
    public static String SimManager_Cant_Get_Simulator_Information;
    public static String SimManager_OK;
	static {
		// initialize resource bundle
		NLS.initializeMessages(BUNDLE_NAME, Messages.class);
	}

	private Messages() {
	}
}
