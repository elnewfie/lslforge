package lslforge;

import org.eclipse.osgi.util.NLS;

public class Messages extends NLS {
	private static final String BUNDLE_NAME = "lslforge.messages"; //$NON-NLS-1$
	public static String LSLForgePlugin_COULD_NOT_DESERIALIZE_META_DATA;
    public static String LSLForgePlugin_NO_META_DATA;
    public static String LSLForgeProjectWizard_CREATE_LSL_PLUS_PROJECT;
	public static String LSLForgeProjectWizardPage_CreateAnLSLForgeProject;
    public static String LSLForgeProjectWizardPage_ENTER_PROJECT_NAME;
    public static String LSLForgeProjectWizardPage_FOLDER_EXISTS_IN_WORKSPACE;
    public static String LSLForgeProjectWizardPage_PROJECT_ALREADY_EXISTS;
	public static String LSLForgeProjectWizardPage_ProjectName;
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
