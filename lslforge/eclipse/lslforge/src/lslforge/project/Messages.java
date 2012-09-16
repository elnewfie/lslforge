package lslforge.project;

import org.eclipse.osgi.util.NLS;

public class Messages extends NLS
{
	private static final String BUNDLE_NAME = "lslforge.project.messages"; //$NON-NLS-1$
	public static String AddSupportHandler_AddSupport;
	public static String AddSupportHandler_AddSupportErr;
	public static String AddSupportHandler_AddSupportReply;
	public static String ProjectContributionItem_addSupport;
	public static String ProjectContributionItem_removeSupport;
	public static String RemoveSupportHandler_removeSupport;
	public static String RemoveSupportHandler_removeSupportErr;
	public static String RemoveSupportHandler_removeSupportReply;
	static {
		// initialize resource bundle
		NLS.initializeMessages(BUNDLE_NAME, Messages.class);
	}

	private Messages() {
	}
}
