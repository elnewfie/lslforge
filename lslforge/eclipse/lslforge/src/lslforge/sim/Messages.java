package lslforge.sim;

import org.eclipse.osgi.util.NLS;

public class Messages extends NLS {
	private static final String BUNDLE_NAME = "lslforge.sim.messages"; //$NON-NLS-1$
	public static String SimProjectNodes_BASE_PERM;
	public static String SimProjectNodes_EVERYBODY_PERM;
	public static String SimProjectNodes_GROUP_PERM;
	public static String SimProjectNodes_NEXT_OWNER_PERM;
	public static String SimProjectNodes_OWNER_PERM;
	static {
		// initialize resource bundle
		NLS.initializeMessages(BUNDLE_NAME, Messages.class);
	}

	private Messages() {
	}
}
