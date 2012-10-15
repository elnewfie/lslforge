package lslforge.outline;

import org.eclipse.osgi.util.NLS;

public class Messages extends NLS
{
	private static final String BUNDLE_NAME = "lslforge.outline.messages"; //$NON-NLS-1$
	public static String LSLForgeOutlinePage_OutlineUnsupported;
	static {
		// initialize resource bundle
		NLS.initializeMessages(BUNDLE_NAME, Messages.class);
	}

	private Messages() {
	}
}
