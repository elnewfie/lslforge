package lslforge.outline.items;

import lslforge.LSLForgePlugin;
import org.eclipse.swt.graphics.Image;

public class Handler extends OutlineItem
{
	private static final Image IMAGE = LSLForgePlugin.createImage("icons/handler.gif"); //$NON-NLS-1$

	public Handler(String name) {
		super(name, IMAGE, 0, 0);
	}
	
	public Handler(String name, int start, int end) {
		super(name, IMAGE, start, end);
	}
}
