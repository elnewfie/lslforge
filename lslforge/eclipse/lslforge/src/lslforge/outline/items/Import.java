package lslforge.outline.items;

import lslforge.LSLForgePlugin;
import org.eclipse.swt.graphics.Image;

public class Import extends OutlineItem
{
	private static final Image IMAGE = LSLForgePlugin.createImage("icons/import.gif"); //$NON-NLS-1$

	public Import(String name) {
		super(name, IMAGE, 0, 0);
	}
	
	public Import(String name, int start, int end) {
		super(name, IMAGE, start, end);
	}
}
