package lslforge.outline.items;

import lslforge.LSLForgePlugin;
import org.eclipse.swt.graphics.Image;

public class Variable extends OutlineItem
{
	private static final Image IMAGE = LSLForgePlugin.createImage("icons/var.gif"); //$NON-NLS-1$
	private final OutlineItem.DataType type;

	public Variable(String name, DataType type) {
		super(name, IMAGE, 0, 0);
		this.type = type;
	}
	
	public Variable(String name, DataType type, int start, int end) {
		super(name, IMAGE, start, end);
		this.type = type;
	}
	
	public DataType getType() {
		return type;
	}
}
