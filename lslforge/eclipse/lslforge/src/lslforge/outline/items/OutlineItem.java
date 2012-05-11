package lslforge.outline.items;

import java.util.ArrayList;
import java.util.List;
import org.eclipse.swt.graphics.Image;

public class OutlineItem
{
	private OutlineItem parent = null;
	private List<OutlineItem> children = null;
	private final Image image;
	private TextPosition textPosition;
	private final String name;

	public OutlineItem(String name, Image image, int start, int end) {
		this.name = name;
		this.image = image;
		this.textPosition = new TextPosition(start, end);
	}

	/**
	 * Attach this outline item object to a parent node in the tree.  If this object already has a parent,
	 * notify the old parent that it is moving.
	 * @param parent The parent node to become a child of.
	 */
	void setParent(OutlineItem parent) {
		if(this.parent != null) parent.removeChild(this);	//Detach from old parent
		this.parent = parent;
	}
	
	void removeChild(OutlineItem child) {
		if(children == null) return;
		
		if(children.contains(child)) children.remove(child);
	}

	public void addChild(OutlineItem child) {
		if (children == null) children = new ArrayList<OutlineItem>();
		children.add(child);
		child.setParent(this);
	}

	public List<OutlineItem> getChildren() {
		return children;
	}
	
	public boolean hasChildren() {
		if(children == null) return false;
		
		return children.size() > 0;
	}

	public Image getImage() {
		return image;
	}
	
	public OutlineItem getParent() {
		return parent;
	}

	public String getName() {
		return name;
	}
	
	public TextPosition getTextPosition() {
		return textPosition;
	}
	
	public void setTextPosition(TextPosition newPosition) {
		this.textPosition = newPosition;
	}
	
	public enum DataType {
		FLOAT, INTEGER, KEY, LIST, ROTATION, STRING, VECTOR, VOID
	}
}
