package lslforge.outline.items;

import java.util.ArrayList;
import java.util.List;
import lslforge.LSLForgePlugin;
import org.eclipse.swt.graphics.Image;

public class Function extends OutlineItem
{
	private static final Image IMAGE = LSLForgePlugin.createImage("icons/function.gif"); //$NON-NLS-1$
	private final OutlineItem.DataType type;
	
	private ArrayList<Parameter> parameters;

	public Function(String name, DataType type) {
		super(name, IMAGE, 0, 0);
		this.type = type;
	}
	
	public Function(String name, DataType type, int start, int end) {
		super(name, IMAGE, start, end);
		this.type = type;
	}
	
	public DataType getType() {
		return type;
	}
	
	public void addParameter(String name, DataType type) {
		if(parameters == null) parameters = new ArrayList<Parameter>();
		
		parameters.add(new Parameter(name, type));
	}
	
	public List<Parameter> getParameters() {
		return parameters;
	}
	
	/**
	 * Generates a LSL-script string matching the definition of this function.
	 * @return String
	 */
	public String toPrototype() {
		StringBuilder sb = new StringBuilder(getName());
		sb.append("("); //$NON-NLS-1$
		if(parameters != null) {
			boolean addComma = false;
			for(Parameter param: parameters) {
				sb.append(addComma ? ", " : ""); //$NON-NLS-1$ //$NON-NLS-2$
				addComma = true;
				sb.append(param.paramType.toString().toLowerCase());
				sb.append(" "); //$NON-NLS-1$
				sb.append(param.paramName);
			}
		}
		
		sb.append(")"); //$NON-NLS-1$
		return sb.toString();
	}
	
	public class Parameter {
		public String paramName;
		public DataType paramType;
		
		public Parameter(String name, DataType type) {
			this.paramName = name;
			this.paramType = type;
		}
	}
}
