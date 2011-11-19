/**
 * 
 */
package lslforge.language_metadata;


public class LSLHandler {
	private String name;
	private LSLParam[] params;
	private String description;
	public String getName() { return name; }
	public String getDescription() { return description; }
	public LSLParam[] getParams() { return params; }
	public String signature() {
		StringBuilder buf = new StringBuilder(name + "("); //$NON-NLS-1$
		String sep = ""; //$NON-NLS-1$
		for (int i = 0; i < params.length; i++) {
			buf.append(sep).append(params[i].getType()).append(" "). //$NON-NLS-1$
			  append(params[i].getName());
			sep = ","; //$NON-NLS-1$
		}
		buf.append(")"); //$NON-NLS-1$
		return buf.toString();
	}
	
	public String fullDescription() {
		return signature() + "\n\n" + description; //$NON-NLS-1$
	}
}