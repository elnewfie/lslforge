/**
 * 
 */
package lslforge.language_metadata;

public class LSLMetaData {
	private LSLHandler[] handlers = new LSLHandler[0];
	private LSLFunction[] functions = new LSLFunction[0];
	private LSLConstant[] constants = new LSLConstant[0];
	public LSLHandler[] getHandlers() { return handlers; }
	public LSLFunction[] getFunctions() { return functions; }
	public LSLConstant[] getConstants() { return constants; }
}