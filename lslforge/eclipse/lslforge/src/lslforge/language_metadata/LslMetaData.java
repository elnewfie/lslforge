/**
 * 
 */
package lslforge.language_metadata;

public class LslMetaData {
	private LslHandler[] handlers = new LslHandler[0];
	private LslFunction[] functions = new LslFunction[0];
	private LslConstant[] constants = new LslConstant[0];
	public LslHandler[] getHandlers() { return handlers; }
	public LslFunction[] getFunctions() { return functions; }
	public LslConstant[] getConstants() { return constants; }
}