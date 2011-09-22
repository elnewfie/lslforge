package lslforge.util;

import org.eclipse.jface.text.rules.IWordDetector;

/**
 * An LSLForge word detector.  An LSLForge word is like a Java word, except that it might
 * contain a '$'.
 */
public class LslWordDetector implements IWordDetector {
	public boolean isWordPart(char character) {
		return Character.isJavaIdentifierPart(character) || character == '$';
	}
	
	public boolean isWordStart(char character) {
		return Character.isJavaIdentifierStart(character) || character == '$';
	}
}
