package lslforge.util;

import org.eclipse.jface.text.rules.IWhitespaceDetector;

/**
 * An LSLForge white space detector.
 */
public class LslWhitespaceDetector implements IWhitespaceDetector {
	public boolean isWhitespace(char character) {
		return Character.isWhitespace(character);
	}
}
