package lslplus.util;

import org.eclipse.jface.text.rules.IWhitespaceDetector;

/**
 * An LSL Plus white space detector.
 */
public class LslWhitespaceDetector implements IWhitespaceDetector {
	public boolean isWhitespace(char character) {
		return Character.isWhitespace(character);
	}
}
