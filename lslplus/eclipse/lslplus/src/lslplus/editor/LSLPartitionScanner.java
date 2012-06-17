package lslplus.editor;


import java.util.ArrayList;
import java.util.List;

import org.eclipse.jface.text.rules.*;

/**
 * This scanner recognizes LSL multi-line comments.  Derived heavily from eclipse examples.
 */
public class LslPartitionScanner extends RuleBasedPartitionScanner {

	public final static String LSL_MULTILINE_COMMENT= "__lsl_multiline_comment"; //$NON-NLS-1$
	public final static String[] LSL_PARTITION_TYPES= new String[] { LSL_MULTILINE_COMMENT };

	private static class WordPredicateRule extends WordRule implements IPredicateRule {
		private IToken fSuccessToken;
		
		public WordPredicateRule(IToken successToken) {
			super(new IWordDetector() {
		        public boolean isWordStart(char c) {
		            return (c == '/');
		        }

		        public boolean isWordPart(char c) {
		            return (c == '*' || c == '/');
		        }
			    
			});
			fSuccessToken= successToken;
			addWord("/**/", fSuccessToken); //$NON-NLS-1$
		}
		
		public IToken evaluate(ICharacterScanner scanner, boolean resume) {
			return super.evaluate(scanner);
		}

		public IToken getSuccessToken() {
			return fSuccessToken;
		}
	}

	/**
	 * Sets up the partitioner.
	 */
	public LslPartitionScanner() {
		super();

		IToken comment= new Token(LSL_MULTILINE_COMMENT);

		List<IRule> rules= new ArrayList<IRule>();

		rules.add(new EndOfLineRule("//", Token.UNDEFINED)); //$NON-NLS-1$

		// rule for string constants
		rules.add(new SingleLineRule("\"", "\"", Token.UNDEFINED, '\\')); //$NON-NLS-2$ //$NON-NLS-1$
		// rule for character constants
		rules.add(new SingleLineRule("'", "'", Token.UNDEFINED, '\\')); //$NON-NLS-2$ //$NON-NLS-1$

		// rule for an empty comment
		rules.add(new WordPredicateRule(comment));

		// Add rule for multi-line comments and.
		rules.add(new MultiLineRule("/*", "*/", comment, (char) 0, true)); //$NON-NLS-1$ //$NON-NLS-2$

		IPredicateRule[] result= new IPredicateRule[rules.size()];
		rules.toArray(result);
		setPredicateRules(result);
	}
}
