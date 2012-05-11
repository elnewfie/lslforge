package lslforge.editor.lsl;


import java.util.ArrayList;
import java.util.Collections;
import java.util.Comparator;
import java.util.List;
import lslforge.LSLForgePlugin;
import lslforge.editor.LSLForgeEditor;
import lslforge.language_metadata.LSLConstant;
import lslforge.language_metadata.LSLFunction;
import lslforge.language_metadata.LSLHandler;
import lslforge.language_metadata.LSLParam;
import lslforge.outline.LSLForgeOutlinePage;
import lslforge.outline.items.Function;
import lslforge.outline.items.OutlineItem;
import lslforge.util.LSLWordDetector;
import lslforge.util.Util;
import org.eclipse.jface.text.BadLocationException;
import org.eclipse.jface.text.IDocument;
import org.eclipse.jface.text.ITextViewer;
import org.eclipse.jface.text.TextPresentation;
import org.eclipse.jface.text.contentassist.CompletionProposal;
import org.eclipse.jface.text.contentassist.ContextInformation;
import org.eclipse.jface.text.contentassist.ICompletionProposal;
import org.eclipse.jface.text.contentassist.IContentAssistProcessor;
import org.eclipse.jface.text.contentassist.IContextInformation;
import org.eclipse.jface.text.contentassist.IContextInformationPresenter;
import org.eclipse.jface.text.contentassist.IContextInformationValidator;
import org.eclipse.jface.text.rules.IWordDetector;
import org.eclipse.swt.graphics.Image;

/**
 * LSLForge Completion Processor.
 */
public class LSLCompletionProcessor implements IContentAssistProcessor {
	private static IWordDetector wordDetector = new LSLWordDetector();
	private final Image functionImage;
	private final Image handlerImage;
	private final Image constantImage;
	private final Image keywordImage;
	
	private final LSLForgeEditor editor;
	
	private static final Comparator<ICompletionProposal> PROPOSAL_COMPARATOR = new Comparator<ICompletionProposal>() {
		public int compare(ICompletionProposal aProposal1, ICompletionProposal aProposal2) {
			String text1 = aProposal1.getDisplayString();
			String text2 = aProposal2.getDisplayString();
			return text1.compareTo(text2);
		}

		@Override
		public int hashCode() {
			return super.hashCode();
		}

		@Override
		public boolean equals(Object aProposal) {
			return false;
		}
	};

	protected static class Validator implements IContextInformationValidator,
	    IContextInformationPresenter {

		protected int fInstallOffset;

		/*
		 * @see IContextInformationValidator#isContextInformationValid(int)
		 */
		public boolean isContextInformationValid(int offset) {
			return Math.abs(fInstallOffset - offset) < 5;
		}

		/*
		 * @see IContextInformationValidator#install(IContextInformation, ITextViewer, int)
		 */
		public void install(IContextInformation info, ITextViewer viewer, int offset) {
			fInstallOffset= offset;
		}
		
		/*
		 * @see org.eclipse.jface.text.contentassist.IContextInformationPresenter#updatePresentation(int, TextPresentation)
		 */
		public boolean updatePresentation(int documentPosition, TextPresentation presentation) {
			return false;
		}
	}

	protected final static String[] fgProposals=
		{ "default", //$NON-NLS-1$
		  "do", //$NON-NLS-1$
		  "else", //$NON-NLS-1$
		  "float", //$NON-NLS-1$
		  "for", //$NON-NLS-1$
		  "if", //$NON-NLS-1$
		  "$import", //$NON-NLS-1$
		  "integer", //$NON-NLS-1$
		  "return", //$NON-NLS-1$
		  "while", //$NON-NLS-1$
		  "vector", //$NON-NLS-1$
		  "string", //$NON-NLS-1$
		  "list", //$NON-NLS-1$
		  "rotation", //$NON-NLS-1$
		  "key", //$NON-NLS-1$
		  "state", //$NON-NLS-1$
		  "jump"}; //$NON-NLS-1$

	protected static CompletionInfo[] possibleProposals = null;
	protected IContextInformationValidator validator = new Validator();

	public LSLCompletionProcessor(LSLForgeEditor editor) {
		this.editor = editor;
		
		functionImage = Util.findDescriptor("$nl$/icons/function.gif").createImage(true); //$NON-NLS-1$
		handlerImage = Util.findDescriptor("$nl$/icons/handler.gif").createImage(); //$NON-NLS-1$
		constantImage = Util.findDescriptor("$nl$/icons/constant.gif").createImage(); //$NON-NLS-1$
        keywordImage = Util.findDescriptor("$nl$/icons/keyword.gif").createImage(); //$NON-NLS-1$
	}
	
	protected CompletionInfo [] getPossibleProposals() {
		if (possibleProposals == null) {
		    LSLHandler handlers[] = LSLForgePlugin.getDefault().getLSLMetaData().getHandlers();
		    CompletionInfo[] handlerNames = Util.arrayMap(new Util.ArrayMapFunc<CompletionInfo>() {
			    		public Class<CompletionInfo> elementType() { return CompletionInfo.class; }
				    	public CompletionInfo map(Object o) {
				    		LSLHandler handler = (LSLHandler) o;
				    		String proto = handler.getName() + formatParams(handler.getParams());
				    		String startLine = proto + " {"; //$NON-NLS-1$
				    		return new CompletionInfo(handler.getName(),
				    				startLine,
				    				proto,
				    				handler.getDescription(), handlerImage,
				    				startLine.length());
				    	}
				    }, handlers);
		    
		    CompletionInfo[] functions = Util.arrayMap(new Util.ArrayMapFunc<CompletionInfo>() {
				public Class<CompletionInfo> elementType() { return CompletionInfo.class; }
				public CompletionInfo map(Object o) {
					LSLFunction f = (LSLFunction) o;
					return new CompletionInfo(f.getName(),
							f.getName() + formatArgs(f.getParams()),
							f.getName() + formatParams(f.getParams()),
							f.getDescription(), functionImage, f.getName().length() + 1);
				}
		    }, LSLForgePlugin.getDefault().getLSLMetaData().getFunctions());

		    CompletionInfo[] constants = Util.arrayMap(
		    		new Util.ArrayMapFunc<CompletionInfo>() {
						public Class<CompletionInfo> elementType() { return CompletionInfo.class; }
						public CompletionInfo map(Object o) {
							LSLConstant k = (LSLConstant) o;
							return new CompletionInfo(k.getName(),k.getName(),
									k.getName() + " - " + k.getType(), //$NON-NLS-1$
									k.getDescription(),constantImage, k.getName().length());
						}
		    		}, LSLForgePlugin.getDefault().getLSLMetaData().getConstants());
		    CompletionInfo[] keywords = Util.arrayMap(
		    		new Util.ArrayMapFunc<CompletionInfo>() {
						public Class<CompletionInfo> elementType() { return CompletionInfo.class; }
						public CompletionInfo map(Object o) {
							String k = (String) o;
							return new CompletionInfo(k,k,k,null,keywordImage,k.length());
						}
		    		}, fgProposals);
		    possibleProposals = (CompletionInfo[])
		        Util.concat(new Object[][] { handlerNames, functions, constants, keywords });
		}
		return possibleProposals;
	}
	
	private static String formatParams(LSLParam[] params) {
		StringBuilder buf = new StringBuilder("("); //$NON-NLS-1$
		String sep = ""; //$NON-NLS-1$
		for (int i = 0; i < params.length; i++) {
			buf.append(sep);
			sep = ", "; //$NON-NLS-1$
			buf.append(params[i].getType()).append(" ").append(params[i].getName()); //$NON-NLS-1$
		}
		return buf.append(")").toString(); //$NON-NLS-1$
	}
	
	private static String formatArgs(LSLParam[] params) {
		StringBuilder buf = new StringBuilder("("); //$NON-NLS-1$
		String sep = ""; //$NON-NLS-1$
		for (int i = 0; i < params.length; i++) {
			buf.append(sep).append(params[i].getName());
			sep = ", "; //$NON-NLS-1$
		}
		return buf.append(")").toString(); //$NON-NLS-1$
	}
	private static class CompletionInfo {
		public CompletionInfo(String matchName,String insertText,String displayText, 
		        String additionalInfo, Image image, int cursorOffset) {
			this.matchName = matchName;
			this.displayText = displayText;
			this.insertText = insertText;
			this.additionalInfo = additionalInfo;
			this.image = image;
			this.cursorOffset = cursorOffset;
		}
		public String matchName;
		public String insertText;
		public String displayText;
		public String additionalInfo;
		public Image image;
		public int cursorOffset;
	}
	/* (non-Javadoc)
	 * Method declared on IContentAssistProcessor
	 */
	public ICompletionProposal[] computeCompletionProposals(ITextViewer viewer, int documentOffset) {
		List<ICompletionProposal> proposals = new ArrayList<ICompletionProposal>();
		Couple c = guessTextLine(viewer.getDocument(), documentOffset);
		String prefix = c.o;
		
		//First, retrieve any proposals from the document outline
		if(editor.getOutlinePage() instanceof LSLForgeOutlinePage) {
			LSLForgeOutlinePage outlinePage = (LSLForgeOutlinePage)editor.getOutlinePage();
			List<OutlineItem> items =  outlinePage.getOutline();
			for(OutlineItem item: items) {
				if(item.getName().startsWith(prefix)) {
					String displayText;
					if(item instanceof Function) {
						displayText = ((Function)item).toPrototype();
					} else {
						displayText = item.getName();
					}
					
					proposals.add(
						new CompletionProposal(
							item.getName(), 
							documentOffset - prefix.length(),
							prefix.length(),
							item.getName().length(), 
							item.getImage(),
							item.getName(),
							new ContextInformation(item.getName(), displayText),
							displayText
						)
					);
				}
			}
		}
		
		
		possibleProposals = getPossibleProposals();
		//String[] rules = fgProposals.getRules(prefix);
		for (int i = 0; i < possibleProposals.length; i++) {
			CompletionInfo prop = possibleProposals[i];
			if (prop.matchName.startsWith(prefix)) {
				proposals.add(new CompletionProposal(prop.insertText,
						documentOffset - prefix.length(), prefix.length(),
						/*prop.insertText.length()*/prop.cursorOffset, prop.image, prop.displayText, 
						new ContextInformation(prop.matchName,prop.displayText),
						prop.additionalInfo)); 
			}
		}
		Collections.sort(proposals, PROPOSAL_COMPARATOR);
		return proposals.toArray(new ICompletionProposal[proposals.size()]);
	}
	
	private Couple guessTextLine(IDocument doc, int offset) {
		try {
		 	// Guess start position
			int start = offset;
			while (start >= 1 && isWordPart(doc.getChar(start - 1))) {
				start--;
			}

			// Guess end position
			int end = offset;
			return new Couple( doc.get(start, end - start),doc.getLineOfOffset(start) + 1);
		} catch (BadLocationException e) {
			return new Couple("",-1); //$NON-NLS-1$
		}
		
	}
	class Couple {
		public Couple(String o, int p) {
			this.o = o;
			this.p = p;
		}
		
		String o;
		int p;
	}
	
	private static final boolean isWordPart(char aChar) {
		return wordDetector.isWordPart(aChar);
	}

	/**
	 * TODO: does nothing now... come up with good Context info...
	 * @param viewer the viewer
	 * @param documentOffset the place in the document we need info for.
	 * @return  the array of context info.
	 */
	public IContextInformation[] computeContextInformation(ITextViewer viewer, int documentOffset) {
		IContextInformation[] result= new IContextInformation[0];
		for (int i= 0; i < result.length; i++) {
			result[i]= new ContextInformation("placeholder!", "placeholder!");  //$NON-NLS-1$//$NON-NLS-2$
		}
		return result;
	}
	
	public char[] getCompletionProposalAutoActivationCharacters() {
		return new char[] { '.', '(' };
	}
	
	public char[] getContextInformationAutoActivationCharacters() {
		return new char[] { '#' };
	}
	
	public IContextInformationValidator getContextInformationValidator() {
		return validator;
	}
	
	public String getErrorMessage() {
		return null;
	}
}
