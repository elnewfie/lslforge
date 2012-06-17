package lslplus.editor;

import org.eclipse.jface.action.IMenuManager;
import org.eclipse.jface.action.Separator;
import org.eclipse.ui.IActionBars;
import org.eclipse.ui.IEditorPart;
import org.eclipse.ui.IWorkbenchActionConstants;
import org.eclipse.ui.editors.text.TextEditorActionContributor;
import org.eclipse.ui.texteditor.ITextEditor;
import org.eclipse.ui.texteditor.ITextEditorActionDefinitionIds;
import org.eclipse.ui.texteditor.RetargetTextEditorAction;

/**
 * Contributes actions to the edit menu.
 */
public class LslPlusActionContributor extends TextEditorActionContributor {

	private RetargetTextEditorAction contentAssistProposal;
	private RetargetTextEditorAction contentAssistTip;

	public LslPlusActionContributor() {
		super();
		contentAssistProposal= new RetargetTextEditorAction(Messages.getResourceBundle(), "ContentAssistProposal."); //$NON-NLS-1$
		contentAssistProposal.setActionDefinitionId(ITextEditorActionDefinitionIds.CONTENT_ASSIST_PROPOSALS); 
		contentAssistTip= new RetargetTextEditorAction(Messages.getResourceBundle(), "ContentAssistTip."); //$NON-NLS-1$
		contentAssistTip.setActionDefinitionId(ITextEditorActionDefinitionIds.CONTENT_ASSIST_CONTEXT_INFORMATION);
	}
	
	public void dispose() {
		mySetActiveEditor(null);
		super.dispose();
	}
	
	public void init(IActionBars bars) {
		super.init(bars);
		
		IMenuManager menuManager= bars.getMenuManager();
		IMenuManager editMenu= menuManager.findMenuUsingPath(IWorkbenchActionConstants.M_EDIT);
		if (editMenu != null) {
			editMenu.add(new Separator());
			editMenu.add(contentAssistProposal);
			editMenu.add(contentAssistTip);
		}	
	}
	
	private void mySetActiveEditor(IEditorPart part) {
		super.setActiveEditor(part);

		ITextEditor editor= null;
		if (part instanceof ITextEditor)
			editor= (ITextEditor) part;

		contentAssistProposal.setAction(getAction(editor, "ContentAssistProposal")); //$NON-NLS-1$
		contentAssistTip.setAction(getAction(editor, "ContentAssistTip")); //$NON-NLS-1$
	}
	
	public void setActiveEditor(IEditorPart part) {
		super.setActiveEditor(part);
		mySetActiveEditor(part);
	}
}
