/*******************************************************************************
 * Copyright (c) 2000, 2005 IBM Corporation and others.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 *     IBM Corporation - initial API and implementation
 *     Robert Greayer - modified extensively (renamed, etc.) to work with
 *                      LSL.
 *******************************************************************************/
package lslforge.editor;

import java.util.HashSet;
import java.util.Iterator;
import lslforge.LSLForgePlugin;
import lslforge.editor.imported.HTMLTextPresenter;
import lslforge.editor.lsl.LSLCodeScanner;
import lslforge.editor.lsl.LSLCompletionProcessor;
import lslforge.editor.lsl.LSLForgeAutoIndentStrategy;
import lslforge.editor.lsl.LSLForgeDoubleClickSelector;
import lslforge.editor.lsl.ScannerChangeListener;
import lslforge.util.LSLColorProvider;
import org.eclipse.jface.text.DefaultIndentLineAutoEditStrategy;
import org.eclipse.jface.text.DefaultInformationControl;
import org.eclipse.jface.text.IAutoEditStrategy;
import org.eclipse.jface.text.IDocument;
import org.eclipse.jface.text.IInformationControl;
import org.eclipse.jface.text.IInformationControlCreator;
import org.eclipse.jface.text.ITextDoubleClickStrategy;
import org.eclipse.jface.text.ITextHover;
import org.eclipse.jface.text.TextAttribute;
import org.eclipse.jface.text.contentassist.ContentAssistant;
import org.eclipse.jface.text.contentassist.IContentAssistant;
import org.eclipse.jface.text.presentation.IPresentationReconciler;
import org.eclipse.jface.text.presentation.PresentationReconciler;
import org.eclipse.jface.text.reconciler.IReconciler;
import org.eclipse.jface.text.reconciler.MonoReconciler;
import org.eclipse.jface.text.rules.BufferedRuleBasedScanner;
import org.eclipse.jface.text.rules.DefaultDamagerRepairer;
import org.eclipse.jface.text.rules.Token;
import org.eclipse.jface.text.source.IAnnotationHover;
import org.eclipse.jface.text.source.ISourceViewer;
import org.eclipse.jface.text.source.SourceViewerConfiguration;
import org.eclipse.swt.graphics.RGB;
import org.eclipse.swt.widgets.Shell;

/**
 * Configuration for an LSL (+) source viewer.
 */
public class LSLSourceViewerConfiguration extends SourceViewerConfiguration 
implements ScannerChangeListener {

    private final HashSet<SourceViewerConfigurationListener> listeners = new HashSet<SourceViewerConfigurationListener>();
    private final LSLCodeScanner scanner;
    private final LSLForgeEditor editor;
    
    static class SingleTokenScanner extends BufferedRuleBasedScanner {
        public SingleTokenScanner(TextAttribute attribute) {
            setDefaultReturnToken(new Token(attribute));
        }
    }

    /**
     * Default constructor.
     * @param editor the editor
     */
    public LSLSourceViewerConfiguration(LSLForgeEditor editor) {
        this.scanner = LSLForgePlugin.getDefault().getLSLCodeScanner();
        scanner.addListener(this);
        this.editor = editor;
    }
    
    public void addListener(SourceViewerConfigurationListener listener) {
        listeners.add(listener);
    }

    public void removeListener(SourceViewerConfigurationListener listener) {
        listeners.remove(listener);
    }
    
    /*
     * (non-Javadoc) Method declared on SourceViewerConfiguration
     */
    @Override
	public IAnnotationHover getAnnotationHover(ISourceViewer sourceViewer) {
        return new LSLAnnotationHover();
    }

    /*
     * @see org.eclipse.jface.text.source.SourceViewerConfiguration#getAutoEditStrategies(org.eclipse.jface.text.source.ISourceViewer,
     *      java.lang.String)
     */
    @Override
	public IAutoEditStrategy[] getAutoEditStrategies(ISourceViewer sourceViewer, String contentType) {
        IAutoEditStrategy strategy = (IDocument.DEFAULT_CONTENT_TYPE.equals(contentType) ? new LSLForgeAutoIndentStrategy()
                : new DefaultIndentLineAutoEditStrategy());
        return new IAutoEditStrategy[] { strategy };
    }

    /*
     * @see org.eclipse.jface.text.source.SourceViewerConfiguration#getConfiguredDocumentPartitioning(org.eclipse.jface.text.source.ISourceViewer)
     */
    @Override
	public String getConfiguredDocumentPartitioning(ISourceViewer sourceViewer) {
        return LSLForgePlugin.LSL_PARTITIONING;
    }

    /*
     * (non-Javadoc) Method declared on SourceViewerConfiguration
     */
    @Override
	public String[] getConfiguredContentTypes(ISourceViewer sourceViewer) {
        return new String[] { IDocument.DEFAULT_CONTENT_TYPE,
                LSLPartitionScanner.LSL_MULTILINE_COMMENT };
    }

    /*
     * (non-Javadoc) Method declared on SourceViewerConfiguration
     */
    @Override
	public IContentAssistant getContentAssistant(ISourceViewer sourceViewer) {

        ContentAssistant assistant = new ContentAssistant();
        assistant.setDocumentPartitioning(getConfiguredDocumentPartitioning(sourceViewer));
        assistant.setContentAssistProcessor(new LSLCompletionProcessor(editor),
                IDocument.DEFAULT_CONTENT_TYPE);

        assistant.enableAutoActivation(true);
        assistant.setAutoActivationDelay(500);
        assistant.setProposalPopupOrientation(IContentAssistant.CONTEXT_INFO_BELOW);
        assistant.setProposalSelectorBackground(LSLForgePlugin.getDefault().getLSLColorProvider()
                .getColor(new RGB(224, 224, 224)));
        assistant.setContextInformationPopupOrientation(IContentAssistant.CONTEXT_INFO_ABOVE);
        assistant.setContextInformationPopupBackground(LSLForgePlugin.getDefault()
                .getLSLColorProvider().getColor(new RGB(255, 255, 50)));
        assistant.setInformationControlCreator(new IInformationControlCreator() {
            public IInformationControl createInformationControl(Shell parent) {
                return new DefaultInformationControl(parent, new HTMLTextPresenter(true));
            }
        });
        return assistant;
    }

    /*
     * (non-Javadoc) Method declared on SourceViewerConfiguration
     */
    public String getDefaultPrefix(ISourceViewer sourceViewer, String contentType) {
        return (IDocument.DEFAULT_CONTENT_TYPE.equals(contentType) ? "//" : null); //$NON-NLS-1$
    }

    /*
     * (non-Javadoc) Method declared on SourceViewerConfiguration
     */
    @Override
	public ITextDoubleClickStrategy getDoubleClickStrategy(ISourceViewer sourceViewer,
            String contentType) {
        return new LSLForgeDoubleClickSelector();
    }

    /*
     * (non-Javadoc) Method declared on SourceViewerConfiguration
     */
    @Override
	public String[] getIndentPrefixes(ISourceViewer sourceViewer, String contentType) {
        return new String[] { "\t", "    " }; //$NON-NLS-1$ //$NON-NLS-2$
    }

    /*
     * (non-Javadoc) Method declared on SourceViewerConfiguration
     */
    @Override
	public IPresentationReconciler getPresentationReconciler(ISourceViewer sourceViewer) {

        LSLColorProvider provider = LSLForgePlugin.getDefault().getLSLColorProvider();
        PresentationReconciler reconciler = new PresentationReconciler();
        reconciler.setDocumentPartitioning(getConfiguredDocumentPartitioning(sourceViewer));

        DefaultDamagerRepairer dr = new DefaultDamagerRepairer(LSLForgePlugin.getDefault()
                .getLSLCodeScanner());
        reconciler.setDamager(dr, IDocument.DEFAULT_CONTENT_TYPE);
        reconciler.setRepairer(dr, IDocument.DEFAULT_CONTENT_TYPE);

        dr = new DefaultDamagerRepairer(new SingleTokenScanner(new TextAttribute(provider
                .getColor(LSLColorProvider.MULTI_LINE_COMMENT_COLOR))));
        reconciler.setDamager(dr, LSLPartitionScanner.LSL_MULTILINE_COMMENT);
        reconciler.setRepairer(dr, LSLPartitionScanner.LSL_MULTILINE_COMMENT);

        return reconciler;
    }

    @Override
    public IReconciler getReconciler(ISourceViewer sourceViewer) {
    	LSLForgeReconcilingStrategy strat = new LSLForgeReconcilingStrategy(this.editor);
    	MonoReconciler reconciler = new MonoReconciler(strat, false);
    	return reconciler;
    }

    @Override
	public int getTabWidth(ISourceViewer sourceViewer) {
        return 4;
    }

    @Override
	public ITextHover getTextHover(ISourceViewer sourceViewer, String contentType) {
        return new LSLTextHover();
    }

    public void dispose() {
        listeners.clear();
        scanner.removeListener(this);
    }

    public void scannerChanged() {
        Iterator<SourceViewerConfigurationListener> i = listeners.iterator();
        
        while (i.hasNext()) {
            i.next().configurationChanged();
        }
    }
    
    
}
