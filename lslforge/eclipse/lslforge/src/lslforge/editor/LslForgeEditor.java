package lslforge.editor;

import java.util.Iterator;
import java.util.List;

import lslforge.LslForgePlugin;
import lslforge.LslProjectNature;
import lslforge.debug.LslLineBreakpoint;
import lslforge.generated.ErrInfo;
import lslforge.generated.ErrInfo_ErrInfo;
import lslforge.generated.Maybe_Just;
import lslforge.generated.TextLocation;
import lslforge.generated.TextLocation_TextLocation;
import lslforge.outline.LslForgeOutlinePage;
import lslforge.util.Util;

import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IMarker;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.debug.core.DebugPlugin;
import org.eclipse.debug.core.IBreakpointManager;
import org.eclipse.debug.core.model.IBreakpoint;
import org.eclipse.jface.action.IAction;
import org.eclipse.jface.action.IMenuManager;
import org.eclipse.jface.text.ITextViewerExtension5;
import org.eclipse.jface.text.Position;
import org.eclipse.jface.text.Region;
import org.eclipse.jface.text.source.Annotation;
import org.eclipse.jface.text.source.IAnnotationModel;
import org.eclipse.jface.text.source.ISourceViewer;
import org.eclipse.jface.text.source.IVerticalRuler;
import org.eclipse.jface.text.source.projection.ProjectionAnnotation;
import org.eclipse.jface.text.source.projection.ProjectionAnnotationModel;
import org.eclipse.jface.text.source.projection.ProjectionSupport;
import org.eclipse.jface.text.source.projection.ProjectionViewer;
import org.eclipse.swt.events.MouseEvent;
import org.eclipse.swt.events.MouseListener;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.ui.editors.text.TextEditor;
import org.eclipse.ui.texteditor.ITextEditorActionDefinitionIds;
import org.eclipse.ui.texteditor.TextOperationAction;
import org.eclipse.ui.views.contentoutline.IContentOutlinePage;

/**
 * LSLForge text editor.
 */
public class LslForgeEditor extends TextEditor implements SourceViewerConfigurationListener, LslProjectNature.RecompileListener {
    public static final String ID = "lslforge.editor.LslForgeEditor"; //$NON-NLS-1$

    /** The projection support */
    private ProjectionSupport fProjectionSupport;
    
    private LslForgeOutlinePage outlinePage;
    /**
     * Create an instance of the editor.
     */
    public LslForgeEditor() {
        super();
    }

    /**
     * The <code>LslForgeEditor</code> implementation of this
     * <code>AbstractTextEditor</code> method extend the actions to add those
     * specific to the receiver
     */
    protected void createActions() {
        super.createActions();

        IAction a = new TextOperationAction(Messages.getResourceBundle(),
                "ContentAssistProposal.", this, ISourceViewer.CONTENTASSIST_PROPOSALS); //$NON-NLS-1$
        a.setActionDefinitionId(ITextEditorActionDefinitionIds.CONTENT_ASSIST_PROPOSALS);
        setAction("ContentAssistProposal", a); //$NON-NLS-1$

        a = new TextOperationAction(Messages.getResourceBundle(),
                "ContentAssistTip.", this, ISourceViewer.CONTENTASSIST_CONTEXT_INFORMATION); //$NON-NLS-1$
        a.setActionDefinitionId(ITextEditorActionDefinitionIds.CONTENT_ASSIST_CONTEXT_INFORMATION);
        setAction("ContentAssistTip", a); //$NON-NLS-1$

    }

    /**
     * Save the contents of the editor.
     * 
     * @param monitor the progress monitor
     */
    public void doSave(IProgressMonitor monitor) {
        setCharSet();
        super.doSave(monitor);
    }

    /**
     * doSaveAs specialization of the AbstractTextEditor's doSaveAs()...
     */
    public void doSaveAs() {
        setCharSet();
        super.doSaveAs();
    }

    private void setCharSet() {
        IFile f = (IFile) getEditorInput().getAdapter(IFile.class);
        try {
            f.setCharset("UTF-8", null); //$NON-NLS-1$
        } catch (CoreException e) {
            Util.error(e, "can't set charset"); //$NON-NLS-1$
        }
    }
    protected void editorContextMenuAboutToShow(IMenuManager menu) {
        super.editorContextMenuAboutToShow(menu);
        addAction(menu, "ContentAssistProposal"); //$NON-NLS-1$
        addAction(menu, "ContentAssistTip"); //$NON-NLS-1$
        addAction(menu, "DefineFoldingRegion"); //$NON-NLS-1$
    }

    /**
     * Adapt this editor to the required type, if possible.
     * @param required the required type
     * @return an adapter for the required type or <code>null</code>
     */
    @SuppressWarnings("unchecked")
	public Object getAdapter(Class required) {
        if (IContentOutlinePage.class.equals(required)) {
            if (outlinePage == null) {
                outlinePage = new LslForgeOutlinePage(this);
            }
            return outlinePage;
        }

        if (fProjectionSupport != null) {
            Object adapter = fProjectionSupport.getAdapter(getSourceViewer(), required);
            if (adapter != null)
                return adapter;
        }

        return super.getAdapter(required);
    }

    
	protected void initializeEditor() {
        super.initializeEditor();
        
        LslSourceViewerConfiguration config = new LslSourceViewerConfiguration(this);
        setSourceViewerConfiguration(config);
        outlinePage = new LslForgeOutlinePage(this);
        config.addListener(this);
    }

    private LslProjectNature nature() {
        IResource resource = (IResource) getEditorInput().getAdapter(IResource.class);
        if (resource != null) {
        	try {
				return (LslProjectNature) resource.getProject().getNature(LslProjectNature.ID);
			} catch (CoreException e) {
				Util.error(e, "can't get project nature"); //$NON-NLS-1$
			}
        }
        return null;
    }
    
    protected ISourceViewer createSourceViewer(Composite parent, IVerticalRuler ruler, int styles) {
        fAnnotationAccess = createAnnotationAccess();
        fOverviewRuler = createOverviewRuler(getSharedColors());

        ISourceViewer viewer = new ProjectionViewer(parent, ruler, getOverviewRuler(),
                isOverviewRulerVisible(), styles);

        // ensure decoration support has been created and configured.
        getSourceViewerDecorationSupport(viewer);

        return viewer;
    }

    public void dispose() {
        LslProjectNature n = nature();
        if (n != null) n.removeRecompileListener(this);
        ((LslSourceViewerConfiguration)this.getSourceViewerConfiguration()).dispose();
        super.dispose();
    }
    
    public void createPartControl(Composite parent) {
        super.createPartControl(parent);
        ProjectionViewer viewer = (ProjectionViewer) getSourceViewer();
        fProjectionSupport = new ProjectionSupport(viewer, getAnnotationAccess(), getSharedColors());
        fProjectionSupport
                .addSummarizableAnnotationType("org.eclipse.ui.workbench.texteditor.error"); //$NON-NLS-1$
        fProjectionSupport
                .addSummarizableAnnotationType("org.eclipse.ui.workbench.texteditor.warning"); //$NON-NLS-1$
        fProjectionSupport.install();
        viewer.doOperation(ProjectionViewer.TOGGLE);


        LslProjectNature n = nature();
        if (n != null) n.addRecompileListener(this);

        this.getVerticalRuler().getControl().addMouseListener(new MouseListener() {

            public void mouseDoubleClick(MouseEvent e) {
                IResource resource = (IResource) getEditorInput().getAdapter(IResource.class);
                
                if (resource != null) {
                    Integer line = new Integer(getVerticalRuler().toDocumentLineNumber(e.y) + 1);
                    try {
                        IMarker m = null;
                        IMarker[] markers = resource.findMarkers(LslLineBreakpoint.MARKER_ID, true, 0);
                        for (int i = 0; i < markers.length; i++) {
                            if (line.equals(markers[i].getAttribute(IMarker.LINE_NUMBER))) {
                                m = markers[i];
                            }
                        }
                        if (m == null) {
                            new LslLineBreakpoint(resource,line.intValue());
                        } else {
                            IBreakpoint bp = breakpointManager().getBreakpoint(m);
                            breakpointManager().removeBreakpoint(bp, true);
                        }
                    } catch (CoreException e1) {
                        Util.error(e1,e1.getLocalizedMessage());
                    }
                        
                } else {
                    if (LslForgePlugin.DEBUG) Util.log("resource is null, can't create breakpoint"); //$NON-NLS-1$
                }
            }

            private IBreakpointManager breakpointManager() {
                return DebugPlugin.getDefault().getBreakpointManager();
            }

            public void mouseDown(MouseEvent e) {
            }

            public void mouseUp(MouseEvent e) {
            }
            
        });
        // IAnnotationModel m = viewer.getAnnotationModel();
        //		
        // IDocument d = viewer.getDocument();

        // try {
        // int offset = d.getLineOffset(2);
        // int len = d.getLineLength(2);
        // m.addAnnotation(new
        // Annotation("org.eclipse.ui.workbench.texteditor.error", true,
        // "Error"),
        // new Position(offset,len));
        // } catch (BadLocationException e) {
        // Util.log(e,"bad location");
        // }

        // m.addAnnotationModelListener(new IAnnotationModelListener() {
        //
        // public void modelChanged(IAnnotationModel model) {
        // Util.log("Model changed!");
        // }
        //			
        // });
    }

    public void clearProjections() {
        ProjectionAnnotationModel pm = getProjectionModel();
        pm.removeAllAnnotations();
    }
    public void addProjection(int start, int length) {
        ProjectionAnnotationModel pm = getProjectionModel();
        
        pm.addAnnotation(new ProjectionAnnotation(), new Position(start, length));
    }

    private ProjectionAnnotationModel getProjectionModel() {
        return ((ProjectionViewer)getSourceViewer()).getProjectionAnnotationModel();
    }
    
    public void annotateErrs(List<ErrInfo> errs) {
         IAnnotationModel am = getSourceViewer().getAnnotationModel(); 
            

        Iterator<?> ai = am.getAnnotationIterator();
        while (ai.hasNext()) {
            Annotation ann = (Annotation) ai.next();
            if (ann.getType().equals("org.eclipse.ui.workbench.texteditor.error")) //$NON-NLS-1$
                am.removeAnnotation(ann);
        }
        
        for (ErrInfo err : errs) {
            ErrInfo_ErrInfo e = (ErrInfo_ErrInfo) err;
            if (e.el1 instanceof Maybe_Just) {
                Maybe_Just<TextLocation> mt = (Maybe_Just<TextLocation>) e.el1;
                TextLocation_TextLocation t = (TextLocation_TextLocation) mt.el1;
                int offs[] = Util.findOffsetsFor(
                        new int[] { t.textLine0 - 1, t.textLine1 - 1}, 
                        new int[] { t.textColumn0 - 1, t.textColumn1 - 1}, 
                        getDocumentProvider().getDocument(getEditorInput()).get());
                if (offs[0] == offs[1]) offs[1]++;
                Position pos = new Position(offs[0], offs[1] - offs[0]);
                Annotation ann = new Annotation(
                        "org.eclipse.ui.workbench.texteditor.error", true, e.el2); //$NON-NLS-1$
                am.addAnnotation(ann, pos);
            }
        }
    }
    
    protected void adjustHighlightRange(int offset, int length) {
        ISourceViewer viewer = getSourceViewer();
        if (viewer instanceof ITextViewerExtension5) {
            ITextViewerExtension5 extension = (ITextViewerExtension5) viewer;
            extension.exposeModelRange(new Region(offset, length));
        }
    }

    public void configurationChanged() {
        this.getSourceViewer().invalidateTextPresentation();
    }
    
    public void updateOutline() {
        asyncExec(new Runnable() {
            public void run() {
                outlinePage.update();
            }
        });
    }
    
    private void asyncExec(Runnable r) {
        LslForgePlugin.getDefault().getWorkbench().getDisplay().asyncExec(r);
    }

	public void recompile() {
		updateOutline();
	}

}
