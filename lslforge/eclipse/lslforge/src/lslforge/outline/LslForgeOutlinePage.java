package lslforge.outline;

import java.util.HashMap;
import java.util.LinkedList;
import java.util.List;
import java.util.concurrent.ExecutionException;

import lslforge.LSLForgeElement;
import lslforge.LSLForgePlugin;
import lslforge.LSLProjectNature;
import lslforge.cserver.CompilationServer.Result;
import lslforge.editor.LSLForgeEditor;
import lslforge.generated.CodeElement_CodeElement;
import lslforge.generated.CompilationCommand;
import lslforge.generated.CompilationCommand_CheckModule;
import lslforge.generated.CompilationCommand_CheckScript;
import lslforge.generated.CompilationResponse;
import lslforge.generated.CompilationResponse_ModuleResponse;
import lslforge.generated.CompilationResponse_ScriptResponse;
import lslforge.generated.Ctx;
import lslforge.generated.Ctx_Ctx;
import lslforge.generated.ErrInfo;
import lslforge.generated.Func;
import lslforge.generated.FuncDec_FuncDec;
import lslforge.generated.Func_Func;
import lslforge.generated.GlobDef;
import lslforge.generated.GlobDef_GF;
import lslforge.generated.GlobDef_GI;
import lslforge.generated.GlobDef_GV;
import lslforge.generated.Handler;
import lslforge.generated.Handler_Handler;
import lslforge.generated.LModule_LModule;
import lslforge.generated.LSLScript_LSLScript;
import lslforge.generated.Maybe;
import lslforge.generated.Maybe_Just;
import lslforge.generated.SourceContext;
import lslforge.generated.SourceContext_SourceContext;
import lslforge.generated.State;
import lslforge.generated.State_State;
import lslforge.generated.TextLocation_TextLocation;
import lslforge.generated.Var;
import lslforge.generated.Var_Var;
import lslforge.util.Util;

import org.eclipse.core.resources.IFile;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.jface.text.BadLocationException;
import org.eclipse.jface.text.IDocument;
import org.eclipse.jface.viewers.ISelection;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.jface.viewers.ITreeContentProvider;
import org.eclipse.jface.viewers.LabelProvider;
import org.eclipse.jface.viewers.SelectionChangedEvent;
import org.eclipse.jface.viewers.TreeViewer;
import org.eclipse.jface.viewers.Viewer;
import org.eclipse.swt.graphics.Image;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.ui.IEditorInput;
import org.eclipse.ui.texteditor.IDocumentProvider;
import org.eclipse.ui.views.contentoutline.ContentOutlinePage;

public class LSLForgeOutlinePage extends ContentOutlinePage {
	private LSLForgeContentProvider lpcp;
	private LSLForgeEditor editor;
	
	private class LSLForgeLabelProvider extends LabelProvider {
	    private Image functionImage = createImage("icons/function.gif"); //$NON-NLS-1$;
	    private Image stateImage = createImage("icons/state.gif"); //$NON-NLS-1$;
	    private Image handlerImage = createImage("icons/handler.gif"); //$NON-NLS-1$;
        private Image varImage = createImage("icons/var.gif"); //$NON-NLS-1$;
        private Image importImage = createImage("icons/import.gif"); //$NON-NLS-1$;
        private LinkedList<Image> images;
	    public Image getImage(Object element) {
	        if (element instanceof String) {
	            return stateImage;
	        } else if (element instanceof Ctx_Ctx) {
                    return handlerImage;
            } else if (element instanceof GlobDef_GV) {
                return varImage;
            } else if (element instanceof GlobDef_GF) {
                return functionImage;
            } else if (element instanceof GlobDef_GI) {
                return importImage;
            }
            return null;
	    }
	    
	    private Image createImage(String path) {
	        if (images == null) images = new LinkedList<Image>();
	        Image i = LSLForgePlugin.createImage(path);
	        if (i != null) images.add(i);
	        return i;
	    }


	    public String getText(Object element) {
	    	// heres where ptn matching would be nice
	    	if (element instanceof LSLScript_LSLScript) return "script"; //$NON-NLS-1$
	    	else if (element instanceof String) return (String) element;
	    	else if (element instanceof Ctx_Ctx) {
	    		Ctx_Ctx<?> x = (Ctx_Ctx<?>) element;
	    		Object c = ctxItem(x);
	    		if (c instanceof Handler_Handler) {
	    			Handler_Handler h = (Handler_Handler) c;
	    			return ctxItem(h.handlerName);
	    		}
	        } else if (element instanceof GlobDef_GV) {
	        	GlobDef_GV gv = (GlobDef_GV) element;
	        	Var_Var v = (Var_Var) ctxItem(gv.el1);
	        	return v.varName;
	        } else if (element instanceof GlobDef_GF) {
	        	GlobDef_GF gf = (GlobDef_GF) element;
	        	Func_Func f = (Func_Func) ctxItem(gf.el1);
	        	FuncDec_FuncDec fd = (FuncDec_FuncDec) f.el1;
	        	return ctxItem(fd.funcName);
	    	} else if (element instanceof GlobDef_GI) {
	    		GlobDef_GI gi = (GlobDef_GI) element;
	    		return ctxItem(gi.el1);
	    	}
	    	return ""; //$NON-NLS-1$
	    }
	    
	    @Override
	    public void dispose() {
	        super.dispose();
	        for (Image i : images) {
	            i.dispose();
	        }
	    }
	}
	
    
	private class LSLForgeContentProvider implements ITreeContentProvider {
		private HashMap<Ctx<Handler>, String> parents  = 
			new HashMap<Ctx<Handler>, String>();
		private HashMap<String,Ctx<State>> states = new HashMap<String, Ctx<State>>();
		
		public Object[] getChildren(Object element) {
		    if (element instanceof String) {
		        State_State s = (State_State) ctxItem(states.get(element));
		        if (s == null) return new Object[0];
		        Object[] children = new Object[s.el2.size()];
                int i = 0;
                for (Ctx<Handler> h : s.el2) {
                    parents.put(h, (String)element);
                    children[i++] = h;
                }
                return children;
		    } else {
	        	return new Object[0];
	    	}
		}

		public Object getParent(Object element) {
			return parents.get(element);
		}

		public boolean hasChildren(Object element) {
	    	if (element instanceof String) {
                State_State s = (State_State) ctxItem(states.get(element));
                return s != null && s.el2.size() > 0;
	        } else {
	        	return false;
	    	}
		}

		public Object[] getElements(Object inputElement) {
		    LSLForgeEditor e = (LSLForgeEditor) inputElement;
		    IDocument d = e.getDocumentProvider().getDocument(e.getEditorInput());
		    String text = d.get();

		    IEditorInput ei = e.getEditorInput();
		    IFile f = (IFile) ei.getAdapter(IFile.class);
		    
		    if (f == null) {
		        Util.error("can't adapt editor input to file!"); //$NON-NLS-1$
		        return null;
		    }
		    
		    LSLForgeElement element = (LSLForgeElement) f.getAdapter(LSLForgeElement.class);
		    
		    if (element == null) {
		        Util.error("can't adapt editor input to LSLForgeElement"); //$NON-NLS-1$
		        return null;
		    }
		    
		    CompilationCommand cmd;
		    
		    boolean isScript = element.isScript();
		    
		    if (isScript) {
		        CompilationCommand_CheckScript cmdcs = new CompilationCommand_CheckScript();
		        cmdcs.el1 = new CodeElement_CodeElement();
		        ((CodeElement_CodeElement)cmdcs.el1).codeElementName = e.getEditorInput().getName();
		        ((CodeElement_CodeElement)cmdcs.el1).codeElementText = text;
		        cmd = cmdcs;
		    } else {
		        CompilationCommand_CheckModule cmdcm = new CompilationCommand_CheckModule();
		        cmdcm.el1 = new CodeElement_CodeElement();
                ((CodeElement_CodeElement)cmdcm.el1).codeElementName = e.getEditorInput().getName();
                ((CodeElement_CodeElement)cmdcm.el1).codeElementText = text;
                cmd = cmdcm;
		    }
		    
		    LSLProjectNature n;
		    try {
                n = (LSLProjectNature) f.getProject().getNature(LSLProjectNature.ID);
                if(n == null) {
                	//Try to add the nature
                	LSLProjectNature.fixProjectNature(f.getProject());
                	n = (LSLProjectNature) f.getProject().getNature(LSLProjectNature.ID);
                }
                	
                if(n == null) {
                	Util.error("This project does not have LSLForge support enabled."); //$NON-NLS-1$
                	return null;
                }
            } catch (CoreException e1) {
                Util.error(e1, "can't get project nature!"); //$NON-NLS-1$
                return null;
            }

            CompilationResponse response;
            try {
            	Result r = n.getCompilationServer().execute(cmd);
                response = r.get();
            } catch (InterruptedException e1) {
                Util.error(e1, "can't check!"); //$NON-NLS-1$
                return null;
            } catch (ExecutionException e1) {
                Util.error(e1, "can't check!"); //$NON-NLS-1$
                return null;
            } catch(Exception e1) {
            	Util.error(e1, "can't check!");	//$NON-NLS-1$
            	return null;
            }
            
            //LSLScript script0 = response.el1.el1;
            
            editor.annotateErrs(getErrInfo(response));
            editor.clearProjections();
				
            Object[] children = new Object[countChildren(response)];
			
            int i = 0;
            for (GlobDef g : getGlobals(response)) {
            	children[i++] = g;
            	
            	if (g instanceof GlobDef_GF) {
            	    GlobDef_GF gf = (GlobDef_GF) g;
            	    Ctx_Ctx<?> c = (Ctx_Ctx<?>) gf.el1;
            	    Maybe<SourceContext> m = c.srcCtx;
            	    addProjectionAnnotation(m,d);
            	}
            }
            
            if (isScript) {
                LSLScript_LSLScript script = 
                    (LSLScript_LSLScript) ((CompilationResponse_ScriptResponse)response).el1.el1;
            
                states.clear();
				for (Ctx<State> cs : script.el3) {
				    Ctx_Ctx<State> ccs = (Ctx_Ctx<State>) cs;
				    State_State s = (State_State) ccs.ctxItem;
				    String name = ctxItem(s.el1);
					children[i++] = name;
					states.put(name , cs);
                    Maybe<SourceContext> m = ccs.srcCtx;
                    addProjectionAnnotation(m,d);
                    
                    for (Ctx<Handler> ch : s.el2) {
                        Ctx_Ctx<Handler> cch = (Ctx_Ctx<Handler>) ch;
                        Maybe<SourceContext> hm = cch.srcCtx;
                        addProjectionAnnotation(hm,d);
                    }
				}
			}
            
            return children;
		}

		private int countChildren(CompilationResponse response) {
		    if (response instanceof CompilationResponse_ModuleResponse) {
		        return 
		            ((LModule_LModule)
		                    ((CompilationResponse_ModuleResponse)response).el1.el1).el1.size();
		    } else {
		        return
                    ((LSLScript_LSLScript)
                            ((CompilationResponse_ScriptResponse)response).el1.el1).el2.size() +
                    ((LSLScript_LSLScript)
                            ((CompilationResponse_ScriptResponse)response).el1.el1).el3.size();
		    }
		}
		private List<GlobDef> getGlobals(CompilationResponse response) {
		    if (response instanceof CompilationResponse_ModuleResponse) {
		        return 
		            ((LModule_LModule)((CompilationResponse_ModuleResponse)response).el1.el1).el1;
		    } else {
                return 
                ((LSLScript_LSLScript)((CompilationResponse_ScriptResponse)response).el1.el1).el2;
		    }
		}
		private List<ErrInfo> getErrInfo(CompilationResponse response) {
		    if (response instanceof CompilationResponse_ModuleResponse) {
		        return ((CompilationResponse_ModuleResponse)response).el1.el2;
		    } else {
		        return ((CompilationResponse_ScriptResponse)response).el1.el2;
		    }
		}
		
        private void addProjectionAnnotation(Maybe<SourceContext> m, IDocument d) {
            try {
                if (m instanceof Maybe_Just) {
                    Maybe_Just<SourceContext> j = (Maybe_Just<SourceContext>) m;
                    SourceContext_SourceContext sc = (SourceContext_SourceContext) j.el1;
                    TextLocation_TextLocation tl = (TextLocation_TextLocation) sc.srcTextLocation;
                    int start = d.getLineOffset(tl.textLine0 - 1);
                    int end = (tl.textLine1 == d.getNumberOfLines()) ? d.getLength() : d.getLineOffset(tl.textLine1);
                    editor.addProjection(start, end - start);
                }
            } catch (BadLocationException e) {
                Util.error(e, "can't add folding region"); //$NON-NLS-1$
            }
        }

		public Ctx<State> getState(String name) {
		    return states.get(name);
		}
		
		public void dispose() {
		}

		public void inputChanged(Viewer viewer, Object oldInput, Object newInput) {
			parents.clear();
		}
	}
	
	public LSLForgeOutlinePage(LSLForgeEditor e) {
		this.editor = e;
	}
	
	private static <T> T ctxItem(Ctx<T> c) {
		T o = ((Ctx_Ctx<T>)c).ctxItem;
		return o;
	}
	
	private static TextLocation_TextLocation getLoc(Object o, LSLForgeContentProvider p) {
	    if (o instanceof String) { // name of state...
	        o = p.getState((String) o);
	    }
		if (o instanceof Ctx_Ctx<?>) {
			Ctx_Ctx<?> ctx = (Ctx_Ctx<?>)o;
			if (ctx.srcCtx instanceof Maybe_Just) {
				Maybe_Just<SourceContext> jsc =
					(Maybe_Just<SourceContext>)ctx.srcCtx;
				SourceContext_SourceContext sc = (SourceContext_SourceContext) jsc.el1;
				return (TextLocation_TextLocation) sc.srcTextLocation;
			} else {
				return null;
			}
		} else if (o instanceof GlobDef_GF) {
			GlobDef_GF ggf = (GlobDef_GF) o;
			Ctx_Ctx<Func> cf = (Ctx_Ctx<Func>) ggf.el1;
			if (cf.srcCtx instanceof Maybe_Just) {
				Maybe_Just<SourceContext> jsc =
				    (Maybe_Just<SourceContext>) cf.srcCtx;
				SourceContext_SourceContext sc = (SourceContext_SourceContext) jsc.el1;
				return (TextLocation_TextLocation) sc.srcTextLocation;
			} else {
				return null;
			}
		} else if (o instanceof GlobDef_GV) {
			GlobDef_GV ggv = (GlobDef_GV) o;
			Ctx_Ctx<Var> cv = (Ctx_Ctx<Var>) ggv.el1;
			if (cv.srcCtx instanceof Maybe_Just) {
				Maybe_Just<SourceContext> jsc =
					(Maybe_Just<SourceContext>) cv.srcCtx;
				SourceContext_SourceContext sc = (SourceContext_SourceContext) jsc.el1;
				return (TextLocation_TextLocation) sc.srcTextLocation;
			} else {
				return null;
			}
		} else if (o instanceof GlobDef_GI) {
			GlobDef_GI ggi = (GlobDef_GI) o;
			Ctx_Ctx<String> ci = (Ctx_Ctx<String>) ggi.el1;
			if (ci.srcCtx instanceof Maybe_Just) {
				Maybe_Just<SourceContext> jsc =
					(Maybe_Just<SourceContext>) ci.srcCtx;
				SourceContext_SourceContext sc = (SourceContext_SourceContext) jsc.el1;
				return (TextLocation_TextLocation) sc.srcTextLocation;
			} else {
				return null;
			}
		} else {
			return null;
		}
	}
	
	public void createControl(Composite parent) {
		super.createControl(parent);
		TreeViewer viewer = getTreeViewer();
		lpcp = new LSLForgeContentProvider();
		viewer.setContentProvider(lpcp);
		viewer.setLabelProvider(new LSLForgeLabelProvider());
		viewer.addSelectionChangedListener(this);
		viewer.setInput(editor);
	}
	
    public void selectionChanged(SelectionChangedEvent event) {
		super.selectionChanged(event);
		ISelection selection = event.getSelection();
		if (selection.isEmpty())
			editor.resetHighlightRange();
		else {
			Object o = ((IStructuredSelection) selection).getFirstElement();
			TextLocation_TextLocation loc = getLoc(o,lpcp);
			IDocumentProvider p = editor.getDocumentProvider();
			String doc = p.getDocument(editor.getEditorInput()).get();
			
            if (loc != null) {
                int[] vals = Util.findOffsetsFor(
                        new int[] { loc.textLine0 - 1, loc.textLine1 - 1 }, 
                        new int[] { loc.textColumn0, loc.textColumn1 }, doc);
                editor.setHighlightRange(vals[0], vals[1] - vals[0], true);
            } else editor.resetHighlightRange();
		}
	}

    public void update() {
        TreeViewer viewer = getTreeViewer();
        if (viewer != null) viewer.refresh();
    }
}