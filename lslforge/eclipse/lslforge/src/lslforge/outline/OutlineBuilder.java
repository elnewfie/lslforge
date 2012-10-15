package lslforge.outline;

import java.util.ArrayList;
import java.util.List;
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
import lslforge.generated.ErrInfo_ErrInfo;
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
import lslforge.generated.LSLType;
import lslforge.generated.LSLType_LLFloat;
import lslforge.generated.LSLType_LLInteger;
import lslforge.generated.LSLType_LLKey;
import lslforge.generated.LSLType_LLList;
import lslforge.generated.LSLType_LLRot;
import lslforge.generated.LSLType_LLString;
import lslforge.generated.LSLType_LLVector;
import lslforge.generated.LSLType_LLVoid;
import lslforge.generated.Maybe;
import lslforge.generated.Maybe_Just;
import lslforge.generated.SourceContext;
import lslforge.generated.SourceContext_SourceContext;
import lslforge.generated.State;
import lslforge.generated.State_State;
import lslforge.generated.TextLocation;
import lslforge.generated.TextLocation_TextLocation;
import lslforge.generated.Var;
import lslforge.generated.Var_Var;
import lslforge.language_metadata.LSLFunction;
import lslforge.outline.DocumentOutline.DocumentType;
import lslforge.outline.items.Function;
import lslforge.outline.items.Import;
import lslforge.outline.items.OutlineItem;
import lslforge.outline.items.OutlineItem.DataType;
import lslforge.outline.items.TextPosition;
import lslforge.outline.items.Variable;
import lslforge.util.Util;
import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IMarker;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.jface.text.BadLocationException;
import org.eclipse.jface.text.IDocument;

public class OutlineBuilder
{
	private final String text;
	private final IFile file;

	private final DocumentType type;
	private List<ErrInfo> errors = null;
	private final IDocument document;

	List<OutlineItem> items = new ArrayList<OutlineItem>();
	
	public OutlineBuilder(LSLForgeEditor editor) {
		document = editor.getDocumentProvider().getDocument(editor.getEditorInput());
		file = (IFile) editor.getEditorInput().getAdapter(IFile.class);
		text = document.get();
		
		LSLForgeElement element = (LSLForgeElement) file.getAdapter(LSLForgeElement.class);

		if (element != null) {
			this.type = element.isScript() ? DocumentType.SCRIPT : DocumentType.MODULE;
		} else {
			this.type = DocumentType.SCRIPT;
		}
	}
	
	public void generateOutline() {
		LSLForgeElement element = (LSLForgeElement) file.getAdapter(LSLForgeElement.class);
		
		if (element == null) {
			Util.error("can't adapt editor input to LSLForgeElement"); //$NON-NLS-1$
			return;
		}

		// Prepare the compilation command
		CompilationCommand cmd;
		CodeElement_CodeElement codeElement = new CodeElement_CodeElement();
		codeElement.codeElementName = file.getName();
		codeElement.codeElementText = text;
		if (type == DocumentType.SCRIPT) {
			cmd = new CompilationCommand_CheckScript();
			((CompilationCommand_CheckScript) cmd).el1 = codeElement;
		} else {
			cmd = new CompilationCommand_CheckModule();
			((CompilationCommand_CheckModule) cmd).el1 = codeElement;
		}

		// Perform the compilation and get the response
		CompilationResponse response;
		try {
			response = compile(file, cmd);
		} catch (Exception e1) {
			Util.error(e1, "Unable to compile script/module"); //$NON-NLS-1$
			return;
		}

		// Parse it, grab the errors
		if (type == DocumentType.SCRIPT) {
			errors = ((CompilationResponse_ScriptResponse) response).el1.el2;
		} else {
			errors = ((CompilationResponse_ModuleResponse) response).el1.el2;
		}
		parseResponse(response);
		parseErrors(file);

		return;
	}

	public List<OutlineItem> getOutline() {
		return items;
	}
	
	public List<ErrInfo> getErrors() {
		return errors;
	}
	
	private CompilationResponse compile(IFile f, CompilationCommand cmd) throws Exception {
		LSLProjectNature n = null;

		try {
			n = (LSLProjectNature) f.getProject().getNature(LSLProjectNature.ID);
			if (n == null) {
				// Try to add the nature
				LSLProjectNature.fixProjectNature(f.getProject());
				n = (LSLProjectNature) f.getProject().getNature(LSLProjectNature.ID);
			}

			if (n == null) throw new Exception("This project does not have LSLForge support enabled."); //$NON-NLS-1$

		} catch (CoreException e1) {
			throw new Exception("can't get project nature!", e1); //$NON-NLS-1$
		}

		try {
			Result r = n.getCompilationServer().execute(cmd);
			return r.get();
		} catch (Exception e1) {
			throw new Exception("Unable to retrieve compile result"); //$NON-NLS-1$
		}
	}

	private void parseErrors(IResource resource) {
		try {
			resource.deleteMarkers(IMarker.PROBLEM, true, IResource.DEPTH_INFINITE);
			
			for(ErrInfo ei: errors) {
				ErrInfo_ErrInfo err = (ErrInfo_ErrInfo)ei;
				
				IMarker marker = resource.createMarker(LSLProjectNature.LSLFORGE_PROBLEM);
                marker.setAttribute(IMarker.MESSAGE, err.el2);
                marker.setAttribute(IMarker.SEVERITY, IMarker.SEVERITY_WARNING);
                if (err.el1 instanceof Maybe_Just) {
                	TextLocation_TextLocation errLoc = (TextLocation_TextLocation)((Maybe_Just<TextLocation>)err.el1).el1;
                    int lineOffset0 = errLoc.textLine0 - 1;
                    int lineOffset1 = errLoc.textLine1 - 1;
                    marker.setAttribute(IMarker.LINE_NUMBER, errLoc.textLine0);
                    int[] offsets = Util.findOffsetsFor(
                    	new int[] { lineOffset0, lineOffset1 }, 
                        new int[] { errLoc.textColumn0 - 1, errLoc.textColumn1 - 1 }, 
                        (IFile)resource
                    );
                    if (offsets != null) {
                        if (offsets[0] == offsets[1]) offsets[1]++;
                        marker.setAttribute(IMarker.CHAR_START, offsets[0]);
                        marker.setAttribute(IMarker.CHAR_END, offsets[1]);
                    }
                }
			}
			
		} catch (CoreException e) {
			Util.error(e.getMessage());
			return;
		}
		
	}
	
	private void parseResponse(CompilationResponse response) {
		List<GlobDef> globals;
		if (type == DocumentType.SCRIPT) {
			globals = ((LSLScript_LSLScript) ((CompilationResponse_ScriptResponse) response).el1.el1).el2;
		} else {
			globals = ((LModule_LModule) ((CompilationResponse_ModuleResponse) response).el1.el1).el1;
		}

		LSLFunction[] functions = LSLForgePlugin.getLLFunctions();
		
		// First, extract our global variables and functions
		for (GlobDef g : globals) {
			// Is it a variable?
			if (g instanceof GlobDef_GV) {
				// Unravel this mess
				GlobDef_GV gf = (GlobDef_GV) g;
				Ctx_Ctx<?> c = (Ctx_Ctx<?>) gf.el1;
				Maybe<SourceContext> m = c.srcCtx;
				
				// Extract the position, name, and datatype
				TextPosition textPos = getTextPosition(m, document);
				String varName = getRawText(g);
				Variable newVar = new Variable(varName, getDataType(g));
				newVar.setTextPosition(textPos);
				
				// Add it
				items.add(newVar);

				// Then is it a function?
			} else if (g instanceof GlobDef_GF) {
				// Unravel this mess
				GlobDef_GF gf = (GlobDef_GF) g;
				Ctx_Ctx<?> c = (Ctx_Ctx<?>) gf.el1;
				Maybe<SourceContext> m = c.srcCtx;
				Func_Func f = (Func_Func)Util.ctxItem(gf.el1);
				FuncDec_FuncDec fd = (FuncDec_FuncDec) f.el1;
				
				
				// Extract the position, name, and datatype
				TextPosition textPos = getTextPosition(m, document);
				String funcName = getRawText(g);
				Function newFunc = new Function(funcName, getDataType(g));
				newFunc.setTextPosition(textPos);
				for(Ctx<Var> parm: fd.funcParms) {
					Var_Var v = (Var_Var)Util.ctxItem(parm);
					newFunc.addParameter(v.varName, getDataType(v.varType));
				}
				newFunc.toPrototype();

				//Newfie - commented out for now, will return in super form later
				//if(parseFuncErrors(functions, m, funcName)) continue;

				items.add(newFunc);

			// An import statement?
			} else if (g instanceof GlobDef_GI) {
				// Unravel this mess
				GlobDef_GI gi = (GlobDef_GI) g;
				Ctx_Ctx<?> c = (Ctx_Ctx<?>) gi.el1;
				Maybe<SourceContext> m = c.srcCtx;

				// Extract the position, name, and datatype
				TextPosition textPos = getTextPosition(m, document);
				Import newImport = new Import(getRawText(g));
				newImport.setTextPosition(textPos);

				items.add(newImport);
			}
		}

		// If it is a script, grab the state definitions
		if (type == DocumentType.SCRIPT) {
			LSLScript_LSLScript script = (LSLScript_LSLScript) ((CompilationResponse_ScriptResponse) response).el1.el1;

			for (Ctx<State> srcContext : script.el3) {
				// Unravel this mess
				Ctx_Ctx<State> ccs = (Ctx_Ctx<State>) srcContext;
				Maybe<SourceContext> m = ccs.srcCtx;
				State_State state = (State_State) ccs.ctxItem;

				// Extract the position and name
				TextPosition textPos = getTextPosition(m, document);
				String stateName = Util.ctxItem(state.el1);

				lslforge.outline.items.State stateItem = new lslforge.outline.items.State(stateName);
				stateItem.setTextPosition(textPos);
				items.add(stateItem);

				// Now repeat for the functions embedded inside the state item
				for (Ctx<Handler> ch : state.el2) {
					Ctx_Ctx<Handler> cch = (Ctx_Ctx<Handler>) ch;
					Maybe<SourceContext> hm = cch.srcCtx;
					Handler_Handler handler = (Handler_Handler) Util.ctxItem(cch);

					textPos = getTextPosition(hm, document);
					String name = Util.ctxItem(handler.handlerName);

					lslforge.outline.items.Handler newHandler = new lslforge.outline.items.Handler(name);
					newHandler.setTextPosition(textPos);
					stateItem.addChild(newHandler);
				}
			}
		}
	}

	private boolean parseFuncErrors(LSLFunction[] functions, Maybe<SourceContext> m, String funcName) {
		//Check if it's already defined LL function
		for(LSLFunction func: functions) {
			if(funcName.equals(func.getName())) {
				//Create an error entry for this
		        Maybe_Just<SourceContext> j = (Maybe_Just<SourceContext>) m;
		        SourceContext_SourceContext sc = (SourceContext_SourceContext) j.el1;
		        TextLocation_TextLocation tl = (TextLocation_TextLocation) sc.srcTextLocation;
				
		        TextLocation_TextLocation locErr = new TextLocation_TextLocation();
		        locErr.textLine0 = tl.textLine0;
		        locErr.textColumn0 = tl.textColumn0;
		        locErr.textLine1 = tl.textLine0;
		        locErr.textColumn1 = tl.textColumn0 + funcName.length();
		        
				ErrInfo_ErrInfo err = new ErrInfo_ErrInfo();
				Maybe_Just<TextLocation> loc = new Maybe_Just<TextLocation>();
				loc.el1 = locErr;
				
				err.el1 = loc;
				err.el2 = funcName + " is an already defined LSL function."; //$NON-NLS-1$
				
				errors.add(err);
				
				//Remove the compiled file if necessary
				
				try {
					IFile compiledName = Util.getScriptCompiledName(file);
					if(compiledName != null)
						compiledName.delete(true, null);
				} catch (CoreException e) { }
				return true;
			}
		}
		
		return false;
	}
	
	public String getRawText(Object element) {
    	// heres where ptn matching would be nice
    	if (element instanceof LSLScript_LSLScript) return "script"; //$NON-NLS-1$
    	else if (element instanceof String) return (String) element;
    	else if (element instanceof Ctx_Ctx) {
    		Ctx_Ctx<?> x = (Ctx_Ctx<?>) element;
    		Object c = Util.ctxItem(x);
    		if (c instanceof Handler_Handler) {
    			Handler_Handler h = (Handler_Handler) c;
    			return Util.ctxItem(h.handlerName);
    		}
        } else if (element instanceof GlobDef_GV) {
        	GlobDef_GV gv = (GlobDef_GV) element;
        	Var_Var v = (Var_Var)Util.ctxItem(gv.el1);
        	return v.varName;
        } else if (element instanceof GlobDef_GF) {
        	GlobDef_GF gf = (GlobDef_GF) element;
        	Func_Func f = (Func_Func)Util.ctxItem(gf.el1);
        	FuncDec_FuncDec fd = (FuncDec_FuncDec) f.el1;
        	return Util.ctxItem(fd.funcName);
    	} else if (element instanceof GlobDef_GI) {
    		GlobDef_GI gi = (GlobDef_GI) element;
    		return Util.ctxItem(gi.el1);
    	}
    	return ""; //$NON-NLS-1$
    }
	
	public DataType getDataType(Object element) {
		LSLType testType = null;
		if (element instanceof LSLType) {
			testType = (LSLType)element;
			
		} else if (element instanceof GlobDef_GV) {
	    	GlobDef_GV gv = (GlobDef_GV) element;
	    	Var_Var v = (Var_Var)Util.ctxItem(gv.el1);
	    	testType = v.varType;
	    	
	    } else if (element instanceof GlobDef_GF) {
	    	GlobDef_GF gf = (GlobDef_GF) element;
	    	Func_Func f = (Func_Func)Util.ctxItem(gf.el1);
	    	FuncDec_FuncDec fd = (FuncDec_FuncDec) f.el1;
	    	testType = fd.funcType;
	    	
	    } else {
	    	return null;
	    }
		
		if(testType instanceof LSLType_LLFloat) {
			return DataType.FLOAT;
		} else if(testType instanceof LSLType_LLInteger) {
			return DataType.INTEGER;
		} else if(testType instanceof LSLType_LLKey) {
			return DataType.KEY;
		} else if(testType instanceof LSLType_LLList) {
			return DataType.LIST;
		} else if(testType instanceof LSLType_LLRot) {
			return DataType.ROTATION;
		} else if(testType instanceof LSLType_LLString) {
			return DataType.STRING;
		} else if(testType instanceof LSLType_LLVector) {
			return DataType.VECTOR;
		} else if(testType instanceof LSLType_LLVoid) {
			return DataType.VOID;
		}
		
		return null;
	}

    private TextPosition getTextPosition(Maybe<SourceContext> m, IDocument d) {
        try {
            if (m instanceof Maybe_Just) {
                Maybe_Just<SourceContext> j = (Maybe_Just<SourceContext>) m;
                SourceContext_SourceContext sc = (SourceContext_SourceContext) j.el1;
                TextLocation_TextLocation tl = (TextLocation_TextLocation) sc.srcTextLocation;
                int start = d.getLineOffset(tl.textLine0 - 1);
                int end = (tl.textLine1 == d.getNumberOfLines()) ? d.getLength() : d.getLineOffset(tl.textLine1);

                return  new TextPosition(start, end);
            }
        } catch (BadLocationException e) {
            Util.error(e, "can't calculate text position"); //$NON-NLS-1$
        }
        
        return null;
    }
	
}
