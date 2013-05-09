package lslforge;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import lslforge.cserver.CompilationServer;
import lslforge.cserver.CompilationServer.Result;
import lslforge.generated.CompilationCommand_Init;
import lslforge.generated.CompilationCommand_RemoveScript;
import lslforge.generated.CompilationCommand_UpdateScript;
import lslforge.generated.CompilationResponse;
import lslforge.generated.CompilationResponse_FullSourceValidation;
import lslforge.generated.CompilationStatus;
import lslforge.generated.CompilationStatus_CompilationStatus;
import lslforge.generated.EPSummary;
import lslforge.generated.EPSummary_EPSummary;
import lslforge.generated.Either_Left;
import lslforge.generated.Either_Right;
import lslforge.generated.ErrInfo;
import lslforge.generated.ErrInfo_ErrInfo;
import lslforge.generated.GlobalSummary;
import lslforge.generated.LSLType;
import lslforge.generated.Maybe_Just;
import lslforge.generated.TextLocation;
import lslforge.generated.TextLocation_TextLocation;
import lslforge.generated.Tuple2;
import lslforge.generated.Tuple3;
import lslforge.language_metadata.LSLParam;
import lslforge.util.Log;
import lslforge.util.Util;
import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IMarker;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IProjectDescription;
import org.eclipse.core.resources.IProjectNature;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.resources.IResourceChangeEvent;
import org.eclipse.core.resources.IResourceChangeListener;
import org.eclipse.core.resources.IResourceDelta;
import org.eclipse.core.resources.IResourceDeltaVisitor;
import org.eclipse.core.resources.IResourceVisitor;
import org.eclipse.core.resources.ResourcesPlugin;
import org.eclipse.core.resources.WorkspaceJob;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IPath;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;

/**
 * Represents LSLForge projects.  LSL modules, scripts and tests are intended
 * to be created within the context of an LSLForge project, and and LSLForge
 * project is simply a basic Eclipse project with an LSL Project Nature associated
 * with it.
 * @author rgreayer
 *
 */
public class LSLProjectNature implements IProjectNature, IResourceChangeListener {
	public static final String OPTIMIZE = "optimize"; //$NON-NLS-1$

	public static interface RecompileListener {
		public void recompile();
	}
	
    private static class BetterDeltaVisitor implements IResourceDeltaVisitor {
		private boolean recompileAll = false;
		private final LinkedList<IResource> newDerivedResources = new LinkedList<IResource>();
		private final LinkedList<LSLForgeElement> addsAndUpdates = new LinkedList<LSLForgeElement>();
		private final LinkedList<LSLForgeElement> removals = new LinkedList<LSLForgeElement>();
		private final IProject project;
		
		public BetterDeltaVisitor(IProject p) {
			project = p;
		}
		
		public List<LSLForgeElement> getAddsAndUpdates() {
			return addsAndUpdates;
		}
		
		public List<LSLForgeElement> getRemovals() {
			return removals;
		}
		
		public List<IResource> getNewDerivedResources() {
			return newDerivedResources;
		}
		
		public boolean isRecompileAll() {
			return recompileAll;
		}
		
		public boolean visit(IResourceDelta delta) throws CoreException {
			IResource resource = delta.getResource();
			if (resource != null) {
				if (resource.getProject() != null && !isRelevant(resource.getProject())) {
					return false; // don't continue down this branch...
				}

				LSLForgeElement element = (LSLForgeElement) resource.getAdapter(LSLForgeElement.class);
				LSLDerivedScript script = (LSLDerivedScript) resource.getAdapter(LSLDerivedScript.class);
				
				if (element != null) {
					if (element.isModule()) {
						recompileAll = recompileAll || 
							(delta.getKind() == IResourceDelta.ADDED ||
						     delta.getKind() == IResourceDelta.REMOVED ||
						     (delta.getKind() == IResourceDelta.CHANGED &&
						      ((delta.getFlags() & IResourceDelta.REPLACED) != 0 ||
						       (delta.getFlags() & IResourceDelta.CONTENT) != 0)));
					}  else {
						//Is it a script, or just a derived file?
						if(script == null) {
							if (delta.getKind() == IResourceDelta.ADDED ||
								(delta.getKind() == IResourceDelta.CHANGED &&
										((delta.getFlags() & IResourceDelta.REPLACED) != 0 ||
											    (delta.getFlags() & IResourceDelta.CONTENT) != 0))) {
								addsAndUpdates.add(element);
							} else if (delta.getKind() == IResourceDelta.REMOVED) {
								removals.add(element);
							}
						}
					}
				} else {
					if (script != null && delta.getKind() == IResourceDelta.ADDED) {
						newDerivedResources.add(resource);
					} else if (script == null) {
						IProject p = (IProject) resource.getAdapter(IProject.class);
						if (p != null && delta.getKind() == IResourceDelta.CHANGED &&
								(delta.getFlags() & IResourceDelta.DESCRIPTION) != 0)  {
						    recompileAll = true;
						}
					}
				}
			}
			return true;
		}
		
	    @SuppressWarnings("nls")
		private boolean isRelevant(IProject proj) {
	    	if (proj == project) return true;
	    	try {
				for (IProject p : project.getReferencedProjects()) {
					if (p == proj) return true;
				}
				return false;
			} catch (CoreException e) {
				Log.error("problem getting referenced projects", e);
				return false;
			}

	    }
	}
	
	public static class EntryPointDefinition {
		public String name;
		public LSLParam[] params;
		public String returnType;
	}
	
	public static class NameTypePair {
	    private String name;
	    private String type;
	    public String getName() { return name; }
	    public String getType() { return type; }
	}
	
	public static class ErrorLocation {
		public int columnEnd;
		public int columnStart;
		public int lineEnd;
		public int lineStart;
	}
	
	public static class Item {
		public EntryPointDefinition[] entryPoints;
		public String name;
		public ItemStatus status;
		private NameTypePair[] globals;
        public void setGlobals(NameTypePair[] globals) {
            this.globals = globals;
        }
        public NameTypePair[] getGlobals() {
            if (globals == null) {
                globals = new NameTypePair[0];
            }
            return globals;
        }
	}
	
	public static class ItemStatus {
		//public ErrorLocation errLoc = null;
		//public String msg = null;
	    public ItemError[] errs = null;
		public boolean ok = true;
	}
	
	public static class ItemError {
	    public ErrorLocation errLoc = null;
	    public String msg = null;
	}
	
	private class SourceListBuilder implements IResourceVisitor {
		private static final String IDENTIFIER_BEGIN = "<identifier>"; //$NON-NLS-1$
		private static final String IDENTIFIER_END = "</identifier>"; //$NON-NLS-1$
		private static final String ITEM_BEGIN = "<item>"; //$NON-NLS-1$
		private static final String ITEM_END = "</item>"; //$NON-NLS-1$
		private static final String MODULES_BEGIN = "<modules>"; //$NON-NLS-1$
		private static final String MODULES_END = "</modules>"; //$NON-NLS-1$
		private static final String PATH_BEGIN = "<path>"; //$NON-NLS-1$
		private static final String PATH_END = "</path>"; //$NON-NLS-1$
		private static final String SCRIPTS_BEGIN = "<scripts>"; //$NON-NLS-1$
		private static final String SCRIPTS_END = "</scripts>"; //$NON-NLS-1$
		private static final String SOURCE_LIST_BEGIN = "<source_files>"; //$NON-NLS-1$
		private static final String SOURCE_LIST_END = "</source_files>"; //$NON-NLS-1$
		private final HashMap<String,String> moduleMap = new HashMap<String,String>();
		private final HashMap<String,String> moduleNameToPath = new HashMap<String,String>();
		private final HashMap<String,String> scriptMap = new HashMap<String,String>();
		private final HashMap<String,String> scriptNameToPath = new HashMap<String,String>();
		private final boolean optimize;
		private boolean modulesOnly = true;
		
		public SourceListBuilder(boolean addOptimizeOption) {
		    optimize = addOptimizeOption;
		}
		
		private void buildItemList(StringBuilder buf, Map<String,String> m) {
			//for (Iterator i = m.entrySet().iterator(); i.hasNext();) {
            for (Map.Entry<String, String> e : m.entrySet()) {
				buf.append(ITEM_BEGIN);
				buf.append(IDENTIFIER_BEGIN);
				buf.append(e.getKey());
				buf.append(IDENTIFIER_END);
				buf.append(PATH_BEGIN);
				buf.append(e.getValue());
				buf.append(PATH_END);
				buf.append(ITEM_END);
			}
		}
		
//		public String getModulePath(String name) {
//			return moduleNameToPath.get(name);
//		}
//
//		public String getScriptPath(String name) {
//			return scriptNameToPath.get(name);
//		}
		
		public boolean visit(IResource resource) throws CoreException {
			LSLForgeElement element = (LSLForgeElement) resource.getAdapter(LSLForgeElement.class);
		
			if (element != null) {
				IFile f = (IFile) resource;
				IPath p = f.getLocation();
				IPath pp = f.getProjectRelativePath();
				String name = resourceToLSLForgeName(resource);
				
				if (element.isModule()) {
				    moduleNameToPath.put(name,pp.toString());
				    moduleMap.put(name,p.toOSString());
				} else if (element.isScript() && !modulesOnly) {
					scriptNameToPath.put(name, pp.toString());
					scriptMap.put(name, p.toOSString());
				}
			}
			return true;
		}
		public String xmlDescriptor() {
			StringBuilder buf = new StringBuilder();
			buf.append(SOURCE_LIST_BEGIN);
			if (optimize) buf.append("<optimize>true</optimize>"); //$NON-NLS-1$
			buf.append(MODULES_BEGIN);
			buildItemList(buf, moduleMap);
			buf.append(MODULES_END);
			buf.append(SCRIPTS_BEGIN);
			buildItemList(buf, scriptMap);
			buf.append(SCRIPTS_END);
			buf.append(SOURCE_LIST_END);
			return buf.toString();
		}
		
		public void setModulesOnly(boolean m) {
			this.modulesOnly = m;
		}
	}
	
	public static class Summary {
//		private Item[] modules; 
//		private Item[] scripts;
		private LinkedList<CompilationStatus> modules;
		private LinkedList<CompilationStatus> scripts;
		
		public LinkedList<CompilationStatus> getModules() {
			return modules;
		}
		public LinkedList<CompilationStatus> getScripts() {
			return scripts;
		}
		public void setModules(LinkedList<CompilationStatus> modules) {
			this.modules = modules;
		}
		public void setScripts(LinkedList<CompilationStatus> scripts) {
			this.scripts = scripts;
		}
	}
	
	public static String ID = "lslforge.LSLForgeNature"; //$NON-NLS-1$
	
	private static final String LSLFORGE = "lslforge"; //$NON-NLS-1$

	public static final String LSLFORGE_PROBLEM = "lslforge.problem"; //$NON-NLS-1$

	private final HashSet<RecompileListener> recompListeners = new HashSet<RecompileListener>();
	private Map<String,LinkedList<EPSummary>> entryPoints;
	private Map<String,LinkedList<GlobalSummary>> globalVariables;
	private IProject project;
	
	private Summary summary;

	private static CompilationServer cserver = null;
	public LSLProjectNature() {
		Log.debug("creating project nature"); //$NON-NLS-1$
		ResourcesPlugin.getWorkspace().addResourceChangeListener(this);
		if(cserver == null) cserver = new CompilationServer();
	}
	
	public CompilationServer getCompilationServer() {
	    return cserver;
	}
	
	private synchronized void checkForErrors(boolean recompileAll, List<LSLForgeElement> scriptChanges,
			List<LSLForgeElement> scriptRemovals) {
		
		try {
			if (scriptRemovals != null) {
				for (LSLForgeElement e : scriptRemovals) {
					String name = resourceToLSLForgeName(e.getResource());
					String path = e.getResource().getLocation().toOSString();
					Log.debug("Removing file " + path); //$NON-NLS-1$
					CompilationCommand_RemoveScript cmd = new CompilationCommand_RemoveScript();
					cmd.el1 = new Tuple2<String, String>();
					cmd.el1.el1 = name;
					cmd.el1.el2 = path;
					Result r = cserver.execute(cmd);
					r.get(); // don't care about response...
				}
			}
			
			CompilationResponse response = null;
			
			if (recompileAll || (scriptChanges != null && scriptChanges.size() != 0)) {
				IProject[] ps = project.getReferencedProjects();
				boolean optimize = 
					LSLForgePlugin.getDefault().getPreferenceStore().getBoolean(OPTIMIZE);
				final lslforge.cserver.SourceListBuilder builder = 
					new lslforge.cserver.SourceListBuilder(optimize);
				
				for (IProject p : ps) {
			        if (p.hasNature(LSLProjectNature.ID)) {
			        	p.accept(builder);
			        }
				}
				
				builder.setModulesOnly(false);
				project.accept(builder);
				if (recompileAll) {
					Log.debug("Recompiling all files"); //$NON-NLS-1$
					Tuple3<Boolean, LinkedList<Tuple2<String, String>>, 
						LinkedList<Tuple2<String, String>>> cinfo = builder
							.compilationInfo();
					CompilationCommand_Init cc = new CompilationCommand_Init();
					cc.el1 = cinfo;

					Result r = cserver.execute(cc);

					response = r.get();
				} else {
					for (LSLForgeElement e : scriptChanges) {
						CompilationCommand_UpdateScript cmd = new CompilationCommand_UpdateScript();
						cmd.el1 = new Tuple2<String, String>();
						cmd.el1.el1 = resourceToLSLForgeName(e.getResource());
						cmd.el1.el2 = e.getResource().getLocation().toOSString();

						Log.debug("Compiling file: " + cmd.el1.el2); //$NON-NLS-1$
						
						//RJN
//						Map<String, Object> extra = new HashMap<String, Object>();
//						extra.put("preprocess", new Object());
						
//						Result r = cserver.execute(cmd, extra);
						Result r = cserver.execute(cmd);
						response = r.get(); // wait for response... only care about last one!
					}
				}
				
				if (!(response instanceof CompilationResponse_FullSourceValidation)) {
					Log.error("unexpected response from compilation server"); //$NON-NLS-1$
					return;
				}
				CompilationResponse_FullSourceValidation validation = 
					(CompilationResponse_FullSourceValidation) response;
				summary = new Summary();
				summary.setModules(validation.el1.el1);
				summary.setScripts(validation.el1.el2);
				final HashMap<String, LinkedList<ErrInfo>> map = new HashMap<String, LinkedList<ErrInfo>>();
				synchronized (this) {
					entryPoints = new HashMap<String, LinkedList<EPSummary>>();
					globalVariables = new HashMap<String, LinkedList<GlobalSummary>>();
					for (CompilationStatus status0 : summary.getModules()) {
						CompilationStatus_CompilationStatus status = (CompilationStatus_CompilationStatus) status0;

						map.put(builder.getModulePath(status.csName),
										(status.csInfo instanceof Either_Right) ? new LinkedList<ErrInfo>()
												: ((Either_Left<LinkedList<ErrInfo>, Tuple2<LinkedList<GlobalSummary>, LinkedList<EPSummary>>>) status.csInfo).el1);
						if (status.csInfo instanceof Either_Right) {
							Either_Right<LinkedList<ErrInfo>, Tuple2<LinkedList<GlobalSummary>, LinkedList<EPSummary>>> info = (Either_Right<LinkedList<ErrInfo>, Tuple2<LinkedList<GlobalSummary>, LinkedList<EPSummary>>>) status.csInfo;

							entryPoints.put(status.csName, info.el1.el2);
							globalVariables.put(status.csName, info.el1.el1);
						}
					}

					for (CompilationStatus status0 : summary.getScripts()) {
						CompilationStatus_CompilationStatus status = (CompilationStatus_CompilationStatus) status0;

						map.put(builder.getScriptPath(status.csName),
										(status.csInfo instanceof Either_Right) ? new LinkedList<ErrInfo>()
												: ((Either_Left<LinkedList<ErrInfo>, Tuple2<LinkedList<GlobalSummary>, LinkedList<EPSummary>>>) status.csInfo).el1);
						if (status.csInfo instanceof Either_Right) {
							Either_Right<LinkedList<ErrInfo>, Tuple2<LinkedList<GlobalSummary>, LinkedList<EPSummary>>> info = (Either_Right<LinkedList<ErrInfo>, Tuple2<LinkedList<GlobalSummary>, LinkedList<EPSummary>>>) status.csInfo;

							entryPoints.put(status.csName, info.el1.el2);
							globalVariables.put(status.csName, info.el1.el1);
						}
					}
				}
				project.accept(new IResourceVisitor() {
					public boolean visit(IResource resource)
							throws CoreException {
						hasError(resource, map);
						return true;
					}
				});
			}
		} catch (CoreException e) {
			Log.error(e);
		} catch (Exception e) {
			Log.error(e);
		}
		
		LSLForgePlugin.getDefault().errorStatusChanged();
		
        WorkspaceJob job = new WorkspaceJob(Messages.ProjectNature_REFRESH) {

            @Override
			public IStatus runInWorkspace(IProgressMonitor monitor)
                    throws CoreException {
                project.refreshLocal(IResource.DEPTH_INFINITE, monitor);
                return new Status(IStatus.OK, "lslforge", Messages.ProjectNature_REFRESHED_OK); //$NON-NLS-1$
            }
            
        };
        
        job.schedule(100);
	}
	
	public void configure() throws CoreException {
	}
	
	public void deconfigure() throws CoreException {
	}
	
	public synchronized String[] getEntryPointNames(String fileName) {
		if (fileName == null) return null;
		LinkedList<EPSummary> eps = entryPoints.get(fileName);
		//EntryPointDefinition[] eps = (EntryPointDefinition[]) entryPoints.get(fileName);
		if (eps == null) return new String[0];
		String[] paths = new String[eps.size()];
		int i = 0;
		for (EPSummary s : eps) paths[i++] = ((EPSummary_EPSummary)s).epName;
		Arrays.sort(paths);
		return paths;
	}
	
	
	public LinkedList<Tuple2<String,LSLType>> getParams(String fileName, final String entryPointName) {
		LinkedList<EPSummary> eps = entryPoints.get(fileName);
		if (eps == null) return null;
		
		EPSummary_EPSummary ep = null;
		for (EPSummary s0 : eps) {
			EPSummary_EPSummary s = (EPSummary_EPSummary) s0;
			
			if (s.epName.equals(entryPointName)) {
				ep = s;
				break;
			}
		}
		if (ep == null) return null;
		return ep.epParams;
	}
	
	public LSLType getReturnType(String fileName, final String entryPointName) {
		LinkedList<EPSummary> eps = entryPoints.get(fileName);
		if (eps == null) return null;
		
		EPSummary_EPSummary ep = null;
		for (EPSummary s0 : eps) {
			EPSummary_EPSummary s = (EPSummary_EPSummary) s0;
			
			if (s.epName.equals(entryPointName)) {
				ep = s;
				break;
			}
		}
		if (ep == null) return null;
	    return ep.epType;
	}
	
	public synchronized String[] getLSLFiles() {
		if (entryPoints == null) return new String[0];
		String[] files = entryPoints.keySet().toArray(new String[entryPoints.size()]);
		Arrays.sort(files);
		return files;
	}
	
	public String[] getLSLScripts() {
	    if (entryPoints == null) return new String[0];
	    ArrayList<String> l = new ArrayList<String>();
	    
	    synchronized (this) {
	    	for (String name : entryPoints.keySet()) {
	            if (name.endsWith(".lslp")) { //$NON-NLS-1$
	                l.add(name);
	            }
	    	}
	    }
	    
	    String[] scripts = l.toArray(new String[l.size()]);
	    Arrays.sort(scripts);
	    return scripts;
	}
	
    public List<String> getLSLModules() {
        ArrayList<String> l = new ArrayList<String>();
        if (entryPoints == null) return l;
        
        synchronized (this) {
	    	for (String name : entryPoints.keySet()) {
	            if (name.endsWith(".lslm")) { //$NON-NLS-1$
	                l.add(name);
	            }
	    	}
        }
        
        Collections.sort(l);
        return l;
    }
    
	
	public IProject getProject() {
	    return project;
	}
	
	public Summary getSummary() {
		return summary;
	}
	
	private void hasError(IResource resource, Map<String,LinkedList<ErrInfo>> summary) {
		if (resource instanceof IFile) {
			IFile f = (IFile) resource;
			if (f.exists()) {
				try {
					
					resource.deleteMarkers(LSLFORGE_PROBLEM, true, IResource.DEPTH_ONE);
					String key = resource.getProjectRelativePath().toString();
					
					LinkedList<ErrInfo> status = summary.get(key);
					if (status != null && status.size() > 0) {
						for (ErrInfo ei0 : status) {
							ErrInfo_ErrInfo err = (ErrInfo_ErrInfo) ei0;
                            IMarker i = resource.createMarker(LSLFORGE_PROBLEM);
                            i.setAttribute(IMarker.MESSAGE, err.el2);
                            i.setAttribute(IMarker.SEVERITY,
                                    IMarker.SEVERITY_ERROR);
                            if (err.el1 instanceof Maybe_Just) {
                            	TextLocation_TextLocation errLoc = (TextLocation_TextLocation)
                            		((Maybe_Just<TextLocation>)err.el1).el1;
                                int lineOffset0 = errLoc.textLine0 - 1;
                                int lineOffset1 = errLoc.textLine1 - 1;
                                i.setAttribute(IMarker.LINE_NUMBER,
                                        errLoc.textLine0);
                                int[] offsets = Util.findOffsetsFor(new int[] {
                                        lineOffset0, lineOffset1 }, new int[] {
                                        errLoc.textColumn0 - 1,
                                        errLoc.textColumn1 - 1 }, f);
                                if (offsets != null) {
                                    if (offsets[0] == offsets[1])
                                        offsets[1]++;
                                    i.setAttribute(IMarker.CHAR_START,
                                            offsets[0]);
                                    i
                                            .setAttribute(IMarker.CHAR_END,
                                                    offsets[1]);
                                }
                            }
                        }
					    
                        Log.info("Marked " + key); //$NON-NLS-1$
					}
				} catch (CoreException e) {
					Log.error("error reading file", e); //$NON-NLS-1$
				}
			}
		}
	}
	
	public void resourceChanged(IResourceChangeEvent event) {
	    Log.info("resource changed in " + this.project.getName()); //$NON-NLS-1$
		final IResourceDelta delta = event.getDelta();

		//DeltaVisitor dv = new DeltaVisitor();
		BetterDeltaVisitor dv = new BetterDeltaVisitor(this.project);
		boolean recompileAll = false;
		try {
		    delta.accept(dv);
		    recompileAll = dv.isRecompileAll();
		} catch (CoreException e) {
			recompileAll = true;
			Log.error(e);
		}
		
		if (recompileAll) onRecompAll();
        scheduleBuild(recompileAll, dv.getAddsAndUpdates(), dv.getRemovals());
		
		final List<IResource> newDerivedResources = dv.getNewDerivedResources();
		if (newDerivedResources != null && newDerivedResources.size() > 0) {
			WorkspaceJob job = new WorkspaceJob("MarkDerived") { //$NON-NLS-1$

				@Override
				public IStatus runInWorkspace(IProgressMonitor monitor)
						throws CoreException {
				    for (IResource r : newDerivedResources) {
						r.setDerived(true, monitor);
						if (r instanceof IFile) {
						    ((IFile)r).setCharset("UTF-8", monitor); //$NON-NLS-1$
						}
					}
					
					return new Status(IStatus.OK, LSLFORGE, Messages.ProjectNature_MARK_DERIVED_COMPLETE);
				}
				
			};
			
			job.schedule();
		}
	}

    public void scheduleBuild(final boolean recompileAll, final List<LSLForgeElement> scriptChanges,
    		final List<LSLForgeElement> scriptRemovals) {
    	Log.info("Scheduling build, recompileAll: " + (recompileAll ? "yes" : "no")); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
        WorkspaceJob job = new WorkspaceJob("EvaluateErrors") { //$NON-NLS-1$

        	@Override
			public IStatus runInWorkspace(IProgressMonitor monitor) {
        		checkForErrors(recompileAll, scriptChanges, scriptRemovals);
        		return new Status(IStatus.OK,LSLFORGE, Messages.ProjectNature_OK);
        	}
        	
        };
        
        job.schedule();
    }
	
	public void setProject(IProject project) {
		this.project = project;
		checkForErrors(true, null, null);
	}

	/**
	 * Gets the XML source list descriptor for the project.
	 * @return the LSL source list XML descriptor for the project
	 * @throws CoreException 
	 */
	public String projectSourceList() throws CoreException {
        final SourceListBuilder builder = new SourceListBuilder(false);
        IProject[] ps = project.getReferencedProjects();
        
        for (IProject p : ps) {
        	p.accept(builder);
        }
        builder.setModulesOnly(false);
        project.accept(builder);
        return builder.xmlDescriptor();
	}

    public synchronized LinkedList<GlobalSummary> getGlobalVariables(String fileName) {
    	globalVariables.get(fileName);
        return globalVariables.get(fileName);
    }
    
    public static String resourceToLSLForgeName(IResource r) {
        return r.getProjectRelativePath().toString().replace('/', '.');
    }
    
    public void addRecompileListener(RecompileListener l) {
    	this.recompListeners.add(l);
    }
    
    public void removeRecompileListener(RecompileListener l) {
    	this.recompListeners.remove(l);
    }
    
    private void onRecompAll() {
    	for (RecompileListener l : recompListeners) {
    		l.recompile();
    	}
    }

    /**
     * Inspect the nature(s) associated with the project and, if LSLPlus appears in the list,
     * replace it with LSLForge.
     * @param project
     */
    public static void fixProjectNature(IProject project) {
		try {
			IProjectDescription description = project.getDescription();
			String[] natures = description.getNatureIds();
			
			for(int i = 0; i < natures.length; i++) {
				if(natures[i].equals("lslplus.lslPlusNature")) { //$NON-NLS-1$
					//Replace with the new nature name
					natures[i] = LSLProjectNature.ID;
					break;
				}
			}
			
			description.setNatureIds(natures);
			project.setDescription(description, null);
		} catch (CoreException e) {
			// Something went wrong
		}
    }


    /**
     * Checks if the specified project supports LSLForge plugin functionality.
     * @param project
     * @return true or false
     * @throws CoreException 
     */
    public static boolean hasProjectNature(IProject project) throws CoreException {
    	return project.getDescription().hasNature(LSLProjectNature.ID);
    }
    
    public static void addProjectNature(IProject project) throws CoreException {
    	//Make sure it doesn't exist already, since we dont want to add it twice
    	if(hasProjectNature(project)) return;
    	
    	//Now add it
    	IProjectDescription desc = project.getDescription();
    	String[] natures = desc.getNatureIds();
    	String[] newNatures = new String[natures.length + 1];
    	for(int i = 0; i < natures.length; i++) {
    		newNatures[i] = natures[i];
    	}
    	newNatures[natures.length] = LSLProjectNature.ID;
    	
    	//And save it
    	desc.setNatureIds(newNatures);
    	project.setDescription(desc, null);
    }
    
    public static void removeProjectNature(IProject project) throws CoreException {
    	//Skip it if it doesn't exist
    	if(!hasProjectNature(project)) return;
    	
    	//Now remove it
    	IProjectDescription desc = project.getDescription();
    	
    	List<String> natures = new ArrayList<String>();
    	for(String theNature: desc.getNatureIds()) {
    		if(!LSLProjectNature.ID.equals(theNature)) {
    			natures.add(theNature);
    		}
    	}
    	
    	//And save it
    	String[] newNatures = new String[natures.size()];
    	natures.toArray(newNatures);
    	desc.setNatureIds(newNatures);
    	project.setDescription(desc, null);
    }    
}
