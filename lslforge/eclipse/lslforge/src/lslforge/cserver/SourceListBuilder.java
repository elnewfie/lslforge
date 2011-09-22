package lslforge.cserver;

import java.util.HashMap;
import java.util.LinkedList;
import java.util.Map;

import lslforge.LslForgeElement;
import lslforge.LslProjectNature;
import lslforge.generated.Tuple2;
import lslforge.generated.Tuple3;

import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.resources.IResourceVisitor;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IPath;

public class SourceListBuilder implements IResourceVisitor {
	private HashMap<String,String> moduleMap = new HashMap<String,String>();
	private HashMap<String,String> moduleNameToPath = new HashMap<String,String>();
	private HashMap<String,String> scriptMap = new HashMap<String,String>();
	private HashMap<String,String> scriptNameToPath = new HashMap<String,String>();
	private boolean optimize;
	private boolean modulesOnly = true;
	
	public SourceListBuilder(boolean addOptimizeOption) {
	    optimize = addOptimizeOption;
	}
	
	public String getModulePath(String name) {
		return moduleNameToPath.get(name);
	}

	public String getScriptPath(String name) {
		return scriptNameToPath.get(name);
	}
	
	public boolean visit(IResource resource) throws CoreException {
		LslForgeElement element = (LslForgeElement) resource.getAdapter(LslForgeElement.class);
	
		if (element != null) {
			IFile f = (IFile) resource;
			IPath p = f.getLocation();
			IPath pp = f.getProjectRelativePath();
			String name = LslProjectNature.resourceToLslForgeName(resource);
			
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
	
	public Tuple3<Boolean, 
		LinkedList<Tuple2<String, String>>, 
		LinkedList<Tuple2<String,String>>> compilationInfo() {
		
		Tuple3<Boolean, 
		LinkedList<Tuple2<String, String>>, 
		LinkedList<Tuple2<String,String>>> result = 
			new Tuple3<Boolean, 
				LinkedList<Tuple2<String,String>>, 
				LinkedList<Tuple2<String,String>>>();
		
		result.el1 = optimize;
	
		LinkedList<Tuple2<String,String>> modules = new LinkedList<Tuple2<String,String>>();
		for (Map.Entry<String, String> entry : moduleMap.entrySet()) {
			Tuple2<String,String> tup = new Tuple2<String, String>();
			tup.el1 = entry.getKey();
			tup.el2 = entry.getValue();
			modules.add(tup);
		}
		
		result.el2 = modules;
		LinkedList<Tuple2<String,String>> scripts = new LinkedList<Tuple2<String,String>>();
		for (Map.Entry<String, String> entry : scriptMap.entrySet()) {
			Tuple2<String,String> tup = new Tuple2<String, String>();
			tup.el1 = entry.getKey();
			tup.el2 = entry.getValue();
			scripts.add(tup);
		}
		
		result.el3 = scripts;
		return result;
	}
	
	public void setModulesOnly(boolean val) {
		this.modulesOnly = val;
	}
}
