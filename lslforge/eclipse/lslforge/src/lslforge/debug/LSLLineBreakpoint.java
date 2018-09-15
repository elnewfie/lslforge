package lslforge.debug;

import java.util.HashMap;
import java.util.Map;

import org.eclipse.core.resources.IMarker;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.resources.IWorkspaceRunnable;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.debug.core.DebugException;
import org.eclipse.debug.core.DebugPlugin;
import org.eclipse.debug.core.model.IBreakpoint;
import org.eclipse.debug.core.model.LineBreakpoint;

public class LSLLineBreakpoint extends LineBreakpoint {

    public static final String MARKER_ID = "lslforge.lslLineBreakpointMarker"; //$NON-NLS-1$

    public LSLLineBreakpoint() { }
    public LSLLineBreakpoint(final IResource resource, final int line) throws DebugException {
        IWorkspaceRunnable wr= new IWorkspaceRunnable() {
            public void run(IProgressMonitor monitor) throws CoreException {
    
                // create the marker
                setMarker(resource.createMarker(MARKER_ID));
                
                Map<String,Object> attributes = new HashMap<String,Object>();
                attributes.put(IBreakpoint.ID, getModelIdentifier());
                attributes.put(IBreakpoint.ENABLED, Boolean.TRUE);
                attributes.put(IMarker.LINE_NUMBER, Integer.valueOf(line));
                // add attributes
                // set attributes
                ensureMarker().setAttributes(attributes);
                
                // add to breakpoint manager if requested
                register();
            }
        };
        run(getMarkerRule(resource), wr);
    }
    public String getModelIdentifier() {
        return LSLDebugTarget.LSLFORGE;
    }

    protected void register() throws CoreException {
        DebugPlugin plugin = DebugPlugin.getDefault();
        if (plugin != null) {
            plugin.getBreakpointManager().addBreakpoint(this);
        } else {
            setRegistered(false);
        }
    }   
}
