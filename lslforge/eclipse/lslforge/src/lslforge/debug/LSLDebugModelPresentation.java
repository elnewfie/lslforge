package lslforge.debug;

import java.util.HashSet;

import org.eclipse.debug.core.DebugException;
import org.eclipse.debug.core.model.IValue;
import org.eclipse.debug.ui.IDebugModelPresentation;
import org.eclipse.debug.ui.ISourcePresentation;
import org.eclipse.debug.ui.IValueDetailListener;
import org.eclipse.jface.viewers.ILabelProviderListener;
import org.eclipse.swt.graphics.Image;
import org.eclipse.ui.IEditorInput;

public class LSLDebugModelPresentation implements IDebugModelPresentation {
    private ISourcePresentation presentation = new LSLSourceLocator();
    private HashSet<ILabelProviderListener> listeners = new HashSet<ILabelProviderListener>();
    public void computeDetail(IValue value, IValueDetailListener listener) {
        String result;
        try {
            result = value.getValueString();
        } catch (DebugException e) {
            result = ""; //$NON-NLS-1$
        }
        listener.detailComputed(value, result);
    }

    public Image getImage(Object element) {
        return null;
    }

    public String getText(Object element) {
        return null;
    }

    public void setAttribute(String attribute, Object value) {
    }

    public void addListener(ILabelProviderListener listener) {
        listeners.add(listener);
    }

    public void dispose() {
    }

    public boolean isLabelProperty(Object element, String property) {
        return false;
    }

    public void removeListener(ILabelProviderListener listener) {
        listeners.remove(listener);
    }

    public String getEditorId(IEditorInput input, Object element) {
        return presentation.getEditorId(input, element);
    }

    public IEditorInput getEditorInput(Object element) {
        return presentation.getEditorInput(element);
    }

}
