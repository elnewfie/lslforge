package lslforge.simview;

import java.util.LinkedList;

import lslforge.LslForgePlugin;
import lslforge.sim.SimStatuses;

import org.eclipse.jface.viewers.ILabelProviderListener;
import org.eclipse.jface.viewers.ITableLabelProvider;
import org.eclipse.swt.graphics.Image;

public class LogViewerLabelProvider implements ITableLabelProvider {
    
    private LinkedList<ILabelProviderListener> listeners = new LinkedList<ILabelProviderListener>();
    private LinkedList<Image> images = new LinkedList<Image>();
    private Image infoImg = createImage("icons/info_st_obj.gif"); //$NON-NLS-1$
    
    public Image getColumnImage(Object element, int columnIndex) {
        if (element instanceof SimStatuses.Message) {
            SimStatuses.Message message = (SimStatuses.Message) element;
            if (columnIndex == 2) {
                if (SimStatuses.Message.INFO_LEVEL.equals(message.getLevel())) {
                    return infoImg;
                }
            }
        }
        return null;
    }

    private Image createImage(String path) {
        // TODO Auto-generated method stub
        Image image = LslForgePlugin.createImage(path);
        images.add(image);
        return image;
    }

    public String getColumnText(Object element, int columnIndex) {
        if (element instanceof SimStatuses.Message) {
            SimStatuses.Message message = (SimStatuses.Message) element;
            if (columnIndex == 0) 
                return SimWatcherViewPart.formatTime(Integer.parseInt(message.getTime()));
            else if (columnIndex == 1) return message.getSource();
            else return message.getText();
        } else if ("archive".equals(element)) { //$NON-NLS-1$
            return "archive"; //$NON-NLS-1$
        } else {
            return ""; //$NON-NLS-1$
        }
    }

    public void addListener(ILabelProviderListener listener) {
        listeners.add(listener);
    }

    public void dispose() {
        listeners.clear();
        
        for (Image element : images) {
            Image img = element;
            img.dispose();
        }
        
        images.clear();
    }

    public boolean isLabelProperty(Object element, String property) {
        return false;
    }

    public void removeListener(ILabelProviderListener listener) {
        listeners.remove(listener);
    }
}
