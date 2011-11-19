package lslforge.editor;

import java.util.Iterator;
import java.util.LinkedList;

import lslforge.LSLForgePlugin;
import lslforge.gentree.Node;
import lslforge.sim.SimProject;
import lslforge.sim.SimProjectNodes;
import lslforge.sim.SimProject.EventHandlerNode;

import org.eclipse.jface.viewers.ITableLabelProvider;
import org.eclipse.jface.viewers.LabelProvider;
import org.eclipse.swt.graphics.Image;

public class SimProjectLabelProvider extends LabelProvider implements ITableLabelProvider {
    private LinkedList<Image> images;
    private Image objectImage = createImage("icons/object.gif"); //$NON-NLS-1$
    private Image primImage = createImage("icons/prim.gif"); //$NON-NLS-1$
    private Image avatarImage = createImage("icons/avatar.gif"); //$NON-NLS-1$
    private Image avatarRefImage = createImage("icons/avatar-ref.gif"); //$NON-NLS-1$
    private Image scriptImage = createImage("icons/obj16/lslforge.gif"); //$NON-NLS-1$
    private Image valImage = createImage("icons/valimage.gif"); //$NON-NLS-1$
    private Image worldImage = createImage("icons/world.gif"); //$NON-NLS-1$
    private Image propertiesImage = createImage("icons/properties.gif"); //$NON-NLS-1$
    private Image gridPositionImage = createImage("icons/grid-position.gif"); //$NON-NLS-1$
    private Image xPositionImage = createImage("icons/x-position.gif"); //$NON-NLS-1$
    private Image yPositionImage = createImage("icons/y-position.gif"); //$NON-NLS-1$
    private Image zPositionImage = createImage("icons/z-position.gif"); //$NON-NLS-1$
    private Image notecardImage = createImage("icons/notecard.gif"); //$NON-NLS-1$
    private Image animationImage = createImage("icons/animation.gif"); //$NON-NLS-1$
    private Image gestureImage = createImage("icons/gesture.gif"); //$NON-NLS-1$
    private Image clothingImage = createImage("icons/clothing.gif"); //$NON-NLS-1$
    private Image soundImage = createImage("icons/sound.gif"); //$NON-NLS-1$
    private Image textureImage = createImage("icons/texture.gif"); //$NON-NLS-1$
    private Image landmarkImage = createImage("icons/landmark.gif"); //$NON-NLS-1$
    private Image bodypartImage = createImage("icons/bodypart.gif"); //$NON-NLS-1$
    private Image createImage(String path) {
        if (images == null) images = new LinkedList<Image>();
        Image i = LSLForgePlugin.createImage(path);
        if (i != null) images.add(i);
        return i;
    }
    

    public Image getColumnImage(Object element, int columnIndex) {
        if (columnIndex > 0) return null;
        if (element instanceof SimProject.ObjectNode) {
            return objectImage;
        } else if (element instanceof SimProject.PrimNode) {
            return primImage;
        } else if (element instanceof SimProject.AvatarNode) {
            return avatarImage;
        } else if (element instanceof SimProject.ScriptNode) {
            return scriptImage;
        } else if (element instanceof SimProject.WorldNode) {
            return worldImage;
        } else if (element instanceof SimProject.PrimPropertiesNode ||
                   element instanceof SimProject.AvatarPropertiesNode ||
                   element instanceof SimProjectNodes.InventoryPropertiesNode) {
            return propertiesImage;
        } else if (element instanceof SimProject.GridPositionNode) {
            return gridPositionImage;
        } else if (element instanceof SimProject.GridCoordinateNode) {
            SimProject.GridCoordinateNode n = (SimProject.GridCoordinateNode) element;
            if (n.getName().startsWith("x")) return xPositionImage; //$NON-NLS-1$
            if (n.getName().startsWith("y")) return yPositionImage; //$NON-NLS-1$
            if (n.getName().startsWith("z")) return zPositionImage; //$NON-NLS-1$
            return valImage;
        } else if (element instanceof SimProject.AvatarReferenceNode) {
            return avatarRefImage;
        } else if (element instanceof SimProjectNodes.NotecardNode) {
            return notecardImage;
        } else if (element instanceof SimProjectNodes.ClothingNode) {
            return clothingImage;
        } else if (element instanceof SimProjectNodes.AnimationNode) {
            return animationImage;
        } else if (element instanceof SimProjectNodes.SoundNode) {
            return soundImage;
        } else if (element instanceof SimProjectNodes.GestureNode) {
            return gestureImage;
        } else if (element instanceof SimProjectNodes.TextureNode) {
            return textureImage;
        } else if (element instanceof SimProjectNodes.LandmarkNode) {
            return landmarkImage;
        } else if (element instanceof SimProjectNodes.BodyPartNode) {
            return bodypartImage;
        } else if (element instanceof SimProjectNodes.InventoryObjectNode) {
            return objectImage;
        } else if (element instanceof EventHandlerNode) {
            return scriptImage;
        } else {
            // TODO: add other node types
            return valImage;
        }
    }

    public String getColumnText(Object element, int columnIndex) {
        if (columnIndex == 0) return ((Node)element).getNameDisplay();
        else if (columnIndex == 1) return ((Node)element).getValue().toString();
        return null;
    }

    @Override
	public void dispose() {
        super.dispose();
        for (Iterator<Image> i = images.iterator(); i.hasNext(); ) {
            Image img = i.next();
            img.dispose();
        }
        
        images.clear();
    }
}
