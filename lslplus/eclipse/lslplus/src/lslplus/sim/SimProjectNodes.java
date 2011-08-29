package lslplus.sim;

import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;

import lslplus.gentree.Node;
import lslplus.gentree.NodeFactory;
import lslplus.gentree.NodeStatus;
import lslplus.sim.SimProject.AvatarNode;
import lslplus.sim.SimProject.HexIntNode;
import lslplus.sim.SimProject.AvatarReferenceNode;
import lslplus.sim.SimProject.FixedFormatNode;
import lslplus.sim.SimProject.HasDerivedValue;
import lslplus.sim.SimProject.PrimNode;
import lslplus.sim.SimProject.StringNode;
import lslplus.sim.SimWorldDef.InventoryItem;
import lslplus.sim.SimWorldDef.ItemPermissions;
import lslplus.sim.SimWorldDef.LVector;
import lslplus.sim.SimWorldDef.Prim;
import lslplus.sim.SimWorldDef.Region;
import lslplus.sim.SimWorldDef.Landmark;

public class SimProjectNodes {

    public static class InventoryPropertiesNode extends FixedFormatNode {

        private static final String BASEP = "basep"; //$NON-NLS-1$
		private static final String OWNERP = "ownerp"; //$NON-NLS-1$
		private static final String GROUPP = "groupp"; //$NON-NLS-1$
		private static final String NEXTP = "nextp"; //$NON-NLS-1$
		private static final String EVERYBODYP = "everybodyp"; //$NON-NLS-1$
		public InventoryPropertiesNode(Node parent, String creator) {
            super(parent, "properties", null); //$NON-NLS-1$
            addChild(new AvatarReferenceNode(this, "creator", creator)); //$NON-NLS-1$
            addBasep();
            addOwnerp();
            addGroupp();
            addEverybodyp();
            addNextp();
        }

		private void addNextp() {
			addChild(new HexIntNode(this,NEXTP, 0x7fffffff, "Next Owner Permissions"));
		}

		private void addEverybodyp() {
			addChild(new HexIntNode(this,EVERYBODYP, 0x7fffffff, "Everybody Permissions"));
		}

		private void addGroupp() {
			addChild(new HexIntNode(this,GROUPP, 0x7fffffff, "Group Permissions"));
		}

		private void addOwnerp() {
			addChild(new HexIntNode(this,OWNERP, 0x7fffffff, "Owner Permissions"));
		}

		private void addBasep() {
			addChild(new HexIntNode(this,BASEP, 0x7fffffff, "Base Permissions"));
		}
		
		@Override
		protected void onFix() {
			super.onFix();
			
			if (this.findChildByName(BASEP) == null) addBasep();
			if (this.findChildByName(OWNERP) == null) addOwnerp();
			if (this.findChildByName(GROUPP) == null) addGroupp();
			if (this.findChildByName(EVERYBODYP) == null) addEverybodyp();
			if (this.findChildByName(NEXTP) == null) addNextp();
		}
		
        public static SimWorldDef.ItemPermissions mkPerms(Map<String,Object> data) {
        	int base = (Integer) data.get(BASEP);
        	int ownerp = (Integer) data.get(OWNERP);
        	int groupp = (Integer) data.get(GROUPP);
        	int everybodyp = (Integer) data.get(EVERYBODYP);
        	int nextp = (Integer) data.get(NEXTP);
        	return new SimWorldDef.ItemPermissions(base,ownerp,groupp,everybodyp,nextp);
        }
        
        public String getNameDisplay() { return "Properties"; } //$NON-NLS-1$ TODO
        public Map<String,Object> getData() {
            HashMap<String,Object> map = new HashMap<String,Object>();
            for (Iterator<Node> i = this.getChildren().iterator(); i.hasNext();) {
                Node n = i.next();
                
                String name = n.getName();
                if (n instanceof AvatarReferenceNode) {
                    map.put(name, n.getValueString());
                } else if (n instanceof HasDerivedValue) {
                    map.put(name, ((HasDerivedValue)n).getDerivedValue());
                } else {
                    map.put(name, n.getValue());
                }
            }
            
            return map;
        }

    }

    public static abstract class InventoryNode extends Node {
        private static final NodeStatus SCRIPT_NAME_IN_USE = 
        	new NodeStatus(false, "Script name already in use"); //$NON-NLS-1$ TODO
        
        public InventoryNode(Node parent, String nodeName, Object value) {
            super(parent, nodeName, value);
            String owner = null;
            if (parent instanceof AvatarNode) {
                owner = ((AvatarNode)parent).getName();
            } else {
                PrimNode node = (PrimNode) parent;
                owner = node.getOwner();
            }
            addChild(new InventoryPropertiesNode(this, owner));
        }
        
        protected void addProperty(Node n) {
            InventoryPropertiesNode pn = (InventoryPropertiesNode) findChildByName("properties"); //$NON-NLS-1$
            pn.addChild(n);
        }
        
        public NodeStatus checkNameString(String name) {
            return SimProject.checkNameUnique(this, name, getParent().getChildren(), SCRIPT_NAME_IN_USE);
        }

        protected Map<String,Object> getProperties() {
            InventoryPropertiesNode node = (InventoryPropertiesNode) findChildByName("properties"); //$NON-NLS-1$
            return node.getData();
        }
        
        public abstract InventoryItem getInventoryItem();
        
        public String getValueString() {
            return ""; //$NON-NLS-1$
        }

        public boolean isDeletable() {
            return true;
        }

        public boolean isNameChangeable() {
            return true;
        }

        public boolean isValueChangeable() {
            return false;
        }

        protected void onUpdate(String s) {
        }
        
        public NodeStatus checkValueString(String s) {
            return NodeStatus.OK;
        }

    }
    
    public static class NotecardLineNode extends StringNode {

        public NotecardLineNode(Node parent, String value) {
            super(parent, "line", value); //$NON-NLS-1$
        }

        public boolean isDeletable() {
            return true;
        }
    }
 
    static final NodeFactory notecardLineFactory = new NodeFactory() {

        public Node createNode(Node parent) {
            return new NotecardLineNode(parent,""); //$NON-NLS-1$
        }

        public String getNodeTypeName() {
            return "Line"; //$NON-NLS-1$ TODO
        }
        
    };

    static final NodeFactory notecardFactory = new NodeFactory() {

        public Node createNode(Node parent) {
            PrimNode node = (PrimNode) parent;
            List<Node> nodes = node.getChildren();
            String name = SimProject.computeNewName(nodes, "New Notecard"); //$NON-NLS-1$ TODO
            
            return new NotecardNode(parent, name, null);
        }

        public String getNodeTypeName() {
            return "Notecard"; //$NON-NLS-1$ TODO
        }
    };
    
    public static class NotecardNode extends InventoryNode {
        private static final NodeFactory[] LEGAL_CHILD_NODES = { notecardLineFactory };
        public NotecardNode(Node parent, String nodeName, Object value) {
            super(parent, nodeName, value);
        }

        public NodeFactory[] legalChildNodes() {
            return LEGAL_CHILD_NODES;
        }

        public InventoryItem getInventoryItem() {
            Map<String,Object> props = getProperties();
            String creator = (String) props.get("creator"); //$NON-NLS-1$
            ItemPermissions perms = InventoryPropertiesNode.mkPerms(props);
            List<Node> lineNodes = findChildrenByType(NotecardLineNode.class);
            String[] lines = new String[lineNodes.size()];
            int j = 0;
            for (Iterator<Node> i = lineNodes.iterator(); i.hasNext(); ) {
                lines[j++] = ((NotecardLineNode)i.next()).getValueString();  
            }
            return new SimWorldDef.Notecard(getName(), creator, lines, perms);
        }
    }
    
    static final NodeFactory textureFactory = new NodeFactory() {

        public Node createNode(Node parent) {
            PrimNode node = (PrimNode) parent;
            List<Node> nodes = node.getChildren();
            String name = SimProject.computeNewName(nodes, "New Texture"); //$NON-NLS-1$ TODO
            
            return new TextureNode(parent, name, null);
        }

        public String getNodeTypeName() {
            return "Texture"; //$NON-NLS-1$ TODO
        }
    };
    
    public static class TextureNode extends InventoryNode {

        public TextureNode(Node parent, String nodeName, Object value) {
            super(parent, nodeName, value);
        }

        public NodeFactory[] legalChildNodes() { return SimProject.EMPTY_FACTORY_LIST; }
        
        public InventoryItem getInventoryItem() {
            Map<String,Object> props = getProperties();
            String creator = (String) props.get("creator"); //$NON-NLS-1$
            ItemPermissions perms = InventoryPropertiesNode.mkPerms(props);
            return new SimWorldDef.Texture(getName(), creator, perms);
        }
    }
    
    static final NodeFactory bodyPartFactory = new NodeFactory() {

        public Node createNode(Node parent) {
            PrimNode node = (PrimNode) parent;
            List<Node> nodes = node.getChildren();
            String name = SimProject.computeNewName(nodes, "New Body Part"); //$NON-NLS-1$ TODO
            
            return new BodyPartNode(parent, name, null);
        }

        public String getNodeTypeName() {
            return "Body Part"; //$NON-NLS-1$ TODO
        }
    };

    public static class BodyPartNode extends InventoryNode {

        public BodyPartNode(Node parent, String nodeName, Object value) {
            super(parent, nodeName, value);
        }

        public NodeFactory[] legalChildNodes() { return SimProject.EMPTY_FACTORY_LIST; }
        
        public InventoryItem getInventoryItem() {
            Map<String,Object> props = getProperties();
            String creator = (String) props.get("creator"); //$NON-NLS-1$
            ItemPermissions perms = InventoryPropertiesNode.mkPerms(props);
            return new SimWorldDef.BodyPart(getName(), creator, perms);
        }
    }
    
    static final NodeFactory gestureFactory = new NodeFactory() {

        public Node createNode(Node parent) {
            PrimNode node = (PrimNode) parent;
            List<Node> nodes = node.getChildren();
            String name = SimProject.computeNewName(nodes, "New Gesture"); //$NON-NLS-1$ TODO
            
            return new GestureNode(parent, name, null);
        }

        public String getNodeTypeName() {
            return "Gesture"; //$NON-NLS-1$ TODO
        }
    };

    public static class GestureNode extends InventoryNode {

        public GestureNode(Node parent, String nodeName, Object value) {
            super(parent, nodeName, value);
        }

        public NodeFactory[] legalChildNodes() { return SimProject.EMPTY_FACTORY_LIST; }
        
        public InventoryItem getInventoryItem() {
            Map<String,Object> props = getProperties();
            String creator = (String) props.get("creator"); //$NON-NLS-1$
            ItemPermissions perms = InventoryPropertiesNode.mkPerms(props);
            return new SimWorldDef.Gesture(getName(), creator, perms);
        }
    }
    
    static final NodeFactory clothingFactory = new NodeFactory() {

        public Node createNode(Node parent) {
            PrimNode node = (PrimNode) parent;
            List<Node> nodes = node.getChildren();
            String name = SimProject.computeNewName(nodes, "New Clothing"); //$NON-NLS-1$ TODO
            
            return new ClothingNode(parent, name, null);
        }

        public String getNodeTypeName() {
            return "Clothing"; //$NON-NLS-1$ TODO
        }
    };

    public static class ClothingNode extends InventoryNode {

        public ClothingNode(Node parent, String nodeName, Object value) {
            super(parent, nodeName, value);
        }

        public NodeFactory[] legalChildNodes() { return SimProject.EMPTY_FACTORY_LIST; }
        
        public InventoryItem getInventoryItem() {
            Map<String,Object> props = getProperties();
            String creator = (String) props.get("creator"); //$NON-NLS-1$
            ItemPermissions perms = InventoryPropertiesNode.mkPerms(props);
            return new SimWorldDef.Clothing(getName(), creator, perms);
        }
    }

    static final NodeFactory soundFactory = new NodeFactory() {

        public Node createNode(Node parent) {
            PrimNode node = (PrimNode) parent;
            List<Node> nodes = node.getChildren();
            String name = SimProject.computeNewName(nodes, "New Sound"); //$NON-NLS-1$ TODO
            
            return new SoundNode(parent, name, null);
        }

        public String getNodeTypeName() {
            return "Sound"; //$NON-NLS-1$ TODO
        }
    };

    public static class SoundNode extends InventoryNode {

        public SoundNode(Node parent, String nodeName, Object value) {
            super(parent, nodeName, value);
            addProperty(new SimProject.ConstrainedFloatNode(this, "duration", //$NON-NLS-1$
            		new Float(1.0f), 0.0f, 10f, "Duration")); //$NON-NLS-1$ TODO
        }

        public NodeFactory[] legalChildNodes() { return SimProject.EMPTY_FACTORY_LIST; }
        
        public InventoryItem getInventoryItem() {
            Map<String,Object> props = getProperties();
            String creator = (String) props.get("creator"); //$NON-NLS-1$
            Float duration = (Float) props.get("duration"); //$NON-NLS-1$
            ItemPermissions perms = InventoryPropertiesNode.mkPerms(props);
            return new SimWorldDef.Sound(getName(), creator, duration.floatValue(),
            		perms);
        }
    }
    
    static final NodeFactory animationFactory = new NodeFactory() {

        public Node createNode(Node parent) {
            PrimNode node = (PrimNode) parent;
            List<Node> nodes = node.getChildren();
            String name = SimProject.computeNewName(nodes, "New Animation"); //$NON-NLS-1$ TODO
            
            return new AnimationNode(parent, name, null);
        }

        public String getNodeTypeName() {
            return "Animation"; //$NON-NLS-1$ TODO
        }
    };

    public static class AnimationNode extends InventoryNode {

        public AnimationNode(Node parent, String nodeName, Object value) {
            super(parent, nodeName, value);
            addProperty(new SimProject.ConstrainedFloatNode(this, "duration", //$NON-NLS-1$
            		new Float(1.0f), 0.0f, 120f, "Duration")); //$NON-NLS-1$ TODO
        }

        public NodeFactory[] legalChildNodes() { return SimProject.EMPTY_FACTORY_LIST; }
        
        public InventoryItem getInventoryItem() {
            Map<String,Object> props = getProperties();
            String creator = (String) props.get("creator"); //$NON-NLS-1$
            Float duration = (Float) props.get("duration"); //$NON-NLS-1$
            ItemPermissions perms = InventoryPropertiesNode.mkPerms(props);
            return new SimWorldDef.Animation(getName(), creator, duration.floatValue(),
            		perms);
        }
    }
    
    static final NodeFactory landmarkFactory = new NodeFactory() {

        public Node createNode(Node parent) {
            PrimNode node = (PrimNode) parent;
            List<Node> nodes = node.getChildren();
            String name = SimProject.computeNewName(nodes, "New Landmark"); //$NON-NLS-1$ TODO
            
            return new LandmarkNode(parent, name, null);
        }

        public String getNodeTypeName() {
            return "Landmark"; //$NON-NLS-1$ TODO
        }
    };
    
    public static class LandmarkNode extends InventoryNode {

        public LandmarkNode(Node parent, String nodeName, Object value) {
            super(parent, nodeName, value);
            addProperty(new SimProject.GridPositionNode(this,"position")); //$NON-NLS-1$
            addProperty(new SimProject.RegionNode(this,"region", "Region"));  //$NON-NLS-1$//$NON-NLS-2$
        }

        public InventoryItem getInventoryItem() {
            Map<String,Object> props = getProperties();
            Region region = (Region) props.get("region"); //$NON-NLS-1$
            LVector position = (LVector) props.get("position"); //$NON-NLS-1$
            String creator = (String) props.get("creator"); //$NON-NLS-1$
            ItemPermissions perms = InventoryPropertiesNode.mkPerms(props);
            return new Landmark(getName(), creator, region, position, perms);
        }

        public NodeFactory[] legalChildNodes() {
            return SimProject.EMPTY_FACTORY_LIST; 
        }
    }
    
    static final NodeFactory inventoryObjectFactory = new NodeFactory() {

        public Node createNode(Node parent) {
            PrimNode node = (PrimNode) parent;
            List<Node> nodes = node.getChildren();
            String name = SimProject.computeNewName(nodes, "Object"); //$NON-NLS-1$ TODO
            
            InventoryObjectNode object = new InventoryObjectNode(parent, name, null);
            
            PrimNode rootPrim = (PrimNode) SimProject.primNodeFactory.createNode(node);
            rootPrim.setName(name);
            object.addChild(rootPrim);
            return object;
        }

        public String getNodeTypeName() {
            return "Inventory Object"; //$NON-NLS-1$ TODO
        }
    };
    
    public static class InventoryObjectNode extends InventoryNode {
        private static final NodeFactory[] LEGAL_CHILDREN = { SimProject.primNodeFactory };
        public InventoryObjectNode(Node parent, String nodeName, Object value) {
            super(parent, nodeName, value);
        }

        public InventoryItem getInventoryItem() {
            List<Node> primNodes = findChildrenByType(PrimNode.class);
            Prim prims[] = new Prim[primNodes.size()];
            int j = 0;
            for (Iterator<Node> i = primNodes.iterator(); i.hasNext(); ) {
                prims[j++] = (Prim) ((PrimNode)i.next()).getDerivedValue();
            }
            
            Map<String,Object> props = getProperties();
            String creator = (String) props.get("creator"); //$NON-NLS-1$
            ItemPermissions perms = InventoryPropertiesNode.mkPerms(props);
            return new SimWorldDef.InventoryObject(getName(),creator,prims,perms);
        }

        public NodeFactory[] legalChildNodes() {
            return LEGAL_CHILDREN;
        }
    }
   
}
