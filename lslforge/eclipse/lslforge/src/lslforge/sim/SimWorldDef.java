package lslforge.sim;

import java.util.List;
import java.util.Map;

import lslforge.util.ListConverter;
import lslforge.util.MapConverter;

import com.thoughtworks.xstream.XStream;
import com.thoughtworks.xstream.io.xml.DomDriver;
import com.thoughtworks.xstream.mapper.Mapper;

/**
 *
 * Classes used for serializing setup info to haskell core.
 */
public class SimWorldDef {
    public static class Script {
        @SuppressWarnings("unused")
		private String primKey;
        @SuppressWarnings("unused")
		private String scriptName;
        @SuppressWarnings("unused")
		private String scriptId;
        
        public Script(String primKey, String scriptName, String scriptId) {
            this.primKey = primKey;
            this.scriptName = scriptName;
            this.scriptId = scriptId;
        }
    }
    
    public static class ScriptInfo {
    	@SuppressWarnings("unused")
		private String scriptName;
    	@SuppressWarnings("unused")
		private String scriptId;
    	public ScriptInfo(String scriptName, String scriptId) {
    		this.scriptName = scriptName;
    		this.scriptId = scriptId;
    	}
    }
    
    public static abstract class InventoryItem {
        @SuppressWarnings("unused")
		private String name;
        @SuppressWarnings("unused")
		private String creator;
        @SuppressWarnings("unused")
        private ItemPermissions perms;
        public InventoryItem(String name, String creator) {
        	this(name,creator,new 
        			ItemPermissions(0xffffffff,0xffffffff,0xffffffff,0xffffffff,0xffffffff));
        }
        
        public InventoryItem(String name, String creator, ItemPermissions perms) {
            this.name = name;
            this.creator = creator;
            this.perms = perms;
        }
    }
    
    public static class ItemPermissions {
    	@SuppressWarnings("unused")
    	private int base,owner,group,everyone,next;
    	public ItemPermissions(int p0, int p1, int p2, int p3, int p4) {
    		this.base = p0;
    		this.owner = p1;
    		this.group = p2;
    		this.everyone = p3;
    		this.next = p4;
    	}
    }
    
    public static class Notecard extends InventoryItem {
        @SuppressWarnings("unused")
        private String[] lines;
        public Notecard(String name, String creator, String[] lines, ItemPermissions perms) {
            super(name,creator, perms);
            this.lines = lines;
        }
    }
    
    public static class Texture extends InventoryItem {
        public Texture(String name, String creator, ItemPermissions perms) {
            super(name, creator, perms);
        }
    }
    
    public static class Clothing extends InventoryItem {
        public Clothing(String name, String creator, ItemPermissions perms) {
            super(name, creator, perms);
        }
    }
    
    public static class BodyPart extends InventoryItem {
        public BodyPart(String name, String creator, ItemPermissions perms) {
            super(name, creator, perms);
        }
    }
    
    public static class Gesture extends InventoryItem {
        public Gesture(String name, String creator, ItemPermissions perms) {
            super(name, creator, perms);
        }
    }
    
    public static class Animation extends InventoryItem {
        @SuppressWarnings("unused")
        private float duration;

        public Animation(String name, String creator, float duration, ItemPermissions perms) {
            super(name, creator, perms);
            this.duration = duration;
        }
    }
    public static class Sound extends InventoryItem {
        @SuppressWarnings("unused")
        private float duration;

        public Sound(String name, String creator, float duration, ItemPermissions perms) {
            super(name, creator, perms);
            this.duration = duration;
        }
    }
    
    public static class Landmark extends InventoryItem {
        @SuppressWarnings("unused")
        private Region region;
        @SuppressWarnings("unused")
        private LVector position;
        public Landmark(String name, String creator, Region region, 
        		LVector position, ItemPermissions perms) {
            super(name,creator, perms);
            this.position = position;
            this.region = region;
        }
    }
    
    public static class InventoryObject extends InventoryItem {
        @SuppressWarnings("unused")
        private Prim[] prims;
        public InventoryObject(String name, String creator, Prim[] prims, ItemPermissions perms) {
            super(name,creator, perms);
            this.prims = prims;
        }
    }
    
    public static class SimObject {
        @SuppressWarnings("unused")
        private String[] primKeys;
        @SuppressWarnings("unused")
        private LVector position;
        public SimObject(String[] primKeys, LVector position) {
            this.primKeys = primKeys;
            this.position = position;
        }    
    }
    
    public static class Prim {
        @SuppressWarnings("unused")
        private String name;
        private String key;
        @SuppressWarnings("unused")
        private ScriptInfo[] scripts;
        @SuppressWarnings("unused")
        private InventoryItem[] inventory;
        @SuppressWarnings("unused")
        private String description;
        @SuppressWarnings("unused")
        private String owner;
        @SuppressWarnings("unused")
        private LVector position;
        @SuppressWarnings("unused")
        private LVector rotation;
        
        public Prim(String name, String key, ScriptInfo[] scripts, InventoryItem[] inventory,
                String description, String owner, LVector position, LVector rotation) {
            this.name = name;
            this.key = key;
            this.scripts = scripts;
            this.inventory = inventory;
            this.description = description;
            this.owner = owner;
            this.position = position;
            this.rotation = rotation;
        }

        public String getKey() {
            return key;
        }
    }
    
    public static class Avatar {
        @SuppressWarnings("unused")
        private String name;
        @SuppressWarnings("unused")
        private float xPos;
        @SuppressWarnings("unused")
        private float yPos;
        @SuppressWarnings("unused")
        private float zPos;
        @SuppressWarnings("unused")
        private String avatarEventHandler;
        
        public Avatar(String name, float x, float y, float z, String avatarEventHandler) {
            this.name = name;
            this.xPos = x;
            this.yPos = y;
            this.zPos = z;
            this.avatarEventHandler = avatarEventHandler;
        }
    }
    
    public static class LVector {
        private float x,y,z;
        public LVector(float x, float y, float z) {
            this.x = x;
            this.y = y;
            this.z = z;
        }
        public float getX() { return x; }
        public float getY() { return y; }
        public float getZ() { return z; }
    }
    
    public static class LRotation {
        @SuppressWarnings("unused")
        private float x,y,z,s;
        
        public LRotation(float x, float y, float z, float s) {
            this.x = x;
            this.y = y;
            this.z = z;
            this.s = s;
        }
    }
    
    public static class Region {
        @SuppressWarnings("unused")
        private int x,y;
        public Region(int x, int y) { this.x = x; this.y = y; }
    }
    
    private static XStream xstream = new XStream(new DomDriver());
    
    public static void configureXStream(XStream xstream) {
        xstream.setMode(XStream.NO_REFERENCES);
        xstream.alias("world-def", SimWorldDef.class); //$NON-NLS-1$
        xstream.alias("avatar", Avatar.class); //$NON-NLS-1$
        xstream.alias("script", ScriptInfo.class); //$NON-NLS-1$
        xstream.alias("object", SimObject.class); //$NON-NLS-1$
        xstream.alias("prim", Prim.class); //$NON-NLS-1$
        Mapper mapper = xstream.getMapper();
        xstream.registerConverter(new MapConverter(mapper));
        xstream.registerConverter(new ListConverter(mapper));
        xstream.alias("list", List.class); //$NON-NLS-1$
        xstream.alias("map", Map.class); //$NON-NLS-1$
        xstream.alias("vector", LVector.class); //$NON-NLS-1$
        xstream.alias("rotation", LRotation.class); //$NON-NLS-1$
        xstream.alias("region", Region.class); //$NON-NLS-1$
        xstream.alias("inventoryItem", InventoryItem.class); //$NON-NLS-1$
        xstream.alias("itemPermissions", ItemPermissions.class); //$NON-NLS-1$
        xstream.aliasType("notecardItem", Notecard.class); //$NON-NLS-1$
        xstream.aliasType("bodyPartItem", BodyPart.class); //$NON-NLS-1$
        xstream.aliasType("textureItem", Texture.class); //$NON-NLS-1$
        xstream.aliasType("animationItem", Animation.class); //$NON-NLS-1$
        xstream.aliasType("soundItem", Sound.class); //$NON-NLS-1$
        xstream.aliasType("gestureItem", Gesture.class); //$NON-NLS-1$
        xstream.aliasType("clothingItem", Clothing.class); //$NON-NLS-1$
        xstream.aliasType("landmarkItem", Landmark.class); //$NON-NLS-1$
        xstream.aliasType("inventoryObjectItem", InventoryObject.class); //$NON-NLS-1$
    }
    
    static {
        configureXStream(xstream);
    }
    
    public static String toXML(SimWorldDef def) {
        return xstream.toXML(def);
    }
    
    @SuppressWarnings("unused")
	private long maxTime;
    @SuppressWarnings("unused")
    private int sliceSize;
    //private Script[] scripts;
    @SuppressWarnings("unused")
    private SimObject[] objects;
    @SuppressWarnings("unused")
    private Prim[] prims;
    @SuppressWarnings("unused")
    private Avatar[] avatars;
    @SuppressWarnings("unused")
    private String simEventHandler;
    
    public SimWorldDef(long maxTime, int sliceSize, SimObject[] objects,
            Prim[] prims, Avatar[] avatars, String simEventHandler) {
        this.maxTime = maxTime;
        this.sliceSize = sliceSize;
        this.objects = objects;
        this.prims = prims;
        this.avatars = avatars;
        this.simEventHandler = simEventHandler;
    }

    public static SimWorldDef mkSimpleWorld(SimKeyManager keyManager, String name) {
        String primKey = keyManager.getNextKey();
        @SuppressWarnings("unused")
        String avKey = keyManager.getNextKey();
        
        SimWorldDef.Prim[] prims = new SimWorldDef.Prim[] {
                new SimWorldDef.Prim("defaultPrim", primKey, new ScriptInfo[] { new ScriptInfo(name,name) }, null, //$NON-NLS-1$
                        "an object", "Default Avatar", new LVector(0,0,0), new LVector(0,0,0))  //$NON-NLS-1$//$NON-NLS-2$
        };
        
        SimWorldDef.SimObject[] objects = new SimWorldDef.SimObject[] {
                new SimWorldDef.SimObject(new String[] { primKey },new LVector(0,0,0))
        };
        
        SimWorldDef.Avatar[] avatars = new SimWorldDef.Avatar[] {
                new SimWorldDef.Avatar("Default Avatar", 128, 128, 0, null) //$NON-NLS-1$
        };
        
        SimWorldDef def = new SimWorldDef(10000000,1000,objects,prims,avatars, null);
        
        return def;
    }
    
    
}
