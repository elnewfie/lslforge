package lslforge.util;

import java.lang.reflect.Constructor;
import java.lang.reflect.InvocationTargetException;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import com.thoughtworks.xstream.XStream;
import com.thoughtworks.xstream.converters.ConversionException;
import com.thoughtworks.xstream.converters.Converter;
import com.thoughtworks.xstream.converters.MarshallingContext;
import com.thoughtworks.xstream.converters.UnmarshallingContext;
import com.thoughtworks.xstream.io.ExtendedHierarchicalStreamWriterHelper;
import com.thoughtworks.xstream.io.HierarchicalStreamReader;
import com.thoughtworks.xstream.io.HierarchicalStreamWriter;
import com.thoughtworks.xstream.io.xml.DomDriver;
import com.thoughtworks.xstream.mapper.Mapper;

public class MapConverter implements Converter {
    private Mapper mapper;
    
    public MapConverter(Mapper mapper) {
        this.mapper = mapper;
    }
	public void marshal(Object source, HierarchicalStreamWriter writer, MarshallingContext context) {
	    @SuppressWarnings("unchecked")
        Map<String,Object> map = (Map<String,Object>) source;
        
        for (Map.Entry<String,Object> entry : map.entrySet()) {
            String key = entry.getKey().toString();
            Object value = entry.getValue();
            writer.startNode("entry"); //$NON-NLS-1$
            writer.startNode("key"); //$NON-NLS-1$
            writer.setValue(key);
            writer.endNode();
            
            ExtendedHierarchicalStreamWriterHelper.startNode(writer, "value", value.getClass()); //$NON-NLS-1$
            if (!value.getClass().equals(Object.class)) {
                writer.addAttribute(mapper.aliasForAttribute("class"), mapper.serializedClass(value.getClass())); //$NON-NLS-1$
            }

            context.convertAnother(value);
            writer.endNode();

            writer.endNode();
        }
        
    }

	@SuppressWarnings("unchecked")
	public Object unmarshal(HierarchicalStreamReader reader, UnmarshallingContext context) {
        @SuppressWarnings("rawtypes")
		Map m = (Map) createCollection(context.getRequiredType());
        while (reader.hasMoreChildren()) {
            reader.moveDown();
            String key = null;
            Object value = null;
            while (reader.hasMoreChildren()) {
                reader.moveDown();
                
                if ("key".equals(reader.getNodeName())) { //$NON-NLS-1$
                    if (key != null) throw new ConversionException("multiple keys in single entry"); //$NON-NLS-1$
                    key = context.convertAnother(m, String.class).toString();
                    
               } else if ("value".equals(reader.getNodeName())) { //$NON-NLS-1$
                    if (value != null) throw new ConversionException("multiple values in single entry"); //$NON-NLS-1$
                    value = readItem(reader, context, m);
                } else {
                    throw new ConversionException("unrecognized InfoMap element"); //$NON-NLS-1$
                }
                
                reader.moveUp();
            }
            reader.moveUp();
            
            m.put(key, value);
        }
        return m;
    }

	public boolean canConvert(@SuppressWarnings("rawtypes") Class type) {
        return Map.class.isAssignableFrom(type);
    }

	private Object createCollection(@SuppressWarnings("rawtypes") Class type) {
        @SuppressWarnings("rawtypes")
		Class defaultType = mapper.defaultImplementationOf(type);
        try {
        	Constructor<?> ctor = defaultType.getConstructor();
            return ctor.newInstance();
            //return defaultType.newInstance();
        } catch (InstantiationException e) {
            throw new ConversionException("Cannot instantiate (InstantiationException)" + defaultType.getName(), e); //$NON-NLS-1$
        } catch (IllegalAccessException e) {
            throw new ConversionException("Cannot instantiate (IllegalAccessException)" + defaultType.getName(), e); //$NON-NLS-1$
        } catch (IllegalArgumentException e) {
            throw new ConversionException("Cannot instantiate (IllegalArgumentException)" + defaultType.getName(), e); //$NON-NLS-1$
        } catch (InvocationTargetException e) {
            throw new ConversionException("Cannot instantiate (InvocationTargetException)" + defaultType.getName(), e); //$NON-NLS-1$
        } catch (NoSuchMethodException e) {
            throw new ConversionException("Cannot instantiate (NoSuchMethodException)" + defaultType.getName(), e); //$NON-NLS-1$
        }
    }
    
    private Object readItem(HierarchicalStreamReader reader, UnmarshallingContext context, Object current) {
        String classAttribute = reader.getAttribute(mapper.aliasForAttribute("class")); //$NON-NLS-1$
        Class<?> type;
        if (classAttribute == null) {
            type = mapper.realClass(reader.getNodeName());
        } else {
            type = mapper.realClass(classAttribute);
        }
        return context.convertAnother(current, type);

    }
    
    public static void main(String[] args) {
        XStream xstream = new XStream(new DomDriver());
        Mapper mapper = xstream.getMapper();
        
        xstream.alias("valueDB", Map.class); //$NON-NLS-1$
        xstream.registerConverter(new MapConverter(mapper), XStream.PRIORITY_VERY_HIGH);
        xstream.alias("list", List.class); //$NON-NLS-1$
        xstream.registerConverter(new ListConverter(mapper));
        HashMap<String,Object> m = new HashMap<String,Object>();
        m.put("hello", "there");  //$NON-NLS-1$//$NON-NLS-2$
        m.put("goodbye", Integer.valueOf(12)); //$NON-NLS-1$
        HashMap<String,Object> m1 = new HashMap<String,Object>();
        m1.put("foo", Float.valueOf(5.0f)); //$NON-NLS-1$
        ArrayList<Object> l = new ArrayList<Object>();
        l.add("bite"); //$NON-NLS-1$
        l.add(Integer.valueOf(90210));
        m1.put("list", l); //$NON-NLS-1$
        m.put("more", m1); //$NON-NLS-1$
        String s = xstream.toXML(m);
        System.out.println(xstream.toXML(m));
        Object o = xstream.fromXML(s);
        String s1 = xstream.toXML(o);
        System.out.println(s1);
        
        System.out.println(s1.equals(s));
        
        
        System.out.println(xstream.toXML(l));
    }
}
