package lslforge.util;

import java.util.Iterator;
import java.util.List;

import com.thoughtworks.xstream.converters.ConversionException;
import com.thoughtworks.xstream.converters.Converter;
import com.thoughtworks.xstream.converters.MarshallingContext;
import com.thoughtworks.xstream.converters.UnmarshallingContext;
import com.thoughtworks.xstream.io.ExtendedHierarchicalStreamWriterHelper;
import com.thoughtworks.xstream.io.HierarchicalStreamReader;
import com.thoughtworks.xstream.io.HierarchicalStreamWriter;
import com.thoughtworks.xstream.mapper.Mapper;

public class ListConverter implements Converter {
    private Mapper mapper;
    public ListConverter(Mapper mapper) {
        this.mapper = mapper;
    }
    
	public void marshal(Object source, HierarchicalStreamWriter writer, MarshallingContext context) {
	    @SuppressWarnings("unchecked")
        List<Object> list = (List<Object>) source;
        
        for (Iterator<Object> iterator = list.iterator(); iterator.hasNext();) {
            Object value = iterator.next();

            ExtendedHierarchicalStreamWriterHelper.startNode(writer, "value", value.getClass()); //$NON-NLS-1$
            if (!value.getClass().equals(Object.class)) {
                writer.addAttribute(mapper.aliasForAttribute("class"), mapper.serializedClass(value.getClass())); //$NON-NLS-1$
            }

            context.convertAnother(value);
            writer.endNode();
        }
    }

	public Object unmarshal(HierarchicalStreamReader reader, UnmarshallingContext context) {
	    @SuppressWarnings("unchecked")
        List<Object> l = (List<Object>) createCollection(context.getRequiredType());
        while (reader.hasMoreChildren()) {
            reader.moveDown();
            Object value = null;
                
            if ("value".equals(reader.getNodeName())) { //$NON-NLS-1$
//Dead code, removed
//                if (value != null) throw new ConversionException("multiple values in single entry"); //$NON-NLS-1$
                value = readItem(reader, context, l);
            } else {
                throw new ConversionException("unrecognized list element"); //$NON-NLS-1$
            }
                
            reader.moveUp();
            
            l.add(value);
        }
        return l;
    }

	public boolean canConvert(@SuppressWarnings("rawtypes") Class type) {
        return List.class.isAssignableFrom(type);
    }

	private Object createCollection(@SuppressWarnings("rawtypes") Class type) {
        @SuppressWarnings("rawtypes")
		Class defaultType = mapper.defaultImplementationOf(type);
        try {
            return defaultType.newInstance();
        } catch (InstantiationException e) {
            throw new ConversionException("Cannot instantiate " + defaultType.getName(), e); //$NON-NLS-1$
        } catch (IllegalAccessException e) {
            throw new ConversionException("Cannot instantiate " + defaultType.getName(), e); //$NON-NLS-1$
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
}
