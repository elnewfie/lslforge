package lslforge.lsltest;

import java.io.InputStream;
import java.util.ArrayList;
import java.util.Iterator;
import java.util.LinkedList;

import lslforge.LslProjectNature;
import lslforge.lsltest.LslTest.ExpectedCall;
import lslforge.lsltest.LslTest.GlobBinding;
import lslforge.lsltest.LslTest.LslFloat;
import lslforge.lsltest.LslTest.LslInteger;
import lslforge.lsltest.LslTest.LslKey;
import lslforge.lsltest.LslTest.LslList;
import lslforge.lsltest.LslTest.LslList1;
import lslforge.lsltest.LslTest.LslRotation;
import lslforge.lsltest.LslTest.LslString;
import lslforge.lsltest.LslTest.LslValue;
import lslforge.lsltest.LslTest.LslVector;
import lslforge.lsltest.LslTest.LslVoid;
import lslforge.lsltest.LslTest.MaybeValue;
import lslforge.util.Util;

import org.eclipse.core.resources.IResource;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IAdaptable;
import org.eclipse.core.runtime.Platform;

import com.thoughtworks.xstream.XStream;
import com.thoughtworks.xstream.converters.SingleValueConverter;
import com.thoughtworks.xstream.converters.basic.FloatConverter;
import com.thoughtworks.xstream.converters.basic.IntConverter;
import com.thoughtworks.xstream.io.xml.DomDriver;

public class LslTestSuite implements IAdaptable {
	private static class XMLSerializer {
		private static XStream xstream;
		
		public static LslTestSuite fromXML(String xml) {
			return (LslTestSuite) xstream.fromXML(xml);
		}
		
		public static String toXML(LslTestSuite suite) {
			return xstream.toXML(suite);
		}
	}
	
	static {
		XStream xstream = createXStream();
        
        XMLSerializer.xstream = xstream;
	}

    private static XStream createXStream() {
        XStream xstream = new XStream(new DomDriver());
		xstream.omitField(LslTest.class, "suite"); //$NON-NLS-1$
		xstream.alias("tests", LslTestSuite.class); //$NON-NLS-1$
		xstream.omitField(LslTestSuite.class, "resource"); //$NON-NLS-1$
		xstream.aliasType("lsl-integer", LslInteger.class); //$NON-NLS-1$
		xstream.aliasType("lsl-string", LslString.class); //$NON-NLS-1$
		xstream.aliasType("lsl-key", LslKey.class); //$NON-NLS-1$
		xstream.aliasType("lsl-float", LslFloat.class); //$NON-NLS-1$
		xstream.aliasType("lsl-vector", LslVector.class); //$NON-NLS-1$
		xstream.aliasType("lsl-rotation", LslRotation.class); //$NON-NLS-1$
		xstream.aliasType("lsl-list", LslList.class); //$NON-NLS-1$
        xstream.aliasType("lsl-list1", LslList1.class); //$NON-NLS-1$
		xstream.aliasType("lsl-void", LslVoid.class); //$NON-NLS-1$
		xstream.alias("lsl-test", LslTest.class); //$NON-NLS-1$
		xstream.alias("globalBinding", GlobBinding.class); //$NON-NLS-1$
		xstream.addImplicitCollection(LslTestSuite.class, "tests", LslTest.class); //$NON-NLS-1$
		xstream.addImplicitCollection(LslList.class, "val"); //$NON-NLS-1$
		xstream.alias("maybe-value", MaybeValue.class); //$NON-NLS-1$
		xstream.omitField(MaybeValue.class, "type"); //$NON-NLS-1$
		xstream.alias("call", ExpectedCall.class); //$NON-NLS-1$
		xstream.registerConverter(new SingleValueConverter() {
		    private FloatConverter conv = new FloatConverter();
            public Object fromString(String arg0) {
                
                return new LslFloat(arg0);
            }

            public String toString(Object arg0) {
                return conv.toString(((LslFloat)arg0).val);
            }

            @SuppressWarnings("unchecked")
			public boolean canConvert(Class arg0) {
                return LslFloat.class.equals(arg0);
            }
		    
		});
        xstream.registerConverter(new SingleValueConverter() {
            private IntConverter conv = new IntConverter();
            public Object fromString(String arg0) {
                
                return new LslInteger(arg0);
            }

            public String toString(Object arg0) {
                return conv.toString(((LslInteger)arg0).val);
            }

            @SuppressWarnings("unchecked")
			public boolean canConvert(Class arg0) {
                return LslInteger.class.equals(arg0);
            }
            
        });
        xstream.registerConverter(new SingleValueConverter() {
            public Object fromString(String arg0) {
                
                return new LslString(arg0);
            }

            public String toString(Object arg0) {
                return ((LslString)arg0).val;
            }

            @SuppressWarnings("unchecked")
			public boolean canConvert(Class arg0) {
                return LslString.class.equals(arg0);
            }
            
        });
        xstream.registerConverter(new SingleValueConverter() {
            public Object fromString(String arg0) {
                
                return new LslKey(arg0);
            }

            public String toString(Object arg0) {
                return ((LslKey)arg0).val;
            }

            @SuppressWarnings("unchecked")
			public boolean canConvert(Class arg0) {
                return LslKey.class.equals(arg0);
            }
            
        });
        xstream.registerConverter(new SingleValueConverter() {
            public Object fromString(String arg0) {
                
                return new LslVector(arg0);
            }

            public String toString(Object arg0) {
                return ((LslVector)arg0).val;
            }

            @SuppressWarnings("unchecked")
			public boolean canConvert(Class arg0) {
                return LslVector.class.equals(arg0);
            }
            
        });
        xstream.registerConverter(new SingleValueConverter() {
            public Object fromString(String arg0) {
                
                return new LslRotation(arg0);
            }

            public String toString(Object arg0) {
                return ((LslRotation)arg0).val;
            }

            @SuppressWarnings("unchecked")
			public boolean canConvert(Class arg0) {
                return LslRotation.class.equals(arg0);
            }
            
        });
        xstream.registerConverter(new SingleValueConverter() {
            public Object fromString(String arg0) {
                
                return new LslList1(arg0);
            }

            public String toString(Object arg0) {
                return ((LslList1)arg0).val;
            }

            @SuppressWarnings("unchecked")
			public boolean canConvert(Class arg0) {
                return LslList1.class.equals(arg0);
            }
            
        });
        return xstream;
    }
	
	private IResource resource;

	private ArrayList<LslTest> tests;
	
	public static LslTestSuite empty() {
		return new LslTestSuite();
	}
	
	public LslTestSuite() {
		this.tests = new ArrayList<LslTest>();
	}
	
	public void setIResource(IResource resource) {
		this.resource = resource;
	}
	
	public void addTest(LslTest test) {
	    test.setSuite(this);
		this.tests.add(test);
	}
	
	public void removeTest(int index) {
	    tests.remove(index);
	}
	
	public String toXml() {
		return XMLSerializer.toXML(this);
	}
	
	public static LslTestSuite fromXml(String xml) {
		return XMLSerializer.fromXML(xml).postInit();
	}
	
	private LslTestSuite postInit() {
	    if (tests == null) tests = new ArrayList<LslTest>();
	    for (Iterator<LslTest> i = tests.iterator(); i.hasNext(); ) {
	        LslTest test = i.next();
	        test.setSuite(this);
	        test.postInit();
	    }
	    
	    return this;
	}
	public static LslTestSuite fromXml(InputStream input, IResource resource) {
		LslTestSuite suite = ((LslTestSuite) XMLSerializer.xstream.fromXML(input)).postInit();
		suite.setIResource(resource);
		return suite;
	}
	
	public static void main(String args[]) {
	    LslTestSuite suite = new LslTestSuite();
	    LslTest test = new LslTest();
	    LinkedList<LslValue> list = new LinkedList<LslValue>();
	    list.add(new LslFloat("1.0")); //$NON-NLS-1$
	    test.setArguments(new LslValue[] { new LslList(list) });
	    test.setExpectedReturn(new MaybeValue()); //new MaybeValue(new LslVector(1.0f,2.0f,3.0f));
	    suite.addTest(test);
		String xml = suite.toXml();
		System.out.println(xml);
		LslTestSuite s = fromXml(xml);
		System.out.println("ok"); //$NON-NLS-1$
		xml = s.toXml();
		System.out.println(xml);
	}

	public LslTest[] getTests() {
	    if (tests == null) tests = new ArrayList<LslTest>();
		return tests.toArray(new LslTest[tests.size()]);
	}

	@SuppressWarnings("unchecked")
	public Object getAdapter(Class adapter) {
        return Platform.getAdapterManager().getAdapter(this, adapter);
    }

	public IResource getResource() {
		return resource;
	}

    public void removeTest(LslTest value) {
        this.tests.remove(value);
    }

    public LslProjectNature nature() {
        try {
            return (LslProjectNature) resource.getProject().getNature(LslProjectNature.ID);
        } catch (CoreException e) {
            Util.error(e, e.getLocalizedMessage());
            return null;
        }
    }
	
}
