package lslforge.lsltest;

import java.io.InputStream;
import java.util.ArrayList;
import java.util.Iterator;
import java.util.LinkedList;

import lslforge.LSLProjectNature;
import lslforge.lsltest.LSLTest.ExpectedCall;
import lslforge.lsltest.LSLTest.GlobBinding;
import lslforge.lsltest.LSLTest.LSLFloat;
import lslforge.lsltest.LSLTest.LSLInteger;
import lslforge.lsltest.LSLTest.LSLKey;
import lslforge.lsltest.LSLTest.LSLList;
import lslforge.lsltest.LSLTest.LSLList1;
import lslforge.lsltest.LSLTest.LSLRotation;
import lslforge.lsltest.LSLTest.LSLString;
import lslforge.lsltest.LSLTest.LSLValue;
import lslforge.lsltest.LSLTest.LSLVector;
import lslforge.lsltest.LSLTest.LSLVoid;
import lslforge.lsltest.LSLTest.MaybeValue;
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

public class LSLTestSuite implements IAdaptable {
	private static class XMLSerializer {
		private static XStream xstream;
		
		public static LSLTestSuite fromXML(String xml) {
			return (LSLTestSuite) xstream.fromXML(xml);
		}
		
		public static String toXML(LSLTestSuite suite) {
			return xstream.toXML(suite);
		}
	}
	
	static {
		XStream xstream = createXStream();
        
        XMLSerializer.xstream = xstream;
	}

    private static XStream createXStream() {
        XStream xstream = new XStream(new DomDriver());
		xstream.omitField(LSLTest.class, "suite"); //$NON-NLS-1$
		xstream.alias("tests", LSLTestSuite.class); //$NON-NLS-1$
		xstream.omitField(LSLTestSuite.class, "resource"); //$NON-NLS-1$
		xstream.aliasType("lsl-integer", LSLInteger.class); //$NON-NLS-1$
		xstream.aliasType("lsl-string", LSLString.class); //$NON-NLS-1$
		xstream.aliasType("lsl-key", LSLKey.class); //$NON-NLS-1$
		xstream.aliasType("lsl-float", LSLFloat.class); //$NON-NLS-1$
		xstream.aliasType("lsl-vector", LSLVector.class); //$NON-NLS-1$
		xstream.aliasType("lsl-rotation", LSLRotation.class); //$NON-NLS-1$
		xstream.aliasType("lsl-list", LSLList.class); //$NON-NLS-1$
        xstream.aliasType("lsl-list1", LSLList1.class); //$NON-NLS-1$
		xstream.aliasType("lsl-void", LSLVoid.class); //$NON-NLS-1$
		xstream.alias("lsl-test", LSLTest.class); //$NON-NLS-1$
		xstream.alias("globalBinding", GlobBinding.class); //$NON-NLS-1$
		xstream.addImplicitCollection(LSLTestSuite.class, "tests", LSLTest.class); //$NON-NLS-1$
		xstream.addImplicitCollection(LSLList.class, "val"); //$NON-NLS-1$
		xstream.alias("maybe-value", MaybeValue.class); //$NON-NLS-1$
		xstream.omitField(MaybeValue.class, "type"); //$NON-NLS-1$
		xstream.alias("call", ExpectedCall.class); //$NON-NLS-1$
		xstream.registerConverter(new SingleValueConverter() {
		    private FloatConverter conv = new FloatConverter();
            public Object fromString(String arg0) {
                
                return new LSLFloat(arg0);
            }

            public String toString(Object arg0) {
                return conv.toString(((LSLFloat)arg0).val);
            }

			public boolean canConvert(@SuppressWarnings("rawtypes") Class arg0) {
                return LSLFloat.class.equals(arg0);
            }
		    
		});
        xstream.registerConverter(new SingleValueConverter() {
            private IntConverter conv = new IntConverter();
            public Object fromString(String arg0) {
                
                return new LSLInteger(arg0);
            }

            public String toString(Object arg0) {
                return conv.toString(((LSLInteger)arg0).val);
            }

			public boolean canConvert(@SuppressWarnings("rawtypes") Class arg0) {
                return LSLInteger.class.equals(arg0);
            }
            
        });
        xstream.registerConverter(new SingleValueConverter() {
            public Object fromString(String arg0) {
                
                return new LSLString(arg0);
            }

            public String toString(Object arg0) {
                return ((LSLString)arg0).val;
            }

			public boolean canConvert(@SuppressWarnings("rawtypes") Class arg0) {
                return LSLString.class.equals(arg0);
            }
            
        });
        xstream.registerConverter(new SingleValueConverter() {
            public Object fromString(String arg0) {
                
                return new LSLKey(arg0);
            }

            public String toString(Object arg0) {
                return ((LSLKey)arg0).val;
            }

			public boolean canConvert(@SuppressWarnings("rawtypes") Class arg0) {
                return LSLKey.class.equals(arg0);
            }
            
        });
        xstream.registerConverter(new SingleValueConverter() {
            public Object fromString(String arg0) {
                
                return new LSLVector(arg0);
            }

            public String toString(Object arg0) {
                return ((LSLVector)arg0).val;
            }

			public boolean canConvert(@SuppressWarnings("rawtypes") Class arg0) {
                return LSLVector.class.equals(arg0);
            }
            
        });
        xstream.registerConverter(new SingleValueConverter() {
            public Object fromString(String arg0) {
                
                return new LSLRotation(arg0);
            }

            public String toString(Object arg0) {
                return ((LSLRotation)arg0).val;
            }

			public boolean canConvert(@SuppressWarnings("rawtypes") Class arg0) {
                return LSLRotation.class.equals(arg0);
            }
            
        });
        xstream.registerConverter(new SingleValueConverter() {
            public Object fromString(String arg0) {
                
                return new LSLList1(arg0);
            }

            public String toString(Object arg0) {
                return ((LSLList1)arg0).val;
            }

			public boolean canConvert(@SuppressWarnings("rawtypes") Class arg0) {
                return LSLList1.class.equals(arg0);
            }
            
        });
        return xstream;
    }
	
	private IResource resource;

	private ArrayList<LSLTest> tests;
	
	public static LSLTestSuite empty() {
		return new LSLTestSuite();
	}
	
	public LSLTestSuite() {
		this.tests = new ArrayList<LSLTest>();
	}
	
	public void setIResource(IResource resource) {
		this.resource = resource;
	}
	
	public void addTest(LSLTest test) {
	    test.setSuite(this);
		this.tests.add(test);
	}
	
	public void removeTest(int index) {
	    tests.remove(index);
	}
	
	public String toXml() {
		return XMLSerializer.toXML(this);
	}
	
	public static LSLTestSuite fromXml(String xml) {
		return XMLSerializer.fromXML(xml).postInit();
	}
	
	private LSLTestSuite postInit() {
	    if (tests == null) tests = new ArrayList<LSLTest>();
	    for (Iterator<LSLTest> i = tests.iterator(); i.hasNext(); ) {
	        LSLTest test = i.next();
	        test.setSuite(this);
	        test.postInit();
	    }
	    
	    return this;
	}
	public static LSLTestSuite fromXml(InputStream input, IResource resource) {
		LSLTestSuite suite = ((LSLTestSuite) XMLSerializer.xstream.fromXML(input)).postInit();
		suite.setIResource(resource);
		return suite;
	}
	
	public static void main(String args[]) {
	    LSLTestSuite suite = new LSLTestSuite();
	    LSLTest test = new LSLTest();
	    LinkedList<LSLValue> list = new LinkedList<LSLValue>();
	    list.add(new LSLFloat("1.0")); //$NON-NLS-1$
	    test.setArguments(new LSLValue[] { new LSLList(list) });
	    test.setExpectedReturn(new MaybeValue()); //new MaybeValue(new LSLVector(1.0f,2.0f,3.0f));
	    suite.addTest(test);
		String xml = suite.toXml();
		System.out.println(xml);
		LSLTestSuite s = fromXml(xml);
		System.out.println("ok"); //$NON-NLS-1$
		xml = s.toXml();
		System.out.println(xml);
	}

	public LSLTest[] getTests() {
	    if (tests == null) tests = new ArrayList<LSLTest>();
		return tests.toArray(new LSLTest[tests.size()]);
	}

	public Object getAdapter(@SuppressWarnings("rawtypes") Class adapter) {
        return Platform.getAdapterManager().getAdapter(this, adapter);
    }

	public IResource getResource() {
		return resource;
	}

    public void removeTest(LSLTest value) {
        this.tests.remove(value);
    }

    public LSLProjectNature nature() {
        try {
            return (LSLProjectNature) resource.getProject().getNature(LSLProjectNature.ID);
        } catch (CoreException e) {
            Util.error(e, e.getLocalizedMessage());
            return null;
        }
    }
	
}
