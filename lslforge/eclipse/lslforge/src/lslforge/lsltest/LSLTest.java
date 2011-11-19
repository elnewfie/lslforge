package lslforge.lsltest;

import java.util.ArrayList;
import java.util.Iterator;
import java.util.LinkedList;
import java.util.List;
import java.util.SortedSet;
import java.util.TreeSet;

import lslforge.LSLProjectNature;
import lslforge.generated.LSLType;
import lslforge.generated.Tuple2;
import lslforge.util.Util;

import com.thoughtworks.xstream.XStream;
import com.thoughtworks.xstream.io.xml.DomDriver;

public class LSLTest {

	private static final String EMPTY_STRING = "\"\""; //$NON-NLS-1$
    private static final String BLANK = ""; //$NON-NLS-1$
	public static int MODULE_TEST = 0;
	public static int HANDLER_TEST = 1;
	public static int SCRIPT_FUNCTION_TEST = 2;
	private String name = BLANK;
	private EntryPoint entryPoint = null;
	private LSLValue[] arguments = null;
	private MaybeValue expectedReturn = null;
	private CallExpectations expectations = new CallExpectations();
	private ArrayList<GlobBinding> initialBindings = new ArrayList<GlobBinding>();
	private ArrayList<GlobBinding> finalBindings = new ArrayList<GlobBinding>();
    private LSLTestSuite suite;
	
	public static class LSLValue {
	}
	
	public static class MaybeValue {
	    private LSLValue val = null;
	    private Class<?> type = null;
	    
	    public MaybeValue() {
	        this.val = null;
	    }
	    public MaybeValue(LSLValue val) {
	        this.val = val;
        }

        public LSLValue getVal() { return val; }
	    
	    public Class<?> getType() { return type; }
	    
	    @Override
		public String toString() {
	        return val == null ? BLANK : val.toString();
	    }

        public void setVal(LSLValue o) {
            this.val = o;
        }
	}
	
	public static class LSLVoid extends LSLValue {
		@Override
		public String toString() { return BLANK; }
	}

	public static class LSLString extends LSLValue {
		public String val;

		public LSLString(String val) {
			this.val = val;
		}
		
		@Override
		public String toString() {
			return val;
		}

	}
	
	public static class LSLInteger extends LSLValue {
		public String val;

		public LSLInteger(String val) {
			this.val = val;
		}
		
		@Override
		public String toString() {
			return val;
		}
	}
	
	public static class LSLFloat extends LSLValue {
		public String val;
		
		public LSLFloat(String val) {
			this.val = val;
		}
		
		@Override
		public String toString() {
			return val;
		}
	}
	
	public static class LSLKey extends LSLValue {
		public String val;

		public LSLKey(String val) {
			this.val = val;
		}
		
		@Override
		public String toString() {
			return val;
		}
	}
	
	public static class LSLList extends LSLValue {
		private List<LSLValue> val;

		public LSLList() {
		    val = new LinkedList<LSLValue>();
		}
		
		public LSLList(List<LSLValue> val) {
			if (val == null) this.val = new LinkedList<LSLValue>();
			else this.val = val;
		}
		
		@Override
		public String toString() {
			StringBuilder buf = new StringBuilder("["); //$NON-NLS-1$
			String sep = BLANK;

			for (Iterator<LSLValue> it = getVal().iterator(); it.hasNext(); ) {
				buf.append(sep).append(it.next().toString());
				sep = ","; //$NON-NLS-1$
			}
			
			return buf.append(']').toString();
		}

        public void setVal(List<LSLValue> val) {
            if (val == null) val = new LinkedList<LSLValue>();
            this.val = val;
        }

        public List<LSLValue> getVal() {
            if (val == null) val = new LinkedList<LSLValue>();
            return val;
        }
	}
	
	public static class LSLList1 extends LSLValue {
	    String val;
	    public LSLList1(String val) { this.val  = val; }
	    public String getVal() { return val; }
	    @Override
		public String toString() { return val; }
	}
	
	public static class LSLVector extends LSLValue {
		String val;
		public LSLVector(String val) {
		    this.val = val;
		}
		
		public String getVal() {
		    return val;
		}
		@Override
		public String toString() {
			return val;
		}
	}
	
    public static class LSLRotation extends LSLValue {
        String val;
        public LSLRotation(String val) {
            this.val = val;
        }
        
        public String getVal() {
            return val;
        }
        @Override
		public String toString() {
            return val;
        }
    }
	
	public static class EntryPoint {
		private String fileName;
		private String path;
        public void setFileName(String fileName) {
            this.fileName = fileName;
        }
        public String getFileName() {
            return fileName;
        }
        public void setPath(String path) {
            this.path = path;
        }
        public String getPath() {
            return path;
        }
        
        @Override
		public boolean equals(Object o) {
            if (o == null || !(o instanceof EntryPoint)) return false;
            EntryPoint other = (EntryPoint) o;
            return Util.safeEquals(fileName, other.fileName) &&
                   Util.safeEquals(path, other.path);
        }
        
		@Override
		public int hashCode() {
			return super.hashCode();
		}
	}
	
	public static class ExpectedCall {
		private String name;
		private LSLValue returns;
		private List<MaybeValue> args = new ArrayList<MaybeValue>();
		
        public void setName(String funcName) {
            this.name = funcName;
        }
        public String getName() {
            return name;
        }
        public void setReturns(LSLValue returns) {
            this.returns = returns;
        }
        public LSLValue getReturns() {
            return returns;
        }
        public void setArgs(List<MaybeValue> args) {
            this.args = args;
        }
        public List<MaybeValue> getArgs() {
            if (args == null) args = new ArrayList<MaybeValue>();
            return args;
        }
	}
	
	public static class CallExpectations {
	    private static final String STRICT = "strict"; //$NON-NLS-1$
        private static final String EXHAUST = "exhaust"; //$NON-NLS-1$
        private static final String NORMAL = "normal"; //$NON-NLS-1$
        private static final String NICE = "nice"; //$NON-NLS-1$
        private String mode = NICE;
	    private ArrayList<ExpectedCall> calls = new ArrayList<ExpectedCall>();
	    private static TreeSet<String> modeSet;
	    
	    static {
	        modeSet = new TreeSet<String>();
	        modeSet.add(NICE);
	        modeSet.add(NORMAL);
	        modeSet.add(EXHAUST);
	        modeSet.add(STRICT);
	    }
	    
	    @SuppressWarnings("unchecked")
		public static SortedSet<String> getModes() {
	        return (SortedSet<String>) modeSet.clone();
	    }
	    
	    public List<ExpectedCall> getExpectedCalls() {
	        if (calls == null) calls = new ArrayList<ExpectedCall>();
	        return calls;
	    }
	    
	    public String getMode() {
	        if (mode == null) mode = NICE;
	        return mode;
	    }
	    
	    public void setMode(String mode) {
	         if (!modeSet.contains(mode)) throw new RuntimeException(Messages.getString("LSLTest.INVALID_MODE") + mode); //$NON-NLS-1$
	         this.mode = mode;
	    }
	    
	    public void addCall() {
	        calls.add(new ExpectedCall());
	    }
	    
	    public void postInit() {
	        if (mode == null) mode = NICE;
	        if (calls == null) calls = new ArrayList<ExpectedCall>();
	    }
	}
	
	public static class GlobBinding {
	    private String name;
	    private LSLValue value;
	    
	    public String getName() { return name; }
	    public LSLValue getValue() { return value; }
        public void setValue(LSLValue o) {
            value = o;
        }
        public void setName(String name) {
            this.name = name;
        }
	}
	
	public void postInit() {
	    if (this.arguments == null) this.arguments = new LSLValue[0];
	    if (this.entryPoint == null) this.entryPoint = new EntryPoint();
	    if (this.expectedReturn == null) this.expectedReturn = new MaybeValue();
	    if (this.name == null) this.name = BLANK;
	    if (this.expectations == null) this.expectations = new CallExpectations();
	    if (this.initialBindings == null) this.initialBindings = new ArrayList<GlobBinding>();
	    this.expectations.postInit();
	}

	@Override
	public String toString() {
		return "Test \"" + name + "\"";  //$NON-NLS-1$//$NON-NLS-2$
	}
	public static void main(String[] args) {
		XStream xstream = new XStream(new DomDriver());
		
		LSLTest tst = new LSLTest();
		tst.name = "Sample"; //$NON-NLS-1$
	    EntryPoint mod = new EntryPoint();
	    tst.entryPoint = mod;
		mod.setPath("sort"); //$NON-NLS-1$
		mod.setFileName("test.lslm"); //$NON-NLS-1$
		tst.expectedReturn = new MaybeValue();
		tst.expectedReturn.setVal(new LSLInteger("2")); //$NON-NLS-1$
		System.out.println(xstream.toXML(tst));
	}

	public static LSLValue defaultValueFor(Class<?> argType) {
		if (LSLString.class.equals(argType)) {
			return new LSLString(EMPTY_STRING);
		} else if (LSLKey.class.equals(argType)) {
			return new LSLKey(EMPTY_STRING);
		} else if (LSLInteger.class.equals(argType)) {
			return new LSLInteger("0"); //$NON-NLS-1$
		} else if (LSLFloat.class.equals(argType)) {
			return new LSLFloat("0.0"); //$NON-NLS-1$
		} else if (LSLList.class.equals(argType)) {
			return new LSLList(null);
		} else if (LSLVector.class.equals(argType)) {
			return new LSLVector("<0,0,0>"); //$NON-NLS-1$
		} else if (LSLRotation.class.equals(argType)) {
			return new LSLRotation("<0,0,0,1>"); //$NON-NLS-1$
		} else return new LSLVoid();
	}
	
    public static String defaultValueFor(String argType) {
        if ("string".equals(argType)) { //$NON-NLS-1$
            return EMPTY_STRING;
        } else if ("key".equals(argType)) { //$NON-NLS-1$
            return EMPTY_STRING;
        } else if ("integer".equals(argType)) { //$NON-NLS-1$
            return "0"; //$NON-NLS-1$
        } else if ("float".equals(argType)) { //$NON-NLS-1$
            return "0.0"; //$NON-NLS-1$
        } else if ("list".equals(argType)) { //$NON-NLS-1$
            return "[]"; //$NON-NLS-1$
        } else if ("vector".equals(argType)) { //$NON-NLS-1$
            return "<0,0,0>"; //$NON-NLS-1$
        } else if ("rotation".equals(argType)) { //$NON-NLS-1$
            return "<0,0,0,1>"; //$NON-NLS-1$
        } else return ""; //$NON-NLS-1$
    }
    
    public void setExpectedReturn(MaybeValue expectedReturn) {
        this.expectedReturn = expectedReturn;
    }
    
    public MaybeValue getExpectedReturn() {
        return expectedReturn;
    }
    
    public CallExpectations getExpectations() {
        if (expectations == null) expectations = new CallExpectations();
        return expectations;
    }

    public void setExpectations(CallExpectations o) {
        this.expectations = o;
    }

    public ArrayList<GlobBinding> getInitialBindings() { return initialBindings; }
    public void setInitialBindings(ArrayList<GlobBinding> bindings) {
        this.initialBindings = bindings;
    }
    
    public static Class<?> stringToLSLType(String s) {
    	if ("integer".equals(s)) return LSLInteger.class; //$NON-NLS-1$
    	else if ("float".equals(s)) return LSLFloat.class; //$NON-NLS-1$
    	else if ("string".equals(s)) return LSLString.class; //$NON-NLS-1$
    	else if ("key".equals(s)) return LSLKey.class; //$NON-NLS-1$
    	else if ("vector".equals(s)) return LSLVector.class; //$NON-NLS-1$
    	else if ("rotation".equals(s)) return LSLRotation.class; //$NON-NLS-1$
    	else if ("list".equals(s)) return LSLList.class; //$NON-NLS-1$
    	else if ("void".equals(s)) return LSLVoid.class; //$NON-NLS-1$
    	else if (BLANK.equals(s)) return LSLVoid.class;
    	return LSLValue.class;
    }

    public static String lslTypeToString(Class<?> c) {
        if (LSLInteger.class.equals(c)) return "integer"; //$NON-NLS-1$
        else if (LSLFloat.class.equals(c)) return "float"; //$NON-NLS-1$
        else if (LSLString.class.equals(c)) return "string"; //$NON-NLS-1$
        else if (LSLKey.class.equals(c)) return "key"; //$NON-NLS-1$
        else if (LSLVector.class.equals(c)) return "vector"; //$NON-NLS-1$
        else if (LSLRotation.class.equals(c)) return "rotation"; //$NON-NLS-1$
        else if (LSLList.class.equals(c)) return "list"; //$NON-NLS-1$
        else if (LSLList1.class.equals(c)) return "list"; //$NON-NLS-1$
        else return "void"; //$NON-NLS-1$
    }
    
    public static LSLValue mkLSLType(String s, String val) {
        if ("integer".equals(s)) return new LSLInteger(val); //$NON-NLS-1$
        else if ("float".equals(s)) return new LSLFloat(val); //$NON-NLS-1$
        else if ("string".equals(s)) return new LSLString(val); //$NON-NLS-1$
        else if ("key".equals(s)) return new LSLKey(val); //$NON-NLS-1$
        else if ("vector".equals(s)) return new LSLVector(val); //$NON-NLS-1$
        else if ("rotation".equals(s)) return new LSLRotation(val); //$NON-NLS-1$
        else if ("list".equals(s)) return new LSLList1(val); //$NON-NLS-1$
        else if ("void".equals(s)) return new LSLVoid(); //$NON-NLS-1$
        else if (BLANK.equals(s)) return new LSLVoid();
        return new LSLVoid();
    }
    public void setFinalBindings(ArrayList<GlobBinding> finalBindings) {
        this.finalBindings = finalBindings;
    }

    public ArrayList<GlobBinding> getFinalBindings() {
        return finalBindings;
    }

    public void setEntryPoint(EntryPoint entryPoint) {
        this.entryPoint = entryPoint;
        
        this.expectations = new CallExpectations();
        this.expectations.postInit();
        this.expectedReturn = new MaybeValue();
        this.initialBindings = new ArrayList<GlobBinding>();
        this.finalBindings = new ArrayList<GlobBinding>();
        
        LinkedList<Tuple2<String,LSLType>> params = nature().getParams(entryPoint.getFileName(), entryPoint.getPath());
        
        this.arguments = new LSLValue[params.size()];
        
        for (int i = 0; i < params.size(); i++) {
            this.arguments[i] = defaultValueFor(stringToLSLType(TestProject.lslTypeToString(params.get(i).el2)));
        }
    }

    public EntryPoint getEntryPoint() {
        return entryPoint;
    }

    public void setSuite(LSLTestSuite lSLTestSuite) {
        this.suite = lSLTestSuite;
    }
    
    private LSLProjectNature nature() {
        return suite.nature();
    }

    public void setName(String name) {
        this.name = name;
    }

    public String getName() {
        return name;
    }

    public void setArguments(LSLValue[] arguments) {
        this.arguments = arguments;
    }

    public LSLValue[] getArguments() {
        return arguments;
    }
}
