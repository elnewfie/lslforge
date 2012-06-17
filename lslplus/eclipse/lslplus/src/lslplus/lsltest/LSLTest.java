package lslplus.lsltest;

import java.util.ArrayList;
import java.util.Iterator;
import java.util.LinkedList;
import java.util.List;
import java.util.SortedSet;
import java.util.TreeSet;

import lslplus.LslProjectNature;
import lslplus.generated.LSLType;
import lslplus.generated.Tuple2;
import lslplus.util.Util;

import com.thoughtworks.xstream.XStream;
import com.thoughtworks.xstream.io.xml.DomDriver;

public class LslTest {

	private static final String EMPTY_STRING = "\"\""; //$NON-NLS-1$
    private static final String BLANK = ""; //$NON-NLS-1$
	public static int MODULE_TEST = 0;
	public static int HANDLER_TEST = 1;
	public static int SCRIPT_FUNCTION_TEST = 2;
	private String name = BLANK;
	private EntryPoint entryPoint = null;
	private LslValue[] arguments = null;
	private MaybeValue expectedReturn = null;
	private CallExpectations expectations = new CallExpectations();
	private ArrayList<GlobBinding> initialBindings = new ArrayList<GlobBinding>();
	private ArrayList<GlobBinding> finalBindings = new ArrayList<GlobBinding>();
    private LslTestSuite suite;
	
	public static class LslValue {
	}
	
	public static class MaybeValue {
	    private LslValue val = null;
	    private Class<?> type = null;
	    
	    public MaybeValue() {
	        this.val = null;
	    }
	    public MaybeValue(LslValue val) {
	        this.val = val;
        }

        public LslValue getVal() { return val; }
	    
	    public Class<?> getType() { return type; }
	    
	    public String toString() {
	        return val == null ? BLANK : val.toString();
	    }

        public void setVal(LslValue o) {
            this.val = o;
        }
	}
	
	public static class LslVoid extends LslValue {
		public String toString() { return BLANK; }
	}

	public static class LslString extends LslValue {
		public String val;

		public LslString(String val) {
			this.val = val;
		}
		
		public String toString() {
			return val;
		}

	}
	
	public static class LslInteger extends LslValue {
		public String val;

		public LslInteger(String val) {
			this.val = val;
		}
		
		public String toString() {
			return val;
		}
	}
	
	public static class LslFloat extends LslValue {
		public String val;
		
		public LslFloat(String val) {
			this.val = val;
		}
		
		public String toString() {
			return val;
		}
	}
	
	public static class LslKey extends LslValue {
		public String val;

		public LslKey(String val) {
			this.val = val;
		}
		
		public String toString() {
			return val;
		}
	}
	
	public static class LslList extends LslValue {
		private List<LslValue> val;

		public LslList() {
		    val = new LinkedList<LslValue>();
		}
		
		public LslList(List<LslValue> val) {
			if (val == null) this.val = new LinkedList<LslValue>();
			else this.val = val;
		}
		
		public String toString() {
			StringBuilder buf = new StringBuilder("["); //$NON-NLS-1$
			String sep = BLANK;

			for (Iterator<LslValue> it = getVal().iterator(); it.hasNext(); ) {
				buf.append(sep).append(it.next().toString());
				sep = ","; //$NON-NLS-1$
			}
			
			return buf.append(']').toString();
		}

        public void setVal(List<LslValue> val) {
            if (val == null) val = new LinkedList<LslValue>();
            this.val = val;
        }

        public List<LslValue> getVal() {
            if (val == null) val = new LinkedList<LslValue>();
            return val;
        }
	}
	
	public static class LslList1 extends LslValue {
	    String val;
	    public LslList1(String val) { this.val  = val; }
	    public String getVal() { return val; }
	    public String toString() { return val; }
	}
	
	public static class LslVector extends LslValue {
		String val;
		public LslVector(String val) {
		    this.val = val;
		}
		
		public String getVal() {
		    return val;
		}
		public String toString() {
			return val;
		}
	}
	
    public static class LslRotation extends LslValue {
        String val;
        public LslRotation(String val) {
            this.val = val;
        }
        
        public String getVal() {
            return val;
        }
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
        
        public boolean equals(Object o) {
            if (o == null || !(o instanceof EntryPoint)) return false;
            EntryPoint other = (EntryPoint) o;
            return Util.safeEquals(fileName, other.fileName) &&
                   Util.safeEquals(path, other.path);
        }
	}
	
	public static class ExpectedCall {
		private String name;
		private LslValue returns;
		private List<MaybeValue> args = new ArrayList<MaybeValue>();
		
        public void setName(String funcName) {
            this.name = funcName;
        }
        public String getName() {
            return name;
        }
        public void setReturns(LslValue returns) {
            this.returns = returns;
        }
        public LslValue getReturns() {
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
	         if (!modeSet.contains(mode)) throw new RuntimeException(Messages.getString("LslTest.INVALID_MODE") + mode); //$NON-NLS-1$
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
	    private LslValue value;
	    
	    public String getName() { return name; }
	    public LslValue getValue() { return value; }
        public void setValue(LslValue o) {
            value = o;
        }
        public void setName(String name) {
            this.name = name;
        }
	}
	
	public void postInit() {
	    if (this.arguments == null) this.arguments = new LslValue[0];
	    if (this.entryPoint == null) this.entryPoint = new EntryPoint();
	    if (this.expectedReturn == null) this.expectedReturn = new MaybeValue();
	    if (this.name == null) this.name = BLANK;
	    if (this.expectations == null) this.expectations = new CallExpectations();
	    if (this.initialBindings == null) this.initialBindings = new ArrayList<GlobBinding>();
	    this.expectations.postInit();
	}

	public String toString() {
		return "Test \"" + name + "\"";  //$NON-NLS-1$//$NON-NLS-2$
	}
	public static void main(String[] args) {
		XStream xstream = new XStream(new DomDriver());
		
		LslTest tst = new LslTest();
		tst.name = "Sample"; //$NON-NLS-1$
	    EntryPoint mod = new EntryPoint();
	    tst.entryPoint = mod;
		mod.setPath("sort"); //$NON-NLS-1$
		mod.setFileName("test.lslm"); //$NON-NLS-1$
		tst.expectedReturn = new MaybeValue();
		tst.expectedReturn.setVal(new LslInteger("2")); //$NON-NLS-1$
		System.out.println(xstream.toXML(tst));
	}

	public static LslValue defaultValueFor(Class<?> argType) {
		if (LslString.class.equals(argType)) {
			return new LslString(EMPTY_STRING);
		} else if (LslKey.class.equals(argType)) {
			return new LslKey(EMPTY_STRING);
		} else if (LslInteger.class.equals(argType)) {
			return new LslInteger("0"); //$NON-NLS-1$
		} else if (LslFloat.class.equals(argType)) {
			return new LslFloat("0.0"); //$NON-NLS-1$
		} else if (LslList.class.equals(argType)) {
			return new LslList(null);
		} else if (LslVector.class.equals(argType)) {
			return new LslVector("<0,0,0>"); //$NON-NLS-1$
		} else if (LslRotation.class.equals(argType)) {
			return new LslRotation("<0,0,0,1>"); //$NON-NLS-1$
		} else return new LslVoid();
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
    
    public static Class<?> stringToLslType(String s) {
    	if ("integer".equals(s)) return LslInteger.class; //$NON-NLS-1$
    	else if ("float".equals(s)) return LslFloat.class; //$NON-NLS-1$
    	else if ("string".equals(s)) return LslString.class; //$NON-NLS-1$
    	else if ("key".equals(s)) return LslKey.class; //$NON-NLS-1$
    	else if ("vector".equals(s)) return LslVector.class; //$NON-NLS-1$
    	else if ("rotation".equals(s)) return LslRotation.class; //$NON-NLS-1$
    	else if ("list".equals(s)) return LslList.class; //$NON-NLS-1$
    	else if ("void".equals(s)) return LslVoid.class; //$NON-NLS-1$
    	else if (BLANK.equals(s)) return LslVoid.class;
    	return LslValue.class;
    }

    public static String lslTypeToString(Class<?> c) {
        if (LslInteger.class.equals(c)) return "integer"; //$NON-NLS-1$
        else if (LslFloat.class.equals(c)) return "float"; //$NON-NLS-1$
        else if (LslString.class.equals(c)) return "string"; //$NON-NLS-1$
        else if (LslKey.class.equals(c)) return "key"; //$NON-NLS-1$
        else if (LslVector.class.equals(c)) return "vector"; //$NON-NLS-1$
        else if (LslRotation.class.equals(c)) return "rotation"; //$NON-NLS-1$
        else if (LslList.class.equals(c)) return "list"; //$NON-NLS-1$
        else if (LslList1.class.equals(c)) return "list"; //$NON-NLS-1$
        else return "void"; //$NON-NLS-1$
    }
    
    public static LslValue mkLslType(String s, String val) {
        if ("integer".equals(s)) return new LslInteger(val); //$NON-NLS-1$
        else if ("float".equals(s)) return new LslFloat(val); //$NON-NLS-1$
        else if ("string".equals(s)) return new LslString(val); //$NON-NLS-1$
        else if ("key".equals(s)) return new LslKey(val); //$NON-NLS-1$
        else if ("vector".equals(s)) return new LslVector(val); //$NON-NLS-1$
        else if ("rotation".equals(s)) return new LslRotation(val); //$NON-NLS-1$
        else if ("list".equals(s)) return new LslList1(val); //$NON-NLS-1$
        else if ("void".equals(s)) return new LslVoid(); //$NON-NLS-1$
        else if (BLANK.equals(s)) return new LslVoid();
        return new LslVoid();
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
        
        this.arguments = new LslValue[params.size()];
        
        for (int i = 0; i < params.size(); i++) {
            this.arguments[i] = defaultValueFor(stringToLslType(TestProject.lslTypeToString(params.get(i).el2)));
        }
    }

    public EntryPoint getEntryPoint() {
        return entryPoint;
    }

    public void setSuite(LslTestSuite lslTestSuite) {
        this.suite = lslTestSuite;
    }
    
    private LslProjectNature nature() {
        return suite.nature();
    }

    public void setName(String name) {
        this.name = name;
    }

    public String getName() {
        return name;
    }

    public void setArguments(LslValue[] arguments) {
        this.arguments = arguments;
    }

    public LslValue[] getArguments() {
        return arguments;
    }
}
