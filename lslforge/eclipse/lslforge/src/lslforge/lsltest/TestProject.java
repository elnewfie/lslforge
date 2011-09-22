package lslforge.lsltest;

import java.io.InputStream;
import java.util.ArrayList;
import java.util.Iterator;
import java.util.LinkedList;
import java.util.List;

import lslforge.LslExpressionValidator;
import lslforge.LslForgePlugin;
import lslforge.LslProjectNature;
import lslforge.generated.GlobalSummary;
import lslforge.generated.GlobalSummary_GlobalSummary;
import lslforge.generated.LSLType;
import lslforge.generated.LSLType_LLFloat;
import lslforge.generated.LSLType_LLInteger;
import lslforge.generated.LSLType_LLKey;
import lslforge.generated.LSLType_LLList;
import lslforge.generated.LSLType_LLRot;
import lslforge.generated.LSLType_LLString;
import lslforge.generated.LSLType_LLVector;
import lslforge.generated.Tuple2;
import lslforge.gentree.Node;
import lslforge.gentree.NodeFactory;
import lslforge.gentree.NodeFactory2;
import lslforge.gentree.NodeStatus;
import lslforge.language_metadata.LslFunction;
import lslforge.language_metadata.LslParam;
import lslforge.lsltest.LslTest.CallExpectations;
import lslforge.lsltest.LslTest.EntryPoint;
import lslforge.lsltest.LslTest.ExpectedCall;
import lslforge.lsltest.LslTest.GlobBinding;
import lslforge.lsltest.LslTest.LslValue;
import lslforge.lsltest.LslTest.MaybeValue;
import lslforge.util.Util;
import lslforge.util.Util.Predicate;

import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.runtime.CoreException;

import com.thoughtworks.xstream.XStream;
import com.thoughtworks.xstream.io.xml.DomDriver;

public class TestProject {
    private static final NodeFactory[] EMPTY_FACTORY_LIST = {};
    public static class SuiteNode extends Node {
        private transient IResource resource;
        private static final NodeFactory[] LEGAL_CHILDREN = { TEST_NODE_FACTORY };
        public SuiteNode(Node parent, String testSuiteName) {
            super(null, testSuiteName, null);
        }

        public NodeStatus checkNameString(String name) {
            return NodeStatus.OK;
        }

        public NodeStatus checkValueString(String s) {
            return NodeStatus.OK;
        }

        public String getValueString() {
            return null;
        }

        public boolean isDeletable() {
            return false;
        }

        public boolean isNameChangeable() {
            return true;
        }

        public boolean isValueChangeable() {
            return false;
        }

        public NodeFactory[] legalChildNodes() {
            return LEGAL_CHILDREN;
        }

        protected void onUpdate(String s) {
        }
        
        public LslProjectNature nature() {
            try {
                return (LslProjectNature) resource.getProject().getNature(LslProjectNature.ID);
            } catch (CoreException e) {
                Util.error(e, e.getLocalizedMessage());
                return null;
            }
        }

        public void setResource(IFile file) {
            this.resource = file;
        }
    }
    
    private static final NodeFactory TEST_NODE_FACTORY = new NodeFactory2() {
        public Node createNode(Node parent) {
            throw new UnsupportedOperationException();
        }

        public String getNodeTypeName() {
            return "Test"; //$NON-NLS-1$ TODO
        }

        public Node createNode(Node parent, String value) {
            return new TestNode(parent,"New  Test", value); //$NON-NLS-1$ TODO
        }

        public String getNodeCreationId() {
            return "entry-point"; //$NON-NLS-1$
        }
    };
    

    public static class TestNode extends Node {

        public TestNode(Node parent, String testName, String value) {
            super(parent, testName, value);
            String[] elements = value.toString().split("/"); //$NON-NLS-1$
            Node root = findRoot();
            LinkedList<Tuple2<String,LSLType>> params =
            	((SuiteNode)root).nature().getParams(elements[0], elements[1]);
            String returnType = lslTypeToString(((SuiteNode)root).nature().getReturnType(elements[0], elements[1]));
            addChild(new ArgumentsListNode(this, "args", params)); //$NON-NLS-1$
            addChild(new ExpectedReturnNode(this,returnType,null));
            addChild(new BindingListNode(this,"initial", "Initial Globals")); //$NON-NLS-1$ //$NON-NLS-2$ TODO
            addChild(new ExpectationsNode(this));
            addChild(new BindingListNode(this,"final", "Final Globals")); //$NON-NLS-1$ //$NON-NLS-2$ TODO
        }

        public ExpectationsNode getExpectations() {
            return (ExpectationsNode) findChildByName("expectations"); //$NON-NLS-1$
        }
        
        public ArgumentsListNode getArguments() {
            return (ArgumentsListNode) findChildByName("args"); //$NON-NLS-1$
        }
        
        public ExpectedReturnNode getReturn() {
            return (ExpectedReturnNode) findChildByName("returns"); //$NON-NLS-1$
        }
        
        public BindingListNode getInitialBindings() {
            return (BindingListNode) findChildByName("initial"); //$NON-NLS-1$
        }

        public BindingListNode getFinalBindings() {
            return (BindingListNode) findChildByName("final"); //$NON-NLS-1$
        }
        
        public NodeStatus checkNameString(String name) {
            return NodeStatus.OK;
        }

        public NodeStatus checkValueString(String s) {
            return NodeStatus.OK;
        }

        public String getValueString() {
            return getValue().toString();
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

        public NodeFactory[] legalChildNodes() {
            return EMPTY_FACTORY_LIST;
        }

        protected void onUpdate(String s) {
        }

        public String getFilename() {
            return getValue().toString().split("/")[0]; //$NON-NLS-1$
        }
        
        public String getPath() {
            return getValue().toString().split("/")[1]; //$NON-NLS-1$
        }
        
    }

    public static class ArgumentsListNode extends Node {

        public ArgumentsListNode(Node parent, String nodeName, 
        		LinkedList<Tuple2<String, LSLType>> params) {
            super(parent, nodeName, null);
            for (Tuple2<String,LSLType> param : params) {
                addChild(new ArgumentNode(this,param.el1, lslTypeToString(param.el2)));
            }
        }

        public String getNameDisplay() { return "Arguments"; } //$NON-NLS-1$ TODO
        public NodeStatus checkNameString(String name) {
            return NodeStatus.OK;
        }

        public NodeStatus checkValueString(String s) {
            return NodeStatus.OK;
        }

        public String getValueString() {
            return null;
        }

        public boolean isDeletable() {
            return false;
        }

        public boolean isNameChangeable() {
            return false;
        }

        public boolean isValueChangeable() {
            return false;
        }

        public NodeFactory[] legalChildNodes() {
            return EMPTY_FACTORY_LIST;
        }

        protected void onUpdate(String s) {
        }
    }
    
    public static class BindingListNode extends Node {
        private static final NodeFactory[] LEGAL_CHILDREN = new NodeFactory[] { BINDING_NODE_FACTORY };
        private String displayName;
        public BindingListNode(Node parent, String nodeName, String displayName) {
            super(parent, nodeName, null);
            this.displayName = displayName;
        }

        
        public String getNameDisplay() {
            return displayName;
        }


        public NodeStatus checkNameString(String name) {
            return NodeStatus.OK;
        }

        public NodeStatus checkValueString(String s) {
            return NodeStatus.OK;
        }

        public String getValueString() {
            return null;
        }

        public boolean isDeletable() {
            return false;
        }

        public boolean isNameChangeable() {
            return false;
        }

        public boolean isValueChangeable() {
            return false;
        }

        public NodeFactory[] legalChildNodes() {
            return LEGAL_CHILDREN;
        }

        protected void onUpdate(String s) {
        }
        
    }
    
    private static final NodeFactory2 BINDING_NODE_FACTORY = new NodeFactory2() {

        public Node createNode(Node parent, String value) {
            SuiteNode root = (SuiteNode) parent.findRoot();
            TestNode test = (TestNode) parent.getParent();
            LinkedList<GlobalSummary> pairs = root.nature().getGlobalVariables(test.getFilename());
            //for (int i = 0; i < pairs.length; i++) {
            for (GlobalSummary gs0 : pairs) {
            	GlobalSummary_GlobalSummary gs = (GlobalSummary_GlobalSummary) gs0;
                if (value.equals(gs.globalName)) {
                    return new BindingNode(parent,gs.globalName, lslTypeToString(gs.globalType));
                }
            }
            return null;
        }

        public String getNodeCreationId() {
            return "globvar"; //$NON-NLS-1$
        }

        public Node createNode(Node parent) {
            throw new UnsupportedOperationException();
        }

        public String getNodeTypeName() {
            return "Assignment"; //$NON-NLS-1$ TODO
        }
        
    };
    public static class BindingNode extends Node {
        private String type;
        public BindingNode(Node parent, String nodeName, String type) {
            super(parent, nodeName, LslTest.defaultValueFor(type));
            this.type = type;
        }

        public NodeStatus checkNameString(String name) {
            return NodeStatus.OK;
        }

        public NodeStatus checkValueString(String s) {
            String result = LslExpressionValidator.validateExpression(type, s);
            if (result == null) return NodeStatus.OK;
            return new NodeStatus(false, result);
        }

        public String getValueString() {
            return (String)getValue();
        }

        public boolean isDeletable() {
            return true;
        }

        public boolean isNameChangeable() {
            return false;
        }

        public boolean isValueChangeable() {
            return true;
        }

        public NodeFactory[] legalChildNodes() {
            return EMPTY_FACTORY_LIST;
        }

        protected void onUpdate(String s) {
            setValue(s);
        }
        
    }
    
    public static class ReturnNode extends Node {
        String type;
        public ReturnNode(Node parent, String name, String type, Object value) {
            super(parent, name , value);
            this.type = type;
        }
        
        public String getNameDisplay() {
            return "Returns (" + type + ")"; //$NON-NLS-1$ //$NON-NLS-2$ TODO
        }
        
        public NodeStatus checkNameString(String name) {
            return NodeStatus.OK;
        }

        public NodeStatus checkValueString(String s) {
            String result = LslExpressionValidator.validateExpression(type, s);
            if (result == null) return NodeStatus.OK;
            return new NodeStatus(false, result);
        }

        public String getValueString() {
            String s = (String)getValue();
            return s == null ? "" : s; //$NON-NLS-1$
        }

        public boolean isDeletable() {
            return false;
        }

        public boolean isNameChangeable() {
            return false;
        }

        public boolean isValueChangeable() {
            return !"void".equals(type); //$NON-NLS-1$
        }

        public NodeFactory[] legalChildNodes() {
            return EMPTY_FACTORY_LIST;
        }

        protected void onUpdate(String s) {
            setValue(s);
        }
    }
    
    public static class ExpectedReturnNode extends ReturnNode {
        public ExpectedReturnNode(Node parent, String type, Object value) {
            super(parent, "returns" , type, value); //$NON-NLS-1$
        }

        public NodeStatus checkValueString(String s) {
            if (s.trim().equals("")) return NodeStatus.OK; //$NON-NLS-1$
            return super.checkValueString(s);
        }

        protected void onUpdate(String s) {
            super.onUpdate("".equals(s.trim())? null : s); //$NON-NLS-1$
        }
        
    }

    public static class ArgumentNode extends Node {
        private String type;
        public ArgumentNode(Node parent, String nodeName, String type) {
            super(parent, nodeName, LslTest.defaultValueFor(type));
            this.type = type;
        }

        public String getNameDisplay() {
            return getName() + " (" + type + ") ";  //$NON-NLS-1$//$NON-NLS-2$
        }
        
        public NodeStatus checkNameString(String name) {
            return NodeStatus.OK;
        }

        public NodeStatus checkValueString(String s) {
            String result = LslExpressionValidator.validateExpression(type, s);
            if (result == null) return NodeStatus.OK;
            return new NodeStatus(false, result);
        }

        public String getValueString() {
            return (String)getValue();
        }

        public boolean isDeletable() {
            return false;
        }

        public boolean isNameChangeable() {
            return false;
        }

        public boolean isValueChangeable() {
            return true;
        }

        public NodeFactory[] legalChildNodes() {
            return EMPTY_FACTORY_LIST;
        }

        protected void onUpdate(String s) {
            setValue(s);
        }

        public void setType(String type) {
            this.type = type;
        }

        public String getType() {
            return type;
        }
        
    }

    public static class ExpectationsNode extends Node {
        private static final NodeFactory[] LEGAL_CHILDREN = { EXPECTED_CALL_FACTORY };
        public ExpectationsNode(Node parent) {
            super(parent, "expectations", null); //$NON-NLS-1$
            addChild(new ExpectationsModeNode(this,"nice")); //$NON-NLS-1$
        }

        public String getNameDisplay() {
            return "Call Expectations"; //$NON-NLS-1$ TODO
        }
        
        public String getMode() {
            ExpectationsModeNode node = (ExpectationsModeNode) findChildByName("mode"); //$NON-NLS-1$
            return node.getValueString();
        }
        
        public List<Node> getExpectedCalls() {
            return findChildrenByType(ExpectedCallNode.class);
        }
        
        public NodeStatus checkNameString(String name) {
            return NodeStatus.OK;
        }

        public NodeStatus checkValueString(String s) {
            return NodeStatus.OK;
        }

        public String getValueString() {
            return null;
        }

        public boolean isDeletable() {
            return false;
        }

        public boolean isNameChangeable() {
            return false;
        }

        public boolean isValueChangeable() {
            return false;
        }

        public NodeFactory[] legalChildNodes() {
            return LEGAL_CHILDREN;
        }

        protected void onUpdate(String s) {
        }
        
    }

    public static class ExpectationsModeNode extends Node {

        public ExpectationsModeNode(Node parent, Object value) {
            super(parent, "mode", value); //$NON-NLS-1$
        }

        public String getNameDisplay() { return "Call handler mode"; } //$NON-NLS-1$ TODO
        
        public String getChoicesId() {
            return "expectations-mode"; //$NON-NLS-1$
        }

        public boolean hasValueChoices() {
            return true;
        }

        public NodeStatus checkNameString(String name) {
            return NodeStatus.OK;
        }

        public NodeStatus checkValueString(String s) {
            return NodeStatus.OK;
        }

        public String getValueString() {
            return (String)getValue();
        }

        public boolean isDeletable() {
            return false;
        }

        public boolean isNameChangeable() {
            return false;
        }

        public boolean isValueChangeable() {
            return true;
        }

        public NodeFactory[] legalChildNodes() {
            return EMPTY_FACTORY_LIST;
        }

        protected void onUpdate(String s) {
            setValue(s);
        }
        
    }

    private static final NodeFactory2 EXPECTED_CALL_FACTORY = new NodeFactory2() {

        public Node createNode(Node parent, String value) {
            return new ExpectedCallNode(parent, "call", value); //$NON-NLS-1$
        }

        public String getNodeCreationId() {
            return "call"; //$NON-NLS-1$
        }

        public Node createNode(Node parent) {
            throw new UnsupportedOperationException();
        }

        public String getNodeTypeName() {
            return "Call"; //$NON-NLS-1$
        }
        
    };
    public static class ExpectedCallNode extends Node {

        public ExpectedCallNode(Node parent, String nodeName, Object value) {
            super(parent, nodeName, value);
            final String name = (String)value;
            LslFunction func = (LslFunction) Util.find(new Predicate() {
                public boolean test(Object o) {
                    return name.equals(((LslFunction)o).getName());
                }
            }, LslForgePlugin.getLLFunctions());
            
            addChild(new ExpectedArgumentsListNode(this,"args", func.getParams())); //$NON-NLS-1$
            addChild(new ReturnNode(this,"returns", func.getReturns(),LslTest.defaultValueFor(func.getReturns()))); //$NON-NLS-1$
        }

        public ExpectedArgumentsListNode getArgumentListNode() {
            return (ExpectedArgumentsListNode) findChildByName("args"); //$NON-NLS-1$
        }
        
        public ReturnNode getReturn() {
            return (ReturnNode) findChildByName("returns"); //$NON-NLS-1$
        }
        
        public NodeStatus checkNameString(String name) {
            return NodeStatus.OK;
        }

        public NodeStatus checkValueString(String s) {
            return NodeStatus.OK;
        }

        public String getValueString() {
            return (String) getValue();
        }

        public boolean isDeletable() {
            return true;
        }

        public boolean isNameChangeable() {
            return false;
        }

        public boolean isValueChangeable() {
            return false;
        }

        public NodeFactory[] legalChildNodes() {
            return EMPTY_FACTORY_LIST;
        }

        protected void onUpdate(String s) {
        }
        
    }

    public static class ExpectedArgumentsListNode extends Node {

        public ExpectedArgumentsListNode(Node parent, String nodeName, LslParam[] params) {
            super(parent, nodeName, null);
            for (int i = 0; i < params.length; i++) {
                addChild(new ExpectedArgumentNode(this,params[i].getName(), params[i].getType()));
            }
        }

        public NodeStatus checkNameString(String name) {
            return NodeStatus.OK;
        }

        public NodeStatus checkValueString(String s) {
            return NodeStatus.OK;
        }

        public String getValueString() {
            return null;
        }

        public boolean isDeletable() {
            return false;
        }

        public boolean isNameChangeable() {
            return false;
        }

        public boolean isValueChangeable() {
            return false;
        }

        public NodeFactory[] legalChildNodes() {
            return EMPTY_FACTORY_LIST;
        }

        protected void onUpdate(String s) {
        }
    }
    
    public static class ExpectedArgumentNode extends ArgumentNode {
     
        public ExpectedArgumentNode(Node parent, String nodeName, String type) {
            super(parent, nodeName, type);
        }

        public NodeStatus checkValueString(String s) {
            if (s.trim().equals("")) return NodeStatus.OK; //$NON-NLS-1$
            return super.checkValueString(s);
        }

        public String getValueString() {
            if (getValue() == null) return ""; //$NON-NLS-1$
            return super.getValueString();
        }

        protected void onUpdate(String s) {
            
            super.onUpdate("".equals(s.trim()) ? null : s); //$NON-NLS-1$
        }
    }
    
    private static XStream xstream = new XStream(new DomDriver());
    private static void configureXStream(XStream xstream) {
        Class<?>[] nodeTypes = new Class[] {
                SuiteNode.class, TestNode.class, ArgumentNode.class,
                ArgumentsListNode.class, BindingListNode.class,
                BindingNode.class, ReturnNode.class, ExpectedReturnNode.class,
                ExpectationsNode.class, ExpectationsModeNode.class,
                ExpectedCallNode.class, ExpectedArgumentsListNode.class,
                ExpectedArgumentNode.class
        };
        
        xstream.omitField(Node.class, "parent"); //$NON-NLS-1$
        xstream.omitField(Node.class, "children"); //$NON-NLS-1$
        xstream.omitField(Node.class, "listeners"); //$NON-NLS-1$
        for (int i = 0; i < nodeTypes.length; i++) {
            Class<?> c = nodeTypes[i];
            String name = c.getSimpleName();
            name = name.substring(0, 1).toLowerCase() + name.substring(1);
            xstream.alias(name, c);
        }
        
        xstream.omitField(SuiteNode.class, "resource"); //$NON-NLS-1$
    }
    
    static {
        configureXStream(xstream);
    }
    
    public static String toXml(SuiteNode world) {
        world.syncChildren();
        return xstream.toXML(world);
    }

    public static SuiteNode fromXml(InputStream contents, IFile file) {
        SuiteNode n = (SuiteNode) xstream.fromXML(contents);
        n.setResource(file);
        n.propagateParent();
        return n;
    }
    
    public static SuiteNode fromLslTestSuite(LslTestSuite suite, boolean[] dirty) {
        SuiteNode suiteNode = new SuiteNode(null, suite.getResource().getProjectRelativePath().lastSegment());
        suiteNode.setResource((IFile)suite.getResource());
        for (int i = 0; i < suite.getTests().length; i++) {
            LslTest t = suite.getTests()[i];
            TestNode node = null;
            try {
                node = new TestNode(suiteNode,t.getName(), t.getEntryPoint().getFileName() +
                        "/" + t.getEntryPoint().getPath()); //$NON-NLS-1$
            } catch (RuntimeException e) {
                Util.error(e, "couldn't load test"); //$NON-NLS-1$
                if (dirty.length >= 0) dirty[0] = true;
                continue;
            }
            
            
            suiteNode.addChild(node);
            
            ArgumentsListNode argsList = (ArgumentsListNode) node.findChildByName("args"); //$NON-NLS-1$
            List<Node> args = argsList.findChildrenByType(ArgumentNode.class);
            if (args.size() == t.getArguments().length) {
                int j = 0;
                for (Node element : args) {
                    LslValue v = t.getArguments()[j++];
                    ArgumentNode argNode = (ArgumentNode) element;
                    argNode.setType(LslTest.lslTypeToString(v.getClass()));
                    argNode.setValue(v.toString());
                }
            }
            
            ExpectedReturnNode returns = (ExpectedReturnNode) node.findChildByName("returns"); //$NON-NLS-1$
            if (t.getExpectedReturn().getVal() != null) {
                returns.type = LslTest.lslTypeToString(t.getExpectedReturn().getVal().getClass());
            }
            returns.setValue(t.getExpectedReturn().getVal()== null ? "" : t.getExpectedReturn().getVal().toString()); //$NON-NLS-1$
            
            BindingListNode initial = (BindingListNode) node.findChildByName("initial"); //$NON-NLS-1$
            
            for (Iterator<GlobBinding> it = t.getInitialBindings().iterator(); it.hasNext(); ) {
                LslTest.GlobBinding binding = it.next();
                BindingNode bn = new BindingNode(initial, binding.getName(), LslTest.lslTypeToString(binding.getValue().getClass()));
                bn.setValue(binding.getValue().toString());
                initial.addChild(bn);
            }
            
            BindingListNode finals = (BindingListNode) node.findChildByName("final"); //$NON-NLS-1$
            
            for (Iterator<GlobBinding> it = t.getFinalBindings().iterator(); it.hasNext(); ) {
                LslTest.GlobBinding binding = it.next();
                BindingNode bn = new BindingNode(finals, binding.getName(), LslTest.lslTypeToString(binding.getValue().getClass()));
                bn.setValue(binding.getValue().toString());
                finals.addChild(bn);
            }
            
            ExpectationsNode expectations = (ExpectationsNode) node.findChildByName("expectations"); //$NON-NLS-1$
            ExpectationsModeNode expectationsMode = (ExpectationsModeNode) expectations.findChildByName("mode"); //$NON-NLS-1$
            expectationsMode.setValue(t.getExpectations().getMode());
            
            List<ExpectedCall> expectedCalls = t.getExpectations().getExpectedCalls();
            
            for (Iterator<ExpectedCall> it = expectedCalls.iterator(); it.hasNext(); ) {
                LslTest.ExpectedCall call = it.next();
                ExpectedCallNode callNode = new ExpectedCallNode(expectations, "call", call.getName()); //$NON-NLS-1$
                ExpectedArgumentsListNode expectedArgs = (ExpectedArgumentsListNode) callNode.findChildByName("args"); //$NON-NLS-1$
                ReturnNode returnVal = (ReturnNode) callNode.findChildByName("returns"); //$NON-NLS-1$
                
                if (returnVal.type.equals(LslTest.lslTypeToString(call.getReturns().getClass()))) {
                    returnVal.setValue(call.getReturns().toString());
                }
                List<MaybeValue> callArgs = call.getArgs();
                List<Node> argNodes = expectedArgs.getChildren();
                Iterator<Node> it2 = argNodes.iterator();
                Iterator<MaybeValue> it1 = callArgs.iterator();
                while (it1.hasNext() && it2.hasNext()) {
                    LslTest.MaybeValue val = it1.next();
                    ExpectedArgumentNode expectNode = (ExpectedArgumentNode) it2.next();
                    String type = expectNode.getType();
                    if (val.getVal() == null) continue;
                    if (!LslTest.lslTypeToString(val.getVal().getClass()).equals(type)) break;
                    expectNode.setValue(val.getVal() == null ? "" : val.getVal().toString()); //$NON-NLS-1$
                }
                expectations.addChild(callNode);
            }
            
        }
        return suiteNode;
    }
    
    public static LslTestSuite toLslTestSuite(SuiteNode suiteNode) {
        LslTestSuite suite = new LslTestSuite();
        suite.setIResource(suiteNode.resource);
        
        List<Node> testNodes = suiteNode.getChildren();
        
        for (Iterator<Node> it = testNodes.iterator(); it.hasNext();) {
            TestNode tn = (TestNode) it.next();
            LslTest test = new LslTest();
            test.setSuite(suite);
            test.setName(tn.getName());
            EntryPoint ep = new EntryPoint();
            ep.setFileName(tn.getFilename());
            ep.setPath(tn.getPath());
            test.setEntryPoint(ep);
            ArgumentsListNode argumentsNode = tn.getArguments();
            List<Node> argumentsList = argumentsNode.getChildren();
            LslValue[] args = new LslValue[argumentsList.size()];
            for (int index = 0; index < args.length; index++) {
                ArgumentNode argNode = (ArgumentNode) argumentsList.get(index);
                args[index] = LslTest.mkLslType(argNode.getType(), argNode.getValueString());
            }
            
            test.setArguments(args);
            ExpectedReturnNode returnNode = tn.getReturn();
            
            if (returnNode.getValue() == null) {
                test.setExpectedReturn(new MaybeValue());
            } else {
                test.setExpectedReturn(new MaybeValue(LslTest.mkLslType(returnNode.type, returnNode.getValueString())));
            }
            
            test.setInitialBindings(convertBindings(tn.getInitialBindings()));
            test.setFinalBindings(convertBindings(tn.getFinalBindings()));
            
            ExpectationsNode expectationsNode = tn.getExpectations();
            CallExpectations expectations = new CallExpectations();
            expectations.setMode(expectationsNode.getMode());
            List<Node> expectedCallNodes = expectationsNode.getExpectedCalls();
            List<ExpectedCall> expectedCalls = expectations.getExpectedCalls();
            for (Iterator<Node> expectIt = expectedCallNodes.iterator(); expectIt.hasNext();) {
                ExpectedCall call = new ExpectedCall();
                ExpectedCallNode callNode = (ExpectedCallNode) expectIt.next();
                
                List<MaybeValue> callArgs = call.getArgs();
                ExpectedArgumentsListNode expectedArgsNode = callNode.getArgumentListNode();
                List<Node> expectedArgNodes = expectedArgsNode.getChildren();
                for (Iterator<Node> argNodeIt = expectedArgNodes.iterator(); argNodeIt.hasNext();) {
                    ExpectedArgumentNode argNode = (ExpectedArgumentNode) argNodeIt.next();
                    if (argNode.getValue() == null) {
                        callArgs.add(new MaybeValue());
                    } else {
                        callArgs.add(new MaybeValue(LslTest.mkLslType(argNode.getType(), 
                                argNode.getValueString())));
                    }
                }
                call.setName(callNode.getValueString());
                
                ReturnNode returnVal = callNode.getReturn();
                call.setReturns(LslTest.mkLslType(returnVal.type, returnVal.getValueString()));
                expectedCalls.add(call);
            }
            test.setExpectations(expectations);
            suite.addTest(test);
        }
        return suite;
        
    }
    
    private static ArrayList<GlobBinding> convertBindings(BindingListNode node) {
        ArrayList<GlobBinding> bindings = new ArrayList<GlobBinding>();
        
        for (Iterator<Node> ib = node.getChildren().iterator(); ib.hasNext();) {
            BindingNode bindingNode = (BindingNode) ib.next();
            GlobBinding binding = new GlobBinding();
            binding.setName(bindingNode.getName());
            binding.setValue(LslTest.mkLslType(bindingNode.type, bindingNode.getValueString()));
            bindings.add(binding);
        }
        
        return bindings;
    }
    
    public static String lslTypeToString(LSLType t) {
    	if (t instanceof LSLType_LLFloat) return "float"; //$NON-NLS-1$
    	if (t instanceof LSLType_LLInteger) return "integer"; //$NON-NLS-1$
    	if (t instanceof LSLType_LLKey) return "key"; //$NON-NLS-1$
    	if (t instanceof LSLType_LLList) return "list"; //$NON-NLS-1$
    	if (t instanceof LSLType_LLRot) return "rotation"; //$NON-NLS-1$
    	if (t instanceof LSLType_LLString) return "string"; //$NON-NLS-1$
    	if (t instanceof LSLType_LLVector) return "vector"; //$NON-NLS-1$
    	return ""; //$NON-NLS-1$
    }
}
