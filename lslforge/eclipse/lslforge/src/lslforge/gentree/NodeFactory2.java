package lslforge.gentree;

public interface NodeFactory2 extends NodeFactory {
    public String getNodeCreationId();
    public Node createNode(Node parent, String value);
}
