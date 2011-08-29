/**
 * 
 */
package lslplus.gentree;


public interface NodeFactory {
    public String getNodeTypeName();
    public Node createNode(Node parent);
}