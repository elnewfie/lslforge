/**
 * 
 */
package lslforge.gentree;

import java.util.ArrayList;
import java.util.Collections;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;


public abstract class Node {
    private String nodeName;
    private Object value;
    private Node parent;
    private ArrayList<Node> children;
    private Node[] childrenArray;
    private HashSet<NodeListener> listeners = new HashSet<NodeListener>();
    
    public Node(Node parent, String nodeName, Object value) {
        this.parent = parent;
        this.nodeName = nodeName;
        this.value = value;
    }
    
    public void addListener(NodeListener l) {
        if (listeners == null) listeners = new HashSet<NodeListener>();
        listeners.add(l);
        
        for (Iterator<Node> i = getChildren().iterator(); i.hasNext();) {
            Node n = i.next();
            n.addListener(l);
        }
    }
    
    public void removeListener(NodeListener l) {
        if (listeners == null) return;
        listeners.remove(l);
        for (Iterator<Node> i = getChildren().iterator(); i.hasNext(); ) {
            Node n = i.next();
            n.removeListener(l);
        }
    }
    
    public String getNameDisplay() {
        return nodeName;
    }
    
    public String getName() {
        return nodeName;
    }
    
    public void setName(String nodeName) {
        this.nodeName = nodeName;
        fireValueChanged();
    }
    
    /**
     * Update name is intended to be called <em>externally</em> from
     * the node tree (i.e. by node tree editors).  Associated with updateName
     * will be all the business logic associated with editing a name of a node.
     * 
     * Contrast with setName, which is intended for simply changing the name of 
     * node without other side effects, except notifying listeners (which should be
     * external!).
     * @param name the new value encoded as a string.
     */
    public void updateName(String name) {
        setName(name);
    }
    
    public Object getValue() {
        return value;
    }
    
    public void setValue(Object value) {
        this.value = value;
        fireValueChanged();
    }
    
    private void fireValueChanged() {
        for (Iterator<NodeListener> i = listeners.iterator(); i.hasNext();) {
            NodeListener listener = i.next();
            listener.nodeValueChanged(this);
        }
    }

    public Node getParent() {
        return parent;
    }
    
    public void setParent(Node parent) {
        this.parent = parent;
    }
    
    public ArrayList<Node> getChildren() {
        if (children == null) {
            if (childrenArray == null) childrenArray = new Node[0];
            children = new ArrayList<Node>();
            Collections.addAll(children, childrenArray);
        }
        return children;
    }
    
    public void addChild(Node n) {
        getChildren().add(n);
        
        propagateListeners(n);
        fireStructureChanged();
    }
    
    public void addChild(Node n, int index) {
        if (index < 0 || index > getChildren().size()) getChildren().add(n);
        else getChildren().add(index, n);
        propagateListeners(n);
        fireStructureChanged();
    }

    private void propagateListeners(Node n) {
        for (Iterator<NodeListener> i = listeners.iterator(); i.hasNext();) {
            NodeListener l = i.next();
            n.addListener(l);
        }
    }

    public void insertChildBefore(Node newChild, Node existingChild) {
        int index = getChildren().indexOf(existingChild);
        if (index < 0) getChildren().add(0, newChild);
        else getChildren().add(index, newChild);
        
        propagateListeners(newChild);
        fireStructureChanged();
    }
    
    public void insertChildAfter(Node newChild, Node existingChild) {
        int index = getChildren().indexOf(existingChild);
        if (index < 0) getChildren().add(newChild);
        else getChildren().add(index + 1, newChild);
        
        propagateListeners(newChild);
        fireStructureChanged();
    }
    
    
    public void removeChild(Node n) {
        if (getChildren().contains(n)) n.onRemove();
        onChildRemoved(n);
        if (getChildren().remove(n)) fireStructureChanged();
    }
    
    private void fireStructureChanged() {
        for (Iterator<NodeListener> i = listeners.iterator(); i.hasNext();) {
            NodeListener listener = i.next();
            listener.nodeStructureChanged(this);
        }
    }

    protected void childUpdated(Node child, Object oldValue) {
    }
    
    protected void onRemove() {
        
    }
    
    /**
     * Called when a child is about to be removed but BEFORE the removal occurs.
     * @param n
     */
    protected void onChildRemoved(Node n) {
        
    }
    
    public abstract NodeFactory[] legalChildNodes();
    
    public abstract String getValueString();
    
    public abstract NodeStatus checkValueString(String s);
    
    /**
     * Update value is intended to be called <em>externally</em> from
     * the node tree (i.e. by node tree editors).  Associated with updateValue
     * will be all the business logic associated with editing a value of a node.
     * 
     * Contrast with setValue, which is intended for simply changing the value of 
     * node without other side effects, except notifying listeners (which should be
     * external!).
     * @param s the new value encoded as a string.
     */
    public final void updateValue(String s) {
        Object oldValue = getValue();
        onUpdate(s);
        notifyAncestors(oldValue, this);
    }
    
    private void notifyAncestors(Object oldValue, Node n) {
        Node cur = this.getParent();
        
        while (cur != null) {
            cur.childUpdated(n, oldValue);
            cur = cur.getParent();
        }
    }
    
    protected abstract void onUpdate(String s);
    public abstract boolean isValueChangeable();
    
    public abstract boolean isNameChangeable();
    
    public abstract NodeStatus checkNameString(String name);
    
    public abstract boolean isDeletable();

    public boolean isClearable() {
        return false;
    }
    
    public void accept(NodeVisitor visitor) {
        visitor.visit(this);
        for (Iterator<?> i = getChildren().iterator(); i.hasNext();) {
            Node n = (Node) i.next();
            n.accept(visitor);
        }
    }
    
    public Node findChildByName(String name) {
        for (Iterator<?> i = getChildren().iterator(); i.hasNext();) {
            Node n = (Node) i.next();
            if (n.getName().equals(name)) {
                return n;
            }
        }
        
        return null;
    }
    
    public List<Node> findChildrenByType(Class<?> c) {
        ArrayList<Node> list = new ArrayList<Node>();
        for (Iterator<?> i = getChildren().iterator(); i.hasNext();) {
            Node n = (Node) i.next();
            if (c.isAssignableFrom(n.getClass())) {
                list.add(n);
            }
        }
        
        return list;
    }
    
    public boolean hasValueChoices() {
        return false;
    }
    
    public String getChoicesId() {
        return null;
    }
    
    public void propagateParent() {
        for (Iterator<?> i = getChildren().iterator(); i.hasNext();) {
            Node n = (Node) i.next();
            n.setParent(this);
            n.propagateParent();
        }
    }
    
    public final void fix() {
        if (this.listeners == null) this.listeners = new HashSet<NodeListener>();
        onFix();
        for (Iterator<?> i = getChildren().iterator(); i.hasNext();) {
            Node n = (Node) i.next();
            n.fix();
        }
    }
    
    protected void onFix() {
    }
    
    public void syncChildren() {
        List<?> children = getChildren();
        childrenArray = children.toArray(new Node[children.size()]);
        
        for (int i = 0; i < childrenArray.length; i++) {
            childrenArray[i].syncChildren();
        }
    }
    
    public Node findRoot() {
        if (getParent() == null) return this;
        return getParent().findRoot();
    }
    
    public boolean isFirstChildOfType(Node n, Class<?> c) {
        List<Node> l = findChildrenByType(c);
        
        return l.size() > 0 && n == l.get(0);
    }
    
    public Node findAncestorOfType(Class<?> c) {
        Node n = this;
        
        while (n != null && !n.getClass().isAssignableFrom(c)) {
            n = n.getParent();
        }
        
        return n;
    }
}