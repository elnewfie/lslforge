package lslplus.editor;

import lslplus.gentree.Node;
import org.eclipse.jface.viewers.ITreeContentProvider;
import org.eclipse.jface.viewers.Viewer;

public class GentreeContentProvider implements ITreeContentProvider {
    private Node root;
    public GentreeContentProvider(Node root) {
        this.root = root;
    }
    
    public Object[] getChildren(Object parentElement) {
        Node node = (Node) parentElement;
        
        return node.getChildren().toArray();
    }

    public Object getParent(Object element) {
        Node node = (Node) element;
        return node.getParent();
    }

    public boolean hasChildren(Object element) {
        Node node = (Node) element;
        return node.getChildren().size() > 0;
    }

    public Object[] getElements(Object inputElement) {
        return new Node[] { root };
    }

    public void dispose() {
    }

    public void inputChanged(Viewer viewer, Object oldInput, Object newInput) {
    }
}
