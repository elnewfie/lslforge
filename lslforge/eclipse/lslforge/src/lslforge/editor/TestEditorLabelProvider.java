package lslforge.editor;

import java.util.Iterator;
import java.util.LinkedList;

import lslforge.LslForgePlugin;
import lslforge.gentree.Node;
import lslforge.lsltest.TestProject;

import org.eclipse.jface.viewers.ITableLabelProvider;
import org.eclipse.jface.viewers.LabelProvider;
import org.eclipse.swt.graphics.Image;

public class TestEditorLabelProvider extends LabelProvider implements ITableLabelProvider {
    private LinkedList<Image> images;
    private Image testImage = createImage("icons/test.gif"); //$NON-NLS-1$
    private Image suiteImage = createImage("icons/tsuite.gif"); //$NON-NLS-1$
    private Image returnsImage = createImage("icons/returns.gif"); //$NON-NLS-1$
    private Image argsImage = createImage("icons/args1.gif"); //$NON-NLS-1$
    @SuppressWarnings("unused")
	private Image functionImage = createImage("icons/function.gif"); //$NON-NLS-1$;
    private Image callImage = createImage("icons/call.gif"); //$NON-NLS-1$;
    @SuppressWarnings("unused")
	private Image callListImage = createImage("icons/calls.gif"); //$NON-NLS-1$;
    private Image expectationsImage = createImage("icons/call-expectations.gif"); //$NON-NLS-1$;
    private Image argImage = createImage("icons/arg.gif"); //$NON-NLS-1$
    private Image modeImage = createImage("icons/mode.gif"); //$NON-NLS-1$
    @SuppressWarnings("unused")
	private Image entryPointImage = createImage("icons/entry-point.gif"); //$NON-NLS-1$
    private Image initialGlobalsImage = createImage("icons/initial-globs.gif"); //$NON-NLS-1$
    private Image finalGlobalsImage = createImage("icons/final-globs.gif"); //$NON-NLS-1$
    private Image initialGlobalImage = createImage("icons/initial-glob.gif"); //$NON-NLS-1$
    private Image finalGlobalImage = createImage("icons/final-glob.gif"); //$NON-NLS-1$
    private Image createImage(String path) {
        if (images == null) images = new LinkedList<Image>();
        Image i = LslForgePlugin.createImage(path);
        if (i != null) images.add(i);
        return i;
    }
    

    public Image getColumnImage(Object element, int columnIndex) {
        if (columnIndex > 0) return null;
        if (element instanceof TestProject.SuiteNode) return suiteImage;
        else if (element instanceof TestProject.TestNode) return testImage;
        else if (element instanceof TestProject.ExpectedReturnNode) return returnsImage;
        else if (element instanceof TestProject.ExpectedCallNode) return callImage;
        else if (element instanceof TestProject.ArgumentNode) return argImage;
        else if (element instanceof TestProject.ArgumentsListNode) return argsImage;
        else if (element instanceof TestProject.BindingListNode) {
            if ("final".equals(((Node)element).getName())) return finalGlobalsImage; //$NON-NLS-1$
            else return initialGlobalsImage;
        } else if (element instanceof TestProject.BindingNode) {
            TestProject.BindingNode node = (TestProject.BindingNode) element;
            if ("final".equals(node.getParent().getName())) return finalGlobalImage; //$NON-NLS-1$
            else return initialGlobalImage;
        } else if (element instanceof TestProject.ExpectationsModeNode) return modeImage;
        else if (element instanceof TestProject.ExpectationsNode) return expectationsImage;
        else if (element instanceof TestProject.ExpectedArgumentsListNode) return argsImage;
        else if (element instanceof TestProject.ExpectedArgumentNode) return argImage;
        else if (element instanceof TestProject.ReturnNode) return returnsImage;
        else {
            // TODO: add other node types
            return null;
        }
    }

    public String getColumnText(Object element, int columnIndex) {
        if (columnIndex == 0) return ((Node)element).getNameDisplay();
        else if (columnIndex == 1) return ((Node)element).getValueString();
        return null;
    }

    public void dispose() {
        super.dispose();
        for (Iterator<Image> i = images.iterator(); i.hasNext(); ) {
            Image img = i.next();
            img.dispose();
        }
        
        images.clear();
    }
}
