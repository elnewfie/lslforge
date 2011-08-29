package lslplus.testview;

import java.util.HashMap;
import java.util.LinkedList;

import lslplus.lsltest.TestResult;
import lslplus.lsltest.TestResult.LogMessage;

import org.eclipse.jface.viewers.ITreeContentProvider;
import org.eclipse.jface.viewers.Viewer;

public class TestResultsContentProvider implements ITreeContentProvider {

    private LinkedList<TestResult> list = new LinkedList<TestResult>();
    private HashMap<Object[],Object> map = new HashMap<Object[],Object>();
    public Object[] getChildren(Object parentElement) {
        if (parentElement instanceof TestResult) {
            TestResult tr = (TestResult) parentElement;
            Object[] result = new Object[] {
                tr.getResultInfo(),
                tr.getMessages()
            };
            map.put(result, parentElement);
            return result;
        } else if (parentElement instanceof LogMessage[]) {
            LogMessage[] messages = (LogMessage[]) parentElement;
            return messages;
        }
        return null;
    }

    public Object getParent(Object element) {
        return map.get(element);
    }

    public boolean hasChildren(Object element) {
        return (element instanceof TestResult) ||
               (element instanceof TestResult.LogMessage[] && ((LogMessage[])element).length > 0);
    }

    public Object[] getElements(Object inputElement) {
        return list.toArray();
    }

    public void dispose() {
    }

    public void inputChanged(Viewer viewer, Object oldInput, Object newInput) {
    }

    public void addResult(TestResult result) {
        list.add(result);
    }
    
    public void clear() {
        list.clear();
        map.clear();
    }
}
