package lslplus.editor;

import java.io.ByteArrayInputStream;
import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.LinkedList;
import java.util.List;

import lslplus.LslPlusPlugin;
import lslplus.LslProjectNature;
import lslplus.generated.GlobalSummary;
import lslplus.generated.GlobalSummary_GlobalSummary;
import lslplus.gentree.Node;
import lslplus.gentree.NodeFactory;
import lslplus.gentree.NodeFactory2;
import lslplus.gentree.NodeListener;
import lslplus.gentree.NodeStatus;
import lslplus.lsltest.LslTest;
import lslplus.lsltest.LslTestSuite;
import lslplus.lsltest.TestProject;
import lslplus.lsltest.TestProject.BindingListNode;
import lslplus.lsltest.TestProject.SuiteNode;
import lslplus.lsltest.TestProject.TestNode;
import lslplus.util.Util;

import org.eclipse.core.commands.ExecutionException;
import org.eclipse.core.commands.operations.AbstractOperation;
import org.eclipse.core.commands.operations.IOperationHistory;
import org.eclipse.core.commands.operations.IUndoContext;
import org.eclipse.core.commands.operations.UndoContext;
import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IAdaptable;
import org.eclipse.core.runtime.IPath;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.jface.action.Action;
import org.eclipse.jface.action.IMenuListener;
import org.eclipse.jface.action.IMenuManager;
import org.eclipse.jface.action.MenuManager;
import org.eclipse.jface.dialogs.Dialog;
import org.eclipse.jface.dialogs.IDialogConstants;
import org.eclipse.jface.viewers.CellEditor;
import org.eclipse.jface.viewers.CellLabelProvider;
import org.eclipse.jface.viewers.ColumnViewer;
import org.eclipse.jface.viewers.ComboBoxCellEditor;
import org.eclipse.jface.viewers.EditingSupport;
import org.eclipse.jface.viewers.ICellEditorValidator;
import org.eclipse.jface.viewers.ISelectionChangedListener;
import org.eclipse.jface.viewers.ITreeSelection;
import org.eclipse.jface.viewers.SelectionChangedEvent;
import org.eclipse.jface.viewers.TextCellEditor;
import org.eclipse.jface.viewers.TreeViewer;
import org.eclipse.jface.viewers.TreeViewerColumn;
import org.eclipse.jface.viewers.ViewerCell;
import org.eclipse.jface.window.Window;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.events.SelectionListener;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Combo;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Event;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Listener;
import org.eclipse.swt.widgets.Menu;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.swt.widgets.Tree;
import org.eclipse.swt.widgets.TreeColumn;
import org.eclipse.ui.IEditorInput;
import org.eclipse.ui.IEditorSite;
import org.eclipse.ui.PartInitException;
import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.operations.UndoRedoActionGroup;
import org.eclipse.ui.part.EditorPart;

public class TestEditor extends EditorPart implements NodeListener {
    private class EntryPointSelectionDialog extends Dialog {
        private String fileName = null;
        private String path = null;
        private Combo combo;
        private Combo combo2;
        protected EntryPointSelectionDialog(Shell parentShell) {
            super(parentShell);
        }

        private Button okButton() {
            return getButton(IDialogConstants.OK_ID);
        }
        
        protected Control createButtonBar(Composite parent) {
             Control c = super.createButtonBar(parent);
             
             okButton().addListener(SWT.Show, new Listener() {
                public void handleEvent(Event event) {
                    okButton().setEnabled(selectionIsValid());
                }
             });
             return c;
        }

        private boolean selectionIsValid() {
            return combo.getSelectionIndex() >= 0 && combo2.getSelectionIndex() >= 0;
        }
        
        protected Control createDialogArea(Composite parent) {
            getShell().setText(Messages.getString("LslTestEditor.ENTER_TEST_ENTRY_POINT")); //$NON-NLS-1$
            Composite  composite = (Composite) super.createDialogArea(parent);
            GridLayout layout = (GridLayout) composite.getLayout();
            layout.numColumns = 2;
            Label fileNameLabel = new Label(composite, SWT.LEFT|SWT.HORIZONTAL);
            fileNameLabel.setText(Messages.getString("LslTestEditor.FILE_NAME")); //$NON-NLS-1$
            Label pathLabel = new Label(composite, SWT.LEFT|SWT.HORIZONTAL);
            pathLabel.setText(Messages.getString("LslTestEditor.ENTRY_POINT")); //$NON-NLS-1$
            
            combo = new Combo(composite, SWT.READ_ONLY|SWT.DROP_DOWN);
            combo2 = new Combo(composite, SWT.READ_ONLY|SWT.DROP_DOWN);
            
            String[] files = nature.getLslFiles();
            combo.setItems(files);
            combo.deselectAll();
            // a mild hack (there are preferred ways, but this is simplest) to widen the disabled combo
            combo2.setItems(new String[] { "MMMMMMMMMMMMMMM"}); //$NON-NLS-1$
            combo2.deselectAll();
            combo2.setEnabled(false);
            
            combo.addSelectionListener(new SelectionListener() {
                public void widgetDefaultSelected(SelectionEvent e) {
                }

                public void widgetSelected(SelectionEvent e) {
                    if (combo.getSelectionIndex() >= 0) {
                        String fileName = combo.getItem(combo.getSelectionIndex());
                        combo2.setItems(nature.getEntryPointNames(fileName));
                        combo2.deselectAll();
                        combo2.setEnabled(true);
                        EntryPointSelectionDialog.this.fileName = fileName;
                        path = null;
                        okButton().setEnabled(false);
                    } else {
                        EntryPointSelectionDialog.this.fileName = null;
                        combo2.setEnabled(false);
                        okButton().setEnabled(false);
                    }
                    if (LslPlusPlugin.DEBUG) Util.log("fileName = " + fileName + ", path = " + path); //$NON-NLS-1$ //$NON-NLS-2$
                }
            });
            
            combo2.addSelectionListener(new SelectionListener() {
                public void widgetDefaultSelected(SelectionEvent e) {
                    okButton().setEnabled(combo2.getSelectionIndex() >= 0);
                }

                public void widgetSelected(SelectionEvent e) {
                    okButton().setEnabled(combo2.getSelectionIndex() >= 0);
                    
                    if (combo2.getSelectionIndex() >= 0) {
                        path = combo2.getItem(combo2.getSelectionIndex());
                    } else {
                        path = null;
                    }
                    if (LslPlusPlugin.DEBUG) Util.log("path = " + path); //$NON-NLS-1$
                }
            });
            return composite;
        }
        
        public String getFilename() {
            return fileName;
        }
        
        public String getPath() {
            return path;
        }
    }
    
    private class GlobalSelectionDialog extends Dialog {
        private GlobalSummary_GlobalSummary pair;
        private Combo combo;
        private HashMap<String,GlobalSummary_GlobalSummary> pairsMap = 
        	new HashMap<String,GlobalSummary_GlobalSummary>();
        protected GlobalSelectionDialog(Shell parentShell, String fileName, List<String> globs) {
            super(parentShell);
            
            pairsMap = determineRemainingGlobals(fileName, globs);
        }

        private HashMap<String,GlobalSummary_GlobalSummary> 
        determineRemainingGlobals(String fileName, List<String> globs) {
            LinkedList<GlobalSummary> pairs = nature.getGlobalVariables(fileName);
//            Set<String> used = Util.mapToSet(new ArrayMapFunc() {
//                public Class elementType() { return String.class; }
//                public Object map(Object o) { return o; }
//            }, globs);
            HashSet<String> used = new HashSet<String>();
            for (String s : globs) {
            	used.add(s);
            }
            
            HashMap<String,GlobalSummary_GlobalSummary> map = 
            	new HashMap<String,GlobalSummary_GlobalSummary>();
            //for (int i = 0; i < pairs.length; i++) {
            for (GlobalSummary s0 : pairs) {
            	GlobalSummary_GlobalSummary s = (GlobalSummary_GlobalSummary) s0;
                if (!used.contains(s.globalName)) {
                    map.put(s.globalName, s);
                }
            }
            return map;
        }
        private Button okButton() {
            return getButton(IDialogConstants.OK_ID);
        }
        
        private boolean selectionIsValid() {
            return combo.getSelectionIndex() >= 0;
        }

        protected Control createButtonBar(Composite parent) {
            Control c = super.createButtonBar(parent);
            okButton().setEnabled(false);
            okButton().addListener(SWT.Show, new Listener() {
               public void handleEvent(Event event) {
                   okButton().setEnabled(selectionIsValid());
               }
            });
            return c;
        }
        
        protected Control createDialogArea(Composite parent) {
            getShell().setText("Select Global"); //$NON-NLS-1$ TODO
            Composite  composite = (Composite) super.createDialogArea(parent);
            Label nameLabel = new Label(composite, SWT.LEFT|SWT.HORIZONTAL);
            nameLabel.setText("Variable Name"); //$NON-NLS-1$ TODO
            
            combo = new Combo(composite, SWT.READ_ONLY|SWT.DROP_DOWN);

            ArrayList<String> l = new ArrayList<String>();
            l.addAll(pairsMap.keySet());
            Collections.sort(l);
            final String[] items = l.toArray(new String[l.size()]);
            combo.setItems(items);
            combo.deselectAll();
            
            combo.addSelectionListener(new SelectionListener() {
                public void widgetDefaultSelected(SelectionEvent e) {
                }

                public void widgetSelected(SelectionEvent e) {
                    if (combo.getSelectionIndex() >= 0) {
                        String name = items[combo.getSelectionIndex()];
                        pair = pairsMap.get(name);
                        okButton().setEnabled(true);
                    } else {
                        okButton().setEnabled(false);
                    }
                }
            });
            
            return composite;
        }

        
        public GlobalSummary_GlobalSummary getPair() { return pair; }
    }
    private class CallSelectionDialog extends Dialog {
        private String name;
        private Combo combo;
        protected CallSelectionDialog(Shell parentShell, String name) {
            super(parentShell);
            this.name = name;
        }
        
        private Button okButton() {
            return getButton(IDialogConstants.OK_ID);
        }
        
        private boolean selectionIsValid() {
            return combo.getSelectionIndex() >= 0;
        }

        protected Control createButtonBar(Composite parent) {
            Control c = super.createButtonBar(parent);
            
            okButton().addListener(SWT.Show, new Listener() {
               public void handleEvent(Event event) {
                   okButton().setEnabled(selectionIsValid());
               }
            });
            return c;
        }
        
        protected Control createDialogArea(Composite parent) {
            getShell().setText("Expected function call"); //$NON-NLS-1$ TODO
            Composite  composite = (Composite) super.createDialogArea(parent);
            Label nameLabel = new Label(composite, SWT.LEFT|SWT.HORIZONTAL);
            nameLabel.setText("Function name"); //$NON-NLS-1$
            
            combo = new Combo(composite, SWT.READ_ONLY|SWT.DROP_DOWN);

            String[] items = LslPlusPlugin.getStatefulFunctions();
            int index = Util.elementIndex(name, items);
            
            combo.setItems(items);
            if (index >= 0) combo.select(index);
            else combo.deselectAll();
            
            combo.addSelectionListener(new SelectionListener() {
                public void widgetDefaultSelected(SelectionEvent e) {
                }

                public void widgetSelected(SelectionEvent e) {
                    if (combo.getSelectionIndex() >= 0) {
                        name = combo.getItem(combo.getSelectionIndex());
                        okButton().setEnabled(true);
                    } else {
                        okButton().setEnabled(false);
                    }
                }
            });
            
            return composite;
        }

        public String getName() { return name; }
    }

    
    private class UpdateValueOperation extends AbstractOperation {
        private Node n;
        private String value;
        private String oldValue;
        public UpdateValueOperation(Node n, String value) {
            super("Update Value"); //$NON-NLS-1$ TODO
            this.n = n;
            this.value = value;
        }

        public IStatus execute(IProgressMonitor monitor, IAdaptable info) throws ExecutionException {
            oldValue = n.getValueString();
            n.updateValue(value);
            incChangeCount();
            return Status.OK_STATUS;
        }

        public IStatus redo(IProgressMonitor monitor, IAdaptable info) throws ExecutionException {
            oldValue = n.getValueString();
            n.updateValue(value);
            incChangeCount();
            return Status.OK_STATUS;
        }

        public IStatus undo(IProgressMonitor monitor, IAdaptable info) throws ExecutionException {
            n.updateValue(oldValue);
            decChangeCount();
            return Status.OK_STATUS;
        }
        
    }

    private class UpdateNameOperation extends AbstractOperation {
        private Node n;
        private String name;
        private String oldName;
        public UpdateNameOperation(Node n, String name) {
            super("Update Value"); //$NON-NLS-1$ TODO
            this.n = n;
            this.name = name;
        }

        public IStatus execute(IProgressMonitor monitor, IAdaptable info) throws ExecutionException {
            oldName = n.getNameDisplay();
            n.updateName(name);
            incChangeCount();
            return Status.OK_STATUS;
        }

        public IStatus redo(IProgressMonitor monitor, IAdaptable info) throws ExecutionException {
            oldName = n.getNameDisplay();
            n.updateName(name);
            incChangeCount();
            return Status.OK_STATUS;
        }

        public IStatus undo(IProgressMonitor monitor, IAdaptable info) throws ExecutionException {
            n.updateName(oldName);
            decChangeCount();
            return Status.OK_STATUS;
        }
        
    }

	private class AddNodeOperation extends AbstractOperation {
	    private Node n;
	    private NodeFactory factory;
	    private Node addedNode = null;
        public AddNodeOperation(Node n, NodeFactory factory) {
            super("Add Child"); //$NON-NLS-1$ TODO
            this.n = n;
            this.factory = factory;
        }

        public IStatus execute(IProgressMonitor monitor, IAdaptable info) throws ExecutionException {
            n.addChild(addedNode = factory.createNode(n));
            incChangeCount();
            return Status.OK_STATUS;
        }

        public IStatus redo(IProgressMonitor monitor, IAdaptable info) throws ExecutionException {
            n.addChild(addedNode = factory.createNode(n));
            incChangeCount();
            return Status.OK_STATUS;
        }

        public IStatus undo(IProgressMonitor monitor, IAdaptable info) throws ExecutionException {
            n.removeChild(addedNode);
            decChangeCount();
            return Status.OK_STATUS;
        }
	    
	}
	
    private class AddAfterOperation extends AbstractOperation {
        private Node n;
        private NodeFactory factory;
        private Node addedNode = null;
        public AddAfterOperation(Node n, NodeFactory factory) {
            super("Add after"); //$NON-NLS-1$ TODO
            this.n = n;
            this.factory = factory;
        }

        public IStatus execute(IProgressMonitor monitor, IAdaptable info) throws ExecutionException {
            Node parent = n.getParent();
            if (parent != null) {
                parent.insertChildAfter(addedNode = factory.createNode(parent), n);
                incChangeCount();
            }
            return Status.OK_STATUS;
        }

        public IStatus redo(IProgressMonitor monitor, IAdaptable info) throws ExecutionException {
            Node parent = n.getParent();
            if (parent != null) {
                parent.insertChildAfter(addedNode = factory.createNode(parent), n);
                incChangeCount();
            }
            return Status.OK_STATUS;
        }

        public IStatus undo(IProgressMonitor monitor, IAdaptable info) throws ExecutionException {
            if (n.getParent() != null) {
                n.getParent().removeChild(addedNode);
                decChangeCount();
            }
            return Status.OK_STATUS;
        }
        
    }
    
    private class AddBeforeOperation extends AbstractOperation {
        private Node n;
        private NodeFactory factory;
        private Node addedNode = null;
        public AddBeforeOperation(Node n, NodeFactory factory) {
            super("Add before"); //$NON-NLS-1$ TODO
            this.n = n;
            this.factory = factory;
        }

        public IStatus execute(IProgressMonitor monitor, IAdaptable info) throws ExecutionException {
            Node parent = n.getParent();
            if (parent != null) {
                incChangeCount();
                parent.insertChildBefore(addedNode = factory.createNode(parent), n);
            }
            return Status.OK_STATUS;
        }

        public IStatus redo(IProgressMonitor monitor, IAdaptable info) throws ExecutionException {
            Node parent = n.getParent();
            if (parent != null) {
                incChangeCount();
                parent.insertChildBefore(addedNode = factory.createNode(parent), n);
            }
            return Status.OK_STATUS;
        }

        public IStatus undo(IProgressMonitor monitor, IAdaptable info) throws ExecutionException {
            if (n.getParent() != null) {
                n.getParent().removeChild(addedNode);
                decChangeCount();
            }
            return Status.OK_STATUS;
        }
    }
    
	private class DeleteNodeOperation extends AbstractOperation {
	    Node parent;
	    Node child;
	    int index;
        public DeleteNodeOperation(Node parent, Node child, int index) {
            super("Delete node"); //$NON-NLS-1$ TODO
            this.parent = parent;
            this.child = child;
            this.index = index;
        }

        public IStatus execute(IProgressMonitor monitor, IAdaptable info) throws ExecutionException {
            parent.removeChild(child);
            incChangeCount();
            return Status.OK_STATUS;
        }

        public IStatus redo(IProgressMonitor monitor, IAdaptable info) throws ExecutionException {
            parent.removeChild(child);
            incChangeCount();
            return Status.OK_STATUS;
        }

        public IStatus undo(IProgressMonitor monitor, IAdaptable info) throws ExecutionException {
            parent.addChild(child,index);
            decChangeCount();
            return Status.OK_STATUS;
        }
	}
	
    private class AddNodeAction extends Action {
        private NodeFactory factory;
        public AddNodeAction(NodeFactory factory) {
            super("Add " + factory.getNodeTypeName()); //$NON-NLS-1$
            this.factory = factory;
        }
        
        public void run() {
            Node n = getSelectedNode();
            NodeFactory f = null;
            if (factory instanceof NodeFactory2) {
                NodeFactory2 fac2 = (NodeFactory2) factory;
                f = handleNodeFactory2(n, fac2);
                if (f == null) return;
            } else {
                f = factory;
            }
            AddNodeOperation operation = new AddNodeOperation(n, f);
            operation.addContext(undoContext);
            try {
                operationsHistory.execute(operation, null, null);
            } catch (ExecutionException e) {
                Util.error(e, e.getLocalizedMessage());
            }
        }
    }
    
    private class AddNodeBeforeAction extends Action {
        private NodeFactory factory;
        public AddNodeBeforeAction(NodeFactory factory) {
            super("Add " + factory.getNodeTypeName()); //$NON-NLS-1$
            this.factory = factory;
        }
        
        public void run() {
            Node n = getSelectedNode();
            NodeFactory f = factory;
            if (f instanceof NodeFactory2) {
                f = handleNodeFactory2(n.getParent(), (NodeFactory2)f);
                if (f == null) return;
            }
            AddBeforeOperation operation = new AddBeforeOperation(n, f);
            operation.addContext(undoContext);
            try {
                operationsHistory.execute(operation, null, null);
            } catch (ExecutionException e) {
                Util.error(e, e.getLocalizedMessage());
            }
        }
    }
    
    private class AddNodeAfterAction extends Action {
        private NodeFactory factory;
        public AddNodeAfterAction(NodeFactory factory) {
            super("Add " + factory.getNodeTypeName()); //$NON-NLS-1$
            this.factory = factory;
        }
        
        public void run() {
            Node n = getSelectedNode();
            NodeFactory f = factory;
            if (f instanceof NodeFactory2) {
                f = handleNodeFactory2(n.getParent(), (NodeFactory2)f);
                if (f == null) return;
            }
            AddAfterOperation operation = new AddAfterOperation(n, f);
            operation.addContext(undoContext);
            try {
                operationsHistory.execute(operation, null, null);
            } catch (ExecutionException e) {
                Util.error(e, e.getLocalizedMessage());
            }
        }
    }
    
    private class DeleteNodeAction extends Action {
        public DeleteNodeAction() {
            super("Delete Node"); //$NON-NLS-1$
        }
        
        public void run() {
            Node n = getSelectedNode();
            int index = n.getParent().getChildren().indexOf(n);
            DeleteNodeOperation operation = new DeleteNodeOperation(n.getParent(), n, index);
            operation.addContext(undoContext);
            try {
                operationsHistory.execute(operation, null, null);
            } catch (ExecutionException e) {
                Util.error(e, e.getLocalizedMessage());
            }
        }
    }
    
    
	private class SimValueEditingSupport extends EditingSupport {
	    private CellEditor curEditor = null;
	    private String[] curChoices = null;
		private SimValueEditingSupport(ColumnViewer viewer, Shell shell) {
			super(viewer);
		}

		protected boolean canEdit(Object element) {
		    Node n = (Node) element;
			return n.isValueChangeable();
		}

		protected CellEditor getCellEditor(Object element) {
			final Node n = (Node) element;
			
			if (n.hasValueChoices()) {
			    String[] choices;
			    if ("expectations-mode".equals(n.getChoicesId())) { //$NON-NLS-1$
			        choices = LslTest.CallExpectations.getModes().toArray(new String[0]);
			    } else {
			        choices = new String[0];
			    }
			    curChoices = choices;
			    curEditor = new ComboBoxCellEditor(fTree, choices);
			} else {
			    curEditor = new TextCellEditor(fTree);
			    curEditor.setValidator(new ICellEditorValidator() {
                    public String isValid(Object value) {
                        if (value == null) return null;
                        NodeStatus status = n.checkValueString(value.toString());
                        if (status.isOk()) return null;
                        return status.toString();
                    }
			        
			    });
			    
			}
			return curEditor;
		}

		protected Object getValue(Object element) {
			Node n = (Node) element;
			if (n.hasValueChoices()) {
			    String value = n.getValueString();
			    int index = -1;
			    for (int i = 0; i < curChoices.length; i++) {
			        if (curChoices[i].equals(value)) {
			            index = i;
			            break;
			        }
			    }
			    
			    return new Integer(index);
			} else {
			    return n.getValueString();
			}
		}

		protected void setValue(Object element, Object value) {
			Node n = (Node) element;
			String newValue = null;
			if (n.hasValueChoices()) {
			    Integer i = (Integer) value;
			    
			    if (i.intValue() < 0) return;
			    newValue = curChoices[i.intValue()];
			    //n.setValueFromString(curChoices[i.intValue()]);
			    
			} else {
			    if (value == null) return;
			    NodeStatus status = n.checkValueString((String)value);
			    if (status.isOk()) newValue = (String)value;
			}
			
			if (newValue.equals(n.getValueString())) return;
			
            UpdateValueOperation operation = new UpdateValueOperation(n, newValue);
            operation.addContext(undoContext);
            try {
                operationsHistory.execute(operation, null, null);
            } catch (ExecutionException e) {
                Util.error(e, e.getLocalizedMessage());
            }
		}
	}

   private class SimNameEditingSupport extends EditingSupport {
        private CellEditor curEditor = null;
        private SimNameEditingSupport(ColumnViewer viewer, Shell shell) {
            super(viewer);
        }

        protected boolean canEdit(Object element) {
            Node n = (Node) element;
            return n.isNameChangeable();
        }

        protected CellEditor getCellEditor(Object element) {
            final Node n = (Node) element;
            
            curEditor = new TextCellEditor(fTree);
            curEditor.setValidator(new ICellEditorValidator() {
                public String isValid(Object value) {
                    NodeStatus status = n.checkNameString(value.toString());
                    if (status.isOk()) return null;
                    return status.toString();
                }
                
            });
            return curEditor;
        }

        protected Object getValue(Object element) {
            Node n = (Node) element;
            return n.getNameDisplay();
        }

        protected void setValue(Object element, Object value) {
            Node n = (Node) element;
            if (value == null) return;            
            NodeStatus status = n.checkNameString((String)value);
            if (!status.isOk()) return;
            if (value.equals(n.getNameDisplay())) return;
            UpdateNameOperation operation = new UpdateNameOperation(n, (String)value);
            operation.addContext(undoContext);
            try {
                operationsHistory.execute(operation, null, null);
            } catch (ExecutionException e) {
                Util.error(e, e.getLocalizedMessage());
            }
        }
    }

	private TreeColumn fColumn1;
	private TreeColumn fColumn2;
	private TreeViewer fTreeViewer;
	private Tree fTree;
	private TestEditorLabelProvider fLabelProvider;
	private SuiteNode world = null;
	@SuppressWarnings("unused")
	private String simProjectName = null;
	private LslProjectNature nature = null;
	private int changeCount = 0;
	private IFile file;
	private DeleteNodeAction fDeleteNodeAction;
	private static final String BLANK = ""; //$NON-NLS-1$
	private IOperationHistory operationsHistory;
	private IUndoContext undoContext;
	
	public TestEditor() {
	}

	void incChangeCount() {
	    int curCount = changeCount;
	    changeCount++;
	    fireIfDirtyStateChanged(curCount);
    }

	void decChangeCount() {
        int curCount = changeCount;
        changeCount--;
        fireIfDirtyStateChanged(curCount);
    }

    private void fireIfDirtyStateChanged(int curCount) {
        if (curCount == 0 && changeCount != 0 ||
            changeCount == 0 && curCount != 0) firePropertyChange(PROP_DIRTY);
    }

	void zeroChangeCount() {
	    int curCount = changeCount;
	    changeCount = 0;
	    fireIfDirtyStateChanged(curCount);
	}
	
    public void doSave(IProgressMonitor monitor) {
		String val = TestProject.toLslTestSuite(world).toXml();
		if (LslPlusPlugin.DEBUG) Util.log("world = " + val); //$NON-NLS-1$
		try {
			file.setContents(new ByteArrayInputStream(val.getBytes()), IResource.FORCE | IResource.KEEP_HISTORY, monitor);
			zeroChangeCount();
		} catch (CoreException e) {
			Util.error(e, e.getLocalizedMessage());
		}
		
	}

	public void doSaveAs() {
	}

	public void init(IEditorSite site, IEditorInput input)
			throws PartInitException {
		this.setSite(site);  
		this.setInput(input);
        operationsHistory = this.getSite().getWorkbenchWindow().getWorkbench().getOperationSupport().getOperationHistory();
		undoContext = new UndoContext();
		this.setPartName(input.getName());
		file = (IFile) input.getAdapter(IFile.class);
		try {
			nature = (LslProjectNature) file.getProject().getNature(LslProjectNature.ID);
		} catch (CoreException e1) {
			throw new PartInitException("Can't get project nature", e1); //$NON-NLS-1$
		}
		IPath fullPath = file.getFullPath();
		simProjectName = fullPath.removeFileExtension().lastSegment();
		if (file != null) {
			try {
			    boolean dirty[] = { false };
				world = TestProject.fromLslTestSuite(LslTestSuite.fromXml(file.getContents(), file), dirty);
				if (dirty[0]) incChangeCount();
			} catch (CoreException e) {
				Util.error(e, "Corrupted sim project file: " + e.getMessage()); //$NON-NLS-1$
				world = null;
			} catch (Exception e) {
                Util.error(e, "Corrupted sim project file: " + e.getMessage()); //$NON-NLS-1$
                world = null;
			    
			}
		}
	}

	public boolean isDirty() {
		return changeCount != 0;
	}

	public boolean isSaveAsAllowed() {
		return false;
	}

	public void createPartControl(Composite parent) {
        PlatformUI.getWorkbench().getHelpSystem().setHelp(parent, "lslplus.simProjectEditor"); //$NON-NLS-1$
		//control = new Composite(parent, SWT.NULL);
		//control.setVisible(true);
		createViewer(parent);
		createUndoRedoActions();
	}

	public void setFocus() {
	}

	private void createViewer(Composite parent) {
		fTreeViewer = new TreeViewer(parent, SWT.SINGLE | SWT.FULL_SELECTION);
		fTree = fTreeViewer.getTree();
		fTree.setLinesVisible(true);
		createColumns(fTree);
		world.addListener(this);
		fTreeViewer.setContentProvider(new GentreeContentProvider(world));
		fTreeViewer.setLabelProvider(fLabelProvider = new TestEditorLabelProvider());
		fTreeViewer.addSelectionChangedListener(new ISelectionChangedListener() {
			public void selectionChanged(SelectionChangedEvent e) {
				//TreeSelection selection = (TreeSelection) e.getSelection();
				
			}
		});
		fTreeViewer.setInput(this);
		
		TreeViewerColumn column2 = new TreeViewerColumn(fTreeViewer, fColumn2);
		column2.setLabelProvider(new CellLabelProvider() {
			public void update(ViewerCell cell) {
			    Node n = (Node) cell.getElement();
			    cell.setImage(fLabelProvider.getColumnImage(n,1));
                if (n == null) cell.setText(BLANK);
				else cell.setText(n.getValueString());
			}
		});
        TreeViewerColumn column1 = new TreeViewerColumn(fTreeViewer, fColumn1);
        column1.setLabelProvider(new CellLabelProvider() {
            public void update(ViewerCell cell) {
                Node n = (Node) cell.getElement();
                cell.setImage(fLabelProvider.getColumnImage(n,0));
                if (n == null) cell.setText(BLANK);
                else cell.setText(n.getNameDisplay());
            }
        });
		
		column2.setEditingSupport(new SimValueEditingSupport(fTreeViewer, getShell()));
		column1.setEditingSupport(new SimNameEditingSupport(fTreeViewer, getShell()));
		
		fDeleteNodeAction = new DeleteNodeAction();
		
		MenuManager popupMenuManager = new MenuManager("#PopupMenu"); //$NON-NLS-1$
		IMenuListener listener = new IMenuListener() {
			public void menuAboutToShow(IMenuManager manager) {
				Node n = getSelectedNode();
				NodeFactory[] factories = n.legalChildNodes();
				
				if (factories.length > 0) {
				    MenuManager addChildSubMenu = new MenuManager("Add Child"); //$NON-NLS-1$ TODO
	                for (int i = 0; i < factories.length; i++) {
	                    manager.add(new AddNodeAction(factories[i]));
	                }
	                manager.add(addChildSubMenu);
				}
				
				Node parent = n.getParent();
				if (parent != null) {
				    factories = parent.legalChildNodes();
				    if (factories.length > 0) {
    				    MenuManager addItemBeforeSubMenu = new MenuManager("Add Item Before"); //$NON-NLS-1$ TODO
                        MenuManager addItemAfterSubMenu = new MenuManager("Add Item After"); //$NON-NLS-1$ TODO
                        
                        for (int i = 0; i < factories.length; i++) {
                            addItemBeforeSubMenu.add(new AddNodeBeforeAction(factories[i]));
                            addItemAfterSubMenu.add(new AddNodeAfterAction(factories[i]));
                        }
                        manager.add(addItemBeforeSubMenu);
                        manager.add(addItemAfterSubMenu);
                        
				    }
				}
				if (n != null && n.isDeletable()) manager.add(fDeleteNodeAction);
			}
		};
		popupMenuManager.addMenuListener(listener);
		popupMenuManager.setRemoveAllWhenShown(true);
		//getSite().registerContextMenu(popupMenuManager, getSite().getSelectionProvider());
		Menu menu = popupMenuManager.createContextMenu(fTree);
		fTree.setMenu(menu);

	}

	private Node getSelectedNode() {
		ITreeSelection selection = (ITreeSelection) fTreeViewer.getSelection();
		return (selection!=null) ? (Node) selection.getFirstElement() : null;
		
	}
	
     
	private void createColumns(Tree tree) {
		fColumn1 = new TreeColumn(tree, SWT.LEFT);
		fColumn1.setText(Messages.getString("LslTestEditor.ITEM"));  //$NON-NLS-1$
		fColumn1.setWidth(200);
		fColumn1.addSelectionListener(new SelectionAdapter() {
			public void widgetSelected(SelectionEvent e) {
			}
		});

		fColumn2 = new TreeColumn(tree, SWT.LEFT);
		fColumn2.setText(Messages.getString("LslTestEditor.VALUE"));  //$NON-NLS-1$
		fColumn2.setWidth(100);
		fColumn2.addSelectionListener(new SelectionAdapter() {
			public void widgetSelected(SelectionEvent e) {
			}
		});

		tree.setHeaderVisible(true);
	}

	public void fillContextMenu(IMenuManager manager) {
	}

	private Shell getShell() {
        return TestEditor.this.getEditorSite().getShell();
    }

    public void nodeStructureChanged(final Node n) {
        asyncExec(new Runnable() {  public void run() {
                fTreeViewer.refresh(n, true);
        }});
    }

    public void nodeValueChanged(final Node n) {
        asyncExec(new Runnable() {  public void run() {
                fTreeViewer.update(n, null);
        }});
    }
    
    private void asyncExec(Runnable r) {
        LslPlusPlugin.getDefault().getWorkbench().getDisplay().asyncExec(r);
    }
    
    protected void createUndoRedoActions() {
        UndoRedoActionGroup group = new UndoRedoActionGroup(getEditorSite(), undoContext, true);
        group.fillActionBars(getEditorSite().getActionBars());
    }
    
    NodeFactory handleNodeFactory2(Node n, NodeFactory2 fac2) {
        
        if (fac2.getNodeCreationId().equals("entry-point")) { //$NON-NLS-1$
            final EntryPointSelectionDialog dlg = new EntryPointSelectionDialog(getShell());
            dlg.open();
            if (dlg.getReturnCode() != Window.OK) return null;
            
            return new NodeFactory() {
                public Node createNode(Node parent) {
                    return new TestProject.TestNode(parent, "New Test", dlg.getFilename() + "/" + dlg.getPath()); //$NON-NLS-1$ //$NON-NLS-2$ TODO
                }

                public String getNodeTypeName() {
                    return null;
                }
            };
        } else if (fac2.getNodeCreationId().equals("globvar")) { //$NON-NLS-1$
            BindingListNode parent = (BindingListNode)n;
            LinkedList<String> used = new LinkedList<String>();
            for (Iterator<Node> i = parent.getChildren().iterator(); i.hasNext();) {
                used.add(i.next().getName());
            }
            TestNode test = (TestNode) parent.getParent();
            String filename = test.getFilename();
            GlobalSelectionDialog dlg = new GlobalSelectionDialog(getShell(), filename, used);
            dlg.open();
            if (dlg.getReturnCode() != Window.OK) return null;
            final GlobalSummary_GlobalSummary pair = dlg.getPair();
            return new NodeFactory() {

                public Node createNode(Node parent) {
                    return new TestProject.BindingNode(parent, pair.globalName, TestProject.lslTypeToString(pair.globalType));
                }

                public String getNodeTypeName() {
                    return null;
                }
                
            };
        } else if (fac2.getNodeCreationId().equals("call")) { //$NON-NLS-1$
            CallSelectionDialog dlg = new CallSelectionDialog(getShell(), null);
            dlg.open();
            if (dlg.getReturnCode() != Window.OK) return null;
            final String name = dlg.getName();
            return new NodeFactory() {
                public Node createNode(Node parent) {
                    return new TestProject.ExpectedCallNode(parent, "call", name); //$NON-NLS-1$
                }

                public String getNodeTypeName() {
                    return null;
                }
                
            };
        }
        return null;
    }
}
