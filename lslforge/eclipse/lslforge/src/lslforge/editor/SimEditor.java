package lslforge.editor;

import java.io.ByteArrayInputStream;
import java.util.LinkedList;
import java.util.List;

import lslforge.LSLForgePlugin;
import lslforge.LSLProjectNature;
import lslforge.gentree.Node;
import lslforge.gentree.NodeFactory;
import lslforge.gentree.NodeListener;
import lslforge.gentree.NodeStatus;
import lslforge.gentree.NodeVisitor;
import lslforge.sim.SimProject;
import lslforge.sim.SimWorldDef;
import lslforge.util.Util;

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
import org.eclipse.jface.action.Separator;
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
import org.eclipse.jface.viewers.TreeSelection;
import org.eclipse.jface.viewers.TreeViewer;
import org.eclipse.jface.viewers.TreeViewerColumn;
import org.eclipse.jface.viewers.ViewerCell;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Menu;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.swt.widgets.Tree;
import org.eclipse.swt.widgets.TreeColumn;
import org.eclipse.ui.IEditorInput;
import org.eclipse.ui.IEditorSite;
import org.eclipse.ui.IWorkbenchActionConstants;
import org.eclipse.ui.PartInitException;
import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.operations.UndoRedoActionGroup;
import org.eclipse.ui.part.EditorPart;

public class SimEditor extends EditorPart implements NodeListener {
    
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
        public DeleteNodeOperation(Node parent, Node child) {
            super("Delete node"); //$NON-NLS-1$ TODO
            this.parent = parent;
            this.child = child;
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
            parent.addChild(child);
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
            AddNodeOperation operation = new AddNodeOperation(n, factory);
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
            AddBeforeOperation operation = new AddBeforeOperation(n, factory);
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
            AddAfterOperation operation = new AddAfterOperation(n, factory);
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
            DeleteNodeOperation operation = new DeleteNodeOperation(n.getParent(), n);
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
			    if ("scripts".equals(n.getChoicesId())) { //$NON-NLS-1$
			        choices = nature.getLSLScripts();
			    } else if ("optional-module".equals(n.getChoicesId())) { //$NON-NLS-1$
			        List<String> modules = nature.getLSLModules();
			        modules.add(0, "(none)"); //$NON-NLS-1$ TODO
			        choices = modules.toArray(new String[modules.size()]);
			    } else if ("avatars".equals(n.getChoicesId())) { //$NON-NLS-1$
			        Node root = n.findRoot();
			        final LinkedList<String> avnames = new LinkedList<String>();
			        root.accept(new NodeVisitor() {
                        public void visit(Node n) {
                            if (n instanceof SimProject.AvatarNode) {
                                avnames.add(n.getNameDisplay());
                            }
                        }
			        });
			        choices = avnames.toArray(new String[avnames.size()]);
			    } else {
			        choices = new String[0];
			    }
			    curChoices = choices;
			    curEditor = new ComboBoxCellEditor(fTree, choices);
			} else {
			    curEditor = new TextCellEditor(fTree);
			    curEditor.setValidator(new ICellEditorValidator() {
                    public String isValid(Object value) {
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
	private SimProjectLabelProvider fLabelProvider;
	private SimProject.WorldNode world = null;
	@SuppressWarnings("unused")
	private String simProjectName = null;
	private LSLProjectNature nature = null;
	private int changeCount = 0;
	private IFile file;
	private DeleteNodeAction fDeleteNodeAction;
	private static final String BLANK = ""; //$NON-NLS-1$
	private IOperationHistory operationsHistory;
	private IUndoContext undoContext;
	
	public SimEditor() {
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
		String val = SimProject.toXml(world);
		if (LSLForgePlugin.DEBUG) Util.log("world = " + val); //$NON-NLS-1$
		try {
			file.setContents(new ByteArrayInputStream(val.getBytes()), IResource.FORCE | IResource.KEEP_HISTORY, monitor);
			zeroChangeCount();
		} catch (CoreException e) {
			Util.error(e, e.getLocalizedMessage());
		}
		
		try {
		    SimWorldDef def = SimProject.toSimWorldDef(world);
		    Util.log(SimWorldDef.toXML(def));
		} catch (Exception e) {
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
			nature = (LSLProjectNature) file.getProject().getNature(LSLProjectNature.ID);
		} catch (CoreException e1) {
			throw new PartInitException("Can't get project nature", e1); //$NON-NLS-1$
		}
		IPath fullPath = file.getFullPath();
		simProjectName = fullPath.removeFileExtension().lastSegment();
		if (file != null) {
			try {
				world = SimProject.fromXml(file.getContents(), file);
			} catch (CoreException e) {
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
        PlatformUI.getWorkbench().getHelpSystem().setHelp(parent, "lslforge.simProjectEditor"); //$NON-NLS-1$
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
		fTreeViewer.setLabelProvider(fLabelProvider = new SimProjectLabelProvider());
		fTreeViewer.addSelectionChangedListener(new ISelectionChangedListener() {
			public void selectionChanged(SelectionChangedEvent e) {
				@SuppressWarnings("unused") // TODO
				TreeSelection selection = (TreeSelection) e.getSelection();
				
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
    				    MenuManager addItemBeforeSubMenu = new MenuManager("Add Item Before"); //$NON-NLS-1$ TODO NLS
                        MenuManager addItemAfterSubMenu = new MenuManager("Add Item After"); //$NON-NLS-1$ TODO NLS
                        
                        for (int i = 0; i < factories.length; i++) {
                            addItemBeforeSubMenu.add(new AddNodeBeforeAction(factories[i]));
                            addItemAfterSubMenu.add(new AddNodeAfterAction(factories[i]));
                        }
                        manager.add(addItemBeforeSubMenu);
                        manager.add(addItemAfterSubMenu);
                        
				    }
				}
				if (n != null && n.isDeletable()) manager.add(fDeleteNodeAction);
				
		        manager.add(new Separator(IWorkbenchActionConstants.MB_ADDITIONS));

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
		fColumn1.setText(Messages.getString("LSLTestEditor.ITEM"));  //$NON-NLS-1$
		fColumn1.setWidth(200);
		fColumn1.addSelectionListener(new SelectionAdapter() {
			public void widgetSelected(SelectionEvent e) {
			}
		});

		fColumn2 = new TreeColumn(tree, SWT.LEFT);
		fColumn2.setText(Messages.getString("LSLTestEditor.VALUE"));  //$NON-NLS-1$
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
        return SimEditor.this.getEditorSite().getShell();
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
        LSLForgePlugin.getDefault().getWorkbench().getDisplay().asyncExec(r);
    }
    
    protected void createUndoRedoActions() {
        UndoRedoActionGroup group = new UndoRedoActionGroup(getEditorSite(), undoContext, true);
        group.fillActionBars(getEditorSite().getActionBars());
    }
    
}
