package lslplus.simview;

import java.util.Collections;
import java.util.LinkedList;

import lslplus.LslPlusPlugin;
import lslplus.SimListener;
import lslplus.SimManager;
import lslplus.sim.SimEvent;
import lslplus.sim.SimEventDefinition;
import lslplus.sim.SimMetaDataListener;
import lslplus.sim.SimStatuses;
import lslplus.sim.SimStatuses.SimState;
import lslplus.util.Util;

import org.eclipse.jface.action.Action;
import org.eclipse.jface.action.IToolBarManager;
import org.eclipse.jface.action.Separator;
import org.eclipse.jface.resource.ImageDescriptor;
import org.eclipse.jface.viewers.TreeViewer;
import org.eclipse.swt.SWT;
import org.eclipse.swt.custom.SashForm;
import org.eclipse.swt.custom.ViewForm;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.events.SelectionListener;
import org.eclipse.swt.graphics.Point;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Combo;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Layout;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.swt.widgets.Tree;
import org.eclipse.swt.widgets.TreeColumn;
import org.eclipse.ui.IActionBars;
import org.eclipse.ui.part.ViewPart;

public class SimWatcherViewPart extends ViewPart implements SimListener, SimMetaDataListener {
    public static final String ID = "lslplus.simWatcher"; //$NON-NLS-1$
    private class StopAction extends Action {
        public StopAction(Shell parentShell) {
            setText("Stop Sim"); //$NON-NLS-1$
            setToolTipText("Stop Sim"); //$NON-NLS-1$
            ImageDescriptor descriptor = LslPlusPlugin
                    .imageDescriptorFromPlugin("icons/stop.gif"); //$NON-NLS-1$
            setHoverImageDescriptor(descriptor);
            setImageDescriptor(descriptor);
            setEnabled(false);
        }

        public void run() {
            getSimManager().stopSim();
        }
    }

    private class UserEventAction extends Action {
        private Shell parentShell;
        private String userEventName;
        public UserEventAction(Shell parentShell, String userEventName, String text, String toolTip,
                String iconPath) {
            this.parentShell = parentShell;
            this.userEventName = userEventName;
            setText(text); 
            setToolTipText(toolTip);
            ImageDescriptor descriptor = LslPlusPlugin
                    .imageDescriptorFromPlugin("icons/chat.gif"); //$NON-NLS-1$
            setHoverImageDescriptor(descriptor);
            setImageDescriptor(descriptor);
            setEnabled(false);
        }

        public void run() {
            SimEventDefinition def = simManager.getAnEventDefinition(userEventName);
            
            if (def == null) {
                Util.error("event definition not found: " + userEventName); //$NON-NLS-1$
                return;
            }
            EventDialog dlg = //new TouchDialog(parentShell);
                    new EventDialog(parentShell, def);
            dlg.open();
            
            SimEvent event = dlg.getEvent();
            if (event != null) {
                simManager.putEvent(event);
            }
        }
    }
    
    private class TouchAction extends Action {
        private Shell parentShell;
        public TouchAction(Shell parentShell) {
            this.parentShell = parentShell;
            setText("Touch"); //$NON-NLS-1$
            setToolTipText("Touch!"); //$NON-NLS-1$
            ImageDescriptor descriptor = LslPlusPlugin
                    .imageDescriptorFromPlugin("icons/touch.gif"); //$NON-NLS-1$
            setHoverImageDescriptor(descriptor);
            setImageDescriptor(descriptor);
            setEnabled(false);
        }

        public void run() {
            SimEventDefinition def = simManager.getAnEventDefinition("Touch Prim"); //$NON-NLS-1$ TODO
            
            if (def == null) {
                Util.error("event definition not found: Touch Prim"); //$NON-NLS-1$
                return;
            }
            EventDialog dlg = //new TouchDialog(parentShell);
                    new EventDialog(parentShell, def);
            dlg.open();
            
            SimEvent event = dlg.getEvent();
            if (event != null) {
                simManager.putEvent(event);
            }
        }
    }

    private Composite parent;
    private Composite counterComposite;
    private SashForm sashForm;
    private TreeViewer logViewer;
    private SimManager simManager;
    private Combo eventsCombo = null;
    private Button eventsButton = null;

    private LogViewerContentProvider logViewerModel;
    private TreeColumn fColumn1;
    private TreeColumn fColumn2;


    private Tree logViewerTree;
    private TouchAction touchAction;
    private StopAction stopAction;
    private TreeColumn fColumn3;
    private LogViewerLabelProvider labelProvider;
    private Label timeText;
    private UserEventAction chatAction;
    private LinkedList<Action> actions;
    private volatile String curTime;
    private boolean refreshPending = false;
    
    public SimWatcherViewPart() {
        simManager = LslPlusPlugin.getDefault().getSimManager();
        simManager.addSimListener(this);
        simManager.addSimMetaDataListener(this);
    }

    public void dispose() {
        super.dispose();
        simManager.removeSimListener(this);
        simManager.removeSimMetaDataListener(this);
        if (labelProvider != null) labelProvider.dispose();
    }

    public boolean isCreated() {
        return counterComposite != null;
    }

    public void createPartControl(Composite parent) {
        this.parent = parent;
        final Shell parentShell = this.getSite().getShell();
        GridLayout gridLayout = new GridLayout();
        gridLayout.marginWidth = 0;
        gridLayout.marginHeight = 0;
        parent.setLayout(gridLayout);
        
        Composite comp = new Composite(parent, SWT.NONE);
        GridLayout layout = new GridLayout();
        layout.numColumns = 3;
//        layout.marginHeight = convertVerticalDLUsToPixels(IDialogConstants.VERTICAL_MARGIN);
//        layout.marginWidth = convertHorizontalDLUsToPixels(IDialogConstants.HORIZONTAL_MARGIN);
//        layout.verticalSpacing = convertVerticalDLUsToPixels(IDialogConstants.VERTICAL_SPACING);
//        layout.horizontalSpacing = convertHorizontalDLUsToPixels(IDialogConstants.HORIZONTAL_SPACING);
        comp.setLayout(layout);
        comp.setLayoutData(new GridData(GridData.FILL_HORIZONTAL));

        Label label0 = new Label(comp, SWT.SHADOW_NONE|SWT.RIGHT|SWT.HORIZONTAL);
        label0.setText("Send an event"); //$NON-NLS-1$ TODO
        eventsCombo = new Combo(comp, SWT.READ_ONLY|SWT.DROP_DOWN);
        populateEventsCombo();
        eventsCombo.deselectAll();
        eventsCombo.addSelectionListener(new SelectionListener() {
            public void widgetDefaultSelected(SelectionEvent e) {
            }

            public void widgetSelected(SelectionEvent e) {
                eventsButton.setEnabled(simManager.isSimActive() && eventsCombo.getSelectionIndex() >= 0);
            }
        });
        eventsCombo.setLayoutData(new GridData(SWT.FILL,SWT.CENTER,true,false));
        eventsButton = new Button(comp, SWT.PUSH|SWT.CENTER);
        eventsButton.setText("Go..."); //$NON-NLS-1$ TODO
        eventsButton.setEnabled(false);
        eventsButton.addSelectionListener(new SelectionListener() {
            public void widgetDefaultSelected(SelectionEvent e) {
            }

            public void widgetSelected(SelectionEvent e) {
                int index = eventsCombo.getSelectionIndex();
                if (index >= 0) {
                    SimEventDefinition def = simManager.getAllEventDefinitions()[index];
                    
                    EventDialog dlg = new EventDialog(parentShell, def);
                    dlg.open();
                    
                    SimEvent event = dlg.getEvent();
                    if (event != null) simManager.putEvent(event);
                    eventsCombo.deselectAll();
                }
            }
            
        });
        
        Composite comp1 = new Composite(parent, SWT.NONE);
        GridLayout layout1 = new GridLayout();
        layout1.numColumns = 2;
//        layout.marginHeight = convertVerticalDLUsToPixels(IDialogConstants.VERTICAL_MARGIN);
//        layout.marginWidth = convertHorizontalDLUsToPixels(IDialogConstants.HORIZONTAL_MARGIN);
//        layout.verticalSpacing = convertVerticalDLUsToPixels(IDialogConstants.VERTICAL_SPACING);
//        layout.horizontalSpacing = convertHorizontalDLUsToPixels(IDialogConstants.HORIZONTAL_SPACING);
        comp1.setLayout(layout1);
        comp1.setLayoutData(new GridData(GridData.FILL_HORIZONTAL));

        Label timeLabel = new Label(comp1, SWT.SHADOW_NONE|SWT.RIGHT|SWT.HORIZONTAL);
        timeLabel.setText("Simulation time:"); //$NON-NLS-1$ TODO
        timeText = new Label(comp1, SWT.SHADOW_ETCHED_IN|SWT.RIGHT|SWT.HORIZONTAL);
        timeText.setText("00:00:00.000"); //$NON-NLS-1$ TODO
        
        SashForm sashForm = createSashForm(parent);
        sashForm.setLayoutData(new GridData(GridData.FILL_BOTH));
        configureToolBar();
    }

    private SashForm createSashForm(Composite parent2) {
        sashForm = new SashForm(parent, SWT.VERTICAL);

        ViewForm top = new ViewForm(sashForm, SWT.NONE);

        Composite empty = new Composite(top, SWT.NONE);
        empty.setLayout(new Layout() {
            protected Point computeSize(Composite composite, int wHint, int hHint,
                    boolean flushCache) {
                return new Point(1, 1);
            }

            protected void layout(Composite composite, boolean flushCache) {
            }
        });
        top.setTopLeft(empty);
        logViewer = createLogViewer(top);
        top.setContent(logViewer.getControl());

        return sashForm;
    }

    private TreeViewer createLogViewer(Composite parent) {
        logViewer = new TreeViewer(parent, SWT.SINGLE);
        logViewerTree = logViewer.getTree();
        createColumns(logViewerTree);
        logViewerModel = new LogViewerContentProvider();
        logViewer.setContentProvider(logViewerModel);
        labelProvider = new LogViewerLabelProvider();
        logViewer.setLabelProvider(labelProvider);
        logViewer.setInput(this);
        return logViewer;
    }
    
    private void createColumns(Tree tree) {
        fColumn1 = new TreeColumn(tree, SWT.LEFT);
        fColumn1.setText("Time");  //$NON-NLS-1$
        fColumn1.setWidth(100);
        fColumn1.addSelectionListener(new SelectionAdapter() {
            public void widgetSelected(SelectionEvent e) {
            }
        });

        fColumn2 = new TreeColumn(tree, SWT.LEFT);
        fColumn2.setText("Source");  //$NON-NLS-1$
        fColumn2.setWidth(300);
        fColumn2.addSelectionListener(new SelectionAdapter() {
            public void widgetSelected(SelectionEvent e) {
            }
        });
        
        fColumn3 = new TreeColumn(tree, SWT.LEFT);
        fColumn3.setText("Message");  //$NON-NLS-1$
        fColumn3.setWidth(350);
        fColumn3.addSelectionListener(new SelectionAdapter() {
            public void widgetSelected(SelectionEvent e) {
            }
        });

        tree.setHeaderVisible(true);
    }

    public void setFocus() {
    }

    private void configureToolBar() {
        IActionBars actionBars = getViewSite().getActionBars();
        IToolBarManager toolBar = actionBars.getToolBarManager();
        actions = new LinkedList<Action>();
        chatAction = new UserEventAction(this.getSite().getShell(),
                "Chat", "Chat", "Chat in Sim",  //$NON-NLS-1$//$NON-NLS-2$ //$NON-NLS-3$ TODO
                "icons/chat.gif"); //$NON-NLS-1$
        touchAction = new TouchAction(this.getSite().getShell());
        stopAction = new StopAction(this.getSite().getShell());
        toolBar.add(chatAction);
        toolBar.add(touchAction);
        toolBar.add(stopAction);
        Collections.addAll(actions, new Action[] { chatAction, touchAction, stopAction });
        enableActions(simManager.isSimActive());
        
        toolBar.add(new Separator());

        actionBars.updateActionBars();
    }

    private void enableActions(boolean enabled) {
        for (Action element : actions) {
            Action action = element;
            action.setEnabled(enabled);
        }
    }

    public void simLaunched() {
        enableActions(true);
        if (logViewerModel != null) {
            this.logViewerModel.clear();
            refreshAsync();
        }
    }

    private Runnable refresher = new Runnable() {
        public void run() {
            logViewer.refresh();
        }
    };

    private void refreshAsync() {
        asyncExec(refresher);
    }
    
    private SimManager getSimManager() {
        return simManager;
    }

    public void simEnded() {
        enableActions(false);
    }

    public void metaDataReady() {
        if (eventsCombo != null) {
            asyncExec(new Runnable() {
                public void run() {
                    populateEventsCombo();
                }
            });
        }
    }

    private void asyncExec(Runnable r) {
        LslPlusPlugin.getDefault().getWorkbench().getDisplay().asyncExec(r);
    }
    
    private void populateEventsCombo() {
        SimEventDefinition[] eventDefs = simManager.getAllEventDefinitions();
        if (eventDefs == null) eventDefs = new SimEventDefinition[0];
        String[] names = new String[eventDefs.length];
        
        for (int i = 0; i < names.length; i++) {
            names[i] = eventDefs[i].getName();
        }
        
        eventsCombo.setItems(names);
    }

    public void newSimState(final SimState state, SimStatuses.Message[] messages) {
        curTime = formatTime(state.getTime());
        this.logViewerModel.addMessages(messages);
        if (checkAndSetRefreshPending()) return;
        asyncExec(new Runnable() {
            public void run() {
                try {
                    timeText.setText(curTime);
                    logViewer.refresh();
                } finally {
                    clearRefreshPending();
                }
            }
        });
    }
    
    private synchronized void clearRefreshPending() { refreshPending = false; }
    
    private synchronized boolean checkAndSetRefreshPending() {
        if (refreshPending) return true;
        refreshPending = true;
        return false;
    }
    
    private static String padInt(int w, int v) {
        String s = Integer.toString(v);
        int pad = w - s.length();
        
        if (pad <= 0) return s;
        StringBuilder buf = new StringBuilder();
        
        for (int i = 0; i < pad; i++) buf.append('0');
        buf.append(s);
        return buf.toString();
    }

    public static String formatTime(int msTime) {
        int milli = msTime % 1000;
        int hours = msTime / (60 * 60 * 1000);
        int minutes = (msTime / (60 * 1000)) % 60;
        int seconds = (msTime / 1000) % 60;
        StringBuilder buf = new StringBuilder(padInt(2,hours));
        buf.append(':').append(padInt(2,minutes));
        buf.append(':').append(padInt(2,seconds));
        buf.append('.').append(padInt(3,milli));
        return buf.toString();
    }
}
