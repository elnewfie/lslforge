package lslforge.testview;

import lslforge.LslForgePlugin;
import lslforge.lsltest.ITestListener;
import lslforge.lsltest.TestManager;
import lslforge.lsltest.TestResult;

import org.eclipse.jface.action.Action;
import org.eclipse.jface.action.IToolBarManager;
import org.eclipse.jface.action.Separator;
import org.eclipse.jface.resource.ImageDescriptor;
import org.eclipse.jface.viewers.TreeViewer;
import org.eclipse.swt.SWT;
import org.eclipse.swt.custom.SashForm;
import org.eclipse.swt.custom.ViewForm;
import org.eclipse.swt.graphics.Point;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Layout;
import org.eclipse.ui.IActionBars;
import org.eclipse.ui.part.ViewPart;

/**
 * Heavily influenced by the JUnit test runner for Eclipse, this test runner
 * view allows the user to see the results of launched tests in a hierarchical
 * fashion. There is a progress bar at the top, showing red for failures, green
 * for good-so-far, a button to rerun the tests, and a viewer to show the result
 * for each test (succes/failure/error) with a display of logging information
 * that was collected during the test.
 * 
 * @author rgreayer
 * 
 */
public class TestRunnerViewPart extends ViewPart implements ITestListener {
    public static final String ID = "lslforge.TestRunnerView"; //$NON-NLS-1$

    private class RerunTestsAction extends Action {

        public RerunTestsAction() {
            setText(Messages.getString("TestRunnerViewPart.RERUN_TESTS")); //$NON-NLS-1$
            setToolTipText(Messages.getString("TestRunnerViewPart.RERUN_TESTS")); //$NON-NLS-1$
            ImageDescriptor descriptor = LslForgePlugin
                    .imageDescriptorFromPlugin("icons/relaunch.gif"); //$NON-NLS-1$
            setHoverImageDescriptor(descriptor);
            setImageDescriptor(descriptor);
            setEnabled(false);
        }

        public void run() {
            rerunTestRun();
        }
    }

    private LslTestProgressBar progressBar;
    private Composite parent;
    private TestResultCountDisplay counterPanel;
    private Composite counterComposite;
    private SashForm sashForm;
    private TreeViewer testViewer;
    private RerunTestsAction rerunTestsAction;
    private TestResultsContentProvider model;
    private TestResultLabelProvider labelProvider;
    private TestManager testManager;

    public TestRunnerViewPart() {
        testManager = LslForgePlugin.getDefault().getTestManager();
        testManager.addResultListener(this);
    }

    public void dispose() {
        super.dispose();
        testManager.removeResultListener(this);
    }

    public boolean isCreated() {
        return counterComposite != null;
    }

    public void createPartControl(Composite parent) {

        this.parent = parent;
        GridLayout gridLayout = new GridLayout();
        gridLayout.marginWidth = 0;
        gridLayout.marginHeight = 0;
        parent.setLayout(gridLayout);
        counterComposite = createProgressCountPanel(parent);
        counterComposite.setLayoutData(new GridData(GridData.GRAB_HORIZONTAL
                | GridData.HORIZONTAL_ALIGN_FILL));
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
                return new Point(1, 1); // (0, 0) does not work with
                                        // super-intelligent ViewForm
            }

            protected void layout(Composite composite, boolean flushCache) {
            }
        });
        top.setTopLeft(empty); // makes ViewForm draw the horizontal separator
                                // line ...
        testViewer = createViewer(top);
        top.setContent(testViewer.getControl());

        return sashForm;
    }

    private TreeViewer createViewer(Composite parent) {
        testViewer = new TreeViewer(parent, SWT.SINGLE);

        model = new TestResultsContentProvider();
        testViewer.setContentProvider(model);
        labelProvider = new TestResultLabelProvider();
        testViewer.setLabelProvider(labelProvider);
        testViewer.setInput(this);
        return testViewer;
    }

    protected Composite createProgressCountPanel(Composite parent) {
        Composite composite = new Composite(parent, SWT.NONE);
        GridLayout layout = new GridLayout();
        composite.setLayout(layout);
        layout.numColumns = 1;

        counterPanel = new TestResultCountDisplay(composite);
        counterPanel.setLayoutData(new GridData(GridData.GRAB_HORIZONTAL
                | GridData.HORIZONTAL_ALIGN_FILL));
        progressBar = new LslTestProgressBar(composite);
        progressBar.setLayoutData(new GridData(GridData.GRAB_HORIZONTAL
                | GridData.HORIZONTAL_ALIGN_FILL));
        return composite;
    }

    public void setFocus() {
    }

    private void configureToolBar() {
        IActionBars actionBars = getViewSite().getActionBars();
        IToolBarManager toolBar = actionBars.getToolBarManager();
        //        
        // fStopAction= new StopAction();
        // fStopAction.setEnabled(false);

        rerunTestsAction = new RerunTestsAction();
        toolBar.add(new Separator());
        toolBar.add(rerunTestsAction);

        actionBars.updateActionBars();
    }

    private void rerunTestRun() {
        testManager.rerunCurrentTests();
    }

    public void newTestResult(TestResult result) {
        this.model.addResult(result);
        refreshAsync();
    }

    public void testLaunched(int numTests) {
        if (model != null) {
            this.model.clear();
            refreshAsync();
        }
    }

    public void testFinished() {
        this.rerunTestsAction.setEnabled(true);
    }

    private Runnable refresher = new Runnable() {
        public void run() {
            testViewer.refresh();
            counterPanel.update(testManager.getNumTests(), testManager.getNumRun(),
                    testManager.getNumErrors(), testManager.getNumFailures());
            progressBar.setMaximum(testManager.getNumTests());
            progressBar.update(testManager.getNumRun(), 
                    testManager.getNumErrors() + testManager.getNumFailures() > 0);
        }
    };

    private void refreshAsync() {
        LslForgePlugin.getDefault().getWorkbench().getDisplay().asyncExec(refresher);
    }
}
