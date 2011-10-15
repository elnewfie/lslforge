package lslforge.testview;


import java.text.MessageFormat;

import lslforge.LSLForgePlugin;

import org.eclipse.swt.SWT;
import org.eclipse.swt.events.DisposeEvent;
import org.eclipse.swt.events.DisposeListener;
import org.eclipse.swt.graphics.Image;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Label;

/**
 * A panel intended to look like the Eclipse JUnit panel for displaying counts of tests that
 * have been run, tests that have failed, and tests that have errors.
 * @author rgreayer
 *
 */
public class TestResultCountDisplay extends Composite implements DisposeListener {
    protected CompositeLabel errorCountLabel;
    protected CompositeLabel failureCountLabel;
    protected CompositeLabel runCountLabel;
    protected int totalCount;

    /**
     * A composite label consists of a name, an image, and some 'mutable' text. 
     */
    private class CompositeLabel extends Composite {
        private Label displayLabel;
        public CompositeLabel(Composite parent, String name, Image image, String initialText) {
            super(parent, SWT.WRAP);
            GridLayout gridLayout = new GridLayout();
            gridLayout.numColumns = 3;
            gridLayout.makeColumnsEqualWidth = false;
            gridLayout.marginWidth = 0;
            setLayout(gridLayout);
            
            // The image associated with the composite
            if (image != null) {
                Label imageLabel = new Label(this, SWT.NONE);
                image.setBackground(imageLabel.getBackground());
                imageLabel.setImage(image);
            }
            // The name text associated with the composite
            Label nameLabel = new Label(this, SWT.NONE);
            nameLabel.setText(name);
            
            // The mutable display
            displayLabel = new Label(this, SWT.NONE);
            displayLabel.setText(initialText);
        }
        
        public void setText(String text) {
            displayLabel.setText(text);
        }
    }

    private final Image errorIcon = LSLForgePlugin.createImage("icons/error.gif"); //$NON-NLS-1$
    private final Image failureIcon = LSLForgePlugin.createImage("icons/failed.gif"); //$NON-NLS-1$
            
    public TestResultCountDisplay(Composite parent) {
        super(parent, SWT.WRAP);
        GridLayout gridLayout= new GridLayout();
        gridLayout.numColumns= 3;
        gridLayout.makeColumnsEqualWidth= false;
        gridLayout.marginWidth= 0;
        setLayout(gridLayout);

        addDisposeListener(this);

        runCountLabel = new CompositeLabel(this, Messages.getString("TestResultCountDisplay.RUNS"), null, " 0/0  "); //$NON-NLS-1$ //$NON-NLS-2$
        errorCountLabel= new CompositeLabel(this, Messages.getString("TestResultCountDisplay.ERRORS"), errorIcon, " 0 ");  //$NON-NLS-1$//$NON-NLS-2$
        failureCountLabel= new CompositeLabel(this, Messages.getString("TestResultCountDisplay.FAILURES"), failureIcon, " 0 ");  //$NON-NLS-1$//$NON-NLS-2$
    }
 
    public void update(int total, int run, int errored, int failed) {
        totalCount = total;
        runCountLabel.setText(MessageFormat.format("{0}/{1}", new Object[] { Integer.toString(run), Integer.toString(total) })); //$NON-NLS-1$
        errorCountLabel.setText(Integer.toString(errored));
        failureCountLabel.setText(Integer.toString(failed));
        redraw();
    }
    
    public void widgetDisposed(DisposeEvent e) {
        if (errorIcon != null) errorIcon.dispose();
        if (failureIcon != null) failureIcon.dispose();
    }
}
