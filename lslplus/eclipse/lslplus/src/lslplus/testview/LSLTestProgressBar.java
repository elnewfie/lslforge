package lslplus.testview;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.ControlEvent;
import org.eclipse.swt.events.ControlListener;
import org.eclipse.swt.events.DisposeEvent;
import org.eclipse.swt.events.DisposeListener;
import org.eclipse.swt.events.PaintEvent;
import org.eclipse.swt.events.PaintListener;
import org.eclipse.swt.graphics.Color;
import org.eclipse.swt.graphics.GC;
import org.eclipse.swt.graphics.Point;
import org.eclipse.swt.graphics.RGB;
import org.eclipse.swt.graphics.Rectangle;
import org.eclipse.swt.widgets.Canvas;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Display;

/**
 * A progress bar with a red/green indication for success or failure.
 */
public class LslTestProgressBar extends Canvas implements DisposeListener, ControlListener,
        PaintListener {
    private static final int BAR_HEIGHT = 16;
    private static final int BAR_WIDTH = 160;
    private static final RGB FAILURE_RGB = new RGB(160, 64, 64);
    private static final RGB STOPPED_RGB = new RGB(128, 128, 128);
    private static final RGB SUCCESS_RGB = new RGB(96, 192, 96);

    private static void drawBevel(Display disp, GC gc, int x, int y, int w, int h) {
        gc.setForeground(disp.getSystemColor(SWT.COLOR_WIDGET_NORMAL_SHADOW));
        gc.drawLine(x, y, x + w - 1, y); // top line
        gc.drawLine(x, y, x, y + h - 1); // left line

        gc.setForeground(disp.getSystemColor(SWT.COLOR_WIDGET_HIGHLIGHT_SHADOW));
        gc.drawLine(x + w, y, x + w, y + h); // right line
        gc.drawLine(x, y + h, x + w, y + h); // bottom line
    }
    
    private boolean error;
    private Color failureColor;
    private int maxProgress = 0;
    private Color okColor;
    private int progress = 0;
    private int progressBarWidth = 0;
    private boolean progressStopped = false;

    private Color stoppedColor;

    public LslTestProgressBar(Composite parent) {
        super(parent, SWT.NONE);
        Display display = parent.getDisplay();
        failureColor = new Color(display, FAILURE_RGB);
        okColor = new Color(display, SUCCESS_RGB);
        stoppedColor = new Color(display, STOPPED_RGB);

        addDisposeListener(this);
        addControlListener(this);
        addPaintListener(this);
    }

    public Point computeSize(int wHint, int hHint, boolean changed) {
        checkWidget();
        return new Point(wHint == SWT.DEFAULT ? BAR_WIDTH : wHint,
                         hHint == SWT.DEFAULT ? BAR_HEIGHT : hHint);
    }

    public void controlMoved(ControlEvent e) {
    }

    public void controlResized(ControlEvent e) {
        progressBarWidth = scale(progress);
        redraw();
    }

    public void paintControl(PaintEvent e) {
        Rectangle rect = getClientArea();
        e.gc.fillRectangle(rect);
        drawBevel(getDisplay(), e.gc, rect.x, rect.y, rect.width - 1, rect.height - 1);

        progressBarWidth = Math.min(rect.width - 2, progressBarWidth);
        paintSegment(e.gc, rect, 1, progressBarWidth);
    }

    private void paintSegment(GC gc, Rectangle rect, int start, int end) {
        gc.setBackground(statusColor());
        start = Math.max(1, start);
        gc.fillRectangle(start, 1, end - start, rect.height - 2);
        gc.dispose();
    }

    public void refresh(boolean hasErrors) {
        error = hasErrors;
        redraw();
    }

    private int scale(int value) {
        if (maxProgress > 0) {
            int width = getClientArea().width;
            if (width != 0) return Math.max(0, value * (width - 2) / maxProgress);
        }
        return value;
    }

    public void setMaximum(int max) {
        maxProgress = max;
    }

    private Color statusColor() {
        return progressStopped ? stoppedColor : (error ? failureColor : okColor);
    }

    public void stop() {
        progressStopped = true;
        redraw();
    }

    public void update(int progress, boolean failures) {
        this.progress = progress;
        int start = error != failures ? 1 : progressBarWidth;
        progressBarWidth = scale(progress);
        error = failures;

        if (progress == maxProgress) progressBarWidth = getClientArea().width - 1;
        GC gc = new GC(this);
        paintSegment(gc, getClientArea(), start, progressBarWidth);
        gc.dispose();
        redraw();
    }

    public void widgetDisposed(DisposeEvent e) {
        failureColor.dispose();
        okColor.dispose();
        stoppedColor.dispose();
    }

}
