package lslforge.testview;

import java.text.MessageFormat;

import lslforge.LSLForgePlugin;
import lslforge.lsltest.TestResult;
import lslforge.lsltest.TestResult.LogMessage;
import lslforge.lsltest.TestResult.ResultInfo;

import org.eclipse.jface.viewers.ITableLabelProvider;
import org.eclipse.jface.viewers.LabelProvider;
import org.eclipse.swt.graphics.Image;

public class TestResultLabelProvider extends LabelProvider implements ITableLabelProvider {
    private final Image fTestErrorIcon= LSLForgePlugin.createImage("icons/testerr.gif"); //$NON-NLS-1$
    private final Image fTestFailureIcon= LSLForgePlugin.createImage("icons/testfail.gif"); //$NON-NLS-1$
    private final Image fOkIcon= LSLForgePlugin.createImage("icons/testok.gif"); //$NON-NLS-1$
    private final Image fSuccessIcon = LSLForgePlugin.createImage("icons/success.gif"); //$NON-NLS-1$
    private final Image fErrorIcon = LSLForgePlugin.createImage("icons/error.gif"); //$NON-NLS-1$
    private final Image fFailureIcon = LSLForgePlugin.createImage("icons/failed.gif"); //$NON-NLS-1$
    private final Image fCallIcon = LSLForgePlugin.createImage("icons/call.gif"); //$NON-NLS-1$
    private final Image fInfoIcon = LSLForgePlugin.createImage("icons/info_st_obj.gif"); //$NON-NLS-1$
    private final Image fReturnIcon = LSLForgePlugin.createImage("icons/returns.gif"); //$NON-NLS-1$
   public TestResultLabelProvider() {
    }

    public Image getColumnImage(Object element, int columnIndex) {
        if (element instanceof TestResult) {
            TestResult result = (TestResult) element;
            switch (result.getResultInfo().getResultCode()) {
            case TestResult.ERROR: return fTestErrorIcon;
            case TestResult.FAILURE: return fTestFailureIcon;
            case TestResult.SUCCESS: return fOkIcon;
            }
        } else if (element instanceof ResultInfo) {
            ResultInfo info = (ResultInfo) element;
            switch (info.getResultCode()) {
            case TestResult.ERROR: return fErrorIcon;
            case TestResult.FAILURE: return fFailureIcon;
            case TestResult.SUCCESS: return fSuccessIcon;
            }
        } else if (element instanceof LogMessage) {
            LogMessage m = (LogMessage) element;
            if (m.getText().startsWith("call:")) { //$NON-NLS-1$
                return fCallIcon;
            } else if (m.getText().startsWith("return:")) { //$NON-NLS-1$
                return fReturnIcon;
            } else {
                return fInfoIcon;
            }
        }
        return null;
    }

    public String getColumnText(Object element, int columnIndex) {
        if (element instanceof TestResult) {
            TestResult result = (TestResult) element;
            String name = result.getName();
            String txt = TestResult.RESULT_TEXT[result.getResultInfo().getResultCode()];
            return MessageFormat.format(Messages.getString("TestResultLabelProvider.NAME_FORMAT"), new Object[] { name, txt }); //$NON-NLS-1$
        } else if (element instanceof LogMessage[]) {
            return Messages.getString("TestResultLabelProvider.LOG"); //$NON-NLS-1$
        } else if (element instanceof LogMessage) {
            LogMessage message = (LogMessage) element;
            return MessageFormat.format(Messages.getString("TestResultLabelProvider.LOG_FORMAT"), new Object[] { new Integer(message.getTime()),  //$NON-NLS-1$
                    message.getText() });
        } else if (element instanceof ResultInfo) {
            ResultInfo info = (ResultInfo) element;
            return info.getResultMessage();
        }
        return null;
    }

    public Image getImage(Object element) {
         return getColumnImage(element, 0);
    }

    public String getText(Object element) {
        return getColumnText(element, 0);
    }
    

}
