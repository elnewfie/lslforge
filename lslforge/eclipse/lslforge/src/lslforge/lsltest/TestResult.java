package lslforge.lsltest;

import com.thoughtworks.xstream.XStream;
import com.thoughtworks.xstream.io.xml.DomDriver;

public class TestResult {
    public static final int SUCCESS = 0;
    public static final int FAILURE = 1;
    public static final int ERROR = 2;

    public static final String[] RESULT_TEXT = {
        Messages.getString("TestResult.SUCCESS"), //$NON-NLS-1$
        Messages.getString("TestResult.FAILURE"), //$NON-NLS-1$
        Messages.getString("TestResult.ERROR") //$NON-NLS-1$
    };
    private String name;
    private ResultInfo resultInfo;
    private LogMessage[] messages;
    
    public static class ResultInfo {
        private int resultCode;
        private String resultMessage;
        public ResultInfo(int code, String message) {
            resultCode = code;
            resultMessage = message;
        }
        
        public void setResultCode(int resultCode) {
            this.resultCode = resultCode;
        }
        public int getResultCode() {
            return resultCode;
        }
        public void setResultMessage(String resultMessage) {
            this.resultMessage = resultMessage;
        }
        public String getResultMessage() {
            return resultMessage;
        }
    }
    
    public static class LogMessage {
        private int time;
        private String text;
        
        public LogMessage(int time, String text) {
            this.time = time;
            this.text = text;
        }
        public void setTime(int time) {
            this.time = time;
        }
        public int getTime() {
            return time;
        }
        public void setText(String text) {
            this.text = text;
        }
        public String getText() {
            return text;
        }
    }

    public void setName(String testName) {
        this.name = testName;
    }

    public String getName() {
        return name;
    }

    public void setMessages(LogMessage[] messages) {
        this.messages = messages;
    }

    public LogMessage[] getMessages() {
        return messages;
    }

    public void setResultInfo(ResultInfo resultInfo) {
        this.resultInfo = resultInfo;
    }

    public ResultInfo getResultInfo() {
        return resultInfo;
    }

    @Override
	public String toString() {
        return name;
    }
    
    private static XStream xstream;
    
    static {
        xstream = new XStream(new DomDriver());
        configureXStream(xstream);
    }
    
    public static void configureXStream(XStream xstream) {
        xstream.alias("test-result", TestResult.class); //$NON-NLS-1$
        xstream.alias("message", LogMessage.class); //$NON-NLS-1$
    }
    
    public static TestResult fromXML(String line) {
        return (TestResult)xstream.fromXML(line);
    }
}
