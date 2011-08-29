/**
 * 
 */
package lslplus.debug;

import java.io.IOException;
import java.io.Reader;
import java.util.HashSet;
import java.util.Iterator;

import org.eclipse.debug.core.IStreamListener;
import org.eclipse.debug.core.model.IStreamMonitor;

public class StreamMonitor implements IStreamMonitor {
	private HashSet<IStreamListener> listeners = new HashSet<IStreamListener>();
	private StringBuffer buf = new StringBuffer();
	private Reader reader;
	
	public StreamMonitor(Reader reader) {
		this.reader = reader;
		t.start();
	}
	
	public void addListener(IStreamListener listener) {
		listeners.add(listener);
	}

	public String getContents() {
		synchronized (buf) {
			String s = buf.toString();
			buf.setLength(0);
			return s;
		}
	}

	public void removeListener(IStreamListener listener) {
		listeners.remove(listener);
	}
	
	private Thread t = new Thread() {
		public void run() {
			char[] cbuf = new char[512];
			int count = 0;
			try {
				while ((count = reader.read(cbuf)) >= 0) {
					if (count > 0) {
						String s = new String(cbuf, 0, count);
						buf.append(cbuf, 0, count);
						notifyListeners(s);
					}
				}
			} catch (IOException e) {
			}
		}
	};
	
	private void notifyListeners(String text) {
		for (Iterator<IStreamListener> i = listeners.iterator(); i.hasNext(); ) {
			IStreamListener listener = i.next();
			listener.streamAppended(text, this);
		}
	}
}