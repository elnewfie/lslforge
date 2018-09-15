package lslforge.cserver;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.PrintStream;
import java.util.HashSet;
import java.util.Map;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.Future;
import java.util.concurrent.LinkedBlockingQueue;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.TimeoutException;
import lslforge.LSLForgePlugin;
import lslforge.generated.CompilationCommand;
import lslforge.generated.CompilationResponse;
import lslforge.generated.InitAll;
import lslforge.util.Log;
import lslforge.util.Util;
import com.thoughtworks.xstream.XStream;
import com.thoughtworks.xstream.XStreamException;
import com.thoughtworks.xstream.io.xml.DomDriver;

public class CompilationServer {
	private static final String COMPILATION_SERVER = "CompilationServer"; //$NON-NLS-1$
	
	private volatile boolean shutdown = false;
	private final LinkedBlockingQueue<CommandAndResult> commandQueue;
	private PrintStream writer;
    private BufferedReader reader;
    private final XStream xstream;
    private Process process;
    private final Thread interactorThread;
	private final Thread monitorThread;
    private final HashSet<RestartListener> listeners = new HashSet<RestartListener>();
    
    //public Process getProcess() {
	//	return process;
	//}
    
    public static class CommandAndResult {
    	private final CompilationCommand command;
    	private final Result result;
    	private Map<String, Object> extras = null;
    	public CommandAndResult(CompilationCommand command) {
    		this.command = command;
    		this.result = new Result();
    	}
    	
    	public CommandAndResult(CompilationCommand command, Map<String, Object> extras) {
    		this.command = command;
    		this.result = new Result();
    		this.extras = extras;
    	}
    	
    	public CompilationCommand getCommand() { return command; }
    	public Result getResult() { return result; }
    	public Map<String, Object> getExtras() { return extras; }
    }
    
    public static class Result implements Future<CompilationResponse> {
    	private CompilationResponse result = null;
    	private Exception e = null;
    	private boolean cancelled;
    	public synchronized CompilationResponse get() 
    	throws InterruptedException, ExecutionException {
    		while (result == null && e == null) {
    			wait();
    		}
    		
    		if (result != null) return result;
    		else throw new ExecutionException(e);
    	}

		public synchronized boolean cancel(boolean arg0) {
			cancelled = true;
			return true;
		}

		public synchronized void put(CompilationResponse v) {
			result = v;
			notifyAll();
		}
		
		public synchronized void put(Exception e) {
			this.e = e;
			notifyAll();
		}
		
		public synchronized CompilationResponse get(long timeout, TimeUnit timeUnit)
				throws InterruptedException, ExecutionException,
				TimeoutException {
			long timeoutMs = timeUnit.toMillis(timeout);
			while (result == null && e == null) {
				long t0 = System.currentTimeMillis();
				wait(timeoutMs);
				if (result == null && e == null && System.currentTimeMillis() > t0 + timeoutMs)
					throw new TimeoutException();
			}
			
			if (result != null) return result;
			else throw new ExecutionException(e);
		}

		public synchronized boolean isCancelled() {
			return cancelled;
		}

		public synchronized boolean isDone() {
			return result != null || e != null;
		}
    }
    
    private final Runnable interactor = new Runnable() {
	    public void run() {
	    	while (!shutdown) {
	    		try {
					CommandAndResult cmdInfo = commandQueue.take();
					synchronized (CompilationServer.this) {
						try {
							String xmlOut = xstream.toXML(cmdInfo.getCommand());
							
							Log.debug("command to server: " + xmlOut); //$NON-NLS-1$
					        writer.println(Util.URIEncode(xmlOut));
					        writer.flush();
							
							String line = reader.readLine();
							line = Util.URIDecode(line);
							CompilationResponse response =  (CompilationResponse) xstream.fromXML(line);
							cmdInfo.getResult().put(response);
						} catch (IOException e) {
							cmdInfo.getResult().put(e);
						} catch (XStreamException e) {
							cmdInfo.getResult().put(e);
						} catch (ClassCastException e) {
							cmdInfo.getResult().put(e);
						}
					}
				} catch (InterruptedException e) {
					Log.error("interrupted!"); //$NON-NLS-1$
				}
	    	}
	    }	
	};
	
	private final Runnable monitor = new Runnable() {

		public void run() {
			while (!shutdown) {
			    try {
					process.waitFor();
					// terminated
					startProcess(); // restart!  this may be foolish...
				} catch (InterruptedException e) {
				}
			}
		}
		
	};

	public CompilationServer() {
		commandQueue = new LinkedBlockingQueue<CommandAndResult>();
		xstream = new XStream(new DomDriver());
		InitAll.initAll(xstream);
		interactorThread = new Thread(interactor);
		monitorThread = new Thread(monitor);
		startProcess();
		
		monitorThread.start();
		interactorThread.start();
	}
    
	public Result execute(CompilationCommand cmd, Map<String, Object> extras) {
    	CommandAndResult cr = new CommandAndResult(cmd, extras);
    	
    	this.commandQueue.add(cr);
    	
    	return cr.getResult();
		
	}
	
    public Result execute(CompilationCommand cmd) {
    	CommandAndResult cr = new CommandAndResult(cmd);
    	
    	this.commandQueue.add(cr);
    	
    	return cr.getResult();
    }
    
    public void startProcess() {
    	synchronized (this) {
	    	process = LSLForgePlugin.launchCoreCommand(COMPILATION_SERVER, true);
	    	reader = new BufferedReader(new InputStreamReader(process.getInputStream()));
	    	writer = new PrintStream(new PrintStream(process.getOutputStream()));
	    	Log.debug("Compilation server process started"); //$NON-NLS-1$
    	}
    	
    	synchronized (listeners) {
    		for (RestartListener l : listeners) {
    			l.restart();
    		}
    	}
    }
    
    public void addListener(RestartListener l) {
    	synchronized (listeners) {
    	    listeners.add(l);
    	}
    }
    
    public void removeListener(RestartListener l) {
    	synchronized (listeners) {
    		listeners.remove(l);
    	}
    }
    
    public void stop() {
    	shutdown = true;
    	interactorThread.interrupt();
    	monitorThread.interrupt();
    	process.destroy();
    	Log.debug("Compilation server process stopped"); //$NON-NLS-1$
    }
}