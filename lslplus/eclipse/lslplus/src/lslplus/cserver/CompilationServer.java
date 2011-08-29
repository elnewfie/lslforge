package lslplus.cserver;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.PrintStream;
import java.util.HashSet;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.Future;
import java.util.concurrent.LinkedBlockingQueue;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.TimeoutException;

import com.thoughtworks.xstream.XStream;
import com.thoughtworks.xstream.core.BaseException;
import com.thoughtworks.xstream.io.xml.DomDriver;

import lslplus.LslPlusPlugin;
import lslplus.generated.CompilationCommand;
import lslplus.generated.CompilationResponse;
import lslplus.generated.InitAll;
import lslplus.util.Util;

public class CompilationServer {
	private static final String COMPILATION_SERVER = "CompilationServer"; //$NON-NLS-1$
	
	private volatile boolean shutdown = false;
	private LinkedBlockingQueue<CommandAndResult> commandQueue;
	private PrintStream writer;
    private BufferedReader reader;
    private XStream xstream;
    private Process process;
    private Thread interactorThread;
	private Thread monitorThread;
    private HashSet<RestartListener> listeners = new HashSet<RestartListener>();
    
    public static class CommandAndResult {
    	private CompilationCommand command;
    	private Result result;
    	public CommandAndResult(CompilationCommand command) {
    		this.command = command;
    		this.result = new Result();
    	}
    	
    	public CompilationCommand getCommand() { return command; }
    	public Result getResult() { return result; }
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
    
    private Runnable interactor = new Runnable() {
	    public void run() {
	    	while (!shutdown) {
	    		try {
					CommandAndResult cmdInfo = commandQueue.take();
					synchronized (CompilationServer.this) {
						try {
							String xmlOut = xstream.toXML(cmdInfo.getCommand());
							Util.log("command to server: " + xmlOut); //$NON-NLS-1$
					        writer.println(Util.URIEncode(xmlOut));
					        writer.flush();
							
							String line = reader.readLine();
							Util.log(Util.URIDecode(line));
							CompilationResponse response = 
								(CompilationResponse) xstream.fromXML(Util.URIDecode(line));
							cmdInfo.getResult().put(response);
						} catch (IOException e) {
							cmdInfo.getResult().put(e);
						} catch (BaseException e) {
							cmdInfo.getResult().put(e);
						} catch (ClassCastException e) {
							cmdInfo.getResult().put(e);
						}
					}
				} catch (InterruptedException e) {
					Util.error("interrupted!"); //$NON-NLS-1$
				}
	    	}
	    }	
	};
	
	private Runnable monitor = new Runnable() {

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
    
    public Result execute(CompilationCommand cmd) {
    	CommandAndResult cr = new CommandAndResult(cmd);
    	
    	this.commandQueue.add(cr);
    	
    	return cr.getResult();
    }
    
    protected void startProcess() {
    	synchronized (this) {
	    	process = LslPlusPlugin.launchCoreCommand(COMPILATION_SERVER, true);
	    	reader = new BufferedReader(new InputStreamReader(process.getInputStream()));
	    	writer = new PrintStream(new PrintStream(process.getOutputStream()));
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
    }
}
