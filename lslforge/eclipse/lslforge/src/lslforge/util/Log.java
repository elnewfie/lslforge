package lslforge.util;

import lslforge.LSLForgePlugin;

import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;

public class Log {
	private static final String pluginId = "lslforge"; 			//$NON-NLS-1$
	private static int minLogLevel = IStatus.INFO;
	
	private Log() {}		//Force as non-instantiable

	public static void debug(String message) {
		log(IStatus.INFO, IStatus.OK, "[debug] " + message, null); //$NON-NLS-1$
	}
	
	public static void debug(Throwable exception) {
		log(IStatus.INFO, IStatus.OK, "[debug] " + exception.getLocalizedMessage(), exception); //$NON-NLS-1$
	}
	
	public static void debug(String message, Throwable exception) {
		log(IStatus.INFO, IStatus.OK, "[debug] " + message, exception);		//$NON-NLS-1$
	}

	public static void info(String message) {
		log(IStatus.INFO, IStatus.OK, message, null);
	}
	
	public static void info(Throwable exception) {
		log(IStatus.INFO, IStatus.OK, exception.getLocalizedMessage(), exception);
	}
	
	public static void info(String message, Throwable exception) {
		log(IStatus.INFO, IStatus.OK, message, exception);
	}
	
	public static void warning(String message) {
		log(IStatus.WARNING, IStatus.OK, message, null);
	}

	public static void warning(Throwable exception) {
		log(IStatus.WARNING, IStatus.OK, exception.getLocalizedMessage(), exception);
	}

	public static void warning(String message, Throwable exception) {
		log(IStatus.WARNING, IStatus.OK, message, exception);
	}

	public static void error(String message) {
		log(IStatus.ERROR, IStatus.OK, message, null);
	}

	public static void error(Throwable exception) {
		log(IStatus.ERROR, IStatus.OK, exception.getLocalizedMessage(), exception);
	}

	public static void error(String message, Throwable exception) {
		log(IStatus.ERROR, IStatus.OK, message, exception);
	}
	
	/**
	 * Record a message to the log.
	 * @param logLevel the severity; one of <code>IStatus.OK</code>, <code>IStatus.ERROR</code>, 
	 * <code>IStatus.INFO</code>, <code>IStatus.WARNING</code>,  or <code>IStatus.CANCEL</code>
	 * @param code the plug-in-specific status code, or <code>IStatus.OK</code>
	 * @param message message text to include in the log
	 * @param exception a low-level exception, or <code>null</code> if not applicable 
	 */
	private static void log(int logLevel, int code, String message, Throwable exception) {
		if(logLevel >= minLogLevel) {
    		IStatus status= new Status(
    				logLevel, 
    				pluginId,
    				IStatus.OK,
    				message, 
    				exception); 
    		LSLForgePlugin.getDefault().getLog().log(status);
		}
	}
	
	/**
	 * Set the minimum logging status level that will be recorded. 
	 * @param level must be one of <code>DEBUG</code>, <code>INFO</code>, <code>WARNING</code> or <code>ERROR</code>
	 */
	public static void setMinLogLevel(String level) {
		level = level.toUpperCase();
		
		if(level.equals("DEBUG")) { //$NON-NLS-1$
			Log.minLogLevel = IStatus.INFO;
			
		} else if(level.equals("INFO")) { //$NON-NLS-1$
			Log.minLogLevel = IStatus.INFO;
			
		} else if(level.equals("WARNING")) { //$NON-NLS-1$
			Log.minLogLevel = IStatus.WARNING;
			
		} else if(level.equals("ERROR")) { //$NON-NLS-1$
			Log.minLogLevel = IStatus.ERROR;
			
		} else {
			throw new RuntimeException("Invalid log level: " + level); //$NON-NLS-1$
		}
	}
}
