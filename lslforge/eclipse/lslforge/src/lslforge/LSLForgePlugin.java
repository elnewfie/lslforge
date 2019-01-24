/*******************************************************************************
 * Copyright (c) 2000, 2005 IBM Corporation and others.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 *     IBM Corporation - initial API and implementation
 *******************************************************************************/
package lslforge;

import java.io.File;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.OutputStream;
import java.io.OutputStreamWriter;
import java.net.URL;
import java.util.List;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import lslforge.decorators.ErrorDecorator;
import lslforge.editor.LSLPartitionScanner;
import lslforge.editor.lsl.LSLCodeScanner;
import lslforge.language_metadata.LSLConstant;
import lslforge.language_metadata.LSLFunction;
import lslforge.language_metadata.LSLHandler;
import lslforge.language_metadata.LSLMetaData;
import lslforge.language_metadata.LSLParam;
import lslforge.lsltest.TestManager;
import lslforge.util.LSLColorProvider;
import lslforge.util.Log;
import lslforge.util.Util;
import lslforge.util.Util.ArrayMapFunc;

import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.ResourcesPlugin;
import org.eclipse.core.runtime.FileLocator;
import org.eclipse.core.runtime.IPath;
import org.eclipse.core.runtime.Path;
import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.jface.resource.ImageDescriptor;
import org.eclipse.jface.util.IPropertyChangeListener;
import org.eclipse.jface.util.PropertyChangeEvent;
import org.eclipse.swt.graphics.Image;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.ui.IWorkbenchPage;
import org.eclipse.ui.PartInitException;
import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.ide.IDE;
import org.eclipse.ui.plugin.AbstractUIPlugin;
import org.osgi.framework.BundleContext;
import org.osgi.framework.BundleEvent;
import org.osgi.framework.BundleListener;

import com.thoughtworks.xstream.XStream;
import com.thoughtworks.xstream.io.xml.DomDriver;

/**
 * The class representing the LSLForge plugin.
 * @author rgreayer
 *
 */
public class LSLForgePlugin extends AbstractUIPlugin implements IPropertyChangeListener {
	public static final String LSLFORGE_NATIVE_PATH = "lslforge.native_path"; //$NON-NLS-1$
    private static final Pattern LSLFORGE_CORE_VERSION_PAT = Pattern.compile("^0\\.1(\\..*)?$"); //$NON-NLS-1$
	private static final String LSLFORGE_CORE_VERSION = "0.1.*"; //$NON-NLS-1$
	private static final String LSL_EXECUTABLE = "LSLForge" + ((File.separatorChar == '\\') ? ".exe" : "");  //$NON-NLS-1$//$NON-NLS-2$ //$NON-NLS-3$
	private static final String LSL_COMMAND = "LSLForge"; //$NON-NLS-1$
	
    private static class ValidationResult {
        public boolean ok;
        public String msg;            
    }
    
    public static final boolean DEBUG = false;

    private static LSLForgePlugin instance;

    public final static String LSL_PARTITIONING = "__lsl_partitioning"; //$NON-NLS-1$
    
    public static final String PLUGIN_ID = "lslforge"; //$NON-NLS-1$
    public static Image createImage(String path) {
        ImageDescriptor descriptor = imageDescriptorFromPlugin(path);
        if (descriptor != null) return descriptor.createImage();
        return null;
    }
    /**
     * Returns the default plug-in instance.
     * 
     * @return the default plug-in instance
     */
    public static LSLForgePlugin getDefault() {
        return instance;
    }
    public static ImageDescriptor imageDescriptorFromPlugin(String path) {
        return imageDescriptorFromPlugin(PLUGIN_ID, path);
    }
    public static void openResource(Shell shell, final IFile resource) {
        final IWorkbenchPage activePage= 
            (PlatformUI.getWorkbench() != null &&
            PlatformUI.getWorkbench().getActiveWorkbenchWindow() != null) ?
            PlatformUI.getWorkbench().getActiveWorkbenchWindow().getActivePage() : null;
            
        if (activePage != null) {
            final Display display= shell.getDisplay();
            if (display != null) {
                display.asyncExec(new Runnable() {
                    public void run() {
                        try {
                            IDE.openEditor(activePage, resource, true);
                        } catch (PartInitException e) {
                            Log.error(e);
                        }
                    }
                });
            }
        }
    }
    
    /**
     * Run executable with an input string.  Return a process so that the output can be monitored
     * as it is produced.
     * @param command the path to the executable
     * @param input the input string
     * @param redir an indicator as to whether stderr should be redirected to stdout
     * @return the process to monitor
     */
    private static Process runCommand(String command, String input, boolean redir) {
        try {
            Process process = launchCoreCommand(command, redir);
            if (process == null) return null;

            OutputStream out = process.getOutputStream();
            OutputStreamWriter writer = new OutputStreamWriter(out);

            writer.write(input);
            writer.close();
            return process;
        } catch (IOException e) {
            Log.error(e);
            return null;
        }
    }

    /**
     * Find and launch a command in the LSLForge Core.
     * @param command the command for the core to execute.
     * @param redir an indicator as to whether stderr should be redirected to stdout
     * @return the process to monitor
     */
    public static Process launchCoreCommand(String command, boolean redir) {
        String path = getDefault().getExecutablePath();
        if (path == null) return null;
        return execute(command, redir, path);
    }
    
    private static Process executeWithRetry(String command, boolean redir, String exeName) {
        Process p = execute(command, redir, exeName);
        
        if (p != null) return p;
        if (File.separatorChar == '\\') return null;
        
        // only want to do this if normal execution failed...
        try {
            Log.info("Changing mod for native executable");
            Util.chmod(new File(exeName));
        } catch (IOException e) {
            Log.error("can't change mode of native executable", e); //$NON-NLS-1$
        }
        return execute(command, redir, exeName);
    }
    
    private static Process execute(String command, boolean redir, String exeName) {
        try {
            ProcessBuilder builder = new ProcessBuilder(new String[] { exeName, command });
            builder.redirectErrorStream(redir);
            Process process = builder.start();

            return process;
        } catch (IOException e) {
            Log.error(e);
            return null;
        }
    }
    
    private String executablePath;
    
    private boolean determineExecutable() {
        String path = getDefault().getPreferenceStore().getString(LSLFORGE_NATIVE_PATH);
        String preferredVersion = null;
        String embeddedVersion = null;
        String installedVersion = null;
        if (path != null && !"".equals(path.trim())) { //$NON-NLS-1$
            preferredVersion = tryTask("Version", path); //$NON-NLS-1$
            if (checkVersion(preferredVersion)) {
                setExecutablePath(path, preferredVersion);
                return true;
            }
        }
        
        URL url = FileLocator.find(getDefault().getBundle(), preferredNativePath(), null);
        if (url != null) {
            try {
                path = FileLocator.toFileURL(url).getFile();
                embeddedVersion = tryTask("Version", path); //$NON-NLS-1$
                
                if (checkVersion(embeddedVersion)) {
                    setExecutablePath(path, embeddedVersion);
                    return true;
                }
            } catch (IOException e) {
                Log.error("can't locate " + url, e); //$NON-NLS-1$
            }
        }
        
        url = FileLocator.find(getDefault().getBundle(), alternateNativePath(), null);
        if (url != null) {
            try {
                path = FileLocator.toFileURL(url).getFile();
                embeddedVersion = tryTask("Version", path); //$NON-NLS-1$
                
                if (checkVersion(embeddedVersion)) {
                    setExecutablePath(path, embeddedVersion);
                    return true;
                }
            } catch (IOException e) {
                Log.error("can't locate " + url, e); //$NON-NLS-1$
            }
        }
        
        installedVersion = tryTask("Version", LSL_COMMAND); //$NON-NLS-1$
        if (checkVersion(installedVersion)) {
            setExecutablePath(LSL_COMMAND, installedVersion);
            return true;
        }
        
        StringBuilder versions = new StringBuilder();
        boolean versionFound = false;
        if (preferredVersion != null) {
            versions.append(preferredVersion).append(
            		" (version of executable set in LSLForge Preferences)\n"); //$NON-NLS-1$ TODO
            versionFound = true;
        }
        if (embeddedVersion != null) {
            versions.append(embeddedVersion).append(" (version installed as part of plugin)\n"); //$NON-NLS-1$ TODO
            versionFound = true;
        }
        if (installedVersion != null) {
            versions.append(installedVersion).append(" (version found on PATH)\n"); //$NON-NLS-1$ TODO
            versionFound = true;
        }
        
        final StringBuilder buf = new StringBuilder();
        if (versionFound) {
            buf.append("The following versions of the LSLForge native executable were found:\n"); //$NON-NLS-1$ TODO
            buf.append(versions);
            buf.append("\nNone of these version are compatible with this plugin, which requires\n"); //$NON-NLS-1$ TODO
            buf.append("version ").append(LSLFORGE_CORE_VERSION).append(".\n");  //$NON-NLS-1$//$NON-NLS-2$ TODO
        } else {
            buf.append("The LSLForge native executable was not found!\n"); //$NON-NLS-1$ TODO
        }
        buf.append("The LSLForge native executable is available from Hackage:\n"); //$NON-NLS-1$ TODO
        buf.append("http://hackage.haskell.org/cgi-bin/hackage-scripts/package/LSLForge\n\n"); //$NON-NLS-1$ TODO
        buf.append("Please also see the Help documentation for LSLForge, under 'Installation'"); //$NON-NLS-1$ TODO
        getWorkbench().getDisplay().asyncExec(new Runnable() {
            public void run() {
                MessageDialog dlg = new MessageDialog(
                        getWorkbench().getActiveWorkbenchWindow().getShell(),
                        "LSLForge Native Executable Problem", //$NON-NLS-1$ TODO
                        null,
                        buf.toString(),
                        MessageDialog.ERROR,
                        new String[] { "Ok" }, //$NON-NLS-1$ TODO
                        0);
                dlg.open();
            }
        });
        Log.info("Not found executable: "+buf.toString());
        
        return false;
    }
    
    private String tryTask(String command, String path) {
        try {
            Process process = executeWithRetry(command, true, path);
            if (process == null) return null;

            OutputStream out = process.getOutputStream();
            OutputStreamWriter writer = new OutputStreamWriter(out);

            writer.close();
            
            return readString(process.getInputStream());
        } catch (IOException e) {
            return null;
        }
    }
    
    private String getExecutablePath() {
        return executablePath;
    }
    
    private void setExecutablePath(String path, String version) {
        Log.info("setExecutablePath: " + path+", Version: "+version); //$NON-NLS-1$
        this.executablePath = path;
    }
    
    private static IPath preferredNativePath() {
        return new Path("os") //$NON-NLS-1$
            .append(System.getProperty("osgi.os", "os")) //$NON-NLS-1$ //$NON-NLS-2$
            .append(System.getProperty("osgi.arch", "arch")) //$NON-NLS-1$ //$NON-NLS-2$
            .append(LSL_EXECUTABLE).removeTrailingSeparator();
    }
    
    private static IPath alternateNativePath() {
        if ("x86_64".equals(System.getProperty("osgi.arch", "arch"))) { //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
            return new Path("os") //$NON-NLS-1$
                .append(System.getProperty("osgi.os", "os")) //$NON-NLS-1$ //$NON-NLS-2$
                .append("x86") //$NON-NLS-1$
                .append(LSL_EXECUTABLE).removeTrailingSeparator();
        } else {
            return null;
        }
    }

    public static String defaultNativePath() {
        try {
            URL url = FileLocator.find(getDefault().getBundle(), new Path("os"), null); //$NON-NLS-1$
            String os = System.getProperty("osgi.os", "os");  //$NON-NLS-1$//$NON-NLS-2$
            String arch = System.getProperty("osgi.arch", "arch");  //$NON-NLS-1$//$NON-NLS-2$
            File f = new File(FileLocator.toFileURL(url).getFile() + File.separatorChar + os + File.separatorChar +
                    arch + File.separatorChar + LSL_EXECUTABLE);
            
            return f.getAbsolutePath();
        } catch (IOException e) {
            Log.error("can't compute default path", e); //$NON-NLS-1$
            return ""; //$NON-NLS-1$
        }
    }
    
    public static String runExecutableAndForget(String command, String input) {
        Process p = runCommand(command, input, true);
        if (p == null) return null;
        return readString(p.getInputStream());
    }
    
    private static String readString(InputStream in) {
        InputStreamReader reader = new InputStreamReader(in);
        StringBuilder buf = new StringBuilder();
        char[] chars = new char[512];
        int count = 0;
        
        try {
            while ((count = reader.read(chars)) >= 0) {
                buf.append(chars, 0, count);
            }
            
            return buf.toString();
        } catch (IOException e) {
            Log.error(e);
            return null;
        } finally {
            try { 
                reader.close();
            } catch (IOException e) {
                Log.error(e);
            }
        }
    }
    
    static String validateExpression(String expression) {
        Log.debug("expression: " + expression); //$NON-NLS-1$
        String result = runExecutableAndForget("ExpressionHandler", expression); //$NON-NLS-1$
        Log.debug("result: " + result); //$NON-NLS-1$
        if (result == null) {
            return "Can't evaluate expression (internal error)"; //$NON-NLS-1$ TODO
        }
        XStream xstream = new XStream(new DomDriver());
        xstream.alias("result", ValidationResult.class); //$NON-NLS-1$
        ValidationResult e = (ValidationResult) xstream.fromXML(result);
        
        if (e.ok) return null;
        return e.msg;
    }
    
    private LSLCodeScanner fCodeScanner;

    private LSLColorProvider fColorProvider;

    private ErrorDecorator fErrorDecorator;

    private LSLPartitionScanner fPartitionScanner;

    private LSLMetaData lSLMetaData = null;

    private TestManager testManager = null;

    private SimManager simManager = null;

    private static String[] statefulFunctions = null;

    /**
     * Creates a new plug-in instance.
     */
    public LSLForgePlugin() {
        instance = this;
    }

    private LSLMetaData buildMetaData() {
        String result = runExecutableAndForget("MetaData", ""); //$NON-NLS-1$//$NON-NLS-2$
        if (result == null) {
            Log.error(Messages.LSLForgePlugin_NO_META_DATA);
            return new LSLMetaData();
        }
        Log.debug("Meta-Data: " + result); //$NON-NLS-1$
        XStream xstream = new XStream(new DomDriver());

        xstream.alias("lslmeta", LSLMetaData.class); //$NON-NLS-1$
        xstream.alias("handler", LSLHandler.class); //$NON-NLS-1$
        xstream.alias("param", LSLParam.class); //$NON-NLS-1$
        xstream.alias("function", LSLFunction.class); //$NON-NLS-1$
        xstream.alias("constant", LSLConstant.class); //$NON-NLS-1$
        LSLMetaData md = null;
        try {
            md = (LSLMetaData) xstream.fromXML(result);
        } catch (Exception e) {
            Log.error(Messages.LSLForgePlugin_COULD_NOT_DESERIALIZE_META_DATA, e);
            md = new LSLMetaData();
        }
        return md;
    }

    public void errorStatusChanged() {
        getWorkbench().getDisplay().asyncExec(new Runnable() {
            public void run() {
                if (fErrorDecorator != null) {
                    fErrorDecorator.errorStatusChanged();
                }
            }
        });
    }

    /**
     * Returns the singleton LSL code scanner.
     * 
     * @return the singleton LSL code scanner
     */
    public LSLCodeScanner getLSLCodeScanner() {
        if (fCodeScanner == null) {
            String[] handlerNames = Util.arrayMap(new Util.ArrayMapFunc<String>() {
                public Class<String> elementType() {
                    return String.class;
                }

                public String map(Object o) {
                    return ((LSLHandler) o).getName();
                }
            }, getLSLMetaData().getHandlers());
            String[] predefFuncs = Util.arrayMap(new Util.ArrayMapFunc<String>() {
                public Class<String> elementType() {
                    return String.class;
                }

                public String map(Object o) {
                    return ((LSLFunction) o).getName();
                }
            }, getLSLMetaData().getFunctions());
            String[] predefConsts = Util.arrayMap(new Util.ArrayMapFunc<String>() {
                public Class<String> elementType() {
                    return String.class;
                }

                public String map(Object o) {
                    return ((LSLConstant) o).getName();
                }
            }, getLSLMetaData().getConstants());
            fCodeScanner = new LSLCodeScanner(getLSLColorProvider(), handlerNames, predefFuncs,
                    predefConsts, this.getPreferenceStore());
        }
        return fCodeScanner;
    }

    /**
     * Returns the singleton Java color provider.
     * 
     * @return the singleton Java color provider
     */
    public LSLColorProvider getLSLColorProvider() {
        if (fColorProvider == null)
            fColorProvider = new LSLColorProvider(this.getPreferenceStore());
        return fColorProvider;
    }
    
    public synchronized LSLMetaData getLSLMetaData() {
        if (lSLMetaData == null) {
            lSLMetaData = buildMetaData();
        }
        return lSLMetaData;
    }
    
    /**
     * Return a scanner for creating LSLForge partitions.
     * 
     * @return a scanner for creating Java partitions
     */
    public LSLPartitionScanner getLSLPartitionScanner() {
        if (fPartitionScanner == null)
            fPartitionScanner = new LSLPartitionScanner();
        return fPartitionScanner;
    }

    public TestManager getTestManager() {
        return testManager;
    }
    
    public void setErrorDecorator(ErrorDecorator errorDecorator) {
        this.fErrorDecorator = errorDecorator;
    }
    public SimManager getSimManager() {
        return simManager;
    }
    public static synchronized String[] getStatefulFunctions() {
        if (LSLForgePlugin.statefulFunctions == null) {
            List<String> funcs = Util.filtMap(new ArrayMapFunc<String>() {
                public Class<String> elementType() { return String.class; }
                public String map(Object o) {
                    LSLFunction f = (LSLFunction) o;
                    return f.isStateless() ? null : f.getName();
                }
            }, getLLFunctions());
            
            LSLForgePlugin.statefulFunctions = funcs.toArray(new String[funcs.size()]);
        }
        
        return LSLForgePlugin.statefulFunctions;
    }
    public static LSLFunction[] getLLFunctions() {
        return getDefault().getLSLMetaData().getFunctions();
    }

    @Override
	public void start(BundleContext context) throws Exception {
    	super.start(context);
    	
    	//Set up our logging
    	if(DEBUG) {
    		Log.setMinLogLevel("debug"); //$NON-NLS-1$
    	} else {
    		//Only interested in unusual log entries and info
    		Log.setMinLogLevel("info"); //$NON-NLS-1$
    	}
    	
        getPreferenceStore().setDefault(LSLFORGE_NATIVE_PATH, ""); //$NON-NLS-1$
        getPreferenceStore().addPropertyChangeListener(this);
        
        performDetermineExectuable();
  
        //Add a listener to the bundle
        context.addBundleListener(new BundleListener() {
			public void bundleChanged(BundleEvent event) {
				//Wait until everything is loaded and ready
				if(event.getType() == BundleEvent.STARTED) {
					//Check open projects and convert them
					for(IProject project: ResourcesPlugin.getWorkspace().getRoot().getProjects()) {
						LSLProjectNature.fixProjectNature(project);
					}
				}
			}
		});
    }
    
    private void performDetermineExectuable() {
    	//checkVersion();
        if (determineExecutable()) {
            testManager = new TestManager();
            simManager  = new SimManager();
        }
    }
    
    private boolean checkVersion(final String version) {
        if (version == null) return false;
        Matcher m = LSLFORGE_CORE_VERSION_PAT.matcher(version.trim());
        boolean matches = m.matches();
        return matches;
    }
    
	// Fires on Preferences property change
    public void propertyChange(PropertyChangeEvent event) {
	    if (LSLFORGE_NATIVE_PATH.equals(event.getProperty())) {
		    Log.debug("Changing ["+event.getProperty()+"]: to: \""+event.getNewValue().toString()+"\" from: \""+event.getOldValue().toString()+"\"");
		    if (event.getNewValue() != null) {
		    	performDetermineExectuable();
		    	try {
		    	  if (LSLProjectNature.getStaticCompilationServer()!=null) {
		    		  //Process previous = LSLProjectNature.getStaticCompilationServer().getProcess();
		    		  //LSLProjectNature.getStaticCompilationServer().execute(new CompilationCommand());
		    		  // Simply adding new executable for now
		    		  LSLProjectNature.getStaticCompilationServer().startProcess();
		    	  }
		    	} catch (Error e) {
		    	  Log.error("Attempting to add new executable failed: "+e.getMessage());
		    	}
		    }
	    }
    }
}
