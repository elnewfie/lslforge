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
import lslforge.editor.LslPartitionScanner;
import lslforge.editor.lsl.LslCodeScanner;
import lslforge.language_metadata.LslConstant;
import lslforge.language_metadata.LslFunction;
import lslforge.language_metadata.LslHandler;
import lslforge.language_metadata.LslMetaData;
import lslforge.language_metadata.LslParam;
import lslforge.lsltest.TestManager;
import lslforge.util.LslColorProvider;
import lslforge.util.Util;
import lslforge.util.Util.ArrayMapFunc;

import org.eclipse.core.resources.IFile;
import org.eclipse.core.runtime.FileLocator;
import org.eclipse.core.runtime.IPath;
import org.eclipse.core.runtime.Path;
import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.jface.resource.ImageDescriptor;
import org.eclipse.swt.graphics.Image;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.ui.IWorkbenchPage;
import org.eclipse.ui.PartInitException;
import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.ide.IDE;
import org.eclipse.ui.plugin.AbstractUIPlugin;
import org.osgi.framework.BundleContext;

import com.thoughtworks.xstream.XStream;
import com.thoughtworks.xstream.io.xml.DomDriver;

/**
 * The class representing the LSLForge plugin.
 * @author rgreayer
 *
 */
public class LslForgePlugin extends AbstractUIPlugin {
	public static final String LSLFORGE_NATIVE_PATH = "lslforge.native_path"; //$NON-NLS-1$
    private static final Pattern LSLFORGE_CORE_VERSION_PAT = Pattern.compile("^0\\.6(\\..*)?$"); //$NON-NLS-1$
	private static final String LSLFORGE_CORE_VERSION = "0.6.*"; //$NON-NLS-1$
	private static final String LSL_EXECUTABLE = "LslForge" + ((File.separatorChar == '\\') ? ".exe" : "");  //$NON-NLS-1$//$NON-NLS-2$ //$NON-NLS-3$
	private static final String LSL_COMMAND = "LslForge"; //$NON-NLS-1$
	
    private static class ValidationResult {
        public boolean ok;
        public String msg;            
    }
    
    public static final boolean DEBUG = true;

    private static LslForgePlugin instance;

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
    public static LslForgePlugin getDefault() {
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
                            Util.error(e, e.getLocalizedMessage());
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
    public static Process runCommand(String command, String input, boolean redir) {
        try {
            Process process = launchCoreCommand(command, redir);
            if (process == null) return null;

            OutputStream out = process.getOutputStream();
            OutputStreamWriter writer = new OutputStreamWriter(out);

            writer.write(input);
            writer.close();
            return process;
        } catch (IOException e) {
            Util.error(e, e.getMessage());
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
            Util.chmod(new File(exeName));
        } catch (IOException e) {
            Util.error(e, "can't change mode of native executable"); //$NON-NLS-1$
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
            Util.error(e, e.getLocalizedMessage());
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
                setExecutablePath(path);
                return true;
            }
        }
        
        URL url = FileLocator.find(getDefault().getBundle(), preferredNativePath(), null);
        if (url != null) {
            try {
                path = FileLocator.toFileURL(url).getFile();
                embeddedVersion = tryTask("Version", path); //$NON-NLS-1$
                
                if (checkVersion(embeddedVersion)) {
                    setExecutablePath(path);
                    return true;
                }
            } catch (IOException e) {
                Util.error(e, "can't locate " + url); //$NON-NLS-1$
            }
        }
        
        url = FileLocator.find(getDefault().getBundle(), alternateNativePath(), null);
        if (url != null) {
            try {
                path = FileLocator.toFileURL(url).getFile();
                embeddedVersion = tryTask("Version", path); //$NON-NLS-1$
                
                if (checkVersion(embeddedVersion)) {
                    setExecutablePath(path);
                    return true;
                }
            } catch (IOException e) {
                Util.error(e, "can't locate " + url); //$NON-NLS-1$
            }
        }
        
        installedVersion = tryTask("Version", LSL_COMMAND); //$NON-NLS-1$
        if (checkVersion(installedVersion)) {
            setExecutablePath(LSL_COMMAND);
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
        buf.append("http://hackage.haskell.org/cgi-bin/hackage-scripts/pacakge/LslForge\n\n"); //$NON-NLS-1$ TODO
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
    
    private void setExecutablePath(String path) {
        Util.log("executablePath = " + path); //$NON-NLS-1$
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
            Util.error(e, "can't compute default path"); //$NON-NLS-1$
            return ""; //$NON-NLS-1$
        }
    }
    
    public static String runTask(String command, String input) {
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
            Util.error(e,e.getLocalizedMessage());
            return null;
        } finally {
            try { 
                reader.close();
            } catch (IOException e) {
                Util.error(e, e.getLocalizedMessage());
            }
        }
    }
    
    static String validateExpression(String expression) {
        if (DEBUG) Util.log("expression: " + expression); //$NON-NLS-1$
        String result = runTask("ExpressionHandler", expression); //$NON-NLS-1$
        if (DEBUG) Util.log("result: " + result); //$NON-NLS-1$
        if (result == null) {
            return "Can't evaluate expression (internal error)"; //$NON-NLS-1$ TODO
        }
        XStream xstream = new XStream(new DomDriver());
        xstream.alias("result", ValidationResult.class); //$NON-NLS-1$
        ValidationResult e = (ValidationResult) xstream.fromXML(result);
        
        if (e.ok) return null;
        return e.msg;
    }
    
    private LslCodeScanner fCodeScanner;

    private LslColorProvider fColorProvider;

    private ErrorDecorator fErrorDecorator;

    private LslPartitionScanner fPartitionScanner;

    private LslMetaData lslMetaData = null;

    private TestManager testManager = null;

    private SimManager simManager = null;

    private static String[] statefulFunctions = null;

    /**
     * Creates a new plug-in instance.
     */
    public LslForgePlugin() {
        instance = this;
    }

    private LslMetaData buildMetaData() {
        String result = runTask("MetaData", ""); //$NON-NLS-1$//$NON-NLS-2$
        if (result == null) {
            Util.error(Messages.LslForgePlugin_NO_META_DATA);
            return new LslMetaData();
        }
        if (DEBUG) Util.log("Meta-Data: " + result); //$NON-NLS-1$
        XStream xstream = new XStream(new DomDriver());

        xstream.alias("lslmeta", LslMetaData.class); //$NON-NLS-1$
        xstream.alias("handler", LslHandler.class); //$NON-NLS-1$
        xstream.alias("param", LslParam.class); //$NON-NLS-1$
        xstream.alias("function", LslFunction.class); //$NON-NLS-1$
        xstream.alias("constant", LslConstant.class); //$NON-NLS-1$
        LslMetaData md = null;
        try {
            md = (LslMetaData) xstream.fromXML(result);
        } catch (Exception e) {
            Util.error(e, Messages.LslForgePlugin_COULD_NOT_DESERIALIZE_META_DATA);
            md = new LslMetaData();
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
    public LslCodeScanner getLslCodeScanner() {
        if (fCodeScanner == null) {
            String[] handlerNames = Util.arrayMap(new Util.ArrayMapFunc<String>() {
                public Class<String> elementType() {
                    return String.class;
                }

                public String map(Object o) {
                    return ((LslHandler) o).getName();
                }
            }, getLslMetaData().getHandlers());
            String[] predefFuncs = Util.arrayMap(new Util.ArrayMapFunc<String>() {
                public Class<String> elementType() {
                    return String.class;
                }

                public String map(Object o) {
                    return ((LslFunction) o).getName();
                }
            }, getLslMetaData().getFunctions());
            String[] predefConsts = Util.arrayMap(new Util.ArrayMapFunc<String>() {
                public Class<String> elementType() {
                    return String.class;
                }

                public String map(Object o) {
                    return ((LslConstant) o).getName();
                }
            }, getLslMetaData().getConstants());
            fCodeScanner = new LslCodeScanner(getLslColorProvider(), handlerNames, predefFuncs,
                    predefConsts, this.getPreferenceStore());
        }
        return fCodeScanner;
    }

    /**
     * Returns the singleton Java color provider.
     * 
     * @return the singleton Java color provider
     */
    public LslColorProvider getLslColorProvider() {
        if (fColorProvider == null)
            fColorProvider = new LslColorProvider(this.getPreferenceStore());
        return fColorProvider;
    }
    
    public synchronized LslMetaData getLslMetaData() {
        if (lslMetaData == null) {
            lslMetaData = buildMetaData();
        }
        return lslMetaData;
    }
    
    /**
     * Return a scanner for creating LSLForge partitions.
     * 
     * @return a scanner for creating Java partitions
     */
    public LslPartitionScanner getLslPartitionScanner() {
        if (fPartitionScanner == null)
            fPartitionScanner = new LslPartitionScanner();
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
        if (LslForgePlugin.statefulFunctions == null) {
            List<String> funcs = Util.filtMap(new ArrayMapFunc<String>() {
                public Class<String> elementType() { return String.class; }
                public String map(Object o) {
                    LslFunction f = (LslFunction) o;
                    return f.isStateless() ? null : f.getName();
                }
            }, getLLFunctions());
            
            LslForgePlugin.statefulFunctions = funcs.toArray(new String[funcs.size()]);
        }
        
        return LslForgePlugin.statefulFunctions;
    }
    public static LslFunction[] getLLFunctions() {
        return getDefault().getLslMetaData().getFunctions();
    }

    public void start(BundleContext context) throws Exception {
    	super.start(context);
        getPreferenceStore().setDefault(LSLFORGE_NATIVE_PATH, ""); //$NON-NLS-1$
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
}
