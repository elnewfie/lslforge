package lslforge.install;

import java.io.File;
import java.io.IOException;
import java.net.URL;

import lslforge.util.Util;

import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.FileLocator;
import org.eclipse.update.core.BaseInstallHandler;
import org.eclipse.update.core.IPluginEntry;
import org.eclipse.update.core.ISite;

public class InstallHandler extends BaseInstallHandler {

    @Override
	public void completeConfigure() throws CoreException {
        try {
            if (File.separatorChar == '\\') return; // windows...
            ISite site = feature.getSite();
            URL url = FileLocator.toFileURL(site.getURL());
            String path = url.getFile();
            File sitedir = new File(path);
            
            IPluginEntry[] entries = feature.getPluginEntries();
            for (int i = 0; i < feature.getPluginEntries().length; i++) {
                String s = entries[i].getVersionedIdentifier().toString();
                String path1 = sitedir.getPath() + File.separator + "plugins" + File.separator + s; //$NON-NLS-1$
                
                File pluginDir = new File(path1);
                doit(pluginDir);
                System.out.println(path1);
            }
        } catch (Exception e) {
            e.printStackTrace();
        }
    }

    private void doit(File f) {
        if (f.isDirectory()) {
            File[] files = f.listFiles();
            for (int i = 0; i < files.length; i++) {
                doit(files[i]);
            }
        } else if ("LSLForge".equals(f.getName())) { //$NON-NLS-1$
            try {
                Util.chmod(f);
            } catch (IOException e) {
                e.printStackTrace();
            }
            System.out.println("doit to: " + f.getAbsolutePath()); //$NON-NLS-1$
        }
    }

}
