package lslplus;

import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;

import lslplus.debug.LslSimProcess;
import lslplus.sim.SimEvent;
import lslplus.sim.SimEventDefinition;
import lslplus.sim.SimEventListener;
import lslplus.sim.SimKeyManager;
import lslplus.sim.SimMetaDataListener;
import lslplus.sim.SimStatuses;
import lslplus.sim.SimStatuses.SimState;
import lslplus.simview.SimWatcherViewPart;
import lslplus.util.Util;

import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.core.runtime.jobs.Job;
import org.eclipse.debug.core.DebugException;
import org.eclipse.ui.IWorkbenchPage;
import org.eclipse.ui.PartInitException;

import com.thoughtworks.xstream.XStream;
import com.thoughtworks.xstream.io.xml.DomDriver;

public class SimManager implements SimEventListener {
    private static final String SIM_META_DATA = "SimMetaData"; //$NON-NLS-1$

    private static class SimMetaData {
        private static XStream xstream;
        
        static {
            xstream = new XStream(new DomDriver());
            xstream.alias("sim-meta-data", SimMetaData.class); //$NON-NLS-1$
            SimEventDefinition.configureXStream(xstream);
        }
        
        public static SimMetaData fromXML(String input) {
            return (SimMetaData)xstream.fromXML(input);
        }
        
        private SimEventDefinition[] eventDescriptors;
        
        public SimEventDefinition[] getEventDescriptors() { return eventDescriptors; }
    }
    
    private HashSet<SimListener> listeners = new HashSet<SimListener>();
    private HashSet<SimEventListener> simEventListeners = new HashSet<SimEventListener>();
    private HashSet<SimMetaDataListener> simMetaDataListeners = new HashSet<SimMetaDataListener>();
    private volatile boolean active  = false;
    private LslSimProcess process = null;
    private SimState simState;
    private volatile HashMap<String,SimEventDefinition> eventDescriptors = null;
    private SimKeyManager keyManager = new SimKeyManager();
    
    public SimManager() {
        buildSimMetaData();
    }
    
    public synchronized void addSimListener(SimListener listener) {
        this.listeners.add(listener);
    }
    public synchronized void removeSimListener(SimListener listener) {
        this.listeners.remove(listener);
    }

    public synchronized void addSimMetaDataListener(SimMetaDataListener listener) {
        this.simMetaDataListeners.add(listener);
    }
    
    public synchronized void removeSimMetaDataListener(SimMetaDataListener listener) {
        this.simMetaDataListeners.remove(listener);
    }
    
    public void addSimEventListener(SimEventListener listener) {
        synchronized (simEventListeners) {
            this.simEventListeners.add(listener);
        }
    }
    
    public void removeSimEventListener(SimEventListener listener) {
        synchronized (simEventListeners) {
            this.simEventListeners.remove(listener);
        }
    }
    
    public synchronized void simLaunched(LslSimProcess process) {
        active = true;
        this.process = process;
        LslPlusPlugin.getDefault().getWorkbench().getDisplay().asyncExec(new Runnable() {
            public void run() { showSimWatcherInActivePage(findSimWatcherInActivePage());}
        });

        fireSimLaunched();
    }
    
    public synchronized void simStopped() {
        if (active) {
            active = false;
            process = null;
            fireSimEnded();
        }
    }

    public synchronized void stopSim() {
        if (process != null) {
            try {
                process.terminate();
            } catch (DebugException e) {
                Util.error(e, e.getLocalizedMessage());
            }
        }
        
        simStopped();
    }
    
    public boolean canLaunch() {
        return !active;
    }

    private SimWatcherViewPart showSimWatcherInActivePage(SimWatcherViewPart simWatcher) {
        IWorkbenchPage page= null;
        try {
            try {
                page= LslPlusPlugin.getDefault().getWorkbench().getActiveWorkbenchWindow().getActivePage();
            } catch (NullPointerException e) {
            }

            if (page == null)
                return null;

            if (simWatcher != null && simWatcher.isCreated()) {
                page.bringToTop(simWatcher);
                return simWatcher;
            }
            //  show the result view if it isn't shown yet
            return (SimWatcherViewPart) page.showView(SimWatcherViewPart.ID);
        } catch (PartInitException pie) {
            Util.error(pie, pie.getLocalizedMessage());
            return null;
        } finally{
            //restore focus stolen by the creation of the result view
//            if (page != null && activePart != null)
//                page.activate(activePart);
        }
    }

    private SimWatcherViewPart findSimWatcherInActivePage() {
        IWorkbenchPage page= LslPlusPlugin.getDefault().getWorkbench().getActiveWorkbenchWindow().getActivePage();
        if (page == null)
            return null;
        return (SimWatcherViewPart) page.findView(SimWatcherViewPart.ID);
    }

    public boolean isSimActive() {
        return active;
    }
    
    private void fireSimLaunched() {
        for (Iterator<SimListener> i = listeners.iterator(); i.hasNext();) {
            SimListener listener = i.next();
            
            listener.simLaunched();
        }
    }
    
    private void fireSimEnded() {
        for (Iterator<SimListener> i = listeners.iterator(); i.hasNext(); ) {
            SimListener listener = i.next();
            listener.simEnded();
        }
    }

    private void fireNewSimState(SimStatuses.Message[] messages) {
        for (Iterator<SimListener> i = listeners.iterator(); i.hasNext(); ) {
            SimListener listener = i.next();
            listener.newSimState(simState, messages);
        }
    }
    
    public synchronized void setSimState(SimState state, SimStatuses.Message[] messages) {
        this.simState = state;
        fireNewSimState(messages);
    }
    
    public synchronized SimState getSimState() { return simState; }
    
    public void putEvent(final SimEvent event) {
        // TODO: there must be an Eclipse approved way to do this (Job?)
        Thread t = new Thread() {
            public void run() {
                try {
                    synchronized (simEventListeners) {
                        for (Iterator<SimEventListener> i = simEventListeners.iterator(); i.hasNext(); ) {
                            SimEventListener l = i.next();
                            l.putEvent(event);
                        }
                    }
                } catch (Exception e) {
                    Util.error(e,e.getLocalizedMessage());
                }
            }
        };
        t.start();
    }
    
    public SimEventDefinition getAnEventDefinition(String name) {
        if (eventDescriptors == null) return null;
        return eventDescriptors.get(name);
    }
    
    public SimEventDefinition[] getAllEventDefinitions() {
        HashMap<String,SimEventDefinition> map = eventDescriptors;
        if (map == null) return null;
        return map.values().toArray(new SimEventDefinition[map.size()]);
    }
    
    private void buildSimMetaData() {
        Job job = new Job("BuildSimMetaData") { //$NON-NLS-1$

            protected IStatus run(IProgressMonitor monitor) {
                String metaDataString = LslPlusPlugin.runTask(SIM_META_DATA, ""); //$NON-NLS-1$
                Util.log("metaDataString = " + metaDataString); //$NON-NLS-1$
                if (metaDataString == null) return new Status(IStatus.ERROR, LslPlusPlugin.PLUGIN_ID,
                        Messages.SimManager_Cant_Get_Simulator_Information);
                SimMetaData metaData = SimMetaData.fromXML(metaDataString);
                
                HashMap<String,SimEventDefinition> map = new HashMap<String, SimEventDefinition>();
                for (int i = 0; i < metaData.getEventDescriptors().length; i++) {
                    SimEventDefinition def = metaData.getEventDescriptors()[i];
                    map.put(def.getName(), def);
                }
                
                eventDescriptors = map;
                fireSimMetaDataReady();
                return new Status(IStatus.OK,LslPlusPlugin.PLUGIN_ID, Messages.SimManager_OK);
            }
            
        };
        
        job.schedule(100L);

    }
    
    protected void fireSimMetaDataReady() {
        for (Iterator<SimMetaDataListener> i = simMetaDataListeners.iterator(); i.hasNext();) {
            SimMetaDataListener l = i.next();
            l.metaDataReady();
        }
    }

    public SimKeyManager getKeyManager() { return keyManager; }
}
