package lslforge.simview;

import java.util.HashMap;

import lslforge.LSLForgePlugin;
import lslforge.SimManager;
import lslforge.sim.SimEvent;
import lslforge.sim.SimEventArg;
import lslforge.sim.SimEventDefinition;
import lslforge.sim.SimParamDefinition;
import lslforge.sim.SimStatuses.NameKeyType;
import lslforge.sim.SimStatuses.SimPrim;
import lslforge.sim.SimStatuses.SimScript;

import org.eclipse.jface.dialogs.Dialog;
import org.eclipse.jface.dialogs.IDialogConstants;
import org.eclipse.jface.window.Window;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.ModifyEvent;
import org.eclipse.swt.events.ModifyListener;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.events.SelectionListener;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Combo;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Event;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Listener;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.swt.widgets.Text;

public class EventDialog extends Dialog {
    private Combo combo;
    private String[] args = null;
    private boolean[] argValid = null;
    private SimEventDefinition desc;
    protected EventDialog(Shell parentShell, SimEventDefinition desc) {
        super(parentShell);
        this.desc = desc;
        args = new String[desc.getParams().length];
        argValid  = new boolean[args.length];
        for (int i = 0; i < argValid.length; i++) argValid[i] = false;
    }

    private Button okButton() {
        return getButton(IDialogConstants.OK_ID);
    }
    
    private boolean selectionIsValid() {
        return combo.getSelectionIndex() >= 0;
    }

    private void updateButtons() {
        boolean valid = true;
        for (int i = 0; valid && i < args.length; i++) {
            valid = argValid[i];
        }
        
        okButton().setEnabled(valid);
    }
    
    protected Control createButtonBar(Composite parent) {
        Control c = super.createButtonBar(parent);
        okButton().setEnabled(false);
        okButton().addListener(SWT.Show, new Listener() {
           public void handleEvent(Event event) {
               okButton().setEnabled(selectionIsValid());
           }
        });
        return c;
    }
    
    protected Control createDialogArea(Composite parent) {
        getShell().setText("Event: " + desc.getName()); //$NON-NLS-1$ TODO
        Composite  composite = (Composite) super.createDialogArea(parent);
        Label descriptionLabel = new Label(composite, SWT.LEAD|SWT.HORIZONTAL|SWT.WRAP);
        descriptionLabel.setText(desc.getDescription());
        
        SimParamDefinition[] parameters = desc.getParams();
        
        boolean skip = false;
        for (int i = 0; i < parameters.length; i++) {
            if (skip) {
                skip = false;
                continue;
            }
            
            Composite comp = new Composite(parent, SWT.NONE);
            GridLayout layout = new GridLayout();
            layout.numColumns = 3;
//            layout.marginHeight = convertVerticalDLUsToPixels(IDialogConstants.VERTICAL_MARGIN);
//            layout.marginWidth = convertHorizontalDLUsToPixels(IDialogConstants.HORIZONTAL_MARGIN);
//            layout.verticalSpacing = convertVerticalDLUsToPixels(IDialogConstants.VERTICAL_SPACING);
//            layout.horizontalSpacing = convertHorizontalDLUsToPixels(IDialogConstants.HORIZONTAL_SPACING);
            comp.setLayout(layout);
            comp.setLayoutData(new GridData(GridData.FILL_BOTH));
            
            Label nameLabel = new Label(comp, SWT.LEAD|SWT.HORIZONTAL);
            nameLabel.setText(parameters[i].getName());
            nameLabel.setToolTipText(parameters[i].getControlID());
            
            String controlID = parameters[i].getControlID();
            if (controlID.startsWith("expression-")) { //$NON-NLS-1$
                final Text text = new Text(comp, SWT.SINGLE|SWT.LEFT);
                final int argIndex = i;
                text.addModifyListener(new ModifyListener() {
                    public void modifyText(ModifyEvent e) {
                        args[argIndex] = text.getText();
                        argValid[argIndex] = text.getText().trim().length() > 0;
                        updateButtons();
                    }
                });
                String type = controlID.substring("expression-".length()); //$NON-NLS-1$
                Label hintLabel = new Label(comp,SWT.LEAD|SWT.HORIZONTAL);
                hintLabel.setText("(enter LSL " + type + " expression)"); // //$NON-NLS-1$ //$NON-NLS-2$ TODO
            } else if ("avatar".equals(controlID)) { //$NON-NLS-1$
                createKeyCombo(i, comp, simManager().getSimState().getAvatars());
                Label hintLabel = new Label(comp,SWT.LEAD|SWT.HORIZONTAL);
                hintLabel.setText(""); //$NON-NLS-1$
            } else if ("prim".equals(controlID)) { //$NON-NLS-1$
                createKeyCombo(i, comp, simManager().getSimState().getPrims());
                Label hintLabel = new Label(comp,SWT.LEAD|SWT.HORIZONTAL);
                hintLabel.setText(""); //$NON-NLS-1$
            } else if ("script".equals(controlID)) { //$NON-NLS-1$
                skip = true;
                final int argIndex = i;
                final Combo combo = new Combo(comp, SWT.READ_ONLY|SWT.DROP_DOWN);
                final SimScript[] scripts = simManager().getSimState().getScripts();
                String[] dropDownRepresentation = new String[scripts.length];
                SimPrim[] prims = simManager().getSimState().getPrims();
                HashMap<String,String> pk2name = new HashMap<String,String>();
                for (int pi = 0; pi < prims.length; pi++) pk2name.put(prims[pi].getKey(), prims[pi].getName());
                for (int si = 0; si < scripts.length; si++) {
                    dropDownRepresentation[si] = scripts[i].getPrimKey() + " (" +  //$NON-NLS-1$ TODO
                        pk2name.get(scripts[i].getPrimKey()).toString() + ") / " + scripts[i].getScriptName(); //$NON-NLS-1$ TODO
                }
                combo.setItems(dropDownRepresentation);
                combo.deselectAll();
                combo.addSelectionListener(new SelectionListener() {
                    public void widgetDefaultSelected(SelectionEvent e) {
                    }

                    public void widgetSelected(SelectionEvent e) {
                        if (combo.getSelectionIndex() >= 0) {
                            argValid[argIndex] = true;
                            argValid[argIndex+1] = true;
                            args[argIndex] = scripts[combo.getSelectionIndex()].getScriptName();
                            args[argIndex+1]  = scripts[combo.getSelectionIndex()].getPrimKey();
                        } else {
                            argValid[argIndex] = false;
                            argValid[argIndex+1] = false;
                            args[argIndex] = null;
                            args[argIndex+1] = null;
                        }
                        updateButtons();
                    }
                });
                Label hintLabel = new Label(comp,SWT.LEAD|SWT.HORIZONTAL);
                hintLabel.setText(""); //$NON-NLS-1$
            }
        }
        
        return composite;
    }

    private void createKeyCombo(final int argIndex, Composite parent, NameKeyType[] nameKeyObj) {
        final Combo combo = new Combo(parent, SWT.READ_ONLY|SWT.DROP_DOWN);
        String[] dropdownRepresentation = new String[nameKeyObj.length];
        final String[] itemKey = new String[dropdownRepresentation.length];
        for (int j = 0; j < dropdownRepresentation.length; j++) {
            dropdownRepresentation[j] = nameKeyObj[j].getCombinedRepresentation();
            itemKey[j] = nameKeyObj[j].getKey();
        }
        combo.setItems(dropdownRepresentation);
        combo.deselectAll();
        
        combo.addSelectionListener(new SelectionListener() {
            public void widgetDefaultSelected(SelectionEvent e) {
            }

            public void widgetSelected(SelectionEvent e) {
                if (combo.getSelectionIndex() >= 0) {
                    argValid[argIndex] = true;
                    args[argIndex] = itemKey[combo.getSelectionIndex()];
                } else {
                    argValid[argIndex] = false;
                    args[argIndex] = null;
                }
                updateButtons();
            }
        });
    }

    private SimManager simManager() {
        return LSLForgePlugin.getDefault().getSimManager();
    }

    public SimEvent getEvent() {
        if (this.getReturnCode() != Window.OK) return null;
        SimEventArg[] simEventArgs = new SimEventArg[args.length];
        for (int i = 0; i < simEventArgs.length; i++) {
            simEventArgs[i] = new SimEventArg(desc.getParams()[i].getName(), args[i]);
        }
        return new SimEvent(desc.getName(), 0, simEventArgs);
    }
}
