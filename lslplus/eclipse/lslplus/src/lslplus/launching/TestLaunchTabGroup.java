package lslplus.launching;

import org.eclipse.debug.core.ILaunchConfiguration;
import org.eclipse.debug.core.ILaunchConfigurationWorkingCopy;
import org.eclipse.debug.ui.AbstractLaunchConfigurationTab;
import org.eclipse.debug.ui.AbstractLaunchConfigurationTabGroup;
import org.eclipse.debug.ui.ILaunchConfigurationDialog;
import org.eclipse.debug.ui.ILaunchConfigurationTab;
import org.eclipse.swt.SWT;
import org.eclipse.swt.widgets.Composite;

/**
 * TODO: do we even need this?
 * @author rgreayer
 *
 */
public class TestLaunchTabGroup extends AbstractLaunchConfigurationTabGroup {

	public TestLaunchTabGroup() {
	}

	public void createTabs(ILaunchConfigurationDialog dialog, String mode) {
		AbstractLaunchConfigurationTab tab = new AbstractLaunchConfigurationTab() {
			
			public void createControl(Composite parent) {
				Composite c = new Composite(parent,SWT.NULL);
				c.setVisible(true);
				setControl(c);
			}

			public String getName() {
				return Messages.getString("TestLaunchTabGroup.INFO"); //$NON-NLS-1$
			}

			public void initializeFrom(ILaunchConfiguration configuration) {
			}

			public void performApply(
					ILaunchConfigurationWorkingCopy configuration) {
			}

			public void setDefaults(
					ILaunchConfigurationWorkingCopy configuration) {
			}
			
		};
		
		this.setTabs(new ILaunchConfigurationTab[] { tab });
	}

}
