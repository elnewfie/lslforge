package lslforge.wizards;



import org.eclipse.ui.INewWizard;

public class NewSimEventHandler extends NewFileSampleWizard implements INewWizard {
	public NewSimEventHandler() {
	    super("New Sim Event Handler","samples/DefaultEventHandler.lslm");  //$NON-NLS-1$//$NON-NLS-2$ TODO
		this.setDefaultPageImageDescriptor(image());
	}
}