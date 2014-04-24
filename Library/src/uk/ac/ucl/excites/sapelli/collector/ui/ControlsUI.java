package uk.ac.ucl.excites.sapelli.collector.ui;

import uk.ac.ucl.excites.sapelli.collector.control.Controller;
import uk.ac.ucl.excites.sapelli.collector.model.Form;

/**
 * Abstract class to represent the controls UI (i.e. back/cancel/fwd buttons, maybe others later)
 * 
 * @author mstevens
 *
 * @param <V>
 * @param <UI>
 */
public abstract class ControlsUI<V, UI extends CollectorUI<V, UI>>
{
	
	static public final int BUTTON_TYPE_BACK = -1;
	static public final int BUTTON_TYPE_CANCEL = 0;
	static public final int BUTTON_TYPE_FORWARD = 1;
	
	protected Controller controller;
	protected UI collectorUI;
	protected boolean enabled;
	
	public ControlsUI(Controller controller, UI collectorUI)
	{
		this.controller = controller;
		this.collectorUI = collectorUI;
		this.enabled = true;
	}
	
	/**
	 * @return a platform-specific UI element (e.g. an Android View instance).
	 */
	protected abstract V getPlatformView();
	
	public abstract void update(Form currentForm, FieldUI<?, V, UI> currentFieldUI);
	
	public void disable()
	{
		enabled = false;
	}
	
	public void enable()
	{
		enabled = true;
	}
	
	public boolean isEnabled()
	{
		return enabled;
	}
	
	/**
	 * It is assumed that this method is only called when enabled=true
	 * 
	 * @param button
	 */
	protected void onButtonClick(int button)
	{
		switch(button)
		{
			// TODO when having switched to enum for button types we can make this code more compact (just having the addLogLine line once using buttontype.toString() instead of the hard coded Strings)
			case BUTTON_TYPE_BACK :
				controller.addLogLine("BACK_BUTTON", controller.getCurrentField().getID());
				controller.goBack(true);
				break;
			case BUTTON_TYPE_CANCEL : 
				controller.addLogLine("CANCEL_BUTTON", controller.getCurrentField().getID());
				controller.cancelAndRestartForm();
				break;
			case BUTTON_TYPE_FORWARD :
				controller.addLogLine("FORWARD_BUTTON", controller.getCurrentField().getID());
				controller.goForward(true);
				break;
			default : return;
		}
	}
	
	public abstract int getCurrentHeightPx();
	
}
