package uk.ac.ucl.excites.sapelli.collector.ui;

import uk.ac.ucl.excites.sapelli.collector.control.Controller;

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
	
	// Statics-------------------------------------------------------
	static public enum Control
	{
		BACK,
		//UP,
		CANCEL,
		FORWARD
	}
	
	static public enum State
	{
		HIDDEN,
		SHOWN_DISABLED, // "grayed out"
		SHOWN_ENABLED
	}
	
	// Dynamics------------------------------------------------------
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
	
	public abstract void update(FieldUI<?, V, UI> currentFieldUI);
	
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
	 * @param control
	 */
	protected void onControlClick(Control control)
	{
		// Log interaction:
		controller.addLogLine("CONTROL_PRESS_" + control.name(), controller.getCurrentField().getID());
		switch(control)
		{
			case BACK :				
				controller.goBack(true);
				break;
			case CANCEL : 
				controller.cancelAndRestartForm();
				break;
			case FORWARD :
				controller.goForward(true);
				break;
			default : return;
		}
	}
	
	public abstract int getCurrentHeightPx();
	
}
