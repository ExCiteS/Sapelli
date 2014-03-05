/**
 * 
 */
package uk.ac.ucl.excites.sapelli.collector.project.ui;


/**
 * State of the control buttons
 * 
 * @author mstevens
 */
public class ControlsState
{
	
	static public enum Control
	{
		BACK,
		UP,
		CANCEL,
		FORWARD
	}
	
	static public enum State
	{
		HIDDEN,
		SHOWN_DISABLED, // "grayed out"
		SHOWN_ENABLED
	}
	
	private State[] controlStates;

	/**
	 * For backwards compatibility until we change the Controller
	 * 
	 * @param backShown
	 * @param cancelShown
	 * @param forwardShown
	 */
	public ControlsState(boolean backShown, boolean cancelShown, boolean forwardShown)
	{
		this(	backShown? State.SHOWN_ENABLED : State.HIDDEN,
				State.HIDDEN,
				cancelShown? State.SHOWN_ENABLED : State.HIDDEN,
				forwardShown? State.SHOWN_ENABLED : State.HIDDEN);
	}
	
	/**
	 * @param back
	 * @param up
	 * @param cancel
	 * @param forward
	 */
	public ControlsState(State back, State up, State cancel, State forward)
	{
		controlStates = new State[Control.values().length];
		controlStates[Control.BACK.ordinal()] = back;
		controlStates[Control.UP.ordinal()] = up;
		controlStates[Control.CANCEL.ordinal()] = cancel;
		controlStates[Control.FORWARD.ordinal()] = forward;
	}

	public int getNumberOfButtonsShown()
	{
		int count = 0;
		for(State s : controlStates)
			if(s != State.HIDDEN)
				count++;
		return count;
	}
	
	public boolean isAnyButtonShown()
	{
		for(State s : controlStates)
			if(s != State.HIDDEN)
				return true;
		return false;
	}
	
	/**
	 * @return the back
	 */
	public State getBack()
	{
		return controlStates[Control.BACK.ordinal()];
	}

	/**
	 * @return the up
	 */
	public State getUp()
	{
		return controlStates[Control.UP.ordinal()];
	}

	/**
	 * @return the cancel
	 */
	public State getCancel()
	{
		return controlStates[Control.CANCEL.ordinal()];
	}

	/**
	 * @return the forward
	 */
	public State getForward()
	{
		return controlStates[Control.FORWARD.ordinal()];
	}

	/**
	 * @return the controlStates
	 */
	public State getState(Control control)
	{
		return controlStates[control.ordinal()];
	}

	@Override
	public boolean equals(Object o)
	{
		if(this == o)
			return true;
		if(!(o instanceof ControlsState))
			return false;
		ControlsState another = (ControlsState) o;
		for(int c = 0; c < controlStates.length; c++)
			if(this.controlStates[c] != another.controlStates[c])
				return false;
		return true;
	}
	
}
