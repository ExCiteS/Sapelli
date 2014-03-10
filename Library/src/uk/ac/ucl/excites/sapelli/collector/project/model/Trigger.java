/**
 * 
 */
package uk.ac.ucl.excites.sapelli.collector.project.model;

import uk.ac.ucl.excites.sapelli.collector.control.Controller;

/**
 * @author Michalis Vitos
 *
 */
public class Trigger implements JumpSource
{

	// Statics----------------------------------------------
	static public final int NO_TIMEOUT = -1;
	
	// Dynamics---------------------------------------------
	protected String key;
	protected int fixedTimer = NO_TIMEOUT;
	protected Field jump;

	public Trigger()
	{
	}

	/**
	 * @return the key
	 */
	public String getKey()
	{
		return key;
	}

	/**
	 * @param key
	 *            the key to set
	 */
	public void setKey(String key)
	{
		this.key = key;
	}

	/**
	 * @return the fixedTimer
	 */
	public int getFixedTimer()
	{
		return fixedTimer;
	}

	/**
	 * @param fixedTimer
	 *            the fixedTimer to set
	 */
	public void setFixedTimer(int fixedTimer)
	{
		if(fixedTimer != NO_TIMEOUT && fixedTimer < 0)
			throw new IllegalArgumentException("Invalid timer duration: " + fixedTimer);
		this.fixedTimer = fixedTimer;
	}

	/**
	 * @return the jump
	 */
	public Field getJump()
	{
		return jump;
	}

	/* (non-Javadoc)
	 * @see uk.ac.ucl.excites.sapelli.collector.project.model.JumpSource#setJump(uk.ac.ucl.excites.sapelli.collector.project.model.Field)
	 */
	@Override
	public void setJump(Field target)
	{
		this.jump = target;
	}

	public boolean enter(Controller controller)
	{
		return controller.enterTrigger(this);
	}
}
