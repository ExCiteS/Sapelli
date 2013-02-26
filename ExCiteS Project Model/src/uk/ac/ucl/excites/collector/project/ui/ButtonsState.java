/**
 * 
 */
package uk.ac.ucl.excites.collector.project.ui;

/**
 * @author mstevens
 *
 */
public class ButtonsState
{
	
	private boolean backShown;
	private boolean cancelShown;
	private boolean forwardShown;
	
	private String backImagePath;
	private String cancelImagePath;
	private String forwardImagePath;
	
	private String backgroundColor;
	
	/**
	 * @param backShown
	 * @param cancelShown
	 * @param forwardShown
	 */
	public ButtonsState(boolean backShown, boolean cancelShown, boolean forwardShown)
	{
		this.backShown = backShown;
		this.cancelShown = cancelShown;
		this.forwardShown = forwardShown;
	}

	public int getNumberOfButtonsShown()
	{
		return (backShown ? 1 : 0) + (cancelShown ? 1 : 0) + (forwardShown ? 1 : 0);
	}

	/**
	 * @return the backShown
	 */
	public boolean isBackShown()
	{
		return backShown;
	}

	/**
	 * @return the cancelShown
	 */
	public boolean isCancelShown()
	{
		return cancelShown;
	}

	/**
	 * @return the forwardShown
	 */
	public boolean isForwardShown()
	{
		return forwardShown;
	}

	/**
	 * @return the backImagePath
	 */
	public String getBackImagePath()
	{
		return backImagePath;
	}

	/**
	 * @param backImagePath the backImagePath to set
	 */
	public void setBackImagePath(String backImagePath)
	{
		this.backImagePath = backImagePath;
	}

	/**
	 * @return the cancelImagePath
	 */
	public String getCancelImagePath()
	{
		return cancelImagePath;
	}

	/**
	 * @param cancelImagePath the cancelImagePath to set
	 */
	public void setCancelImagePath(String cancelImagePath)
	{
		this.cancelImagePath = cancelImagePath;
	}

	/**
	 * @return the forwardImagePath
	 */
	public String getForwardImagePath()
	{
		return forwardImagePath;
	}

	/**
	 * @param forwardImagePath the forwardImagePath to set
	 */
	public void setForwardImagePath(String forwardImagePath)
	{
		this.forwardImagePath = forwardImagePath;
	}
	
	/**
	 * @return the backgroundColor
	 */
	public String getBackgroundColor()
	{
		return backgroundColor;
	}

	/**
	 * @param backgroundColor the backgroundColor to set
	 */
	public void setBackgroundColor(String backgroundColor)
	{
		this.backgroundColor = backgroundColor;
	}

	@Override
	public boolean equals(Object o)
	{
		if(!(o instanceof ButtonsState))
			return false;
		ButtonsState another = (ButtonsState) o;
		return backShown == another.backShown && cancelShown == another.cancelShown && forwardShown == another.forwardShown;
	}
	
}
