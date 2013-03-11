/**
 * 
 */
package uk.ac.ucl.excites.collector.project.model;

import uk.ac.ucl.excites.collector.project.ui.CollectorUI;

/**
 * @author Michalis Vitos, mstevens
 * 
 */
public class Audio extends MediaAttachment
{

	public Audio(String id)
	{
		super(id);
	}

	private String startRecImageLogicalPath;
	private String stopRecImageLogicalPath;
	
	/**
	 * @return the startRecImageLogicalPath
	 */
	public String getStartRecImageLogicalPath()
	{
		return startRecImageLogicalPath;
	}

	/**
	 * @param startRecImageLogicalPath the startRecImageLogicalPath to set
	 */
	public void setStartRecImageLogicalPath(String startRecImageLogicalPath)
	{
		this.startRecImageLogicalPath = startRecImageLogicalPath;
	}

	/**
	 * @return the stopRecImageLogicalPath
	 */
	public String getStopRecImageLogicalPath()
	{
		return stopRecImageLogicalPath;
	}

	/**
	 * @param stopRecImageLogicalPath the stopRecImageLogicalPath to set
	 */
	public void setStopRecImageLogicalPath(String stopRecImageLogicalPath)
	{
		this.stopRecImageLogicalPath = stopRecImageLogicalPath;
	}

	@Override
	public void setIn(CollectorUI fv)
	{
		fv.setAudio(this);
	}

}
