/**
 * 
 */
package uk.ac.ucl.excites.collector.project.model;

import uk.ac.ucl.excites.collector.project.ui.CollectorUI;

/**
 * @author Michalis Vitos, mstevens
 * 
 */
public class AudioField extends MediaField
{

	private static final String MEDIA_TYPE_3GPP = "MEDIA_TYPE_3GPP";
	private static final String EXTENSION_3GPP = "3gp";

	public AudioField(Form form, String id)
	{
		super(form, id);
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

	@Override
	public String getMediaType()
	{
		return MEDIA_TYPE_3GPP; //TODO support for other types
	}

	@Override
	public String getFileExtension(String mediaType)
	{
		if(mediaType == MEDIA_TYPE_3GPP)
			return EXTENSION_3GPP;
		//else if //...
		else
			return EXTENSION_3GPP; //or the default
	}
	
}
