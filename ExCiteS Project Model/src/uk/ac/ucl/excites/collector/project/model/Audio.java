/**
 * 
 */
package uk.ac.ucl.excites.collector.project.model;

/**
 * @author Michalis Vitos
 * 
 */
public class Audio extends MediaAttachment
{

	public Audio(String id)
	{
		super(id);
	}

	private String pathRecordingImage;
	private String pathStopImage;

	/**
	 * @return the pathRecordingImage
	 */
	public String getPathRecordingImage()
	{
		return pathRecordingImage;
	}

	/**
	 * @param pathRecordingImage
	 *            the pathRecordingImage to set
	 */
	public void setPathRecordingImage(String pathRecordingImage)
	{
		this.pathRecordingImage = pathRecordingImage;
	}

	/**
	 * @return the pathStopImage
	 */
	public String getPathStopImage()
	{
		return pathStopImage;
	}

	/**
	 * @param pathStopImage
	 *            the pathStopImage to set
	 */
	public void setPathStopImage(String pathStopImage)
	{
		this.pathStopImage = pathStopImage;
	}

}
