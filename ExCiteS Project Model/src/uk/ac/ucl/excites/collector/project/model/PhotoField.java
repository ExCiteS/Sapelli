/**
 * 
 */
package uk.ac.ucl.excites.collector.project.model;

import uk.ac.ucl.excites.collector.project.ui.CollectorUI;

/**
 * @author Michalis Vitos, mstevens
 *
 */
public class PhotoField extends MediaField
{

	//STATICS--------------------------------------------------------
	static private final String MEDIA_TYPE_JPEG = "PHOTO_JPEG";
	static private final String EXTENSION_JPEG = "jpg";
	
	static public final int CAMERA_BACK = 0;
	static public final int CAMERA_FRONT = 1;
	
	static public final int DEFAULT_CAMERA = CAMERA_BACK;
	static public final boolean DEFAULT_USE_NATIVE_APP = false;
	
	//DYNAMICS-------------------------------------------------------
	private int camera;
	private boolean useNativeApp;

	private String captureButtonImageLogicalPath;
	private String approveButtonImageLogicalPath;
	private String discardButtonImageLogicalPath;
	
	public PhotoField(Form form, String id)
	{
		super(form, id);
		camera = DEFAULT_CAMERA;
		useNativeApp = DEFAULT_USE_NATIVE_APP;
	}

	@Override
	public void setIn(CollectorUI fv)
	{
		fv.setPhoto(this);
	}

	/**
	 * @return the camera
	 */
	public int getCamera()
	{
		return camera;
	}

	/**
	 * @param camera the camera to set
	 */
	public void setCamera(String camera)
	{
		if("back".equalsIgnoreCase(camera))
			this.camera = CAMERA_BACK;
		else if("front".equalsIgnoreCase(camera))
			this.camera = CAMERA_FRONT;
		else
			this.camera = DEFAULT_CAMERA;
	}

	/**
	 * @return the useNativeApp
	 */
	public boolean isUseNativeApp()
	{
		return useNativeApp;
	}

	/**
	 * @param useNativeApp the useNativeApp to set
	 */
	public void setUseNativeApp(boolean useNativeApp)
	{
		this.useNativeApp = useNativeApp;
	}

	/**
	 * @return the captureButtonImageLogicalPath
	 */
	public String getCaptureButtonImageLogicalPath()
	{
		return captureButtonImageLogicalPath;
	}

	/**
	 * @param captureButtonImageLogicalPath the captureButtonImageLogicalPath to set
	 */
	public void setCaptureButtonImageLogicalPath(String captureButtonImageLogicalPath)
	{
		this.captureButtonImageLogicalPath = captureButtonImageLogicalPath;
	}

	/**
	 * @return the approveButtonImageLogicalPath
	 */
	public String getApproveButtonImageLogicalPath()
	{
		return approveButtonImageLogicalPath;
	}

	/**
	 * @param approveButtonImageLogicalPath the approveButtonImageLogicalPath to set
	 */
	public void setApproveButtonImageLogicalPath(String approveButtonImageLogicalPath)
	{
		this.approveButtonImageLogicalPath = approveButtonImageLogicalPath;
	}

	/**
	 * @return the discardButtonImageLogicalPath
	 */
	public String getDiscardButtonImageLogicalPath()
	{
		return discardButtonImageLogicalPath;
	}

	/**
	 * @param discardButtonImageLogicalPath the discardButtonImageLogicalPath to set
	 */
	public void setDiscardButtonImageLogicalPath(String discardButtonImageLogicalPath)
	{
		this.discardButtonImageLogicalPath = discardButtonImageLogicalPath;
	}

	@Override
	public String getMediaType()
	{
		return MEDIA_TYPE_JPEG;
	}

	@Override
	public String getFileExtension(String mediaType)
	{
		return EXTENSION_JPEG;
	}
	
}
