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
	
	static public enum FlashMode
	{
		AUTO,
		ON,
		OFF
	}
	
	static public final boolean DEFAULT_USE_NATIVE_APP = false;
	static public final boolean DEFAULT_USE_FRONT_FACING_CAMERA = false;
	static public final FlashMode DEFAULT_FLASH_MODE = FlashMode.AUTO;
	
	//DYNAMICS-------------------------------------------------------
	private boolean useNativeApp;
	private boolean useFrontFacingCamera;
	private FlashMode flashMode;
	
	private String captureButtonImageLogicalPath;
	private String approveButtonImageLogicalPath;
	private String discardButtonImageLogicalPath;
	
	public PhotoField(Form form, String id)
	{
		super(form, id);
		useNativeApp = DEFAULT_USE_NATIVE_APP;
		useFrontFacingCamera = DEFAULT_USE_FRONT_FACING_CAMERA;
		flashMode = DEFAULT_FLASH_MODE;
	}

	@Override
	public void setIn(CollectorUI fv)
	{
		fv.setPhoto(this);
	}

	/**
	 * @return the useFrontFacingCamera
	 */
	public boolean isUseFrontFacingCamera()
	{
		return useFrontFacingCamera;
	}

	/**
	 * @param useFrontFacingCamera the useFrontFacingCamera to set
	 */
	public void setUseFrontFacingCamera(boolean useFrontFacingCamera)
	{
		this.useFrontFacingCamera = useFrontFacingCamera;
	}

	/**
	 * @return the flashMode
	 */
	public FlashMode getFlashMode()
	{
		return flashMode;
	}

	/**
	 * @param flashMode the flashMode to set
	 */
	public void setFlashMode(FlashMode flashMode)
	{
		this.flashMode = flashMode;
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
