package uk.ac.ucl.excites.sapelli.collector.model.fields;

import java.io.File;
import java.util.Set;

import uk.ac.ucl.excites.sapelli.collector.io.FileStorageProvider;
import uk.ac.ucl.excites.sapelli.collector.model.Field;
import uk.ac.ucl.excites.sapelli.collector.model.Form;
import uk.ac.ucl.excites.sapelli.collector.ui.CollectorUI;
import uk.ac.ucl.excites.sapelli.collector.ui.fields.FieldUI;
import uk.ac.ucl.excites.sapelli.shared.util.CollectionUtils;

/**
 * A Field that allows the capture of "drawings" that the user produces through some medium (e.g. a touchscreen). The produced captures are handled in a similar way to photos.
 * @author benelliott
 *
 */
public class DrawingField extends MediaField
{
	static private final String MEDIA_TYPE_PNG = "DRAWING_PNG";
	static private final String EXTENSION_PNG = "png";
	public static final String DEFAULT_CANVAS_COLOR = "#BABABA"; // light grey
	public static final String DEFAULT_STROKE_COLOR = "#000000"; // black
	public static final float DEFAULT_STROKE_WIDTH = 20f;

	private String canvasColor = DEFAULT_CANVAS_COLOR;
	private String strokeColor = DEFAULT_STROKE_COLOR;
	private float strokeWidth = DEFAULT_STROKE_WIDTH;
	private String captureButtonImageRelativePath;
	private String addDrawingImageRelativePath;
	private String canvasImageRelativePath;

	public DrawingField(Form form, String id, String caption)
	{
		super(form, id, caption);		
	}

	@Override
	public <V, UI extends CollectorUI<V, UI>> FieldUI<? extends Field, V, UI> createUI(UI collectorUI)
	{
		return collectorUI.createDrawingUI(this);
	}
	
	@Override
	public String getMediaType()
	{
		return MEDIA_TYPE_PNG;
	}

	@Override
	protected String getFileExtension(String mediaType)
	{
		return EXTENSION_PNG;
	}

	/**
	 * @return the captureButtonImageRelativePath
	 */
	public String getCaptureButtonImageRelativePath()
	{
		return captureButtonImageRelativePath;
	}

	/**
	 * @param captureButtonImageRelativePath the captureButtonImageRelativePath to set
	 */
	public void setCaptureButtonImageRelativePath(String captureButtonImageRelativePath)
	{
		this.captureButtonImageRelativePath = captureButtonImageRelativePath;
	}
	
	/**
	 * @return the canvasImageRelativePath
	 */
	public String getCanvasImageRelativePath()
	{
		return canvasImageRelativePath;
	}

	/**
	 * @param canvasImageRelativePath the canvasImageRelativePath to set
	 */
	public void setCanvasImageRelativePath(String canvasImageRelativePath)
	{
		this.canvasImageRelativePath = canvasImageRelativePath;
	}

	/**
	 * @return the addDrawingImageRelativePath
	 */
	public String getAddDrawingImageRelativePath()
	{
		return addDrawingImageRelativePath;
	}

	/**
	 * @param addDrawingImageRelativePath the addDrawingImageRelativePath to set
	 */
	public void setAddDrawingImageRelativePath(String addDrawingImageRelativePath)
	{
		this.addDrawingImageRelativePath = addDrawingImageRelativePath;
	}
	
	@Override
	public void addFiles(Set<File> filesSet, FileStorageProvider fileStorageProvider)
	{
		super.addFiles(filesSet, fileStorageProvider); // !!!
		CollectionUtils.addIgnoreNull(filesSet, fileStorageProvider.getProjectImageFile(form.project, captureButtonImageRelativePath));
		CollectionUtils.addIgnoreNull(filesSet, fileStorageProvider.getProjectImageFile(form.project, discardButtonImageRelativePath));
		CollectionUtils.addIgnoreNull(filesSet, fileStorageProvider.getProjectImageFile(form.project, addDrawingImageRelativePath));
		CollectionUtils.addIgnoreNull(filesSet, fileStorageProvider.getProjectImageFile(form.project, canvasImageRelativePath));
	}
	
	/**
	 * @return the canvasColor
	 */
	public String getCanvasColor()
	{
		return canvasColor;
	}

	/**
	 * @param canvasColor the canvasColor to set
	 */
	public void setCanvasColor(String canvasColor)
	{
		this.canvasColor = canvasColor;
	}

	/**
	 * @return the strokeColor
	 */
	public String getStrokeColor()
	{
		return strokeColor;
	}

	/**
	 * @param strokeColor the strokeColor to set
	 */
	public void setStrokeColor(String strokeColor)
	{
		this.strokeColor = strokeColor;
	}

	/**
	 * @return the strokeWidth
	 */
	public float getStrokeWidth()
	{
		return strokeWidth;
	}

	/**
	 * @param strokeWidth the strokeWidth to set
	 */
	public void setStrokeWidth(float strokeWidth)
	{
		this.strokeWidth = strokeWidth;
	}

	@Override
	public boolean equals(Object obj)
	{
		if(this == obj)
			return true; // references to same object
		if(obj instanceof DrawingField)
		{
			DrawingField that = (DrawingField) obj;
			return	super.equals(that) && // MediaField#equals(Object)
					(this.captureButtonImageRelativePath != null ? this.captureButtonImageRelativePath.equals(that.captureButtonImageRelativePath) : that.captureButtonImageRelativePath == null) &&
					(this.discardButtonImageRelativePath != null ? this.discardButtonImageRelativePath.equals(that.discardButtonImageRelativePath) : that.discardButtonImageRelativePath == null) &&
					(this.canvasImageRelativePath != null ? this.canvasImageRelativePath.equals(that.canvasImageRelativePath) : that.canvasImageRelativePath == null) &&
					(this.addDrawingImageRelativePath != null ? this.addDrawingImageRelativePath.equals(that.addDrawingImageRelativePath) : that.addDrawingImageRelativePath == null) &&
					this.canvasColor == that.canvasColor &&
					this.strokeColor == that.strokeColor &&
					this.strokeWidth == that.strokeWidth;
		}
		else
			return false;
	}
	
	@Override
	public int hashCode()
	{
		int hash = super.hashCode(); // MediaField#hashCode()
		hash = 31 * hash + (captureButtonImageRelativePath == null ? 0 : captureButtonImageRelativePath.hashCode());
		hash = 31 * hash + (discardButtonImageRelativePath == null ? 0 : discardButtonImageRelativePath.hashCode());
		hash = 31 * hash + (canvasImageRelativePath == null ? 0 : canvasImageRelativePath.hashCode());	
		hash = 31 * hash + (addDrawingImageRelativePath == null ? 0 : addDrawingImageRelativePath.hashCode());	
		hash = 31 * hash + canvasColor.hashCode();
		hash = 31 * hash + strokeColor.hashCode();
		hash = 31 * hash + (int)strokeWidth;
		return hash;
	}

}
