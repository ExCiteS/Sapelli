package uk.ac.ucl.excites.sapelli.collector.model.fields;

import java.io.File;
import java.util.ArrayList;
import java.util.List;

import uk.ac.ucl.excites.sapelli.collector.io.FileStorageProvider;
import uk.ac.ucl.excites.sapelli.collector.model.Field;
import uk.ac.ucl.excites.sapelli.collector.model.Form;
import uk.ac.ucl.excites.sapelli.collector.ui.CollectorUI;
import uk.ac.ucl.excites.sapelli.collector.ui.fields.FieldUI;
import uk.ac.ucl.excites.sapelli.shared.util.CollectionUtils;

/**
 * 
 * @author benelliott
 *
 */
public class DrawingField extends MediaField
{
	static private final String MEDIA_TYPE_JPEG = "DRAWING_JPEG";
	static private final String EXTENSION_JPEG = "jpg";
	
	private String captureButtonImageRelativePath;

	public DrawingField(Form form, String id, String caption)
	{
		super(form, id, caption);
		// TODO
		
	}

	@Override
	public <V, UI extends CollectorUI<V, UI>> FieldUI<? extends Field, V, UI> createUI(UI collectorUI)
	{
		return collectorUI.createDrawingUI(this);
	}
	
	@Override
	public String getMediaType()
	{
		return MEDIA_TYPE_JPEG;
	}

	@Override
	protected String getFileExtension(String mediaType)
	{
		return EXTENSION_JPEG;
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
	
	@Override
	public List<File> getFiles(FileStorageProvider fileStorageProvider)
	{
		List<File> paths = new ArrayList<File>();
		CollectionUtils.addIgnoreNull(paths, fileStorageProvider.getProjectImageFile(form.project, captureButtonImageRelativePath));
		CollectionUtils.addIgnoreNull(paths, fileStorageProvider.getProjectImageFile(form.project, discardButtonImageRelativePath));
		return paths;
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
					(this.discardButtonImageRelativePath != null ? this.discardButtonImageRelativePath.equals(that.discardButtonImageRelativePath) : that.discardButtonImageRelativePath == null);
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
		return hash;
	}

}
