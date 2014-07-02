/**
 * 
 */
package uk.ac.ucl.excites.sapelli.collector.model.fields;

import java.io.File;
import java.util.ArrayList;
import java.util.List;

import uk.ac.ucl.excites.sapelli.collector.model.Form;
import uk.ac.ucl.excites.sapelli.collector.model.Project;
import uk.ac.ucl.excites.sapelli.collector.ui.CollectorUI;
import uk.ac.ucl.excites.sapelli.collector.ui.fields.AudioUI;
import uk.ac.ucl.excites.sapelli.shared.util.CollectionUtils;

/**
 * @author Michalis Vitos, mstevens
 * 
 */
public class AudioField extends MediaField
{

	static public final boolean DEFAULT_USE_NATIVE_APP = false;
	static private final String MEDIA_TYPE_3GPP = "MEDIA_TYPE_3GPP";
	static private final String EXTENSION_3GPP = "3gp";

	private String startRecImageRelativePath;
	private String stopRecImageRelativePath;
	
	public AudioField(Form form, String id, String caption)
	{
		super(form, id, caption);
		useNativeApp = DEFAULT_USE_NATIVE_APP;
	}
	
	/**
	 * @return the startRecImageRelativePath
	 */
	public String getStartRecImageRelativePath()
	{
		return startRecImageRelativePath;
	}

	/**
	 * @param startRecImageRelativePath the startRecImageRelativePath to set
	 */
	public void setStartRecImageRelativePath(String startRecImageRelativePath)
	{
		this.startRecImageRelativePath = startRecImageRelativePath;
	}

	/**
	 * @return the stopRecImageRelativePath
	 */
	public String getStopRecImageRelativePath()
	{
		return stopRecImageRelativePath;
	}

	/**
	 * @param stopRecImageRelativePath the stopRecImageRelativePath to set
	 */
	public void setStopRecImageRelativePath(String stopRecImageRelativePath)
	{
		this.stopRecImageRelativePath = stopRecImageRelativePath;
	}

	@Override
	public String getMediaType()
	{
		return MEDIA_TYPE_3GPP; //TODO support for other types
	}

	@Override
	protected String getFileExtension(String mediaType)
	{
		if(mediaType == MEDIA_TYPE_3GPP)
			return EXTENSION_3GPP;
		//else if //...
		else
			return EXTENSION_3GPP; //or the default
	}
	
	@Override
	public List<File> getFiles(Project project)
	{
		List<File> paths = new ArrayList<File>();
		CollectionUtils.addIgnoreNull(paths, project.getImageFile(startRecImageRelativePath));
		CollectionUtils.addIgnoreNull(paths, project.getImageFile(stopRecImageRelativePath));
		return paths;
	}

	@Override
	public <V, UI extends CollectorUI<V, UI>> AudioUI<V, UI> createUI(UI collectorUI)
	{
		return collectorUI.createAudioUI(this);
	}
	
	@Override
	public boolean equals(Object obj)
	{
		if(this == obj)
			return true; // references to same object
		if(obj instanceof AudioField)
		{
			AudioField that = (AudioField) obj;
			return	super.equals(that) && // MediaField#equals(Object)
					(this.startRecImageRelativePath != null ? this.startRecImageRelativePath.equals(that.startRecImageRelativePath) : that.startRecImageRelativePath == null) &&
					(this.stopRecImageRelativePath != null ? this.stopRecImageRelativePath.equals(that.stopRecImageRelativePath) : that.stopRecImageRelativePath == null);
		}
		else
			return false;
	}
	
	@Override
	public int hashCode()
	{
		int hash = super.hashCode(); // MediaField#hashCode()
		hash = 31 * hash + (startRecImageRelativePath == null ? 0 : startRecImageRelativePath.hashCode());
		hash = 31 * hash + (stopRecImageRelativePath == null ? 0 : stopRecImageRelativePath.hashCode());
		return hash;
	}

}
