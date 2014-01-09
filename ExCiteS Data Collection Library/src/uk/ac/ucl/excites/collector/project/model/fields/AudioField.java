/**
 * 
 */
package uk.ac.ucl.excites.collector.project.model.fields;

import java.io.File;
import java.util.ArrayList;
import java.util.List;

import uk.ac.ucl.excites.collector.project.model.Form;
import uk.ac.ucl.excites.collector.project.model.Project;
import uk.ac.ucl.excites.collector.project.ui.CollectorUI;
import uk.ac.ucl.excites.collector.project.ui.Controller;
import uk.ac.ucl.excites.collector.project.ui.FieldUI;
import uk.ac.ucl.excites.util.CollectionUtils;

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

	private String startRecImageRelativePath;
	private String stopRecImageRelativePath;
	
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
	public boolean enter(Controller controller)
	{
		return controller.enterAudioField(this);
	}

	@Override
	public FieldUI createUI(CollectorUI collectorUI)
	{
		return collectorUI.createAudioUI(this);
	}

}
