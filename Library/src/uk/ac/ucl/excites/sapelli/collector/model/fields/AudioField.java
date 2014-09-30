/**
 * Sapelli data collection platform: http://sapelli.org
 * 
 * Copyright 2012-2014 University College London - ExCiteS group
 * 
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 * 
 *     http://www.apache.org/licenses/LICENSE-2.0
 * 
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and 
 * limitations under the License.
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
	private static final String MEDIA_TYPE_3GPP = "MEDIA_TYPE_3GPP";
	private static final String EXTENSION_3GPP = "3gp";

	public AudioField(Form form, String id, boolean multiple, String caption)
	{
		super(form, id, multiple, caption);
		useNativeApp = DEFAULT_USE_NATIVE_APP;
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
	public <V, UI extends CollectorUI<V, UI>> AudioUI<V, UI> createUI(UI collectorUI)
	{
		return collectorUI.createAudioUI(this);
	}

}
