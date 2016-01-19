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

package uk.ac.ucl.excites.sapelli.collector.io;

import java.io.File;

import uk.ac.ucl.excites.sapelli.collector.model.ProjectDescriptor;
import uk.ac.ucl.excites.sapelli.shared.io.FileStorageException;

/**
 * @author mstevens, Michalis Vitos
 *
 */
public class AndroidFileStorageProvider extends FileStorageProvider
{
	
	/**
	 * @see http://www.makeuseof.com/tag/hide-private-picture-folders-gallery-android
	 */
	static public final String NO_MEDIA_FILE = ".nomedia";

	public AndroidFileStorageProvider(File sapelliFolder, File downloadsFolder)
	{
		super(sapelliFolder, downloadsFolder);
	}

	@Override
	public File getTempFolder(boolean create) throws FileStorageException
	{
		return createNoMediaIn(super.getTempFolder(create));
	}
	
	@Override
	public File getProjectInstallationFolder(ProjectDescriptor projectDescr, boolean create) throws FileStorageException
	{
		return createNoMediaIn(super.getProjectInstallationFolder(projectDescr, create));
	}
	
	@Override
	public File getAttachmentsFolder(boolean create) throws FileStorageException
	{
		return createNoMediaIn(super.getAttachmentsFolder(create));
	}

	private File createNoMediaIn(File folder)
	{
		try
		{
			(new File(folder, NO_MEDIA_FILE)).createNewFile();
		}
		catch(Exception ignore) {}
		return folder;
	}
	
}
