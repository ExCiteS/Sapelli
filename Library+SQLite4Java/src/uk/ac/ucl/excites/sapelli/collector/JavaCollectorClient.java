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

package uk.ac.ucl.excites.sapelli.collector;

import java.io.File;

import uk.ac.ucl.excites.sapelli.collector.db.CollectorRecordStoreUpgrader;
import uk.ac.ucl.excites.sapelli.collector.db.ProjectRecordStore;
import uk.ac.ucl.excites.sapelli.collector.db.ProjectStore;
import uk.ac.ucl.excites.sapelli.collector.io.FileStorageProvider;
import uk.ac.ucl.excites.sapelli.shared.db.exceptions.DBException;
import uk.ac.ucl.excites.sapelli.storage.db.RecordStore;
import uk.ac.ucl.excites.sapelli.storage.db.sql.sqlite.java.JavaSQLiteRecordStore;

/**
 * @author mstevens
 *
 */
public class JavaCollectorClient extends CollectorClient
{
	
	static private final String DATABASE_BASENAME = "Sapelli";
	
	private final FileStorageProvider fileStorageProvider;
	
	/**
	 * @param sapelliFolder
	 * @param downloadsFolder
	 */
	public JavaCollectorClient(File sapelliFolder, File downloadsFolder)
	{
		this.fileStorageProvider = new FileStorageProvider(sapelliFolder, downloadsFolder);
	}

	/* (non-Javadoc)
	 * @see uk.ac.ucl.excites.sapelli.storage.StorageClient#createRecordStore()
	 */
	@Override
	protected RecordStore createRecordStore() throws DBException
	{
		return new JavaSQLiteRecordStore(this, fileStorageProvider.getDBFolder(true), DATABASE_BASENAME, CURRENT_COLLECTOR_RECORDSTORE_VERSION, new CollectorRecordStoreUpgrader(this));
	}

	/* (non-Javadoc)
	 * @see uk.ac.ucl.excites.sapelli.collector.CollectorClient#createProjectStore()
	 */
	@Override
	protected ProjectStore createProjectStore() throws DBException
	{
		return new ProjectRecordStore(this, fileStorageProvider);
	}

	/**
	 * @return the fileStorageProvider
	 */
	public FileStorageProvider getFileStorageProvider()
	{
		return fileStorageProvider;
	}

}
