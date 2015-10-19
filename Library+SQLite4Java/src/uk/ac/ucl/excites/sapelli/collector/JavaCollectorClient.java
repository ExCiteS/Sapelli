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
import java.util.List;

import uk.ac.ucl.excites.sapelli.collector.db.CollectorSQLRecordStoreUpgrader;
import uk.ac.ucl.excites.sapelli.collector.db.ProjectRecordStore;
import uk.ac.ucl.excites.sapelli.collector.db.ProjectStore;
import uk.ac.ucl.excites.sapelli.collector.io.FileStorageProvider;
import uk.ac.ucl.excites.sapelli.shared.db.StoreHandle.StoreSetter;
import uk.ac.ucl.excites.sapelli.shared.db.exceptions.DBException;
import uk.ac.ucl.excites.sapelli.storage.db.RecordStore;
import uk.ac.ucl.excites.sapelli.storage.db.sql.SQLRecordStoreUpgrader;
import uk.ac.ucl.excites.sapelli.storage.db.sql.sqlite.java.JavaSQLiteRecordStore;

/**
 * @author mstevens
 *
 */
public class JavaCollectorClient extends CollectorClient implements SQLRecordStoreUpgrader.UpgradeCallback
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
	
	@Override
	protected void createAndSetRecordStore(StoreSetter<RecordStore> setter) throws DBException
	{
		setter.setAndInitialise(new JavaSQLiteRecordStore(this, fileStorageProvider.getDBFolder(true), DATABASE_BASENAME, CURRENT_COLLECTOR_RECORDSTORE_VERSION, new CollectorSQLRecordStoreUpgrader(this, this, fileStorageProvider)));
	}

	@Override
	protected void createAndSetProjectStore(StoreSetter<ProjectStore> setter) throws DBException
	{
		setter.setAndInitialise(new ProjectRecordStore(this, fileStorageProvider));
	}

	/**
	 * @return the fileStorageProvider
	 */
	public FileStorageProvider getFileStorageProvider()
	{
		return fileStorageProvider;
	}

	@Override
	public void upgradePerformed(int fromVersion, int toVersion, List<String> warnings)
	{
		System.out.println("Database upgraded from v" + fromVersion + " to v" + toVersion + ".");
		if(!warnings.isEmpty())
		{
			System.out.println("Warnings:");
			for(String warning : warnings)
				System.out.println(" - " + warning);
		}
	}

	@Override
	public void logError(String msg, Throwable throwable)
	{
		System.err.println("ERROR@" + getClass().getSimpleName() + ":" + msg);
		if(throwable != null)
			throwable.printStackTrace(System.err);
	}

	@Override
	public void logWarning(String msg)
	{
		System.out.println("WARNING@" + getClass().getSimpleName() + ":" + msg);
	}

	@Override
	public void logInfo(String msg)
	{
		System.out.println("INFO@" + getClass().getSimpleName() + ":" + msg);
	}

}
