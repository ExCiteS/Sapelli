/**
 * Sapelli data collection platform: http://sapelli.org
 * 
 * Copyright 2012-2016 University College London - ExCiteS group
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

package uk.ac.ucl.excites.sapelli.shared.db;

import java.io.File;
import java.util.HashSet;
import java.util.Set;
import java.util.Stack;

import uk.ac.ucl.excites.sapelli.shared.db.StoreHandle.StoreUser;
import uk.ac.ucl.excites.sapelli.shared.db.exceptions.DBException;

/**
 * Helper class to backup Store instance (which may themselves depend on other Store instances)
 * 
 * @author mstevens
 */
public class StoreBackupper implements StoreUser
{
	
	public static void Backup(File destinationFolder, boolean labelFilesAsBackup, Store store) throws DBException
	{
		// Create backupper instance:
		StoreBackupper backupper = new StoreBackupper(destinationFolder, labelFilesAsBackup);
		// Add store for backup:
		backupper.addStoreForBackup(store);
		// Run backup:
		backupper.backup();
	}

	public static void Backup(File destinationFolder, boolean labelFilesAsBackup, StoreHandle<?>... storesHandlesToBackup) throws DBException
	{
		// Check if we were actually passed at least 1 StoreHandle:
		if(storesHandlesToBackup == null || storesHandlesToBackup.length == 0)
			return;
		// Create backupper instance:
		StoreBackupper backupper = new StoreBackupper(destinationFolder, labelFilesAsBackup);
		
		// Add stores for backup:
		for(StoreHandle<?> storeHandle : storesHandlesToBackup)
		{
			Store store = null;
			try
			{
				store = storeHandle.getStore(backupper);
			}
			catch(DBException ignore) {}
			backupper.addStoreForBackup(store);
		}
		// Run backup:
		try
		{
			backupper.backup();
		}
		// Unregister with handles:
		finally
		{
			for(StoreHandle<?> storeHandle : storesHandlesToBackup)
				storeHandle.doneUsing(backupper);
		}
	}
	
	private final File destinationFolder;
	private final boolean labelFilesAsBackup;
	private final Stack<Store> toBackup;
	private final Set<Store> backedUp;
	
	/**
	 * @param destinationFolder
	 * @param labelFilesAsBackup whether or not the files which are created should be labels (in their filename) as backups or not (in which case the name of the original file, if there is one, will be used)
	 */
	private StoreBackupper(File destinationFolder, boolean labelFilesAsBackup)
	{
		this.destinationFolder = destinationFolder;
		this.labelFilesAsBackup = labelFilesAsBackup;
		toBackup = new Stack<Store>();
		backedUp = new HashSet<Store>();
	}

	public void addStoreForBackup(Store store)
	{
		if(store != null && !toBackup.contains(store) && !backedUp.contains(store))
			this.toBackup.push(store);
	}
	
	private void backup() throws DBException
	{
		if(!toBackup.isEmpty())
		{
			// Backup store at the top of the stack:
			Store bStore = toBackup.pop();
			bStore.backup(this, destinationFolder);
			backedUp.add(bStore);
			
			// Recurse (until stack is empty):
			backup();
		}
	}

	/**
	 * @return the labelFilesAsBackup
	 */
	public boolean isLabelFilesAsBackup()
	{
		return labelFilesAsBackup;
	}
	
}
