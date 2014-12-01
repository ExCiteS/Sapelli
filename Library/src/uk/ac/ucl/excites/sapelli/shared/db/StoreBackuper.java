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

package uk.ac.ucl.excites.sapelli.shared.db;

import java.io.File;
import java.util.HashSet;
import java.util.Set;
import java.util.Stack;

import uk.ac.ucl.excites.sapelli.shared.db.exceptions.DBException;

/**
 * Helper class to backup Store instance (which may themselves depend on other Store instances)
 * 
 * @author mstevens
 */
public class StoreBackuper
{

	private final boolean labelFilesAsBackup;
	private final Stack<Store> toBackup;
	private final Set<Store> backedUp;
	
	/**
	 * @param labelFilesAsBackup whether or not the files which are created should be labels (in their filename) as backups or not (in which case the name of the original file, if there is one, will be used)
	 * @param storesToBackup
	 */
	public StoreBackuper(boolean labelFilesAsBackup, Store... storesToBackup)
	{
		this.labelFilesAsBackup = labelFilesAsBackup;
		toBackup = new Stack<Store>();
		backedUp = new HashSet<Store>();
		if(storesToBackup != null)
			for(Store store : storesToBackup)
				if(!toBackup.contains(store))
					toBackup.push(store);
	}
	
	public void addStoreForBackup(Store store)
	{
		if(!toBackup.contains(store) && !backedUp.contains(store))
			this.toBackup.push(store);
	}
	
	public void backup(File destinationFolder) throws DBException
	{
		if(!toBackup.isEmpty())
		{
			// Backup store at the top of the stack:
			Store bStore = toBackup.pop();
			bStore.backup(this, destinationFolder);
			backedUp.add(bStore);
			
			// Recurse (until stack is empty):
			backup(destinationFolder);
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
