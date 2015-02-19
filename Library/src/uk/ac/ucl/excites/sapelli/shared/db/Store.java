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

import uk.ac.ucl.excites.sapelli.shared.db.exceptions.DBException;

/**
 * Abstract class for "stores" (i.e. database access objects)
 * 
 * @author mstevens
 */
public abstract class Store
{
	
	private boolean closed = false;

	/**
	 * Called by GC
	 * 
	 * @see java.lang.Object#finalize()
	 */
	public void finalize()
	{
		try
		{
			close();
		}
		catch(DBException dbE)
		{
			dbE.printStackTrace(System.err);
		}
	}
	
	public void close() throws DBException
	{
		if(!closed)
		{
			doClose();
			closed = true;
		}
	}
	
	/**
	 * Close the Store and any underlying connections/files/resources
	 * 
	 * @throws DBException
	 */
	protected abstract void doClose() throws DBException;

	public boolean isClosed()
	{
		return closed;
	}
	
	public abstract void backup(StoreBackupper backupper, File destinationFolder) throws DBException;
	
}
