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

import java.io.Closeable;
import java.io.File;

import uk.ac.ucl.excites.sapelli.shared.db.exceptions.DBException;

/**
 * Abstract class for "stores" (i.e. database access objects)
 * 
 * @author mstevens
 */
public abstract class Store implements Closeable
{
	
	private boolean initialising = false;
	private boolean initialised = false;
	private boolean closed = false;
	
	public final void initialise() throws DBException
	{
		if(initialised)
			return;
		initialising = true;
		try
		{
			doInitialise();
		}
		finally
		{
			initialising = false;
		}
		// Only if no exception was thrown:
		initialised = true;
	}
	
	protected void doInitialise() throws DBException
	{
		// does nothing by default
	}
	
	/**
	 * @return the initialising
	 */
	protected final boolean isInitialising()
	{
		return initialising;
	}
	
	/**
	 * @return the initialised
	 */
	public final boolean isInitialised()
	{
		return initialised;
	}

	/**
	 * Called by GC
	 * 
	 * @see java.lang.Object#finalize()
	 */
	public final void finalize()
	{
		close();
	}
	
	public final void close()
	{
		if(!closed)
		{
			try
			{
				doClose();
			}
			catch(Exception e)
			{
				e.printStackTrace(System.err);
			}
			closed = true;
		}
	}
	
	/**
	 * Close the Store and any underlying connections/files/resources
	 * 
	 * @throws DBException
	 */
	protected abstract void doClose() throws DBException;

	public final boolean isClosed()
	{
		return closed;
	}
	
	public abstract void backup(StoreBackupper backupper, File destinationFolder) throws DBException;
	
}
