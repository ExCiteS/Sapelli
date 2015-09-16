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

import java.lang.ref.WeakReference;
import java.util.HashSet;
import java.util.Set;

import uk.ac.ucl.excites.sapelli.shared.db.exceptions.DBException;
import uk.ac.ucl.excites.sapelli.shared.util.ExceptionHandle;

/**
 * 
 * 
 * @author mstevens
 */
public class StoreHandle<S extends Store>
{

	private final StoreCreator<S> storeCreator;
	private WeakReference<S> storeRef;
	private Set<Integer> users;
	
	public StoreHandle(StoreCreator<S> storeCreator)
	{
		if(storeCreator == null)
			throw new NullPointerException("StoreCreator cannot be null");
		this.storeCreator = storeCreator;
	}
	
	private S getStoreFromWeakRef()
	{
		return storeRef != null ? storeRef.get() : null; 
	}
	
	/**
	 * @param user
	 * @return
	 * @throws DBException
	 */
	public S getStore(StoreUser user) throws DBException
	{
		// Get or create Store object:
		S store = getStoreFromWeakRef();
		if(store == null || store.isClosed())
		{
			store = storeCreator.createStore();
			storeRef = new WeakReference<S>(store);
		}
		
		// Register user:
		if(users == null)
			users = new HashSet<Integer>();
		users.add(System.identityHashCode(user)); // add to set of users currently using the store

		// Return store:
		return store;
	}
	
	/**
	 * Helper method to run operation(s) against the Store without the caller needing to be a StoreUser.
	 * The Store is released after running the operation.
	 * Any exception occurring upon getting the Store or executing the operation will be passed to the given {@link ExceptionHandle} (if not null).
	 * 
	 * @param operation
	 * @param exceptionHandle (may be null, but then there is no way to access inner exceptions)
	 */
	public void execute(StoreOperation<S> operation, ExceptionHandle exceptionHandle)
	{
		try
		{
			operation.execute(getStore(operation));
		}
		catch(Exception e)
		{
			if(exceptionHandle != null)
				exceptionHandle.setError(e);
			else
				e.printStackTrace(System.err);
		}
		finally
		{
			doneUsing(operation);
		}
	}
	
	/**
	 * Helper method to run operation(s) against the Store without the caller needing to be a StoreUser.
	 * The Store is released after running the operation.
	 * Any exception occurring upon getting the Store or executing the operation will be passed to the given {@link ExceptionHandle} (if not null).
	 * 
	 * @param operation
	 * @param exceptionHandle (may be null, but then there is no way to access inner exceptions)
	 * @return the object returned by {@link StoreOperationWithReturn#execute(Object)}, or null in case an exception occurs
	 */
	public <R> R executeWithReturn(StoreOperationWithReturn<S, R> operation, ExceptionHandle exceptionHandle)
	{
		try
		{
			return operation.execute(getStore(operation));
		}
		catch(Exception e)
		{
			if(exceptionHandle != null)
				exceptionHandle.setError(e);
			else
				e.printStackTrace(System.err);
			return null;
		}
		finally
		{
			doneUsing(operation);
		}
	}
	
	/**
	 * Called by a Store user to signal that will no longer use the Store
	 * 
	 * @param user
	 */
	public void doneUsing(StoreUser user)
	{
		// Just in case...
		if(users == null)
			return;
		
		// Remove client for this store:
		users.remove(System.identityHashCode(user));
		
		// Finalise if no longer used by other clients:
		if(users.isEmpty())
		{
			try
			{
				S store = getStoreFromWeakRef();
				if(store != null)
					store.close();
			}
			catch(DBException dbE)
			{
				dbE.printStackTrace(System.err);
			}
		}
	}
	
	/**
	 * @author mstevens
	 *
	 * @param <S>
	 */
	public interface StoreCreator<S>
	{
		
		public S createStore() throws DBException;
		
	}
	
	/**
	 * To be implemented by classes that will request access to a Store managed by a StoreHandle.
	 * The implicit contract is that the StoreUser class will call {@link StoreHandle#doneUsing(StoreUser)} when it is done using the Store object.
	 * 
	 * @author mstevens
	 */
	public interface StoreUser
	{
		
	}
	
	/**
	 * @author mstevens
	 *
	 * @param <S>
	 */
	static public abstract class StoreOperation<S> implements StoreUser
	{
		
		public abstract void execute(final S store) throws Exception;
		
	}
	
	/**
	 * @author mstevens
	 *
	 * @param <S>
	 * @parem <R> return type
	 */
	static public abstract class StoreOperationWithReturn<S, R> implements StoreUser
	{
		
		public abstract R execute(final S store) throws Exception;
		
	}

}