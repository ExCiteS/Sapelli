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
	 * 
	 * @param operation
	 * @throws T Throwable type of the operation
	 */
	public <T extends Throwable> void executeNoDBEx(StoreOperation<S, T> operation) throws T
	{
		try
		{
			execute(operation);
		}
		catch(DBException e)
		{
			e.printStackTrace(System.err); // not re-thrown
		}
	}
	
	/**
	 * Helper method to run operation(s) against the Store without the caller needing to be a StoreUser.
	 * The Store is released after running the operation.
	 * 
	 * @param operation
	 * @throws DBException
	 * @throws T Throwable type of the operation
	 */
	public <T extends Throwable> void execute(StoreOperation<S, T> operation) throws DBException, T
	{
		try
		{
			operation.execute(getStore(operation));
		}
		finally
		{
			doneUsing(operation);
		}
	}
	
	/**
	 * Helper method to run operation(s) against the Store without the caller needing to be a StoreUser.
	 * The Store is released after running the operation.
	 * 
	 * @param operation
	 * @return the object returned by {@link StoreOperationWithReturn#execute(Object)}, or {@code null} in case an exception occurs
	 * @throws T Throwable type of the operation
	 */
	public <R, T extends Throwable> R executeWithReturnNoDBEx(StoreOperationWithReturn<S, R, T> operation) throws T
	{
		try
		{
			return executeWithReturn(operation);
		}
		catch(DBException e)
		{
			e.printStackTrace(System.err); // not re-thrown
			return null;
		}
	}
	
	/**
	 * Helper method to run operation(s) against the Store without the caller needing to be a StoreUser.
	 * The Store is released after running the operation.
	 * 
	 * @param operation
	 * @return the object returned by {@link StoreOperationWithReturn#execute(Object)}
	 * @throws DBException
	 * @throws T Throwable type of the operation
	 */
	public <R, T extends Throwable> R executeWithReturn(StoreOperationWithReturn<S, R, T> operation) throws DBException, T
	{
		try
		{
			return operation.execute(getStore(operation));
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
	 * @param <T> Throwable type
	 */
	static public abstract class StoreOperation<S, T extends Throwable> implements StoreUser
	{
		
		public abstract void execute(final S store) throws T;
		
	}
	
	/**
	 * @author mstevens
	 *
	 * @param <S>
	 */
	static public abstract class StoreOperationNoException<S> extends StoreOperation<S, RuntimeException>
	{
		
		public abstract void execute(final S store);
		
	}
	
	/**
	 * @author mstevens
	 *
	 * @param <S>
	 * @param <R> return type
	 * @param <T> Throwable type
	 */
	static public abstract class StoreOperationWithReturn<S, R, T extends Throwable> implements StoreUser
	{
		
		public abstract R execute(final S store) throws T;
		
	}
	
	/**
	 * @author mstevens
	 *
	 * @param <S>
	 * @param <R> return type
	 */
	static public abstract class StoreOperationWithReturnNoException<S, R> extends StoreOperationWithReturn<S, R, RuntimeException>
	{
		
		public abstract R execute(final S store);
		
	}

}