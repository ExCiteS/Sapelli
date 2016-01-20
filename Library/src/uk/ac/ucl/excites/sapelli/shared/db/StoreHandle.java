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
import uk.ac.ucl.excites.sapelli.shared.util.Console;

/**
 * 
 * 
 * @author mstevens
 */
public class StoreHandle<S extends Store>
{
	
	private final Console console;

	private final StoreCreator<S> storeCreator;
	
	/**
	 * Only used during getStore()
	 */
	private S storeStrongRef;
	
	private WeakReference<S> storeWeakRef;
	
	private final Set<Integer> users = new HashSet<Integer>();
	
	public StoreHandle(Console console, StoreCreator<S> storeCreator)
	{
		if(console == null)
			throw new NullPointerException("Console cannot be null");
		this.console = console;
		if(storeCreator == null)
			throw new NullPointerException("StoreCreator cannot be null");
		this.storeCreator = storeCreator;
	}
	
	private S getStoreFromWeakRef()
	{
		return (storeWeakRef != null ? storeWeakRef.get() : null);
	}
	
	/**
	 * Returns the held or a newly created Store instance.
	 * 
	 * When a new instance must be created {@link StoreCreator#createAndSetStore(StoreSetter)} is used,
	 * which in turns calls {@link StoreSetter#setAndInitialise(Store)} on the given StoreSetter. The
	 * Store instance is initialised from {@link StoreSetter#setAndInitialise(Store)}, but only after
	 * a temporary strong reference to the Store has been created. This store reference is used to answer
	 * calls to getStore() that happen *during* the initialisation(/upgrade) process. 
	 * 
	 * @param user
	 * @return
	 * @throws DBException
	 */
	public S getStore(StoreUser user) throws DBException
	{
		boolean hadStrongRef = false;
		
		if(storeStrongRef != null)
			// Remember that we had a strong ref already (this means we are handling 2nd call to getStore() before an earlier one has completed):
			hadStrongRef = true;
		else
			// Get store from weak ref:		
			storeStrongRef = getStoreFromWeakRef();
		
		// If there is no Store instance or it is closed:
		if(storeStrongRef == null || storeStrongRef.isClosed())
		{
			// Forget about all users:
			users.clear();
			
			// Try creating & initialising the store:
			try
			{
				storeCreator.createAndSetStore(new StoreSetter<S>() // throws DBException if creation fails
				{
					@Override
					public S setAndInitialise(S store) throws DBException
					{
						// First set a strong reference to the store (to be able to respond to secondary calls to getStore() *during* initialisation):
						storeStrongRef = store;
						// Initialise (& possibly upgrade) the store:
						store.initialise(); // may throw DBException
						// Return store:
						return store;
					}
				});
			}
			catch(DBException dbE)
			{
				// Clear store reference:
				storeStrongRef = null;
				// Forget about all users (a user may have registered during initialisation)
				users.clear();
				// Re-throw:
				throw dbE;
			}
		}
				
		// Register user:
		users.add(System.identityHashCode(user)); // add to set of users currently using the store

		// Hold on to Store:
		S holdStore = storeStrongRef;
		
		// Wipe strong ref, unless we already had it when this method was called:
		if(!hadStrongRef)
			storeStrongRef = null;
		
		// Set the weak reference:
		storeWeakRef = new WeakReference<S>(holdStore);
		
		// Return store:
		return holdStore;
	}
	
	/**
	 * Helper method to run operation(s) against the Store without the caller needing to be a StoreUser.
	 * The Store is released after running the operation.
	 * 
	 * @param operation
	 */
	public <T extends Throwable> void executeNoEx(StoreOperation<S, T> operation)
	{
		try
		{
			execute(operation);
		}
		catch(Throwable t)
		{
			console.logError("StoreOperation execution error", t); // not re-thrown
		}
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
			console.logError("StoreOperation execution error", e); // not re-thrown
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
	 */
	public <R, T extends Throwable> R executeWithReturnNoEx(StoreOperationWithReturn<S, R, T> operation)
	{
		try
		{
			return executeWithReturn(operation);
		}
		catch(Throwable e)
		{
			console.logError("StoreOperation execution error", e); // not re-thrown
			return null;
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
			console.logError("StoreOperation execution error", e); // not re-thrown
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
		// Remove client for this store:
		users.remove(System.identityHashCode(user));
		
		// Finalise if no longer used by other clients...
		if(users.isEmpty() && storeStrongRef == null) // ...and there is no strong reference
		{
			S store = getStoreFromWeakRef();
			if(store != null)
			{
				try
				{
					store.close();
				}
				catch(Exception e)
				{
					e.printStackTrace(System.err);
				}
			}
		}
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
	public interface StoreSetter<S extends Store>
	{
		
		public S setAndInitialise(S store) throws DBException;
		
	}
	
	/**
	 * @author mstevens
	 *
	 * @param <S>
	 */
	public interface StoreCreator<S extends Store>
	{
		
		public void createAndSetStore(StoreSetter<S> setter) throws DBException;
		
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