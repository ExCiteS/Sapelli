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

package uk.ac.ucl.excites.sapelli.storage;

import uk.ac.ucl.excites.sapelli.storage.StorageClient.RecordOperation;
import uk.ac.ucl.excites.sapelli.storage.db.RecordStore;
import uk.ac.ucl.excites.sapelli.storage.model.RecordReference;

/**
 * An interface to be implemented by classes that need to be informed about storage events
 * (represented as {@link RecordOperation}s) produced by a {@link RecordStore}.
 * Registering of such observers happens through {@link StorageClient#addObserver(StorageObserver)}.
 * 
 * @author mstevens
 */
public interface StorageObserver
{
	
	/**
	 * @param operation
	 * @param recordRef
	 * @param recordStore
	 */
	public void storageEvent(RecordOperation operation, RecordReference recordRef, RecordStore recordStore);
	
}