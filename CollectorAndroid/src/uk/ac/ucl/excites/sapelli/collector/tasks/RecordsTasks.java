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

package uk.ac.ucl.excites.sapelli.collector.tasks;

import java.io.File;
import java.util.ArrayList;
import java.util.List;

import uk.ac.ucl.excites.sapelli.collector.CollectorClient;
import uk.ac.ucl.excites.sapelli.collector.R;
import uk.ac.ucl.excites.sapelli.collector.activities.BaseActivity;
import uk.ac.ucl.excites.sapelli.collector.fragments.ExportFragment;
import uk.ac.ucl.excites.sapelli.collector.util.AsyncTaskWithWaitingDialog;
import uk.ac.ucl.excites.sapelli.shared.db.StoreHandle.StoreUser;
import uk.ac.ucl.excites.sapelli.shared.util.CollectionUtils;
import uk.ac.ucl.excites.sapelli.shared.util.ExceptionHelpers;
import uk.ac.ucl.excites.sapelli.storage.db.RecordStore;
import uk.ac.ucl.excites.sapelli.storage.eximport.ExportResult;
import uk.ac.ucl.excites.sapelli.storage.eximport.Exporter;
import uk.ac.ucl.excites.sapelli.storage.eximport.Importer;
import uk.ac.ucl.excites.sapelli.storage.eximport.csv.CSVRecordsExporter;
import uk.ac.ucl.excites.sapelli.storage.eximport.csv.CSVRecordsExporter.Separator;
import uk.ac.ucl.excites.sapelli.storage.eximport.xml.XMLRecordsExporter;
import uk.ac.ucl.excites.sapelli.storage.eximport.xml.XMLRecordsImporter;
import uk.ac.ucl.excites.sapelli.storage.eximport.xml.XMLRecordsExporter.CompositeMode;
import uk.ac.ucl.excites.sapelli.storage.model.Record;
import uk.ac.ucl.excites.sapelli.storage.queries.RecordsQuery;
import android.util.Log;

/**
 * A collection of async tasks to deal with record query, export & delete operations.
 * 
 * @author mstevens
 */
public final class RecordsTasks
{
	
	private RecordsTasks() { /* should never be instantiated */ }

	/**
	 * @author mstevens
	 *
	 * @param <I>
	 * @param <O>
	 */
	static protected abstract class RecordStoreTask<I, O> extends AsyncTaskWithWaitingDialog<I, O> implements StoreUser
	{

		protected final CollectorClient client;
		protected Exception failure = null;
		
		public RecordStoreTask(BaseActivity owner)
		{
			this(owner, null);
		}
		
		public RecordStoreTask(BaseActivity owner, String waitingMsg)
		{
			super(owner, waitingMsg);
			this.client = owner.getCollectorApp().collectorClient;
		}

		@Override
		protected O doInBackground(I... params)
		{
			try
			{
				// Get RecordStore instance:
				RecordStore recordStore = client.recordStoreHandle.getStore(this);
				
				// Do the real work:
				return doInBackgroundWith(recordStore, params);
			}
			catch(Exception e)
			{
				//e.printStackTrace(System.err);
				Log.d(getClass().getSimpleName(), ExceptionHelpers.getMessageAndCause(e));
				failure = e;
				return null;
			}
			finally
			{
				client.recordStoreHandle.doneUsing(this);
			}
		}
		
		protected abstract O doInBackgroundWith(RecordStore recordStore, I... params) throws Exception;
		
	}
	
	static public class QueryTask extends RecordStoreTask<RecordsQuery, List<Record>>
	{

		private final QueryCallback callback;
		
		public QueryTask(BaseActivity owner, QueryCallback callback)
		{
			super(owner, owner.getString(R.string.exportFetching));
			this.callback = callback;
		}

		@Override
		protected List<Record> doInBackgroundWith(RecordStore recordStore, RecordsQuery... query) throws Exception
		{
			return recordStore.retrieveRecords(query[0]); // Retrieve by query
		}
		
		@Override
		protected void onPostExecute(List<Record> result)
		{
			super.onPostExecute(result); // dismiss dialog
			if(failure != null)
				callback.queryFailure(failure);
			else
				callback.querySuccess(result);
		}

	}
	
	public interface QueryCallback
	{
		
		public void querySuccess(List<Record> result);
		
		public void queryFailure(Exception reason);
		
	}
	
	@SuppressWarnings("unchecked")
	static public void runExportTask(List<Record> records, ExportFragment exportFragment, File exportFolder, String exportDesc, ExportCallback callback)
	{
		switch(exportFragment.getSelectedFormat())
		{
			case CSV:
				new CSVExportTask(exportFragment.getOwner(), exportFolder, exportFragment.getCSVSeparator(), exportDesc, callback).execute(records);
				break;
			case XML:
				new RecordsTasks.XMLExportTask(exportFragment.getOwner(), exportFolder, exportFragment.getXMLCompositeMode(), exportDesc, callback).execute(records);
				break;
			default:
				throw new IllegalStateException("Unknown export format: " + exportFragment.getSelectedFormat().toString());
		}
	}
	
	static public class DeleteTask extends RecordStoreTask<List<Record>, List<Record>>
	{

		private final DeleteCallback callback;
		
		public DeleteTask(BaseActivity owner, DeleteCallback callback)
		{
			super(owner);
			this.callback = callback;
		}

		@Override
		protected List<Record> doInBackgroundWith(RecordStore recordStore, List<Record>... params) throws Exception
		{
			// Delete records:
			List<Record> recordsToDelete = params[0];
			publishProgress(context.getString(R.string.deletingXRecords, recordsToDelete.size()));
			recordStore.delete(recordsToDelete);
			return recordsToDelete;
		}
		
		@Override
		protected void onPostExecute(List<Record> result)
		{
			super.onPostExecute(result); // dismiss dialog
			if(callback != null)
			{
				if(failure != null)
					callback.deleteFailure(failure);
				else
					callback.deleteSuccess(result);
			}
		}
		
	}
	
	public interface DeleteCallback
	{
		
		public void deleteSuccess(List<Record> deletedRecords);
		
		public void deleteFailure(Exception reason);
		
	}
	
	static public class StoreTask extends RecordStoreTask<List<Record>, List<Record>>
	{

		private final StoreCallback callback;
		
		public StoreTask(BaseActivity owner, StoreCallback callback)
		{
			super(owner);
			this.callback = callback;
		}

		@Override
		protected List<Record> doInBackgroundWith(RecordStore recordStore, List<Record>... params) throws Exception
		{
			// Delete records:
			List<Record> recordsToStore = params[0];
			publishProgress(context.getString(R.string.storingXRecords, recordsToStore.size()));
			recordStore.store(recordsToStore);
			return recordsToStore;
		}
		
		@Override
		protected void onPostExecute(List<Record> result)
		{
			super.onPostExecute(result); // dismiss dialog
			if(callback != null)
			{
				if(failure != null)
					callback.storeFailure(failure);
				else
					callback.storeSuccess(result);
			}
		}
		
	}
	
	public interface StoreCallback
	{
		
		public void storeSuccess(List<Record> storedRecords);
		
		public void storeFailure(Exception reason);
		
	}
	
	static public class ExportTask extends AsyncTaskWithWaitingDialog<List<Record>, ExportResult>
	{

		private Exporter exporter;
		private String selectionDescr;
		private final ExportCallback callback; 
		
		public ExportTask(BaseActivity owner, Exporter exporter, String selectionDescr, ExportCallback callback)
		{
			super(owner);
			this.exporter = exporter;
			this.selectionDescr = selectionDescr;
			this.callback = callback;
		}

		@Override
		protected ExportResult doInBackground(List<Record>... params)
		{
			List<Record> records = params[0];
			onProgressUpdate(context.getString(R.string.exportXRecords, records.size()));
			return exporter.export(records, selectionDescr);
		}
		
		@Override
		protected void onPostExecute(ExportResult result)
		{
			super.onPostExecute(result); // dismiss dialog
			callback.exportDone(result);
		}
		
	}
	
	static public class XMLExportTask extends ExportTask
	{
		
		public XMLExportTask(BaseActivity owner, File exportFolder, CompositeMode compositeMode, String selectionDescr, ExportCallback callback)
		{
			super(owner, new XMLRecordsExporter(exportFolder, compositeMode), selectionDescr, callback);
		}
		
	}
	
	static public class CSVExportTask extends ExportTask
	{
		
		public CSVExportTask(BaseActivity owner, File exportFolder, Separator separator, String selectionDescr, ExportCallback callback)
		{
			super(owner, new CSVRecordsExporter(exportFolder, separator), selectionDescr, callback);
		}
		
	}
	
	public interface ExportCallback
	{
		
		public void exportDone(ExportResult result);
		
	}
	
	static public class ImportTask extends AsyncTaskWithWaitingDialog<File, List<Record>>
	{

		private Importer importer;
		private Exception failure;
		private final ImportCallback callback;
		
		public ImportTask(BaseActivity owner, Importer importer, ImportCallback callback)
		{
			super(owner);
			this.importer = importer;
			this.callback = callback;
		}

		@Override
		protected List<Record> doInBackground(File... files)
		{
			List<Record> result = new ArrayList<Record>();
			try
			{
				for(File file : files)
				{
					onProgressUpdate(context.getString(R.string.importFromX, file.getAbsolutePath()));
					CollectionUtils.addAllIgnoreNull(result, importer.importFrom(file));
				}
			}
			catch(Exception e)
			{
				failure = e;
				return null;
			}
			return result;
		}
		
		@Override
		protected void onPostExecute(List<Record> result)
		{
			super.onPostExecute(result); // dismiss dialog
			if(failure != null)
				callback.importFailure(failure);
			else
				callback.importSuccess(result, importer.getWarnings());
		}
		
	}
	
	static public class XMLImportTask extends ImportTask
	{
		
		public XMLImportTask(BaseActivity owner, ImportCallback callback)
		{
			super(owner, new XMLRecordsImporter(owner.getCollectorApp().collectorClient), callback);
		}
		
	}
	
	public interface ImportCallback
	{
		
		public void importSuccess(List<Record> result, List<String> warnings);
		
		/**
		 * @param reason see {@link Importer#importFrom(File)} for possible Exception types
		 */
		public void importFailure(Exception reason);
		
	}
	
}
