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
import java.util.List;

import uk.ac.ucl.excites.sapelli.collector.CollectorClient;
import uk.ac.ucl.excites.sapelli.collector.R;
import uk.ac.ucl.excites.sapelli.collector.activities.BaseActivity;
import uk.ac.ucl.excites.sapelli.collector.util.AsyncTaskWithWaitingDialog;
import uk.ac.ucl.excites.sapelli.shared.db.StoreHandle.StoreUser;
import uk.ac.ucl.excites.sapelli.shared.util.ExceptionHelpers;
import uk.ac.ucl.excites.sapelli.storage.db.RecordStore;
import uk.ac.ucl.excites.sapelli.storage.eximport.ExportResult;
import uk.ac.ucl.excites.sapelli.storage.eximport.Exporter;
import uk.ac.ucl.excites.sapelli.storage.eximport.csv.CSVRecordsExporter;
import uk.ac.ucl.excites.sapelli.storage.eximport.csv.CSVRecordsExporter.Separator;
import uk.ac.ucl.excites.sapelli.storage.eximport.xml.XMLRecordsExporter;
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

	static public class QueryTask extends AsyncTaskWithWaitingDialog<RecordsQuery, List<Record>> implements StoreUser
	{

		private final CollectorClient client;
		private final QueryCallback callback;
		private Exception failure = null;
		
		public QueryTask(BaseActivity owner, QueryCallback callback)
		{
			super(owner, owner.getString(R.string.exportFetching));
			this.client = owner.getCollectorApp().collectorClient;
			this.callback = callback;
		}

		@Override
		protected List<Record> doInBackground(RecordsQuery... query)
		{
			try
			{
				// Get RecordStore instance:
				RecordStore recordStore = client.recordStoreHandle.getStore(this);
				
				// Retrieve by query:
				return recordStore.retrieveRecords(query[0]);
			}
			catch(Exception e)
			{
				e.printStackTrace(System.err);
				Log.d("QueryTask", ExceptionHelpers.getMessageAndCause(e));
				failure = e;
				return null;
			}
			finally
			{
				client.recordStoreHandle.doneUsing(this);
			}
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
	
	static public class Delete extends AsyncTaskWithWaitingDialog<List<Record>, Void> implements StoreUser
	{

		private final CollectorClient client;
		private final DeleteCallback callback;
		private Exception failure = null;
		
		public Delete(BaseActivity owner, DeleteCallback callback)
		{
			super(owner);
			this.client = owner.getCollectorApp().collectorClient;
			this.callback = callback;
		}

		@Override
		protected Void doInBackground(List<Record>... params)
		{
			List<Record> recordsToDelete = params[0];
			try
			{
				// Get RecordStore instance:
				RecordStore recordStore = client.recordStoreHandle.getStore(this);
				
				// Delete records:
				publishProgress(context.getString(R.string.exportDeletingX, recordsToDelete.size()));
				recordStore.delete(recordsToDelete);
			}
			catch(Exception e)
			{
				e.printStackTrace(System.err);
				Log.d("DeleteTask", ExceptionHelpers.getMessageAndCause(e));
				failure = e;
			}
			finally
			{
				client.recordStoreHandle.doneUsing(this);
			}
			return null;
		}
		
		@Override
		protected void onPostExecute(Void result)
		{
			super.onPostExecute(result); // dismiss dialog
			callback.deleteFailure(failure);
		}
		
	}
	
	public interface DeleteCallback
	{
		
		public void deleteFailure(Exception reason);
		
	}
	
}
