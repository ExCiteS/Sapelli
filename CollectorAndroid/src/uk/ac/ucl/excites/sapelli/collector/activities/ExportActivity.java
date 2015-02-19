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

package uk.ac.ucl.excites.sapelli.collector.activities;

import java.util.HashSet;
import java.util.List;
import java.util.Set;

import org.joda.time.DateTime;

import uk.ac.ucl.excites.sapelli.collector.R;
import uk.ac.ucl.excites.sapelli.collector.fragments.ExportFragment;
import uk.ac.ucl.excites.sapelli.collector.model.Form;
import uk.ac.ucl.excites.sapelli.collector.model.Project;
import uk.ac.ucl.excites.sapelli.collector.util.AsyncTaskWithWaitingDialog;
import uk.ac.ucl.excites.sapelli.shared.util.ExceptionHelpers;
import uk.ac.ucl.excites.sapelli.storage.eximport.ExportResult;
import uk.ac.ucl.excites.sapelli.storage.eximport.Exporter;
import uk.ac.ucl.excites.sapelli.storage.model.Record;
import uk.ac.ucl.excites.sapelli.storage.model.Schema;
import uk.ac.ucl.excites.sapelli.storage.queries.Order;
import uk.ac.ucl.excites.sapelli.storage.queries.RecordsQuery;
import uk.ac.ucl.excites.sapelli.storage.queries.Source;
import uk.ac.ucl.excites.sapelli.storage.queries.constraints.AndConstraint;
import uk.ac.ucl.excites.sapelli.storage.queries.constraints.RuleConstraint;
import uk.ac.ucl.excites.sapelli.storage.types.TimeStamp;
import android.util.Log;

/**
 * Activty that handles the export of Sapelli Collector records
 * 
 *  !! IMPORTANT !!! don't move Async Tasks to ExportFragment to avoid state loss on detach
 * 
 * @author mstevens, Julia
 */
public class ExportActivity extends ProjectActivity {

	// Dynamics--------------------------------------------
	private Exporter exporter;
	private String selectionDesc;
	private DateTime[] dateRange = new DateTime[2];
	private Boolean exportAll;
	private Project selectedProject;

	public void export(Exporter exporter, String selectionDesc, DateTime[] dateRange, Project selectedProject, Boolean exportAll) {
		this.exporter = exporter;
		this.selectionDesc = selectionDesc;
		this.dateRange = dateRange;
		this.selectedProject = selectedProject;
		this.exportAll = exportAll;
		new QueryTask().execute();
	}

	private void queryCallback(List<Record> result) {
		if (result.isEmpty())
			showOKDialog(getString(R.string.title_activity_export), getString(R.string.exportNoRecordsFound));
		else {
			//	Export!:
			new ExportTask(result, exporter, selectionDesc).execute(); // TODO: check whether they are not null
		}
	}
	
	private void queryCallback(Exception queryFailure)
	{
		showErrorDialog(getString(R.string.exportQueryFailed, ExceptionHelpers.getMessageAndCause(queryFailure)), true);
	}
	
	private void exportCallback(final ExportResult result)
	{
		if(result == null)
			return; // just in case (shouldn't happen)
		
		// Runnable to delete exported records:
		Runnable deleteTask = new Runnable()
		{
			@Override
			public void run()
			{
				new DeleteTask(result.getExportedRecords()).execute();
			}
		};
		
		// Deal with result:

		if (result.wasSuccessful()) {

			if (result.getNumberedOfExportedRecords() > 0)
				// show dialog, OK will run deleteTask:
				showYesNoDialog(R.string.exportSuccessTitle, getString(R.string.exportSuccessMsg, result.getNumberedOfExportedRecords(), result.getDestination()) + '\n' + getString(R.string.exportDeleteConfirm), false, deleteTask, false);

			else
				// show dialog:
				showOKDialog(R.string.exportSuccessTitle, getString(R.string.exportNothing), false);
		} else {
			if (result.getNumberedOfExportedRecords() > 0)
				// show dialog, OK will run deleteTask:
				showYesNoDialog(R.string.exportPartialSuccessTitle, getString(R.string.exportPartialSuccessMsg, result.getNumberedOfExportedRecords(), result.getDestination(), result.getNumberOfUnexportedRecords(), ExceptionHelpers.getMessageAndCause(result.getFailureReason())) + '\n' + getString(R.string.exportDeleteConfirm), false, deleteTask, false);
			else
				// show dialog:
				showOKDialog(R.string.exportFailureTitle, getString(R.string.exportFailureMsg, result.getDestination(), ExceptionHelpers.getMessageAndCause(result.getFailureReason())), false);
		}

	}

	/**
	 * @param failure
	 *            exception that caused record deletion to fail (may be null)
	 */
	private void deleteCallback(Exception failure) {
		if (failure != null)
			showOKDialog(R.string.exportFailureTitle, getString(R.string.exportDeleteFailureMsg, ExceptionHelpers.getMessageAndCause(failure)), false);

	}
	
	private class QueryTask extends AsyncTaskWithWaitingDialog<Void, List<Record>>
	{

		private Exception failure = null;
		
		public QueryTask()
		{
			super(ExportActivity.this, getString(R.string.exportFetching));
		}

		@Override
		protected List<Record> doInBackground(Void... params)
		{
			try
			{
				// Schemas (when list stays empty all records of any schema/project/form will be fetched):
				Set<Schema> schemata = new HashSet<Schema>();
				if(selectedProject != null && !exportAll)
					schemata.addAll(project.getModel().getSchemata());
				// Date range:
				AndConstraint constraints = new AndConstraint();
				if(dateRange[ExportFragment.DT_RANGE_IDX_FROM] != null)
					constraints.addConstraint(new RuleConstraint(Form.COLUMN_TIMESTAMP_START, RuleConstraint.Comparison.GREATER_OR_EQUAL, new TimeStamp(dateRange[ExportFragment.DT_RANGE_IDX_FROM])));
				if(dateRange[ExportFragment.DT_RANGE_IDX_TO] != null)
					constraints.addConstraint(new RuleConstraint(Form.COLUMN_TIMESTAMP_START, RuleConstraint.Comparison.SMALLER_OR_EQUAL, new TimeStamp(dateRange[ExportFragment.DT_RANGE_IDX_TO])));
				// TODO Exclude previously exported:
				// Retrieve by query:
				return recordStore.retrieveRecords(new RecordsQuery(Source.From(schemata), Order.UNDEFINED, constraints)); // TODO order by form, deviceid, timestamp
			}
			catch(Exception e)
			{
				e.printStackTrace(System.err);
				Log.d("QueryTask", ExceptionHelpers.getMessageAndCause(e));
				failure = e;
				return null;
			}
		}
		
		@Override
		protected void onPostExecute(List<Record> result)
		{
			super.onPostExecute(result); // dismiss dialog
			if(failure != null)
				queryCallback(failure);
			else
				queryCallback(result);
		}
		
	}
	
	private class ExportTask extends AsyncTaskWithWaitingDialog<Void, ExportResult>
	{

		private List<Record> records;
		private Exporter exporter;
		private String selectionDescr;
		
		public ExportTask(List<Record> records, Exporter exporter, String selectionDescr)
		{
			super(ExportActivity.this, getString(R.string.exportXRecords, records.size()));
			this.records = records;
			this.exporter = exporter;
			this.selectionDescr = selectionDescr;
		}

		@Override
		protected ExportResult doInBackground(Void... params)
		{
			return exporter.export(records, selectionDescr);
		}
		
		@Override
		protected void onPostExecute(ExportResult result)
		{
			super.onPostExecute(result); // dismiss dialog
			exportCallback(result);
		}
		
	}
	
	private class DeleteTask extends AsyncTaskWithWaitingDialog<Void, Void>
	{

		private List<Record> records;
		private Exception failure = null;
		
		public DeleteTask(List<Record> recordsToDelete)
		{
			super(ExportActivity.this, getString(R.string.exportDeletingX, recordsToDelete.size()));
			this.records = recordsToDelete;
		}

		@Override
		protected Void doInBackground(Void... params)
		{
			try
			{
				recordStore.delete(records);
			}
			catch(Exception e)
			{
				e.printStackTrace(System.err);
				Log.d("DeleteTask", ExceptionHelpers.getMessageAndCause(e));
				failure = e;
			}
			return null;
		}
		
		@Override
		protected void onPostExecute(Void result)
		{
			super.onPostExecute(result); // dismiss dialog
			deleteCallback(failure);
		}
		
	}

}
