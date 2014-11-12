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

import java.io.File;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

import org.joda.time.DateTime;
import org.joda.time.format.DateTimeFormat;
import org.joda.time.format.DateTimeFormatter;

import uk.ac.ucl.excites.sapelli.collector.R;
import uk.ac.ucl.excites.sapelli.collector.model.Form;
import uk.ac.ucl.excites.sapelli.collector.util.AsyncTaskWithWaitingDialog;
import uk.ac.ucl.excites.sapelli.shared.util.ExceptionHelpers;
import uk.ac.ucl.excites.sapelli.shared.util.StringUtils;
import uk.ac.ucl.excites.sapelli.storage.eximport.ExportResult;
import uk.ac.ucl.excites.sapelli.storage.eximport.Exporter;
import uk.ac.ucl.excites.sapelli.storage.eximport.Exporter.Format;
import uk.ac.ucl.excites.sapelli.storage.eximport.csv.CSVRecordsExporter;
import uk.ac.ucl.excites.sapelli.storage.eximport.csv.CSVRecordsExporter.Separator;
import uk.ac.ucl.excites.sapelli.storage.eximport.xml.XMLRecordsExporter;
import uk.ac.ucl.excites.sapelli.storage.eximport.xml.XMLRecordsExporter.CompositeMode;
import uk.ac.ucl.excites.sapelli.storage.model.Record;
import uk.ac.ucl.excites.sapelli.storage.model.Schema;
import uk.ac.ucl.excites.sapelli.storage.queries.Order;
import uk.ac.ucl.excites.sapelli.storage.queries.RecordsQuery;
import uk.ac.ucl.excites.sapelli.storage.queries.Source;
import uk.ac.ucl.excites.sapelli.storage.queries.constraints.AndConstraint;
import uk.ac.ucl.excites.sapelli.storage.queries.constraints.RuleConstraint;
import uk.ac.ucl.excites.sapelli.storage.types.TimeStamp;
import android.annotation.SuppressLint;
import android.app.AlertDialog;
import android.app.AlertDialog.Builder;
import android.app.Dialog;
import android.content.DialogInterface;
import android.os.Bundle;
import android.util.Log;
import android.view.View;
import android.view.View.OnClickListener;
import android.widget.AdapterView;
import android.widget.AdapterView.OnItemSelectedListener;
import android.widget.ArrayAdapter;
import android.widget.Button;
import android.widget.DatePicker;
import android.widget.LinearLayout;
import android.widget.RadioButton;
import android.widget.Spinner;
import android.widget.TimePicker;

/**
 * Activity that deals with exporting of Sapelli Collector records to various output formats
 * 
 * @author mstevens, Michalis Vitos
 */
public class ExportActivity extends ProjectActivity implements OnClickListener
{
	
	// Statics---------------------------------------------
	static private final int DT_RANGE_IDX_FROM = 0;
	static private final int DT_RANGE_IDX_TO = 1;
	static private final DateTimeFormatter dateTimeFormatter = DateTimeFormat.forPattern("yyyy-MM-dd' 'HH:mm");
	
	// Dynamics--------------------------------------------
	private DateTime[] dateRange = new DateTime[2];
	private File exportFolder;
	
	// UI Elements
	private RadioButton radioSelectedProject;
	private RadioButton radioAllProjects;

	private Button btnFrom;
	private Button btnTo;
	private Button btnDestination;
	
	private Spinner spinOutputFormat;
	private Spinner spinXMLMode;
	private Spinner spinCSVSeparator;
	
	@Override
	protected void onCreate(Bundle savedInstanceState)
	{
		super.onCreate(savedInstanceState); // sets app, projectStore & recordStore members!
		setContentView(R.layout.activity_export);
		
		this.loadProject(false); // loads project specified by intent (if one was selected)
		
		// UI elements...
		radioSelectedProject = (RadioButton) findViewById(R.id.radioExportSelectedProj);
		radioAllProjects = (RadioButton) findViewById(R.id.radioExportAllProj);
		if(project != null)
		{
			radioSelectedProject.setText(getString(R.string.exportSelectedProj, project));
			radioSelectedProject.setChecked(true);
		}
		else
		{
			radioSelectedProject.setVisibility(View.GONE);
			radioAllProjects.setChecked(true);
		}
		
		// Time range:
		btnFrom = (Button) findViewById(R.id.btnExportFromDate);
		btnFrom.setOnClickListener(this);
		btnTo = (Button) findViewById(R.id.btnExportToDate);
		btnTo.setOnClickListener(this);
		updateDateRange(DT_RANGE_IDX_FROM, null);
		updateDateRange(DT_RANGE_IDX_TO, null);
		
		// Output destination:
		btnDestination = (Button) findViewById(R.id.btnDestination);
		btnDestination.setEnabled(false); // TODO make export path configurable (for now it is not)
		
		// Output formats:
		spinOutputFormat = (Spinner) findViewById(R.id.spinExportFormat);
		final ArrayAdapter<Format> formatAdapter = new ArrayAdapter<Format>(this, android.R.layout.simple_spinner_item, Format.values());
		formatAdapter.setDropDownViewResource(android.R.layout.simple_spinner_dropdown_item);
		spinOutputFormat.setAdapter(formatAdapter);
		final LinearLayout xmlOptions = (LinearLayout) findViewById(R.id.layoutXMLOptions);
		final LinearLayout csvOptions = (LinearLayout) findViewById(R.id.layoutCSVOptions);
		spinOutputFormat.setOnItemSelectedListener(new OnItemSelectedListener()
		{
			@Override
			public void onItemSelected(AdapterView<?> parent, View view, int position, long id)
			{
				boolean xml = formatAdapter.getItem(position) == Format.XML; //while there are only 2 formats we don't need a switch/case 
				xmlOptions.setVisibility(xml ? View.VISIBLE : View.GONE);
				csvOptions.setVisibility(xml ? View.GONE : View.VISIBLE);
			}

			@Override
			public void onNothingSelected(AdapterView<?> parent) { /* ignore */ }
		});
		
		// XML options:
		spinXMLMode = (Spinner) findViewById(R.id.spinXMLMode);
		ArrayAdapter<CompositeMode> xmlModeAdapter = new ArrayAdapter<CompositeMode>(this, android.R.layout.simple_spinner_item, CompositeMode.values());
		xmlModeAdapter.setDropDownViewResource(android.R.layout.simple_spinner_dropdown_item);
		spinXMLMode.setAdapter(xmlModeAdapter);
		spinXMLMode.setSelection(xmlModeAdapter.getPosition(XMLRecordsExporter.DEFAULT_COMPOSITE_MODE));
		
		// CSV options:
		spinCSVSeparator = (Spinner) findViewById(R.id.spinCSVSeparator);
		ArrayAdapter<Separator> csvModeAdapter = new ArrayAdapter<Separator>(this, android.R.layout.simple_spinner_item, Separator.values());
		csvModeAdapter.setDropDownViewResource(android.R.layout.simple_spinner_dropdown_item);
		spinCSVSeparator.setAdapter(csvModeAdapter);
		spinCSVSeparator.setSelection(csvModeAdapter.getPosition(CSVRecordsExporter.DEFAULT_SEPARATOR));
		
		//	OK & Cancel buttons:
		((Button) findViewById(R.id.btnExportOK)).setOnClickListener(this);
		((Button) findViewById(R.id.btnExportCancel)).setOnClickListener(this);
	}
	
	@Override
	public void onResume()
	{
		super.onResume();

		// All storage operations should happen in onResume, otherwise the storage will be unassesible

		// Export path:
		exportFolder = fileStorageProvider.getExportFolder(true);

		// Set UI:
		btnDestination.setText(exportFolder.getAbsolutePath());
	};

	@Override
	public void onClick(View v)
	{
		switch(v.getId())
		{
			case R.id.btnExportFromDate :
				setDateRange(DT_RANGE_IDX_FROM);
				break;
			case R.id.btnExportToDate :
				setDateRange(DT_RANGE_IDX_TO);
				break;
			case R.id.btnExportOK :
				new QueryTask().execute();
				break;
			case R.id.btnExportCancel :
				this.finish();
				break;
		}
	}
	
	@SuppressLint("InflateParams")
	private void setDateRange(final int dtRangeIdx)
	{
		// Init current date time to show in dialog:
		DateTime current = dateRange[dtRangeIdx];
		if(current == null)
			switch(dtRangeIdx)
			{
				case DT_RANGE_IDX_FROM :
					// default "from" time is today at 00:00:00:
					DateTime now = DateTime.now();
					current = new DateTime(now.getYear(), now.getMonthOfYear(), now.getDayOfMonth(), 0, 0);
					break;
				case DT_RANGE_IDX_TO :
					// default "to" time is *now*:
					current = DateTime.now();
					break;
			}
		
		// UI elements:
		View view = getLayoutInflater().inflate(R.layout.dialog_datetime, null);
		final DatePicker datePicker = (DatePicker) view.findViewById(R.id.DTdatePicker);
		datePicker.updateDate(current.getYear(), current.getMonthOfYear(), current.getDayOfMonth());
		final TimePicker timePicker = (TimePicker) view.findViewById(R.id.DTtimePicker);
		timePicker.setIs24HourView(true);
		timePicker.setCurrentHour(current.getHourOfDay());
		timePicker.setCurrentMinute(current.getMinuteOfHour());
		
		// Create the dialog
		AlertDialog.Builder builder = new Builder(this);
		// Set the title:
		builder.setTitle(getString(dtRangeIdx == DT_RANGE_IDX_FROM ? R.string.exportDateRangeFrom : R.string.exportDateRangeTo, '\u2026')) 
		// Set UI:
		.setView(view)
		// Set the buttons:
		//	OK:
		.setPositiveButton(android.R.string.ok, new Dialog.OnClickListener()
		{
			@Override
			public void onClick(DialogInterface dialog, int which)
			{
				updateDateRange(dtRangeIdx,
								new DateTime(	datePicker.getYear(),
												datePicker.getMonth(),
												datePicker.getDayOfMonth(),
												timePicker.getCurrentHour(),
												timePicker.getCurrentMinute(),
												0,
												0));
			}
		})
		//	Cancel:
		.setNegativeButton(android.R.string.cancel, new Dialog.OnClickListener()
		{
			@Override
			public void onClick(DialogInterface dialog, int which)
			{
				dialog.dismiss();
			}
		})
		//	Any time:
		.setNeutralButton(StringUtils.capitalizeFirstLetter(getString(R.string.exportDateRangeAnytime)), new Dialog.OnClickListener()
		{
			@Override
			public void onClick(DialogInterface dialog, int which)
			{
				updateDateRange(dtRangeIdx, null); // null = *any time* (no limit set)
			}
		})
		// Create the dialog and show it.
		.create().show();
	}
	
	private void updateDateRange(int dtRangeIdx, DateTime newDT)
	{
		dateRange[dtRangeIdx] = newDT;
		Button btn = dtRangeIdx == DT_RANGE_IDX_FROM ? btnFrom : btnTo;
		btn.setText(getString(	dtRangeIdx == DT_RANGE_IDX_FROM ? R.string.exportDateRangeFrom : R.string.exportDateRangeTo,
								dateRange[dtRangeIdx] == null ? getString(R.string.exportDateRangeAnytime) : dateTimeFormatter.print(dateRange[dtRangeIdx])));
	}
	
	private void queryCallback(List<Record> result)
	{
		if(result == null || result.isEmpty())
			showOKDialog(getString(R.string.title_activity_export), getString(R.string.exportNoRecordsFound));
		else
		{
			// Get Exporter:
			Exporter exporter = null;
			switch((Format) spinOutputFormat.getSelectedItem())
			{
				case CSV:
					exporter = new CSVRecordsExporter(exportFolder, (Separator) spinCSVSeparator.getSelectedItem());
					break;
				case XML:
					exporter = new XMLRecordsExporter(exportFolder, (CompositeMode) spinXMLMode.getSelectedItem());
					break;
				default:
					throw new IllegalStateException("Unknown export format: " + ((Format) spinOutputFormat.getSelectedItem()).toString());
			}
			
			// TODO Generate selection description String:
			String selectionDesc = "TODO";
			
			// Export!:
			new ExportTask(result, exporter, selectionDesc).execute();
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
		if(result.wasSuccessful())
		{
			if(result.getNumberedOfExportedRecords() > 0)
				// show dialog, OK will run deleteTask, cancel will finish activity:
				showYesNoDialog(R.string.exportSuccessTitle,
								getString(R.string.exportSuccessMsg, result.getNumberedOfExportedRecords(), result.getDestination()) + '\n' + getString(R.string.exportDeleteConfirm),
								false,
								deleteTask,
								true);
			else
				// show dialog & finish activity on OK:
				showOKDialog(R.string.exportSuccessTitle, getString(R.string.exportNothing), true);
		}
		else
		{
			if(result.getNumberedOfExportedRecords() > 0)
				// show dialog, OK will run deleteTask, cancel will finish activity:
				showYesNoDialog(R.string.exportPartialSuccessTitle,
								getString(R.string.exportPartialSuccessMsg, result.getNumberedOfExportedRecords(), result.getDestination(), result.getNumberOfUnexportedRecords(), ExceptionHelpers.getMessageAndCause(result.getFailureReason())) + '\n' + getString(R.string.exportDeleteConfirm),
								false,
								deleteTask,
								true);
			else
				// show dialog & finish activity on OK:
				showOKDialog(	R.string.exportFailureTitle,
								getString(R.string.exportFailureMsg, result.getDestination(), ExceptionHelpers.getMessageAndCause(result.getFailureReason())),
								true);
		}
	}
	
	/**
	 * @param failure exception that caused record deletion to fail (may be null)
	 */
	private void deleteCallback(Exception failure)
	{
		if(failure != null)
			showOKDialog(R.string.exportFailureTitle, getString(R.string.exportDeleteFailureMsg, ExceptionHelpers.getMessageAndCause(failure)), true);
		else
			this.finish();
	}
	
	private class QueryTask extends AsyncTaskWithWaitingDialog<Void, Void, List<Record>>
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
				if(project != null && radioSelectedProject.isChecked())
					schemata.addAll(project.getModel().getSchemata());
				// Date range:
				AndConstraint constraints = new AndConstraint();
				if(dateRange[DT_RANGE_IDX_FROM] != null)
					constraints.addConstraint(new RuleConstraint(Form.COLUMN_TIMESTAMP_START, RuleConstraint.Comparison.GREATER_OR_EQUAL, new TimeStamp(dateRange[DT_RANGE_IDX_FROM])));
				if(dateRange[DT_RANGE_IDX_TO] != null)
					constraints.addConstraint(new RuleConstraint(Form.COLUMN_TIMESTAMP_START, RuleConstraint.Comparison.SMALLER_OR_EQUAL, new TimeStamp(dateRange[DT_RANGE_IDX_TO])));
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
	
	private class ExportTask extends AsyncTaskWithWaitingDialog<Void, Void, ExportResult>
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
	
	private class DeleteTask extends AsyncTaskWithWaitingDialog<Void, Void, Void>
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
