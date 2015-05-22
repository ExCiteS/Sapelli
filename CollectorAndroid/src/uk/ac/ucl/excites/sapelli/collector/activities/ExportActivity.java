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
import uk.ac.ucl.excites.sapelli.collector.fragments.ExportFormatFragment;
import uk.ac.ucl.excites.sapelli.collector.model.Form;
import uk.ac.ucl.excites.sapelli.collector.tasks.RecordsTasks;
import uk.ac.ucl.excites.sapelli.shared.util.ExceptionHelpers;
import uk.ac.ucl.excites.sapelli.shared.util.StringUtils;
import uk.ac.ucl.excites.sapelli.storage.eximport.ExportResult;
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
import android.view.View;
import android.view.View.OnClickListener;
import android.widget.Button;
import android.widget.DatePicker;
import android.widget.RadioButton;
import android.widget.TimePicker;

/**
 * Activity that deals with exporting of Sapelli Collector records to various output formats
 * 
 * @author mstevens, Michalis Vitos
 */
public class ExportActivity extends ProjectActivity implements OnClickListener, RecordsTasks.QueryCallback, RecordsTasks.ExportCallback, RecordsTasks.DeleteCallback
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
	
	private ExportFormatFragment frgFormat;
	
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
		
		// Export format fragment:
		frgFormat = (ExportFormatFragment) getSupportFragmentManager().findFragmentById(R.id.frgFormat);
		
		//	OK & Cancel buttons:
		((Button) findViewById(R.id.btnExportOK)).setOnClickListener(this);
		((Button) findViewById(R.id.btnExportCancel)).setOnClickListener(this);
	}
	
	@Override
	public void onResume()
	{
		super.onResume();

		// All storage operations should happen in onResume, otherwise the storage will be unaccessible

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
				buildAndRunQuery();
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
	
	private void buildAndRunQuery()
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
		// TODO Exclude previously exported:
		// TODO Exclude collector-internal schemas!!!
		// Retrieve by query:
		new RecordsTasks.QueryTask(this, this).execute(new RecordsQuery(Source.From(schemata), Order.UNDEFINED, constraints)); // TODO order by form, deviceid, timestamp
	}
	
	@Override
	public void querySuccess(List<Record> result)
	{
		if(result == null || result.isEmpty())
			showOKDialog(R.string.title_activity_export, R.string.exportNoRecordsFound);
		else
		{
			// TODO Generate selection description String:
			String selectionDesc = "TODO";
			
			// Run the right export task:
			frgFormat.runExportTask(result, this, exportFolder, selectionDesc, this);
		}
	}
	
	@Override
	public void queryFailure(Exception reason)
	{
		showErrorDialog(getString(R.string.exportQueryFailed, ExceptionHelpers.getMessageAndCause(reason)), true);
	}
	
	@Override
	public void exportDone(final ExportResult result)
	{
		if(result == null)
			return; // just in case (shouldn't happen)
		
		// Runnable to delete exported records:
		Runnable deleteTask = new Runnable()
		{
			@SuppressWarnings("unchecked")
			@Override
			public void run()
			{
				new RecordsTasks.Delete(ExportActivity.this, ExportActivity.this).execute(result.getExportedRecords());
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
	 * @param reason exception that caused record deletion to fail (may be null)
	 */
	@Override
	public void deleteFailure(Exception reason)
	{
		if(reason != null)
			showOKDialog(R.string.exportFailureTitle, getString(R.string.exportDeleteFailureMsg, ExceptionHelpers.getMessageAndCause(reason)), true);
		else
			this.finish();
	}	

}
