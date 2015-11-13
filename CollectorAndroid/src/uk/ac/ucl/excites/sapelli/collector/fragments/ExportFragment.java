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

package uk.ac.ucl.excites.sapelli.collector.fragments;

import java.io.File;
import java.util.List;

import org.joda.time.DateTime;
import org.joda.time.format.DateTimeFormat;
import org.joda.time.format.DateTimeFormatter;

import android.annotation.SuppressLint;
import android.app.AlertDialog;
import android.app.Dialog;
import android.content.DialogInterface;
import android.os.Bundle;
import android.support.v7.app.AppCompatActivity;
import android.view.View;
import android.view.View.OnClickListener;
import android.widget.AdapterView;
import android.widget.AdapterView.OnItemSelectedListener;
import android.widget.ArrayAdapter;
import android.widget.Button;
import android.widget.CheckBox;
import android.widget.DatePicker;
import android.widget.LinearLayout;
import android.widget.Spinner;
import android.widget.TimePicker;
import uk.ac.ucl.excites.sapelli.collector.CollectorClient;
import uk.ac.ucl.excites.sapelli.collector.R;
import uk.ac.ucl.excites.sapelli.collector.activities.ProjectManagerActivity;
import uk.ac.ucl.excites.sapelli.collector.model.Form;
import uk.ac.ucl.excites.sapelli.collector.model.Project;
import uk.ac.ucl.excites.sapelli.collector.tasks.RecordsTasks;
import uk.ac.ucl.excites.sapelli.shared.util.ExceptionHelpers;
import uk.ac.ucl.excites.sapelli.shared.util.StringUtils;
import uk.ac.ucl.excites.sapelli.storage.eximport.ExportResult;
import uk.ac.ucl.excites.sapelli.storage.eximport.Exporter.Format;
import uk.ac.ucl.excites.sapelli.storage.eximport.SimpleExporter;
import uk.ac.ucl.excites.sapelli.storage.eximport.csv.CSVRecordsExporter;
import uk.ac.ucl.excites.sapelli.storage.eximport.csv.CSVRecordsExporter.Separator;
import uk.ac.ucl.excites.sapelli.storage.eximport.xml.XMLRecordsExporter;
import uk.ac.ucl.excites.sapelli.storage.eximport.xml.XMLRecordsExporter.CompositeMode;
import uk.ac.ucl.excites.sapelli.storage.model.Record;
import uk.ac.ucl.excites.sapelli.storage.queries.Order;
import uk.ac.ucl.excites.sapelli.storage.queries.RecordsQuery;
import uk.ac.ucl.excites.sapelli.storage.queries.constraints.AndConstraint;
import uk.ac.ucl.excites.sapelli.storage.queries.constraints.RuleConstraint;
import uk.ac.ucl.excites.sapelli.storage.queries.sources.Source;
import uk.ac.ucl.excites.sapelli.storage.types.TimeStamp;

/**
 * 
 * @author mstevens, Julia
 */
public class ExportFragment extends ProjectManagerFragment implements OnClickListener
{
	
	// STATIC -------------------------------------------------------
	static private final int DT_RANGE_IDX_FROM = 0;
	static private final int DT_RANGE_IDX_TO = 1;
	static private final DateTimeFormatter dateTimeFormatter = DateTimeFormat.forPattern("yyyy-MM-dd' 'HH:mm");
	
	static public final Format DEFAULT_FORMAT = Format.CSV;
	
	static private final String DIALOG_TITLE_KEY = "title";
	static private final String DIALOG_MESSAGE_KEY = "msg";
	static private final String DIALOG_SHOW_CANCEL_KEY = "showCancel";
	
	static public void ShowExportAllDialog(AppCompatActivity owner, boolean showCancel)
	{
		ShowDialog(	owner,
					new ExportFragment(), // will export all projects, 
					owner.getString(R.string.exportAllProjTitle),
					owner.getString(R.string.exportAllProjMsg),
					showCancel);
	}
	
	static public void ShowChoseFormatDialog(AppCompatActivity owner, String title, String msg, boolean showCancel, FormatDialogCallback callback)
	{
		ShowDialog(	owner,
					new ExportFragment(callback), // only to choose format
					title,
					msg,
					showCancel);
	}
	
	static private void ShowDialog(AppCompatActivity owner, ExportFragment fragment, String title, String msg, boolean showCancel)
	{
		// Pass arguments:
		Bundle args = new Bundle();
		args.putString(DIALOG_TITLE_KEY, title);
		args.putString(DIALOG_MESSAGE_KEY, msg); 
		args.putBoolean(DIALOG_SHOW_CANCEL_KEY, showCancel);
		fragment.setArguments(args);
		
		// Show dialog:
		fragment.show(owner.getSupportFragmentManager(), title.replace(' ', '_'));
	}
	
	// DYNAMIC ------------------------------------------------------
	private Project projectToExport;
	
	private FormatDialogCallback formatDialogCallback;
	
	private final DateTime[] dateRange = new DateTime[2];
	private String selectionDesc;
	private File exportFolder;
	
	// UI Elements
	private Button btnFrom;
	private Button btnTo;
	private CheckBox checkExcludePreviouslyExported;
	private Button btnDestination;
	private Spinner spinOutputFormat;
	private Spinner spinXMLMode;
	private Spinner spinCSVSeparator;
	
	/**
	 * Only for exporting of data from all projects. 
	 */
	public ExportFragment()
	{
		this(null, null);
	}
	
	/**
	 * Exports data of specific project
	 * 
	 * @param projectToExport the {@link Project} to export data for, pass {@code null} to export data of all projects (including deleted ones)
	 */
	public ExportFragment(Project projectToExport)
	{
		this(projectToExport, null);
	}
	
	/**
	 * @param formatDialogCallback
	 */
	private ExportFragment(FormatDialogCallback formatDialogCallback)
	{
		this(null, formatDialogCallback);
	}
	
	/**
	 * @param projectToExport
	 * @param formatDialogCallback
	 */
	private ExportFragment(Project projectToExport, FormatDialogCallback formatDialogCallback)
	{
		this.projectToExport = projectToExport;
		this.formatDialogCallback = formatDialogCallback;
	}
	
	/**
	 * @param projectToExport the projectToExport to set
	 */
	public void setProjectToExport(Project projectToExport)
	{
		this.projectToExport = projectToExport;
	}
	
	@Override
	protected Integer getLayoutID()
	{
		return R.layout.fragment_export;
	}

	public boolean isFormatChosingMode()
	{
		return formatDialogCallback != null;
	}
	
	@SuppressWarnings("unused")
	@Override
	protected void setupUI(View rootLayout)
	{
		// Time range:
		if(isFormatChosingMode())
			rootLayout.findViewById(R.id.groupExportDateRange).setVisibility(View.GONE);
		else
		{
			btnFrom = (Button) rootLayout.findViewById(R.id.btnExportFromDate);
			btnFrom.setOnClickListener(this);
			btnTo = (Button) rootLayout.findViewById(R.id.btnExportToDate);
			btnTo.setOnClickListener(this);
			updateDateRange(DT_RANGE_IDX_FROM, null);
			updateDateRange(DT_RANGE_IDX_TO, null);
		}
		
		// Exclude previously exported:
		checkExcludePreviouslyExported = (CheckBox) rootLayout.findViewById(R.id.checkExcludePreviouslyExported);
		
		// Output destination:
		exportFolder = getOwner().getFileStorageProvider().getExportFolder(true);
		if(isFormatChosingMode() || true) // for now we always hide the output destination
			rootLayout.findViewById(R.id.groupExportDestination).setVisibility(View.GONE);
		else
		{
			btnDestination = (Button) rootLayout.findViewById(R.id.btnDestination);
			btnDestination.setText(exportFolder.getAbsolutePath());
			btnDestination.setEnabled(false); // TODO make export path configurable? (for now it is not)
		}
		
		// Output format:
		spinOutputFormat = (Spinner) rootLayout.findViewById(R.id.spinExportFormat);
		final ArrayAdapter<Format> formatAdapter = new ArrayAdapter<Format>(rootLayout.getContext(), android.R.layout.simple_spinner_item, Format.values());
		formatAdapter.setDropDownViewResource(android.R.layout.simple_spinner_dropdown_item);
		spinOutputFormat.setAdapter(formatAdapter);
		spinOutputFormat.setSelection(formatAdapter.getPosition(DEFAULT_FORMAT));
		final LinearLayout xmlOptions = (LinearLayout) rootLayout.findViewById(R.id.layoutXMLOptions);
		final LinearLayout csvOptions = (LinearLayout) rootLayout.findViewById(R.id.layoutCSVOptions);
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
		//	XML options:
		spinXMLMode = (Spinner) rootLayout.findViewById(R.id.spinXMLMode);
		ArrayAdapter<CompositeMode> xmlModeAdapter = new ArrayAdapter<CompositeMode>(rootLayout.getContext(), android.R.layout.simple_spinner_item, CompositeMode.values());
		xmlModeAdapter.setDropDownViewResource(android.R.layout.simple_spinner_dropdown_item);
		spinXMLMode.setAdapter(xmlModeAdapter);
		spinXMLMode.setSelection(xmlModeAdapter.getPosition(XMLRecordsExporter.DEFAULT_COMPOSITE_MODE));
		//	CSV options:
		spinCSVSeparator = (Spinner) rootLayout.findViewById(R.id.spinCSVSeparator);
		ArrayAdapter<Separator> csvModeAdapter = new ArrayAdapter<Separator>(rootLayout.getContext(), android.R.layout.simple_spinner_item, Separator.values());
		csvModeAdapter.setDropDownViewResource(android.R.layout.simple_spinner_dropdown_item);
		spinCSVSeparator.setAdapter(csvModeAdapter);
		spinCSVSeparator.setSelection(csvModeAdapter.getPosition(CSVRecordsExporter.DEFAULT_SEPARATOR));
	}
	
	@SuppressLint("InflateParams")
	@Override
	public Dialog onCreateDialog(Bundle savedInstanceState)
	{
		AlertDialog.Builder builder = new AlertDialog.Builder(getOwner())
		.setTitle(getArguments().getString(DIALOG_TITLE_KEY))
		.setIcon(R.drawable.ic_export_black_36dp)
		.setMessage(getArguments().getString(DIALOG_MESSAGE_KEY))
		.setPositiveButton(android.R.string.ok, new DialogInterface.OnClickListener()
		{
			@Override
			public void onClick(DialogInterface dialog, int which)
			{
				if(!isFormatChosingMode())
					runExport();
				else
					formatDialogCallback.onFormatChosen(ExportFragment.this);
			}
		});
		if(getArguments().getBoolean(DIALOG_SHOW_CANCEL_KEY))
			builder.setNegativeButton(android.R.string.cancel, null);
		AlertDialog dialog = builder.create();

		// Set view:
		int lrSpacingPx = getDialogLeftRightPaddingPx();
		dialog.setView(getRootLayout(), lrSpacingPx, getDialogMessageToViewSpacingPx(), lrSpacingPx, 0);

		return dialog;
	}
	
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
		View view = getOwner().getLayoutInflater().inflate(R.layout.dialog_datetime, null);
		final DatePicker datePicker = (DatePicker) view.findViewById(R.id.DTdatePicker);
		datePicker.updateDate(current.getYear(), current.getMonthOfYear(), current.getDayOfMonth());
		final TimePicker timePicker = (TimePicker) view.findViewById(R.id.DTtimePicker);
		timePicker.setIs24HourView(true);
		timePicker.setCurrentHour(current.getHourOfDay());
		timePicker.setCurrentMinute(current.getMinuteOfHour());
		
		// Create the dialog
		AlertDialog.Builder builder = new AlertDialog.Builder(getOwner());
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
	
	public Format getSelectedFormat()
	{
		return (Format) spinOutputFormat.getSelectedItem();
	}
	
	public Separator getCSVSeparator()
	{
		return (Separator) spinCSVSeparator.getSelectedItem();
	}
	
	public CompositeMode getXMLCompositeMode()
	{
		return (CompositeMode) spinXMLMode.getSelectedItem();
	}
	
	/**
	 * @author mstevens
	 */
	public static interface FormatDialogCallback
	{
		
		public abstract void onFormatChosen(ExportFragment formatFragment);
		
	}
	
	public void runExport()
	{
		new ExportRunner(getOwner()).run();
	}
	
	/**
	 * The primary purpose of this inner class is to keep hold of the activity. This is necessary because when the
	 * ExportFragment is shown as a dialog it is being detached from the activity from the moment the dialog is dismissed.
	 * 
	 * TODO consider making this a static class (meaning we need to pass all export parameters)
	 * 
	 * @author mstevens
	 */
	private class ExportRunner implements RecordsTasks.QueryCallback, RecordsTasks.ExportCallback, RecordsTasks.DeleteCallback
	{
	
		private final ProjectManagerActivity activity;
		
		private ExportResult exportResult = null;
		
		/**
		 * @param activity
		 */
		public ExportRunner(ProjectManagerActivity activity)
		{
			if(activity == null)
				throw new NullPointerException(ProjectManagerActivity.class.getSimpleName() + " cannot be null!");
			this.activity = activity;
		}
		
		public void run()
		{
			// Thrown away old state:
			exportResult = null;
			
			// Define query Source:
			Source source;
			if(projectToExport != null)
				source = Source.From(projectToExport.getModel());
			else
				source = Source.With(CollectorClient.SCHEMA_FLAGS_COLLECTOR_DATA);
			
			// Define constraints:
			AndConstraint constraints = new AndConstraint();
			//	Date range:
			if(dateRange[DT_RANGE_IDX_FROM] != null)
				constraints.addConstraint(new RuleConstraint(Form.COLUMN_TIMESTAMP_START, RuleConstraint.Comparison.GREATER_OR_EQUAL, new TimeStamp(dateRange[DT_RANGE_IDX_FROM])));
			if(dateRange[DT_RANGE_IDX_TO] != null)
				constraints.addConstraint(new RuleConstraint(Form.COLUMN_TIMESTAMP_START, RuleConstraint.Comparison.SMALLER_OR_EQUAL, new TimeStamp(dateRange[DT_RANGE_IDX_TO])));
			//	TODO Exclude previously exported
			
			// Retrieve by query:
			new RecordsTasks.QueryTask(activity, this).execute(SimpleExporter.GetRecordsQuery(source, Order.UNDEFINED, RecordsQuery.NO_LIMIT, constraints, checkExcludePreviouslyExported.isChecked()));
			// TODO order by form, deviceid, timestamp
			// TODO let ExportFragment & Backup share this code somehow
		}
		
		private String getString(int resId)
		{
			return activity.getString(resId);
		}

		private String getString(int resId, Object... formatArgs)
		{
			return activity.getString(resId, formatArgs);
		}

		@Override
		public void querySuccess(List<Record> result)
		{
			if(result == null || result.isEmpty())
				activity.showOKDialog(R.string.title_activity_export, R.string.exportNoRecordsFound, R.drawable.ic_export_black_36dp);
			else
			{
				// TODO Generate selection description String:
				String selectionDesc = "TODO";
				
				// Run the right export task:
				RecordsTasks.runExportTask(activity, result, ExportFragment.this, exportFolder, selectionDesc, this);
			}
		}
		
		@Override
		public void queryFailure(Exception reason)
		{
			activity.showErrorDialog(getString(R.string.exportQueryFailed, ExceptionHelpers.getMessageAndCause(reason)), true);
		}
		
		private Runnable getDoneExportingCallbackRunnable(final ExportResult result, final boolean dataDeleted)
		{
			return new Runnable()
			{
				@Override
				public void run()
				{
					activity.onDataExportDone(result, dataDeleted);
				}
			};
		}
		
		@Override
		public void exportDone(final ExportResult result)
		{
			if(result == null)
				return; // just in case (shouldn't happen)
			
			// Hold on to result:
			this.exportResult = result;
			
			// Deal with result:
			if(result.getNumberedOfExportedRecords() > 0)
			{
				// Runnable to delete exported records:
				Runnable deleteRunnable = new Runnable()
				{
					@SuppressWarnings("unchecked")
					@Override
					public void run()
					{
						new RecordsTasks.DeleteTask(activity, ExportFragment.ExportRunner.this).execute(result.getExportedRecords());
					}
				};
				// show dialog, OK will run deleteRunnable:
				activity.showYesNoDialog(
					result.wasSuccessful() ?
						R.string.exportSuccessTitle :
						R.string.exportPartialSuccessTitle,
					result.wasSuccessful() ? 
							getString(R.string.exportSuccessMsg, result.getNumberedOfExportedRecords(), result.getDestination()) + '\n' + getString(R.string.exportDeleteConfirm) :
							getString(R.string.exportPartialSuccessMsg, result.getNumberedOfExportedRecords(), result.getDestination(), result.getNumberOfUnexportedRecords(), ExceptionHelpers.getMessageAndCause(result.getFailureReason())) + '\n' + getString(R.string.exportDeleteConfirm),
					deleteRunnable,
					false,
					getDoneExportingCallbackRunnable(result, false),
					false);
			}
			else
				// show dialog:
				activity.showOKDialog(
					result.wasSuccessful() ?
						R.string.exportSuccessTitle :
						R.string.exportFailureTitle,
					result.wasSuccessful() ?
						getString(R.string.exportNothing) : 
						getString(R.string.exportFailureMsg, result.getDestination(), ExceptionHelpers.getMessageAndCause(result.getFailureReason())),
					false,
					getDoneExportingCallbackRunnable(result, false));
		}
		
		@Override
		public void deleteSuccess(List<Record> deletedRecords)
		{
			getDoneExportingCallbackRunnable(exportResult, true).run();
		}
		
		/**
		 * @param reason exception that caused record deletion to fail (may be null)
		 */
		@Override
		public void deleteFailure(Exception reason)
		{
			if(reason != null)
				activity.showOKDialog(
					R.string.exportFailureTitle,
					getString(R.string.exportDeleteFailureMsg, ExceptionHelpers.getMessageAndCause(reason)),
					false,
					getDoneExportingCallbackRunnable(exportResult, false));
		}
	
	}
	
}
