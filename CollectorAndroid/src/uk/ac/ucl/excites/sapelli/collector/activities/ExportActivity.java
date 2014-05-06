package uk.ac.ucl.excites.sapelli.collector.activities;

import java.util.ArrayList;
import java.util.List;
import java.util.Locale;

import org.joda.time.DateTime;
import org.joda.time.format.DateTimeFormat;
import org.joda.time.format.DateTimeFormatter;

import uk.ac.ucl.excites.sapelli.collector.CollectorApp;
import uk.ac.ucl.excites.sapelli.collector.R;
import uk.ac.ucl.excites.sapelli.collector.model.Form;
import uk.ac.ucl.excites.sapelli.collector.model.Project;
import uk.ac.ucl.excites.sapelli.collector.util.AsyncTaskWithWaitingDialog;
import uk.ac.ucl.excites.sapelli.shared.db.StoreClient;
import uk.ac.ucl.excites.sapelli.shared.util.StringUtils;
import uk.ac.ucl.excites.sapelli.storage.db.RecordStore;
import uk.ac.ucl.excites.sapelli.storage.eximport.ExImportHelper;
import uk.ac.ucl.excites.sapelli.storage.eximport.ExImportHelper.Format;
import uk.ac.ucl.excites.sapelli.storage.eximport.ExportResult;
import uk.ac.ucl.excites.sapelli.storage.eximport.Exporter;
import uk.ac.ucl.excites.sapelli.storage.eximport.xml.XMLRecordsExporter;
import uk.ac.ucl.excites.sapelli.storage.eximport.xml.XMLRecordsExporter.CompositeMode;
import uk.ac.ucl.excites.sapelli.storage.model.Record;
import uk.ac.ucl.excites.sapelli.storage.model.Schema;
import uk.ac.ucl.excites.sapelli.storage.queries.RecordsQuery;
import android.app.AlertDialog;
import android.app.AlertDialog.Builder;
import android.app.Dialog;
import android.content.DialogInterface;
import android.os.Bundle;
import android.view.View;
import android.view.View.OnClickListener;
import android.widget.AdapterView;
import android.widget.AdapterView.OnItemSelectedListener;
import android.widget.ArrayAdapter;
import android.widget.Button;
import android.widget.CheckBox;
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
public class ExportActivity extends BaseActivity implements OnClickListener, StoreClient
{
	
	// Statics---------------------------------------------
	static private final int DT_RANGE_IDX_FROM = 0;
	static private final int DT_RANGE_IDX_TO = 1;
	static private final DateTimeFormatter dateTimeFormatter = DateTimeFormat.forPattern("yyyy-MM-dd' 'HH:mm");
	
	// Dynamics--------------------------------------------
	private Project selectedProject;
	DateTime[] dateRange = new DateTime[2];
	
	// UI Elements
	private RadioButton radioSelectedProject;
	private RadioButton radioAllProjects;

	private Button btnFrom;
	private Button btnTo;
	
	private Spinner spinOutputFormat;
	private Spinner spinXMLMode;
	private Spinner spinCSVSeparator;
	
	private CheckBox chbxRemove;
	
	@Override
	protected void onCreate(Bundle savedInstanceState)
	{
		super.onCreate(savedInstanceState);
		setContentView(R.layout.activity_export);
		this.app = (CollectorApp) getApplication();
		
		//TODO load project
		
		// UI elements...
		radioSelectedProject = (RadioButton) findViewById(R.id.radioExportSelectedProj);
		radioAllProjects = (RadioButton) findViewById(R.id.radioExportAllProj);
		if(selectedProject != null)
		{
			radioSelectedProject.setText(String.format(Locale.getDefault(), getString(R.string.exportSelectedProj), selectedProject));
			radioSelectedProject.setChecked(true);
		}
		else
		{
			radioSelectedProject.setVisibility(View.GONE);
			radioAllProjects.setChecked(true);
		}
		
		btnFrom = (Button) findViewById(R.id.btnExportFromDate);
		btnFrom.setOnClickListener(this);
		btnTo = (Button) findViewById(R.id.btnExportToDate);
		btnTo.setOnClickListener(this);
		updateDateRange(DT_RANGE_IDX_FROM, null);
		updateDateRange(DT_RANGE_IDX_TO, null);
		
		spinOutputFormat = (Spinner) findViewById(R.id.spinExportFormat);
		final ArrayAdapter<Format> formatAdapter = new ArrayAdapter<Format>(this, android.R.layout.simple_spinner_item, ExImportHelper.Format.values());
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
		
		spinXMLMode = (Spinner) findViewById(R.id.spinXMLMode);
		ArrayAdapter<CompositeMode> xmlModeAdapter = new ArrayAdapter<CompositeMode>(this, android.R.layout.simple_spinner_item, XMLRecordsExporter.CompositeMode.values());
		xmlModeAdapter.setDropDownViewResource(android.R.layout.simple_spinner_dropdown_item);
		spinXMLMode.setAdapter(xmlModeAdapter);
		
		spinCSVSeparator = (Spinner) findViewById(R.id.spinCSVSeparator);
		//TODO csv separator...
		
		chbxRemove = (CheckBox) findViewById(R.id.chbxExportRemove);
		
		((Button) findViewById(R.id.btnExportOK)).setOnClickListener(this);
		((Button) findViewById(R.id.btnExportCancel)).setOnClickListener(this);
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
			case R.id.btnExportOK :
				new QueryTask(radioSelectedProject.isSelected() ? selectedProject : null, dateRange[DT_RANGE_IDX_FROM], dateRange[DT_RANGE_IDX_TO]).execute();
				break;
			case R.id.btnExportCancel :
				this.finish();
				break;
		}
	}
	
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
		// Set the title
		builder.setTitle(String.format(Locale.getDefault(), getString(dtRangeIdx == DT_RANGE_IDX_FROM ? R.string.exportDateRangeFrom : R.string.exportDateRangeTo), "...")) 
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
				updateDateRange(dtRangeIdx, null); // null = *any time*
			}
		})
		// Create the dialog and show it.
		.create().show();
	}
	
	private void updateDateRange(int dtRangeIdx, DateTime newDT)
	{
		dateRange[dtRangeIdx] = newDT;
		Button btn = dtRangeIdx == DT_RANGE_IDX_FROM ? btnFrom : btnTo;
		btn.setText(String.format(	Locale.getDefault(),
									getString(dtRangeIdx == DT_RANGE_IDX_FROM ?
											R.string.exportDateRangeFrom :
											R.string.exportDateRangeTo),
									dateRange[dtRangeIdx] == null ?
											getString(R.string.exportDateRangeAnytime) :
											dateTimeFormatter.print(dateRange[dtRangeIdx])));
	}
	
	private void queryCallback(List<Record> result)
	{
		if(result == null)
			showErrorDialog("Failed to query for records.", true);
		else if(result.isEmpty())
			//Dialogs.showWarningDialog(this, "There are no records to export.", true);
			;
		else
		{
			//Exporter exporter = ExImportHelper.getExporter(null, , xmlCompositeMode, csvSeparator)
			//new ExportTask(result.size(), exporter, selectionDescr).execute();
		}	
	}
	
	private void exportCallback(ExportResult result)
	{
		if(result.wasSuccessful())
		{
			
			
		}
	}
	
	private class QueryTask extends AsyncTaskWithWaitingDialog<Void, Void, List<Record>>
	{

		private Project project;
		
		public QueryTask(Project project, DateTime from, DateTime to)
		{
			super(ExportActivity.this, "Fetching records..."); //TODO multilang
			this.project = project;
		}

		@Override
		protected List<Record> doInBackground(Void... params)
		{
			RecordStore store = null;
			try
			{
				store = app.getRecordStore(ExportActivity.this);
				// Schemas (when list stays empty all records of any schema/project/form will be fetched):
				List<Schema> schemata = new ArrayList<Schema>();
				if(project != null)
					for(Form f : project.getForms())
						schemata.add(f.getSchema());
				// Date range:
				//if(fromTime != null)
					
					
				return store.retrieveRecords(new RecordsQuery(schemata));
			}
			catch(Exception e)
			{
				e.printStackTrace(System.err);
				return null;
			}
			finally
			{
				app.discardStoreUsage(store, ExportActivity.this);
			}
		}
		
		@Override
		protected void onPostExecute(List<Record> result)
		{
			super.onPostExecute(result); // dismiss dialog
			queryCallback(result);
		}
	}
	
	private class ExportTask extends AsyncTaskWithWaitingDialog<List<Record>, Void, ExportResult>
	{

		private Exporter exporter;
		private String selectionDescr;
		
		public ExportTask(int numberOfRecords, Exporter exporter, String selectionDescr)
		{
			super(ExportActivity.this, String.format(Locale.getDefault(), "Exporting %d records...", numberOfRecords)); //TODO multilang
			this.exporter = exporter;
			this.selectionDescr = selectionDescr;
		}

		@Override
		protected ExportResult doInBackground(List<Record>... params)
		{
			if(params != null && params.length == 1)
				return exporter.export(params[0], selectionDescr);
			return null;
		}
		
		@Override
		protected void onPostExecute(ExportResult result)
		{
			super.onPostExecute(result); // dismiss dialog
			exportCallback(result);
		}
		
	}

}
