package uk.ac.ucl.excites.sapelli.collector.fragments;

import java.io.File;

import org.joda.time.DateTime;
import org.joda.time.format.DateTimeFormat;
import org.joda.time.format.DateTimeFormatter;

import uk.ac.ucl.excites.sapelli.collector.CollectorApp;
import uk.ac.ucl.excites.sapelli.collector.R;
import uk.ac.ucl.excites.sapelli.collector.activities.ProjectManagerActivity;
import uk.ac.ucl.excites.sapelli.shared.util.StringUtils;
import uk.ac.ucl.excites.sapelli.storage.eximport.Exporter;
import uk.ac.ucl.excites.sapelli.storage.eximport.Exporter.Format;
import uk.ac.ucl.excites.sapelli.storage.eximport.csv.CSVRecordsExporter;
import uk.ac.ucl.excites.sapelli.storage.eximport.csv.CSVRecordsExporter.Separator;
import uk.ac.ucl.excites.sapelli.storage.eximport.xml.XMLRecordsExporter;
import uk.ac.ucl.excites.sapelli.storage.eximport.xml.XMLRecordsExporter.CompositeMode;
import android.app.AlertDialog;
import android.app.AlertDialog.Builder;
import android.app.Dialog;
import android.content.DialogInterface;
import android.os.Bundle;
import android.support.v4.app.DialogFragment;
import android.view.LayoutInflater;
import android.view.View;
import android.view.View.OnClickListener;
import android.view.ViewGroup;
import android.widget.AdapterView;
import android.widget.AdapterView.OnItemSelectedListener;
import android.widget.ArrayAdapter;
import android.widget.Button;
import android.widget.DatePicker;
import android.widget.LinearLayout;
import android.widget.LinearLayout.LayoutParams;
import android.widget.Spinner;
import android.widget.TimePicker;
import android.widget.ViewSwitcher;

/**
 * Fragment that defines the layout and sets parameters for exporting of Sapelli
 * Collector records to various output formats
 * 
 * @author mstevens, Michalis Vitos, Julia
 */
public class ExportFragment extends DialogFragment implements OnClickListener {

	// Statics---------------------------------------------
	static public final int DT_RANGE_IDX_FROM = 0;
	static public final int DT_RANGE_IDX_TO = 1;
	static private final String EXPORT_ALL = "exportAll";
	static private final DateTimeFormatter dateTimeFormatter = DateTimeFormat.forPattern("yyyy-MM-dd' 'HH:mm");

	// Dynamics--------------------------------------------
	private DateTime[] dateRange = new DateTime[2];
	private File exportFolder;
	private String selectionDesc;

	// UI Elements
	private Button btnFrom;
	private Button btnTo;
	private Button btnDestination;

	private Spinner spinOutputFormat;
	private Spinner spinXMLMode;
	private Spinner spinCSVSeparator;

	private CollectorApp app;
	private ProjectManagerActivity projectManagerActivity;
	protected LinearLayout exportFragment;
	private LayoutInflater inflater;
	private ViewSwitcher viewSwitcher;
	private LinearLayout rootLayout;

	public static ExportFragment newInstance(boolean exportAll) {
		ExportFragment myFragment = new ExportFragment();
		Bundle args = new Bundle();
		args.putBoolean(EXPORT_ALL, exportAll);
		myFragment.setArguments(args);
		return myFragment;
	}

	@Override
	public void onActivityCreated(Bundle savedInstanceState) {
		super.onActivityCreated(savedInstanceState);

		projectManagerActivity = (ProjectManagerActivity) getActivity();
		this.app = (CollectorApp) projectManagerActivity.getApplication(); // sets app

		// Export path:
		exportFolder = app.getFileStorageProvider().getExportFolder(true);
		btnDestination.setText(exportFolder.getAbsolutePath());
	}

	@Override
	public View onCreateView(LayoutInflater inflater, ViewGroup container, Bundle savedInstanceState) {

		this.inflater = inflater;

		rootLayout = new LinearLayout(getActivity());
		rootLayout.setOrientation(LinearLayout.VERTICAL);
		rootLayout.setLayoutParams(new LayoutParams(LayoutParams.MATCH_PARENT, LayoutParams.MATCH_PARENT));
		exportFragment = (LinearLayout) inflater.inflate(R.layout.fragment_export, container, false);
		rootLayout.addView(exportFragment);
		exportFragment.setBackgroundResource(R.layout.drop_shadow);

		// Time range:
		btnFrom = (Button) ((ViewGroup) exportFragment).findViewById(R.id.btnExportFromDate);
		btnFrom.setOnClickListener(this);
		btnTo = (Button) ((ViewGroup) exportFragment).findViewById(R.id.btnExportToDate);
		btnTo.setOnClickListener(this);
		updateDateRange(DT_RANGE_IDX_FROM, null);
		updateDateRange(DT_RANGE_IDX_TO, null);

		// Output destination:
		btnDestination = (Button) ((ViewGroup) exportFragment).findViewById(R.id.btnDestination);
		//				btnDestination.setText(exportFolder.getAbsolutePath());
		btnDestination.setEnabled(false); // TODO make export path configurable (for now it is not)

		// Output formats:
		spinOutputFormat = (Spinner) ((ViewGroup) exportFragment).findViewById(R.id.spinExportFormat);
		final ArrayAdapter<Format> formatAdapter = new ArrayAdapter<Format>(getActivity(), android.R.layout.simple_spinner_item, Format.values());
		formatAdapter.setDropDownViewResource(android.R.layout.simple_spinner_dropdown_item);
		spinOutputFormat.setAdapter(formatAdapter);
		final LinearLayout xmlOptions = (LinearLayout) ((ViewGroup) exportFragment).findViewById(R.id.layoutXMLOptions);
		final LinearLayout csvOptions = (LinearLayout) ((ViewGroup) exportFragment).findViewById(R.id.layoutCSVOptions);
		spinOutputFormat.setOnItemSelectedListener(new OnItemSelectedListener() {
			@Override
			public void onItemSelected(AdapterView<?> parent, View view, int position, long id) {
				boolean xml = formatAdapter.getItem(position) == Format.XML; //while there are only 2 formats we don't need a switch/case 
				xmlOptions.setVisibility(xml ? View.VISIBLE : View.GONE);
				csvOptions.setVisibility(xml ? View.GONE : View.VISIBLE);
			}

			@Override
			public void onNothingSelected(AdapterView<?> parent) { /* ignore */
			}
		});

		// XML options:
		spinXMLMode = (Spinner) ((ViewGroup) exportFragment).findViewById(R.id.spinXMLMode);
		ArrayAdapter<CompositeMode> xmlModeAdapter = new ArrayAdapter<CompositeMode>(getActivity(), android.R.layout.simple_spinner_item, CompositeMode.values());
		xmlModeAdapter.setDropDownViewResource(android.R.layout.simple_spinner_dropdown_item);
		spinXMLMode.setAdapter(xmlModeAdapter);
		spinXMLMode.setSelection(xmlModeAdapter.getPosition(XMLRecordsExporter.DEFAULT_COMPOSITE_MODE));

		// CSV options:
		spinCSVSeparator = (Spinner) ((ViewGroup) exportFragment).findViewById(R.id.spinCSVSeparator);
		ArrayAdapter<Separator> csvModeAdapter = new ArrayAdapter<Separator>(getActivity(), android.R.layout.simple_spinner_item, Separator.values());
		csvModeAdapter.setDropDownViewResource(android.R.layout.simple_spinner_dropdown_item);
		spinCSVSeparator.setAdapter(csvModeAdapter);
		spinCSVSeparator.setSelection(csvModeAdapter.getPosition(CSVRecordsExporter.DEFAULT_SEPARATOR));

		//	OK & More buttons:
		((Button) ((ViewGroup) exportFragment).findViewById(R.id.btnExportOK)).setOnClickListener(this);
		((Button) ((ViewGroup) exportFragment).findViewById(R.id.btn_exportSettings)).setOnClickListener(this);
		viewSwitcher = (ViewSwitcher) ((ViewGroup) exportFragment).findViewById(R.id.viewSwitcher);

		return rootLayout;
	}

	@Override
	public void onClick(View v) {
		switch (v.getId()) {
		case R.id.btnExportFromDate:
			setDateRange(DT_RANGE_IDX_FROM);
			break;
		case R.id.btnExportToDate:
			setDateRange(DT_RANGE_IDX_TO);
			break;
		case R.id.btnExportOK:
			projectManagerActivity.export(getExporter(), selectionDesc, dateRange, projectManagerActivity.getSelectedProject(false), getArguments().getBoolean(EXPORT_ALL, false)); //TODO: SelectionDest
			if (getDialog() != null)
				dismiss(); //dismiss ExportFragment if it is a dialog
			break;
		case R.id.btn_exportSettings:
			if (viewSwitcher.getCurrentView() == ((LinearLayout) ((ViewGroup) exportFragment).findViewById(R.id.setExportDate))) {

				viewSwitcher.setInAnimation(getActivity(), R.anim.view_transition_in_left);
				viewSwitcher.setOutAnimation(getActivity(), R.anim.view_transition_out_left);
				viewSwitcher.showNext();
				((Button) ((ViewGroup) exportFragment).findViewById(R.id.btn_exportSettings)).setText(R.string.criteria);
			} else {
				viewSwitcher.setInAnimation(getActivity(), R.anim.view_transition_in_right);
				viewSwitcher.setOutAnimation(getActivity(), R.anim.view_transition_out_right);
				viewSwitcher.showPrevious();
				((Button) ((ViewGroup) exportFragment).findViewById(R.id.btn_exportSettings)).setText(R.string.output);
			}
			break;
		}
	}

	private void setDateRange(final int dtRangeIdx) {
		// Init current date time to show in dialog:
		DateTime current = dateRange[dtRangeIdx];
		if (current == null)
			switch (dtRangeIdx) {
			case DT_RANGE_IDX_FROM:
				// default "from" time is today at 00:00:00:
				DateTime now = DateTime.now();
				current = new DateTime(now.getYear(), now.getMonthOfYear(), now.getDayOfMonth(), 0, 0);
				break;
			case DT_RANGE_IDX_TO:
				// default "to" time is *now*:
				current = DateTime.now();
				break;
			}

		// UI elements:
		View view = inflater.inflate(R.layout.dialog_datetime, null);
		final DatePicker datePicker = (DatePicker) view.findViewById(R.id.DTdatePicker);
		datePicker.updateDate(current.getYear(), current.getMonthOfYear(), current.getDayOfMonth());
		final TimePicker timePicker = (TimePicker) view.findViewById(R.id.DTtimePicker);
		timePicker.setIs24HourView(true);
		timePicker.setCurrentHour(current.getHourOfDay());
		timePicker.setCurrentMinute(current.getMinuteOfHour());

		// Create the dialog
		AlertDialog.Builder builder = new Builder(getActivity());
		// Set the title:
		builder.setTitle(getString(dtRangeIdx == DT_RANGE_IDX_FROM ? R.string.exportDateRangeFrom : R.string.exportDateRangeTo, ':'))
		// Set UI:
				.setView(view)
				// Set the buttons:
				//	OK:
				.setPositiveButton(android.R.string.ok, new Dialog.OnClickListener() {
					@Override
					public void onClick(DialogInterface dialog, int which) {
						updateDateRange(dtRangeIdx, new DateTime(datePicker.getYear(), datePicker.getMonth(), datePicker.getDayOfMonth(), timePicker.getCurrentHour(), timePicker.getCurrentMinute(), 0, 0));
					}
				})
				//	Cancel:
				.setNegativeButton(android.R.string.cancel, new Dialog.OnClickListener() {
					@Override
					public void onClick(DialogInterface dialog, int which) {
						dialog.dismiss();
					}
				})
				//	Any time:
				.setNeutralButton(StringUtils.capitalizeFirstLetter(getString(R.string.exportDateRangeAnytime)), new Dialog.OnClickListener() {
					@Override
					public void onClick(DialogInterface dialog, int which) {
						updateDateRange(dtRangeIdx, null); // null = *any time* (no limit set)
					}
				})
				// Create the dialog and show it.
				.create().show();
	}

	private void updateDateRange(int dtRangeIdx, DateTime newDT) {
		dateRange[dtRangeIdx] = newDT;
		Button btn = dtRangeIdx == DT_RANGE_IDX_FROM ? btnFrom : btnTo;
		btn.setText(getString(dtRangeIdx == DT_RANGE_IDX_FROM ? R.string.exportDateRangeFrom : R.string.exportDateRangeTo, dateRange[dtRangeIdx] == null ? getString(R.string.exportDateRangeAnytime) : dateTimeFormatter.print(dateRange[dtRangeIdx])));
	}

	private Exporter getExporter() {
		// Get Exporter:
		Exporter exporter = null;
		switch ((Format) spinOutputFormat.getSelectedItem()) {
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
		selectionDesc = "TODO";
		return exporter;
	}

	/**
	 * @return the rootLayout
	 */
	public LinearLayout getExportLayout() {
		return exportFragment;
	}

}
