package uk.ac.ucl.excites.sapelli.collector.activities;

import java.util.ArrayList;
import java.util.Calendar;

import uk.ac.ucl.excites.sapelli.collector.R;
import uk.ac.ucl.excites.sapelli.collector.model.Project;
import android.app.AlertDialog;
import android.content.Context;
import android.content.DialogInterface;
import android.view.LayoutInflater;
import android.view.View;
import android.widget.AdapterView;
import android.widget.ArrayAdapter;
import android.widget.Button;
import android.widget.CheckBox;
import android.widget.DatePicker;
import android.widget.Spinner;
import android.widget.TextView;

public class ExportDialog extends AlertDialog.Builder
{
	private Context context;
	private Project selectedProject;

	// UI
	private LayoutInflater layoutInflater;
	private View exportView;

	// UI Elements
	private Button button_from;
	private Button button_to;
	private TextView data_range_from;
	private TextView data_range_to;
	private Spinner projects;
	private Spinner output_format;
	private Spinner xml_mode;
	private CheckBox remove_data;
	private TextView xml_mode_text;

	public ExportDialog(Context context, Project selectedProject)
	{
		// Set the theme
		// super(context, R.style.ExportDialog);
		super(context);
		this.context = context;
		this.selectedProject = selectedProject;

		// Set view
		layoutInflater = LayoutInflater.from(context);
		exportView = layoutInflater.inflate(R.layout.export_layout, null);
		this.setView(exportView);

		// Get UI elements
		projects = (Spinner) exportView.findViewById(R.id.select_project);
		output_format = (Spinner) exportView.findViewById(R.id.output_format);
		xml_mode = (Spinner) exportView.findViewById(R.id.xml_mode);
		data_range_from = (TextView) exportView.findViewById(R.id.text_from);
		data_range_to = (TextView) exportView.findViewById(R.id.text_to);
		button_from = (Button) exportView.findViewById(R.id.button_from);
		button_to = (Button) exportView.findViewById(R.id.button_to);
		remove_data = (CheckBox) exportView.findViewById(R.id.remove_after_export);
		xml_mode_text = (TextView) exportView.findViewById(R.id.xml_mode_text);

		createSpinners();
		createDataButtons();
	}

	private void createSpinners()
	{
		// Project spinner
		ArrayList<String> projectSpinnerArray = new ArrayList<String>();
		if(selectedProject != null)
			projectSpinnerArray.add("All records of selected project.");
		projectSpinnerArray.add("All records of all projects.");

		ArrayAdapter<String> spinnerArrayAdapter = new ArrayAdapter<String>(context, android.R.layout.simple_spinner_item, projectSpinnerArray);
		spinnerArrayAdapter.setDropDownViewResource(android.R.layout.simple_spinner_dropdown_item);
		projects.setAdapter(spinnerArrayAdapter);

		// Output format spinner
		ArrayList<String> outputSpinnerArray = new ArrayList<String>();
		outputSpinnerArray.add("XML");
		outputSpinnerArray.add("CSV");

		spinnerArrayAdapter = new ArrayAdapter<String>(context, android.R.layout.simple_spinner_item, outputSpinnerArray);
		spinnerArrayAdapter.setDropDownViewResource(android.R.layout.simple_spinner_dropdown_item);
		output_format.setAdapter(spinnerArrayAdapter);

		output_format.setOnItemSelectedListener(new AdapterView.OnItemSelectedListener()
		{
			public void onItemSelected(AdapterView<?> parent, View view, int pos, long id)
			{
				String item = (String) parent.getItemAtPosition(pos);

				if(item.equals("XML"))
				{
					xml_mode.setVisibility(View.VISIBLE);
					xml_mode_text.setVisibility(View.VISIBLE);
				}
				else
				{
					xml_mode.setVisibility(View.GONE);
					xml_mode_text.setVisibility(View.GONE);
				}
			}

			public void onNothingSelected(AdapterView<?> parent)
			{
			}
		});

		// XML Mode spinner
		ArrayList<String> xmlSpinnerArray = new ArrayList<String>();
		xmlSpinnerArray.add("Flat tags");
		xmlSpinnerArray.add("String");
		xmlSpinnerArray.add("Nested tags");

		spinnerArrayAdapter = new ArrayAdapter<String>(context, android.R.layout.simple_spinner_item, xmlSpinnerArray);
		spinnerArrayAdapter.setDropDownViewResource(android.R.layout.simple_spinner_dropdown_item);
		xml_mode.setAdapter(spinnerArrayAdapter);
	}

	private void createDataButtons()
	{
		button_from.setOnClickListener(new View.OnClickListener()
		{
			@Override
			public void onClick(View v)
			{
				// Get view
				LayoutInflater li = LayoutInflater.from(context);
				final View timeView = li.inflate(R.layout.export_layout_time, null);

				AlertDialog.Builder alertDialogBuilder = new AlertDialog.Builder(context);

				// set prompts.xml to alertdialog builder
				alertDialogBuilder.setView(timeView);

				// set dialog message
				alertDialogBuilder.setCancelable(false).setPositiveButton("Set", new DialogInterface.OnClickListener()
				{
					public void onClick(DialogInterface dialog, int id)
					{
						DatePicker datepicker = (DatePicker) timeView.findViewById(R.id.datePicker);
						data_range_from.setText(getDateFromDatePicket(datepicker));
					}
				}).setNegativeButton("Cancel", new DialogInterface.OnClickListener()
				{
					public void onClick(DialogInterface dialog, int id)
					{
						data_range_from.setText("");
						dialog.cancel();
					}
				});

				// create alert dialog
				AlertDialog alertDialog = alertDialogBuilder.create();

				// show it
				alertDialog.show();
			}
		});

		button_to.setOnClickListener(new View.OnClickListener()
		{
			@Override
			public void onClick(View v)
			{
				// Get view
				LayoutInflater li = LayoutInflater.from(context);
				final View timeView = li.inflate(R.layout.export_layout_time, null);

				AlertDialog.Builder alertDialogBuilder = new AlertDialog.Builder(context);

				// set prompts.xml to alertdialog builder
				alertDialogBuilder.setView(timeView);

				// set dialog message
				alertDialogBuilder.setCancelable(false).setPositiveButton("Set", new DialogInterface.OnClickListener()
				{
					public void onClick(DialogInterface dialog, int id)
					{
						DatePicker datepicker = (DatePicker) timeView.findViewById(R.id.datePicker);
						data_range_to.setText(getDateFromDatePicket(datepicker));
					}
				}).setNegativeButton("Cancel", new DialogInterface.OnClickListener()
				{
					public void onClick(DialogInterface dialog, int id)
					{
						data_range_to.setText("");
						dialog.cancel();
					}
				});

				// create alert dialog
				AlertDialog alertDialog = alertDialogBuilder.create();

				// show it
				alertDialog.show();
			}
		});

	}

	public static String getDateFromDatePicket(DatePicker datePicker)
	{
		int day = datePicker.getDayOfMonth();
		int month = datePicker.getMonth();
		int year = datePicker.getYear();

		Calendar calendar = Calendar.getInstance();
		calendar.set(year, month, day);

		return calendar.getTime().toString();
	}

	/**
	 * @return the data_range_from
	 */
	public TextView getData_range_from()
	{
		return data_range_from;
	}

	/**
	 * @return the data_range_to
	 */
	public TextView getData_range_to()
	{
		return data_range_to;
	}

	/**
	 * @return the projects
	 */
	public String getProjects()
	{
		return projects.getSelectedItem().toString();
	}

	/**
	 * @return the output_format
	 */
	public String getOutput_format()
	{
		return output_format.getSelectedItem().toString();
	}

	/**
	 * @return the xml_mode
	 */
	public String getXml_mode()
	{
		return xml_mode.getSelectedItem().toString();
	}

	/**
	 * @return the remove_data
	 */
	public boolean getRemove_data()
	{
		return remove_data.isSelected();
	}
}
