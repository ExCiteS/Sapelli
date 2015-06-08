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

import uk.ac.ucl.excites.sapelli.collector.R;
import uk.ac.ucl.excites.sapelli.collector.activities.BaseActivity;
import uk.ac.ucl.excites.sapelli.collector.tasks.RecordsTasks;
import uk.ac.ucl.excites.sapelli.collector.tasks.RecordsTasks.ExportCallback;
import uk.ac.ucl.excites.sapelli.storage.eximport.Exporter.Format;
import uk.ac.ucl.excites.sapelli.storage.eximport.csv.CSVRecordsExporter;
import uk.ac.ucl.excites.sapelli.storage.eximport.csv.CSVRecordsExporter.Separator;
import uk.ac.ucl.excites.sapelli.storage.eximport.xml.XMLRecordsExporter;
import uk.ac.ucl.excites.sapelli.storage.eximport.xml.XMLRecordsExporter.CompositeMode;
import uk.ac.ucl.excites.sapelli.storage.model.Record;
import android.os.Bundle;
import android.support.v4.app.Fragment;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;
import android.widget.AdapterView;
import android.widget.AdapterView.OnItemSelectedListener;
import android.widget.ArrayAdapter;
import android.widget.LinearLayout;
import android.widget.Spinner;

/**
 * @author mstevens
 *
 */
public class ExportFormatFragment extends Fragment
{
	
	static public final Format DEFAULT_FORMAT = Format.CSV;
	
	private Spinner spinOutputFormat;
	private Spinner spinXMLMode;
	private Spinner spinCSVSeparator;
	
	@Override
	public View onCreateView(LayoutInflater inflater, ViewGroup container, Bundle savedInstanceState)
	{
		// Inflate the layout for this fragment:
		LinearLayout rootLayout = (LinearLayout) inflater.inflate(R.layout.fragment_export_format, container, false);
		
		// Output formats:
		spinOutputFormat = (Spinner) rootLayout.findViewById(R.id.spinExportFormat);
		final ArrayAdapter<Format> formatAdapter = new ArrayAdapter<Format>(inflater.getContext(), android.R.layout.simple_spinner_item, Format.values());
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
		
		// XML options:
		spinXMLMode = (Spinner) rootLayout.findViewById(R.id.spinXMLMode);
		ArrayAdapter<CompositeMode> xmlModeAdapter = new ArrayAdapter<CompositeMode>(inflater.getContext(), android.R.layout.simple_spinner_item, CompositeMode.values());
		xmlModeAdapter.setDropDownViewResource(android.R.layout.simple_spinner_dropdown_item);
		spinXMLMode.setAdapter(xmlModeAdapter);
		spinXMLMode.setSelection(xmlModeAdapter.getPosition(XMLRecordsExporter.DEFAULT_COMPOSITE_MODE));
		
		// CSV options:
		spinCSVSeparator = (Spinner) rootLayout.findViewById(R.id.spinCSVSeparator);
		ArrayAdapter<Separator> csvModeAdapter = new ArrayAdapter<Separator>(inflater.getContext(), android.R.layout.simple_spinner_item, Separator.values());
		csvModeAdapter.setDropDownViewResource(android.R.layout.simple_spinner_dropdown_item);
		spinCSVSeparator.setAdapter(csvModeAdapter);
		spinCSVSeparator.setSelection(csvModeAdapter.getPosition(CSVRecordsExporter.DEFAULT_SEPARATOR));
		
		// Return view:
		return rootLayout;
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
	
	@SuppressWarnings("unchecked")
	public void runExportTask(List<Record> records, BaseActivity owner, File exportFolder, String exportDesc, ExportCallback callback)
	{
		switch(getSelectedFormat())
		{
			case CSV:
				new RecordsTasks.CSVExportTask(owner, exportFolder, getCSVSeparator(), exportDesc, callback).execute(records);
				break;
			case XML:
				new RecordsTasks.XMLExportTask(owner, exportFolder, getXMLCompositeMode(), exportDesc, callback).execute(records);
				break;
			default:
				throw new IllegalStateException("Unknown export format: " + getSelectedFormat().toString());
		}
	}
	
	@Override
	public void onDestroyView()
	{
		super.onDestroyView();
		if(!getActivity().isFinishing())
			forget();
	}

	/**
	 * Call to avoid duplicate id exception before the fragment (or rather the <fragment> XML that loads is)
	 * is inflated a second time within the lifetime of an activity.
	 */
	public void forget()
	{
		Fragment fragment = getFragmentManager().findFragmentById(getId());
		if(fragment != null)
			getFragmentManager().beginTransaction().remove(fragment).commit();
	}
	
}