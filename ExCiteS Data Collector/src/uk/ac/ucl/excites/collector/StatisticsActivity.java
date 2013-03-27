/**
 * 
 */
package uk.ac.ucl.excites.collector;

/**
 * @author Julia
 * 
 */
import java.io.BufferedReader;
import java.io.File;
import java.io.FileNotFoundException;
import java.io.FileReader;
import java.io.IOException;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import uk.ac.ucl.excites.CollectorApp;
import uk.ac.ucl.excites.collector.project.db.DataAccess;
import uk.ac.ucl.excites.collector.project.model.Form;
import uk.ac.ucl.excites.collector.project.model.Project;
import uk.ac.ucl.excites.storage.model.Record;
import android.app.ExpandableListActivity;
import android.os.Bundle;
import android.view.Gravity;
import android.view.LayoutInflater;
import android.view.View;
import android.widget.ExpandableListView;
import android.widget.PopupWindow;
import android.widget.RelativeLayout.LayoutParams;
import android.widget.SimpleExpandableListAdapter;
import android.widget.TextView;
import android.widget.Toast;

public class StatisticsActivity extends ExpandableListActivity
{

	private static final String KeyGROUP = "Project";
	private static final String KeyCHILD_LABEL = "ChildLabel";
	private static final String KeyCHILD = "FormOrLogChild";
	
	List<Project> projects;
	PopupWindow popupWindow;

	private ArrayList<Map<String, String>> groupContent = new ArrayList<Map<String, String>>();
	private ArrayList<ArrayList<Map<String, ?>>> childContent = new ArrayList<ArrayList<Map<String, ?>>>();

	public void onCreate(Bundle savedInstanceState)
	{

		super.onCreate(savedInstanceState);
		setContentView(R.layout.activity_statistics);

	}
	
	@Override
	public void onResume()
	{
		super.onResume();
		
		//Populate UI:
		DataAccess dao = ((CollectorApp) getApplication()).getDatabaseInstance();
		try
		{
			projects = new ArrayList<Project>(dao.retrieveProjects());
			List<Form> forms;
			List<Record> records;
			for(Project project : projects)
			{
				forms = project.getForms();
				for(Form form : forms)
				{
					records = dao.retrieveRecords(form.getSchema());
					Map<String, String> statisticsMap = new HashMap<String, String>();
					String statistics = "Project: " + project.getName() + "\n" + form.getName() + ": " + records.size() + " records stored";
					statisticsMap.put(KeyGROUP, statistics);
					groupContent.add(statisticsMap);
					childContent.add(listFormsAndLogFiles(dao, project));
				}
			}
		}
		catch(IOException e)
		{
			e.printStackTrace();
			return;
		}

		// define adapter for ListView
		SimpleExpandableListAdapter expListAdapter = new SimpleExpandableListAdapter(this, groupContent, // groupContent describes first-level entries
				R.layout.expandable_list_item, // Layout for first-level entries
				new String[] { KeyGROUP }, // Key in groupData maps to display
				new int[] { android.R.id.text1 }, // Data under the key goes into this TextView
				childContent, // childContent describes second-level entries
				R.layout.expandable_list_child_item, // Layout for second-level entries
				new String[] { KeyCHILD_LABEL }, // Key in childData maps to display
				new int[] { android.R.id.text1 } // Data under the keys above go into this TextView
		);
		setListAdapter(expListAdapter); // setting the adapter in the list.
	}

	/**
	 * Generates List of log files per project
	 * 
	 * @param path
	 *            of log files
	 * @return list of log file names
	 * @throws IOException 
	 */
	private ArrayList<Map<String, ?>> listFormsAndLogFiles(DataAccess dao, Project project) throws IOException
	{
		ArrayList<Map<String, ?>> children = new ArrayList<Map<String, ?>>();
		//Forms:
		for(Form f : project.getForms())
		{
			List<Record> records = new ArrayList<Record>(dao.retrieveRecords(f.getSchema()));
			if(!records.isEmpty())
			{
				Map<String, Object> formMap = new HashMap<String, Object>();
				FormChild c = new FormChild(f, records);
				formMap.put(KeyCHILD_LABEL, c.toString());
				formMap.put(KeyCHILD, c);
				children.add(formMap);
			}
		}
		//Log files:
		String path = project.getLogFolderPath();
		File folder = new File(path);
		File[] listOfFiles;
		
		if(folder.listFiles() != null)
		{
			listOfFiles = folder.listFiles();
			for(int i = 0; i < listOfFiles.length; i++)
			{
				if(listOfFiles[i].isFile())
				{
					Map<String, Object> logMap = new HashMap<String, Object>();
					Child c = new LogChild(listOfFiles[i].getName());
					logMap.put(KeyCHILD_LABEL, c.toString());
					logMap.put(KeyCHILD, c);
					children.add(logMap);
				}
			}
		}
		return children;
	}

	// show log on child click
	public boolean onChildClick(ExpandableListView parent, View v, int groupPosition, int childPosition, long id)
	{
		try
		{
			Child clickedChild = (Child) childContent.get(groupPosition).get(childPosition).get(KeyCHILD);
			if(clickedChild instanceof LogChild)				
				displayText(readFile(projects.get(groupPosition).getLogFolderPath() + ((LogChild) clickedChild).filename));
			else if (clickedChild instanceof FormChild)
			{
				String text = "";
				for(Record r : ((FormChild) clickedChild).getRecords())
				{
					text += r.toString() +"\n\n";	
				}
				displayText(text);
			}
		
		}
		catch(FileNotFoundException e)
		{
			e.printStackTrace();
		}
		catch(IOException e)
		{
			e.printStackTrace();
		}
		return true;
	}

	
	/**
	 * @param text
	 * @throws FileNotFoundException
	 */
	public void displayText(String text) throws FileNotFoundException
	{
		LayoutInflater layoutInflater = (LayoutInflater) getBaseContext().getSystemService(LAYOUT_INFLATER_SERVICE);
		View popupView = layoutInflater.inflate(R.layout.show_log, null);
		((TextView) popupView.findViewById(R.id.LogTextView)).setText(text);
		popupWindow = new PopupWindow(popupView, LayoutParams.MATCH_PARENT, LayoutParams.MATCH_PARENT);
		popupWindow.showAtLocation(this.findViewById(R.id.StatisticsView), Gravity.CENTER, 0, 0);
	}

	/**
	 * Reads text from file
	 * 
	 * @param path
	 * @return String containing text from file
	 * @throws FileNotFoundException
	 */
	private String readFile(String path) throws FileNotFoundException
	{
		StringBuilder text = new StringBuilder();
		try
		{
			BufferedReader br = new BufferedReader(new FileReader(path));
			String line;

			while((line = br.readLine()) != null)
			{
				text.append(line);
				text.append('\n');
			}
			br.close();
		}
		catch(IOException e)
		{
			Toast.makeText(getBaseContext(), "Failed to read file", Toast.LENGTH_SHORT).show();
		}
		return text.toString();
	}

	//	gets called when pop up window is dismissed
	public void dismiss(View view)
	{
		popupWindow.dismiss();
	}
	
	public class Child
	{
		
	}
	
	public class LogChild extends Child
	{
		
		private String filename;
		
		public LogChild(String filename)
		{
			this.filename = filename;
		}
		
		@Override
		public String toString()
		{
			return filename;
		}
	}
	
	public class FormChild extends Child
	{
		
		private Form form;
		private List<Record> records;
		
		public FormChild(Form form, List<Record> records)
		{
			this.form = form;
			this.records = records;
		}
		
		@Override
		public String toString()
		{
			return form.getName() + " records";
		}

		public List<Record> getRecords()
		{
			return records;
		}
		
	}

}
