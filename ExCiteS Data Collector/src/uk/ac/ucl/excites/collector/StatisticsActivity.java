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

	static public final String PARAMETER_DB_FOLDER_PATH = "DBFolderPath";
	private static final String KeyGROUP = "Project";
	private static final String KeyCHILD = "Log";

	private String dbFolderPath;
	private DataAccess dao;

	List<Project> projects;
	PopupWindow popupWindow;

	private ArrayList<Map<String, String>> groupContent = new ArrayList<Map<String, String>>();
	private ArrayList<ArrayList<Map<String, String>>> childContent = new ArrayList<ArrayList<Map<String, String>>>();

	public void onCreate(Bundle savedInstanceState)
	{

		super.onCreate(savedInstanceState);
		setContentView(R.layout.activity_statistics);

		// get db instance
		dbFolderPath = getIntent().getExtras().getString(PARAMETER_DB_FOLDER_PATH);
		dao = DataAccess.getInstance(dbFolderPath);

		try
		{
			createLists(); // generate content for expandable lists
		}
		catch(IOException e)
		{
			e.printStackTrace();
		}

		// define adapter for ListView
		SimpleExpandableListAdapter expListAdapter = new SimpleExpandableListAdapter(this, groupContent, // groupContent describes first-level entries
				R.layout.expandable_list_item, // Layout for first-level entries
				new String[] { KeyGROUP }, // Key in groupData maps to display
				new int[] { android.R.id.text1 }, // Data under the key goes into this TextView
				childContent, // childContent describes second-level entries
				R.layout.expandable_list_child_item, // Layout for second-level entries
				new String[] { KeyCHILD }, // Key in childData maps to display
				new int[] { android.R.id.text1 } // Data under the keys above go into this TextView
		);
		setListAdapter(expListAdapter); // setting the adapter in the list.

	}

	/**
	 * Generates content to show in expandable list
	 * 
	 * @throws IOException
	 */
	private void createLists() throws IOException
	{
		projects = dao.retrieveProjects();
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
				ArrayList<Map<String, String>> children = listLogFiles(project.getLogFolderPath());
				childContent.add(children);
			}
		}
	}

	/**
	 * Generates List of log files per project
	 * 
	 * @param path
	 *            of log files
	 * @return list of log file names
	 */
	private ArrayList<Map<String, String>> listLogFiles(String path)
	{
		ArrayList<Map<String, String>> children = new ArrayList<Map<String, String>>();
		File folder = new File(path);
		File[] listOfFiles;
		if(folder.listFiles() != null)
		{
			listOfFiles = folder.listFiles();

			for(int i = 0; i < listOfFiles.length; i++)
			{
				if(listOfFiles[i].isFile())
				{
					String fileName = listOfFiles[i].getName();
					Map<String, String> logMap = new HashMap<String, String>();
					logMap.put(KeyCHILD, fileName);
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
			showLog(projects.get(groupPosition).getLogFolderPath() + childContent.get(groupPosition).get(childPosition).get(KeyCHILD));
		}
		catch(FileNotFoundException e)
		{
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
		catch(IOException e)
		{
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
		return true;
	}

	
	/**
	 * @param path
	 * @throws FileNotFoundException
	 */
	public void showLog(String path) throws FileNotFoundException
	{

		LayoutInflater layoutInflater = (LayoutInflater) getBaseContext().getSystemService(LAYOUT_INFLATER_SERVICE);
		View popupView = layoutInflater.inflate(R.layout.show_log, null);
		((TextView) popupView.findViewById(R.id.LogTextView)).setText(readFile(path));
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

	@Override
	protected void onPause()
	{
		// close database
		super.onPause();
		if(dao != null)
			dao.closeDB();

	}
}
