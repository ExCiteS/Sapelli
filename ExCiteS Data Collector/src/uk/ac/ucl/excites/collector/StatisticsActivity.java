/**
 * 
 */
package uk.ac.ucl.excites.collector;

import java.util.List;

import uk.ac.ucl.excites.collector.project.db.DataAccess;
import uk.ac.ucl.excites.collector.project.model.Form;
import uk.ac.ucl.excites.collector.project.model.Project;
import uk.ac.ucl.excites.storage.model.Record;
import uk.ac.ucl.excites.storage.model.Schema;
import android.app.Activity;
import android.os.Bundle;

/**
 * @author Julia
 * 
 */
public class StatisticsActivity extends Activity
{
	static public final String PARAMETER_DB_FOLDER_PATH = "DBFolderPath";

	private String dbFolderPath;

	private DataAccess dao;

	@Override
	protected void onCreate(Bundle savedInstanceState)
	{
		super.onCreate(savedInstanceState);
		setContentView(R.layout.activity_statistics);

		Bundle extras = getIntent().getExtras();
		dbFolderPath = extras.getString(PARAMETER_DB_FOLDER_PATH);

		// Get DataAccess object
		dao = DataAccess.getInstance(dbFolderPath);

		displayStoredRecords();

	}

	public void displayStoredRecords()
	{
		List<Project> projects = dao.retrieveProjects();
		List<Form> forms;
		List<Record> records;

		for(Project project : projects)
		{
			forms = project.getForms();

			for(Form form : forms)
			{
				records = dao.retrieveRecords(form.getSchema());
				System.out.println("Project: " + project + "\n" + form.getName() + ": " + records.size() + " records stored");
			}
		}
	}

	@Override
	protected void onPause()
	{
		// close database
		super.onPause();
		if(dao != null)
			dao.closeDB();
	}

	@Override
	protected void onResume()
	{
		// open database
		super.onResume();
		if(dao != null)
		{
			dao.openDB();
		}
	}

}
