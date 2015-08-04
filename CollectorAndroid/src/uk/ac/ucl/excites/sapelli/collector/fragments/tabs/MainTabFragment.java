package uk.ac.ucl.excites.sapelli.collector.fragments.tabs;

import java.io.File;
import java.util.List;

import android.content.Context;
import android.view.View;
import android.view.View.OnClickListener;
import android.widget.Button;
import android.widget.ImageView;
import android.widget.TextView;
import uk.ac.ucl.excites.sapelli.collector.R;
import uk.ac.ucl.excites.sapelli.collector.fragments.ProjectManagerTabFragment;
import uk.ac.ucl.excites.sapelli.collector.model.Project;
import uk.ac.ucl.excites.sapelli.collector.tasks.MediaTasks;
import uk.ac.ucl.excites.sapelli.collector.tasks.RecordsTasks;
import uk.ac.ucl.excites.sapelli.collector.util.ProjectRunHelpers;
import uk.ac.ucl.excites.sapelli.storage.model.Record;
import uk.ac.ucl.excites.sapelli.storage.queries.RecordsQuery;
import uk.ac.ucl.excites.sapelli.storage.queries.Source;

/**
 * Fragment that defines the project manager layout per project (tabs)
 * 
 * @author Julia, mstevens
 */
public class MainTabFragment extends ProjectManagerTabFragment implements OnClickListener
{
	
	private TextView lblNumberOfRecords;
	private TextView lblNumberOfMediaFiles;

	public static MainTabFragment newInstance()
	{
		MainTabFragment f = new MainTabFragment();
		return f;
	}
	
	@Override
	protected Integer getLayoutID()
	{
		return R.layout.tab_main;
	}

	@Override
	protected void setupUI(final View rootLayout)
	{
		((Button) rootLayout.findViewById(R.id.addShortcut)).setOnClickListener(this);
		((Button) rootLayout.findViewById(R.id.removeShortcut)).setOnClickListener(this);
		//	Set shortcut icon:
		final Project project = getProject();
		if(project != null)
			((ImageView) rootLayout.findViewById(R.id.imgShortcut)).setImageDrawable(ProjectRunHelpers.getShortcutDrawable(getOwner(), getOwner().getCollectorApp().getFileStorageProvider(), project));
		lblNumberOfRecords = ((TextView) rootLayout.findViewById(R.id.lblNumberOfRecords));
		lblNumberOfMediaFiles = ((TextView) rootLayout.findViewById(R.id.lblNumberOfMediaFiles));
		
		/*// Shortcut switch:
		Switch shortcutSwitch = (Switch) ((ViewGroup) rootLayout).findViewById(R.id.btn_shortcut);
		shortcutSwitch.setOnCheckedChangeListener(new CompoundButton.OnCheckedChangeListener()
		{
			@Override
			public void onCheckedChanged(CompoundButton buttonView, boolean isChecked)
			{
				if(activity == null)
					return;
				if(isChecked)
					activity.createShortcut();
				else
					activity.removeShortcut();
			}
		 });*/
	}

	/* (non-Javadoc)
	 * @see android.support.v4.app.Fragment#onResume()
	 */
	@Override
	public void onResume()
	{
		super.onResume();
		
		if(getProject() == null)
			return; // just in case
		
		// Update records & media stats:
		new RecordsTasks.QueryTask(getOwner(), new RecordsTasks.QueryCallback()
		{
			@SuppressWarnings("unchecked")
			@Override
			public void querySuccess(List<Record> result)
			{
				lblNumberOfRecords.setText("" + result.size());
				// Run media scan:
				new MediaTasks.ScanTask(getOwner(), getProject(), new MediaTasks.ScanCallback()
				{
					
					@Override
					public void scanSuccess(List<File> mediaFiles)
					{
						lblNumberOfMediaFiles.setText("" + mediaFiles.size());
					}
					
					@Override
					public void scanFailure(Exception reason)
					{
						lblNumberOfMediaFiles.setText(getString(R.string.error));
					}
				}).execute(result);
			}
			
			@Override
			public void queryFailure(Exception reason)
			{
				lblNumberOfRecords.setText(getString(R.string.error));
			}
		}).execute(new RecordsQuery(Source.From(getProject().getModel().getSchemata())));
	}

	@Override
	public void onClick(View v)
	{
		if(getOwner() != null)
			switch(v.getId())
			{
				case R.id.addShortcut:
					getOwner().createShortcut();
					break;
				case R.id.removeShortcut:
					getOwner().removeShortcut();
					break;
			}
	}

	@Override
	public String getTabTitle(Context context)
	{
		return context.getString(R.string.tab_main);
	}

}
