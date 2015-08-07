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

package uk.ac.ucl.excites.sapelli.collector.fragments.tabs;

import java.io.File;
import java.util.List;

import org.apache.commons.io.FileUtils;

import android.content.Context;
import android.view.View;
import android.view.View.OnClickListener;
import android.widget.Button;
import android.widget.ImageView;
import android.widget.TextView;
import uk.ac.ucl.excites.sapelli.collector.R;
import uk.ac.ucl.excites.sapelli.collector.activities.ProjectManagerActivity;
import uk.ac.ucl.excites.sapelli.collector.fragments.ProjectManagerTabFragment;
import uk.ac.ucl.excites.sapelli.collector.model.Project;
import uk.ac.ucl.excites.sapelli.collector.tasks.ProjectTasks;
import uk.ac.ucl.excites.sapelli.collector.tasks.RecordsTasks;
import uk.ac.ucl.excites.sapelli.collector.ui.ProjectManagerPagerAdapter;
import uk.ac.ucl.excites.sapelli.collector.util.ProjectRunHelpers;
import uk.ac.ucl.excites.sapelli.shared.util.ExceptionHelpers;
import uk.ac.ucl.excites.sapelli.storage.model.Record;

/**
 * Fragment that defines the project manager layout per project (tabs)
 * 
 * @author Julia, mstevens
 */
public class MainTabFragment extends ProjectManagerTabFragment implements OnClickListener
{
	
	private TextView lblNumberOfRecords;
	private TextView lblNumberOfMediaFiles;
	private Button btnExportData;
	private Button btnDeleteData;
	
	@Override
	protected Integer getLayoutID()
	{
		return R.layout.tab_main;
	}

	@Override
	protected void setupUI(final View rootLayout)
	{
		// Shortcut icon & buttons:
		ImageView imgShortcut = (ImageView) rootLayout.findViewById(R.id.imgShortcut);
		Button btnAddShortcut = (Button) rootLayout.findViewById(R.id.btnAddShortcut);
		btnAddShortcut.setOnClickListener(this);
		Button btnRemoveShortcut = (Button) rootLayout.findViewById(R.id.btnRemoveShortcut);
		btnRemoveShortcut.setOnClickListener(this);
		
		// Collected data labels & buttons:
		lblNumberOfRecords = ((TextView) rootLayout.findViewById(R.id.lblNumberOfRecords));
		lblNumberOfMediaFiles = ((TextView) rootLayout.findViewById(R.id.lblNumberOfMediaFiles));
		(btnExportData = (Button) rootLayout.findViewById(R.id.btnExportData)).setOnClickListener(this);
		(btnDeleteData = (Button) rootLayout.findViewById(R.id.btnDeleteData)).setOnClickListener(this);
		
		final Project project = getProject(false);
		if(project != null)
			// Set shortcut icon:
			imgShortcut.setImageDrawable(ProjectRunHelpers.getShortcutDrawable(getOwner(), getOwner().getCollectorApp().getFileStorageProvider(), project));
		btnAddShortcut.setEnabled(project != null);
		btnRemoveShortcut.setEnabled(project != null);
	}

	/* (non-Javadoc)
	 * @see android.support.v4.app.Fragment#onResume()
	 */
	@Override
	public void onResume()
	{
		super.onResume();
		
		refreshDataStats();
	}
	
	private void refreshDataStats()
	{
		Project project = getProject(false);
		if(getProject(false) == null)
		{	// just in case...
			btnExportData.setEnabled(false);
			btnDeleteData.setEnabled(false);
			return;
		}
		// Update records & media stats:
		ProjectTasks.RunProjectDataQueries(getOwner(), project, new ProjectTasks.ProjectDataCallaback()
		{
			@Override
			public void projectDataQuerySuccess(List<Record> records, List<File> mediaFiles)
			{
				lblNumberOfRecords.setText("" + records.size());
				lblNumberOfMediaFiles.setText("" + mediaFiles.size());
				btnExportData.setEnabled(!records.isEmpty());
				btnDeleteData.setEnabled(!records.isEmpty());
			}
			
			@Override
			public void projectDataQueryFailure(Exception reason)
			{
				lblNumberOfRecords.setText(getString(R.string.error));
				lblNumberOfMediaFiles.setText(getString(R.string.error));
			}
		});
	}
	
	@Override
	public void onClick(View v)
	{
		if(getOwner() == null)
			return;
		Project project = getOwner().getCurrentProject(false);
		if(project != null)
			switch(v.getId())
			{
				case R.id.btnAddShortcut :
					ProjectRunHelpers.createShortcut(getOwner(), getOwner().getFileStorageProvider(), getOwner().getCurrentProject(true));
					break;
				case R.id.btnRemoveShortcut :
					ProjectRunHelpers.removeShortcut(getOwner(), getOwner().getCurrentProject(true));
					break;
				case R.id.btnExportData :
					getOwner().switchToTab(ProjectManagerPagerAdapter.getTabIndex(ExportTabFragment.class));
					break;
				case R.id.btnDeleteData :
					deleteData();
					break;
			}
	}
	
	public void deleteData()
	{
		final ProjectManagerActivity owner = getOwner();
		final Project project = getProject(false);
		if(owner == null || project == null)
			return; // just in case
		ProjectTasks.RunProjectDataQueries(owner, project, new ProjectTasks.ProjectDataCallaback()
		{
			@Override
			public void projectDataQuerySuccess(final List<Record> records, final List<File> mediaFiles)
			{
				// Confirm deletion ...
				owner.showOKCancelDialog(
					R.string.delete_data,
					mediaFiles.isEmpty() ?
						String.format(owner.getString(R.string.deleteRecordsConfirmation), records.size(), project.toString(false)) :
						String.format(owner.getString(R.string.deleteRecordsAndMediaConfirmation), records.size(), mediaFiles.size(), project.toString(false)),
					R.drawable.ic_delete_black_36dp,
					false,
					new Runnable()
					{
						@SuppressWarnings("unchecked")
						@Override
						public void run()
						{
							// Delete records ...
							new RecordsTasks.DeleteTask(owner, new RecordsTasks.DeleteCallback()
							{
								@Override
								public void deleteSuccess(List<Record> deletedRecords)
								{
									// ... and media files ...
									for(File mediaFile : mediaFiles)
										FileUtils.deleteQuietly(mediaFile);
									refreshDataStats();
								}
								
								@Override
								public void deleteFailure(Exception reason)
								{
									owner.showErrorDialog(String.format(owner.getString(R.string.exportDeleteFailureMsg), ExceptionHelpers.getMessageAndCause(reason)));
									refreshDataStats();
								}
							}).execute(records);
						}
					},
					false);
			}
			
			@Override
			public void projectDataQueryFailure(Exception reason)
			{
				refreshDataStats();
			}
		});
	}
	
	@Override
	public String getTabTitle(Context context)
	{
		return context.getString(R.string.tab_main);
	}

}
