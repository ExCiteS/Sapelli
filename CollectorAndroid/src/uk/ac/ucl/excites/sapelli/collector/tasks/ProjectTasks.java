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

package uk.ac.ucl.excites.sapelli.collector.tasks;

import java.io.File;
import java.util.Collections;
import java.util.List;

import org.apache.commons.io.FileUtils;

import android.util.Log;
import uk.ac.ucl.excites.sapelli.collector.R;
import uk.ac.ucl.excites.sapelli.collector.activities.BaseActivity;
import uk.ac.ucl.excites.sapelli.collector.db.ProjectStore;
import uk.ac.ucl.excites.sapelli.collector.model.MediaFile;
import uk.ac.ucl.excites.sapelli.collector.model.Project;
import uk.ac.ucl.excites.sapelli.collector.model.ProjectDescriptor;
import uk.ac.ucl.excites.sapelli.collector.transmission.SchedulingHelpers;
import uk.ac.ucl.excites.sapelli.collector.transmission.SendSchedule;
import uk.ac.ucl.excites.sapelli.collector.util.AsyncTaskWithWaitingDialog;
import uk.ac.ucl.excites.sapelli.collector.util.CollectorAttachmentUtils;
import uk.ac.ucl.excites.sapelli.collector.util.ProjectRunHelpers;
import uk.ac.ucl.excites.sapelli.shared.io.FileHelpers;
import uk.ac.ucl.excites.sapelli.shared.util.ExceptionHelpers;
import uk.ac.ucl.excites.sapelli.storage.model.Record;
import uk.ac.ucl.excites.sapelli.storage.queries.RecordsQuery;
import uk.ac.ucl.excites.sapelli.storage.queries.sources.Source;

/**
 * A collection of async tasks to deal with loading, removed projects and handling their data.
 * 
 * @author mstevens
 */
public final class ProjectTasks
{
	
	private ProjectTasks() { /* should never be instantiated */ }
	
	/**
	 * Runs asynchronous queries to find the records and media files associated with a given Project
	 * 
	 * @param owner
	 * @param project
	 * @param excludeNonExistingFiles whether or not to exclude non-existing MediaFiles
	 * @param callback
	 */
	static public void RunProjectDataQueries(final BaseActivity owner, final Project project, final boolean excludeNonExistingFiles, final ProjectDataCallback callback)
	{
		if(project == null)
			return;
		new RecordsTasks.QueryTask(owner, new RecordsTasks.QueryCallback()
		{
			@SuppressWarnings("unchecked")
			@Override
			public void querySuccess(final List<Record> records)
			{
				// Run media scan:
				new MediaFilesQueryTask(owner, project, excludeNonExistingFiles, new MediaFilesQueryCallback()
				{
					
					@Override
					public void mediaQuerySuccess(List<MediaFile> mediaFiles)
					{
						callback.projectDataQuerySuccess(records, mediaFiles);
					}
					
					@Override
					public void mediaQueryFailure(Exception reason)
					{
						callback.projectDataQueryFailure(reason);
					}
				}).execute(records);
			}
			
			@Override
			public void queryFailure(Exception reason)
			{
				callback.projectDataQueryFailure(reason);
			}
		}).execute(new RecordsQuery(Source.From(project.getModel())));
	}
	
	static public interface ProjectDataCallback
	{
		
		public void projectDataQuerySuccess(List<Record> records, List<MediaFile> mediaFiles);
		
		public void projectDataQueryFailure(Exception reason);
		
	}
	
	/**
	 * Finds media files ("attachments") associated with a given set of records created using a given project or projects. 
	 * 
	 * @author mstevens
	 */
	static public class MediaFilesQueryTask extends AsyncTaskWithWaitingDialog<BaseActivity, List<Record>, List<MediaFile>>
	{

		private final MediaFilesQueryCallback callback;
		private final List<Project> projects;
		private final boolean excludeNonExisting;
		private Exception failure = null;
		
		/**
		 * @param owner
		 * @param project project project used to create the records to find media attachments for
		 * @param excludeNonExisting whether or not to exclude non-existing MediaFiles
		 * @param callback
		 */
		public MediaFilesQueryTask(BaseActivity owner, Project project, boolean excludeNonExisting, MediaFilesQueryCallback callback)
		{
			this(owner, Collections.singletonList(project), excludeNonExisting, callback);
		}
		
		/**
		 * @param owner
		 * @param projects projects used to create the records to find media attachments for
		 * @param excludeNonExisting whether or not to exclude non-existing MediaFiles
		 * @param callback
		 */
		public MediaFilesQueryTask(BaseActivity owner, List<Project> projects, boolean excludeNonExisting, MediaFilesQueryCallback callback)
		{
			super(owner, owner.getString(R.string.mediaScanning));
			this.projects = projects;
			this.excludeNonExisting = excludeNonExisting;
			this.callback = callback;
		}

		@Override
		@SafeVarargs
		protected final List<MediaFile> runInBackground(List<Record>... params)
		{
			List<Record> records = params[0];
			try
			{
				return CollectorAttachmentUtils.getMediaFiles(projects, records, getContext().getFileStorageProvider(), excludeNonExisting); 
			}
			catch(Exception e)
			{
				Log.e(getClass().getName(), ExceptionHelpers.getMessageAndCause(e), e);
				failure = e;
				return Collections.<MediaFile> emptyList();
			}
		}
		
		@Override
		protected void onPostExecute(List<MediaFile> result)
		{
			super.onPostExecute(result); // dismiss dialog
			if(failure != null)
				callback.mediaQueryFailure(failure);
			else
				callback.mediaQuerySuccess(result);
		}
		
	}
	
	public interface MediaFilesQueryCallback
	{
		
		public void mediaQuerySuccess(List<MediaFile> mediaFiles);
		
		public void mediaQueryFailure(Exception reason);
		
	}
	
	static private abstract class ProjectStoreTask<I, O> extends AsyncTaskWithWaitingDialog<BaseActivity, I, O>
	{
		
		protected final ProjectStore projectStore;
		
		@SuppressWarnings("unused")
		public ProjectStoreTask(BaseActivity owner, ProjectStore projectStore)
		{
			this(owner, projectStore, null);
		}
		
		public ProjectStoreTask(BaseActivity owner, ProjectStore projectStore, String waitingMsg)
		{
			super(owner, waitingMsg);
			this.projectStore = projectStore;
		}
		
	}
	
	/**
	 * @author mstevens
	 */
	static public class ReloadProjectTask extends ProjectStoreTask<ProjectDescriptor, Project>
	{
		
		private ReloadProjectCallback callback;
		
		public ReloadProjectTask(BaseActivity owner, ProjectStore projectStore, ReloadProjectCallback callback)
		{
			super(owner, projectStore, owner.getString(R.string.projectLoading));
			this.callback = callback;
		}

		@Override
		protected Project runInBackground(ProjectDescriptor... params)
		{
			return projectStore.retrieveProject(params[0]);
		}

		@Override
		protected void onPostExecute(Project project)
		{
			super.onPostExecute(project); // dismiss dialog
			if(callback != null) // project may be null if task was cancelled
			{
				if(project != null)
					callback.projectReloaded(project);
				else
					callback.projectReloadFailure();
			}
		}

	}
	
	public interface ReloadProjectCallback
	{
		
		public void projectReloaded(Project project);
		
		public void projectReloadFailure();
		
	}
	
	/**
	 * @author mstevens
	 */
	static public class RemoveProjectTask extends ProjectStoreTask<ProjectDescriptor, Void>
	{
	
		private final RemoveProjectCallback callback;
		
		public RemoveProjectTask(BaseActivity owner, ProjectStore projectStore, RemoveProjectCallback callback)
		{
			super(owner, projectStore, owner.getString(R.string.projectRemoving));
			this.callback = callback;
		}

		@Override
		protected Void runInBackground(ProjectDescriptor... params)
		{
			ProjectDescriptor projDescr = params[0];
			if(projDescr != null)
			{
				// Cancel data sending alarms:
				for(SendSchedule schedule : projectStore.retrieveSendSchedulesForProject(projDescr))
					SchedulingHelpers.Cancel(getContext().getApplicationContext(), schedule);
				
				// Remove project from store:
				projectStore.delete(projDescr);
				
				BaseActivity owner = getContext();
				if(owner != null)
				{
					File projectInstallationFolder = owner.getFileStorageProvider().getProjectInstallationFolder(projDescr, false);
					
					// Remove installation folder:
					FileUtils.deleteQuietly(projectInstallationFolder); // this deletes: .../Projects/(projectName)[ (projectVariant)]/v(projectVersion)/
					// Delete parent folder as well if it is empty:
					FileHelpers.deleteDirectoryIfEmpty(projectInstallationFolder.getParentFile());  // this deletes: .../Projects/(projectName)[ (projectVariant)]/
					
					//(projectName)[ (projectVariant)]/v(projectVersion)
					// Remove shortcut:
					ProjectRunHelpers.removeShortcut(owner, projDescr);
				
					// Remove as active project
					if(owner.getPreferences().getActiveProjectSignature().equals(projDescr.getSignatureString()))
						owner.getPreferences().clearActiveProjectSignature();
				}
			}
			return null;
		}

		@Override
		protected void onPostExecute(Void result)
		{
			super.onPostExecute(result); // dismiss dialog
			if(callback != null)
				callback.projectRemoved();
		}

	}
	
	public interface RemoveProjectCallback
	{
		
		public void projectRemoved();
		
	}
	
}
