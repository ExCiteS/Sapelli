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

package uk.ac.ucl.excites.sapelli.collector.load;

import java.io.File;
import java.io.InputStream;
import java.util.List;

import uk.ac.ucl.excites.sapelli.collector.R;
import uk.ac.ucl.excites.sapelli.collector.db.ProjectStore;
import uk.ac.ucl.excites.sapelli.collector.io.FileStorageException;
import uk.ac.ucl.excites.sapelli.collector.io.FileStorageProvider;
import uk.ac.ucl.excites.sapelli.collector.model.Project;
import uk.ac.ucl.excites.sapelli.collector.util.AsyncTaskWithWaitingDialog;
import uk.ac.ucl.excites.sapelli.shared.io.FileHelpers;
import android.content.Context;

/**
 * @author mstevens
 *
 */
public class AndroidProjectLoaderStorer extends ProjectLoaderStorer
{

	private final Context context;
	
	public AndroidProjectLoaderStorer(Context context, FileStorageProvider fileStorageProvider, ProjectStore projectStore) throws FileStorageException
	{
		super(fileStorageProvider, projectStore, new AndroidPostProcessor(context, fileStorageProvider));
		this.context = context;
	}
	
	/**
	 * Asynchronous loading & storing of a Project from a sapelli file (provided as a File object)
	 * 
	 * @see uk.ac.ucl.excites.sapelli.collector.load.ProjectLoaderStorer#loadAndStore(java.io.File, java.lang.String, uk.ac.ucl.excites.sapelli.collector.load.ProjectLoaderStorer.Callback)
	 */
	@Override
	public void loadAndStore(final File sapelliFile, final String sourceURI, final FileSourceCallback callback)
	{
		// Try opening input stream from file:
		InputStream in;
		try
		{
			in = FileHelpers.openInputStream(sapelliFile, true);
		}
		catch(Exception e)
		{
			callback.projectLoadStoreFailure(sapelliFile, sourceURI, e);
			return;
		}
		// Input stream successfully opened, try loading project:
		new AsyncProjectLoadStoreTask(context, new StreamSourceCallback()
		{
			@Override
			public void projectLoadStoreSuccess(Project project, List<String> warnings)
			{
				callback.projectLoadStoreSuccess(sapelliFile, sourceURI, project, warnings);
			}
			
			@Override
			public void projectLoadStoreFailure(Exception cause)
			{
				callback.projectLoadStoreFailure(sapelliFile, sourceURI, cause);
			}
		}).execute(in);
	}

	/**
	 * Asynchronous loading & storing of a Project from a sapelli file (provided as an InputStream)
	 * 
	 * @see uk.ac.ucl.excites.sapelli.collector.load.ProjectLoaderStorer#loadAndStore(java.io.InputStream, uk.ac.ucl.excites.sapelli.collector.load.ProjectLoaderStorer.StreamSourceCallback)
	 */
	@Override
	public void loadAndStore(InputStream sapelliFileInputStream, StreamSourceCallback callback)
	{
		new AsyncProjectLoadStoreTask(context, callback).execute(sapelliFileInputStream);
	}
	
	/**
	 * @author mstevens
	 */
	private class AsyncProjectLoadStoreTask extends AsyncTaskWithWaitingDialog<Context, InputStream, Project>
	{

		private final StreamSourceCallback callback;
		private Exception failure;
		
		public AsyncProjectLoadStoreTask(Context context, StreamSourceCallback callback)
		{
			super(context, context.getString(R.string.projectLoading));
			this.callback = callback;
		}

		@Override
		protected Project runInBackground(InputStream... params)
		{
			try
			{
				InputStream sapelliFileInputStream;
				if(params.length < 1 || (sapelliFileInputStream = params[0]) == null)
					throw new NullPointerException("Provide a non-null InputStream.");
				return AndroidProjectLoaderStorer.super.loadAndStore(sapelliFileInputStream);
			}
			catch(Exception e)
			{
				failure = e;
				return null;
			}
		}

		@Override
		protected void onPostExecute(Project project)
		{
			// Hide dialog:
			super.onPostExecute(project);
			// Report back if needed:
			if(callback == null)
				return;
			else if(project == null || failure != null)
				callback.projectLoadStoreFailure(failure); // report failure
			else
				callback.projectLoadStoreSuccess(project, loader.getWarnings()); // report success
		}

	}
	
}
