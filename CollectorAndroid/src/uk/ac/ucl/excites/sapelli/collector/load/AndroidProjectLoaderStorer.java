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

import uk.ac.ucl.excites.sapelli.collector.R;
import uk.ac.ucl.excites.sapelli.collector.db.ProjectStore;
import uk.ac.ucl.excites.sapelli.collector.io.FileStorageException;
import uk.ac.ucl.excites.sapelli.collector.io.FileStorageProvider;
import uk.ac.ucl.excites.sapelli.collector.model.Project;
import uk.ac.ucl.excites.sapelli.collector.util.AsyncTaskWithWaitingDialog;
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
		super(fileStorageProvider, projectStore, new AndroidPostProcessor());
		this.context = context;
	}
	
	/**
	 * Asynchronous loading & storing of a Project from a sapelli file (provided as a File object)
	 * 
	 * @see uk.ac.ucl.excites.sapelli.collector.load.ProjectLoaderStorer#loadAndStore(java.io.File, java.lang.String, uk.ac.ucl.excites.sapelli.collector.load.ProjectLoaderStorer.Callback)
	 */
	@Override
	public void loadAndStore(File sapelliFile, String sourceURI, FileSourceCallback callback)
	{
		new AsyncProjectLoadStoreTask(context, sapelliFile, sourceURI, callback).execute();
	}

	/**
	 * Asynchronous loading & storing of a Project from a sapelli file (provided as an InputStream)
	 * 
	 * @see uk.ac.ucl.excites.sapelli.collector.load.ProjectLoaderStorer#loadAndStore(java.io.InputStream, uk.ac.ucl.excites.sapelli.collector.load.ProjectLoaderStorer.StreamSourceCallback)
	 */
	@Override
	public void loadAndStore(InputStream sapelliFileInputStream, StreamSourceCallback callback)
	{
		new AsyncProjectLoadStoreTask(context, sapelliFileInputStream, callback).execute();
	}

	private class AsyncProjectLoadStoreTask extends AsyncTaskWithWaitingDialog<Void, Void, Project>
	{

		private File sapelliFile;
		private String sourceURI;
		private Callback callback;
		private final InputStream sapelliFileInputStream;
		
		private Exception failure;
		
		public AsyncProjectLoadStoreTask(Context context, File sapelliFile, String sourceURI, FileSourceCallback callback)
		{
			super(context, context.getString(R.string.projectLoading));
			this.sapelliFile = sapelliFile;
			this.sourceURI = sourceURI;
			this.callback = callback;
			InputStream in = null;
			try
			{
				in = ProjectLoader.openStream(sapelliFile);
			}
			catch(Exception e)
			{
				failure = e;
			}
			sapelliFileInputStream = in;
		}
		
		public AsyncProjectLoadStoreTask(Context context, InputStream sapelliFileInputStream, StreamSourceCallback callback)
		{
			super(context, context.getString(R.string.projectLoading));
			this.sapelliFileInputStream = sapelliFileInputStream;
			this.callback = callback;
		}

		@Override
		protected Project doInBackground(Void... params)
		{
			if(failure != null || sapelliFileInputStream == null)
				return null;
			try
			{
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
			if(callback == null)
				return;
			if(callback instanceof FileSourceCallback)
			{
				if(project == null || failure != null)
					// Report failure:
					((FileSourceCallback) callback).projectLoadStoreFailure(sapelliFile, sourceURI, failure);
				else
					// Report success:
					((FileSourceCallback) callback).projectLoadStoreSuccess(sapelliFile, sourceURI, project, loader.getWarnings());
			}
			else if(callback instanceof StreamSourceCallback)
			{
				if(project == null || failure != null)
					// Report failure:
					((StreamSourceCallback) callback).projectLoadStoreFailure(failure);
				else
					// Report success:
					((StreamSourceCallback) callback).projectLoadStoreSuccess(project, loader.getWarnings());
			}
		}
		
	}
	
}
