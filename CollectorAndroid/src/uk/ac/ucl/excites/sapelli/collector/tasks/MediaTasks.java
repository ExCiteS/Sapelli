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
import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import android.util.Log;
import uk.ac.ucl.excites.sapelli.collector.R;
import uk.ac.ucl.excites.sapelli.collector.activities.BaseActivity;
import uk.ac.ucl.excites.sapelli.collector.io.FileStorageProvider;
import uk.ac.ucl.excites.sapelli.collector.model.Field;
import uk.ac.ucl.excites.sapelli.collector.model.Form;
import uk.ac.ucl.excites.sapelli.collector.model.Project;
import uk.ac.ucl.excites.sapelli.collector.model.fields.MediaField;
import uk.ac.ucl.excites.sapelli.collector.util.AsyncTaskWithWaitingDialog;
import uk.ac.ucl.excites.sapelli.shared.db.StoreHandle.StoreUser;
import uk.ac.ucl.excites.sapelli.shared.util.ExceptionHelpers;
import uk.ac.ucl.excites.sapelli.storage.model.Record;
import uk.ac.ucl.excites.sapelli.storage.model.Schema;

/**
 * @author mstevens
 *
 */
public final class MediaTasks
{
	
	private MediaTasks() { /* should never be instantiated */ }
	
	/**
	 * Finds media files ("attachments") associated with a given set of records created using a given project or projects. 
	 * 
	 * @author mstevens
	 */
	static public class ScanTask extends AsyncTaskWithWaitingDialog<List<Record>, List<File>> implements StoreUser
	{

		private final BaseActivity owner;
		private final ScanCallback callback;
		private final Map<Schema, Form> schema2Form;
		private Exception failure = null;
		
		/**
		 * @param owner
		 * @param project project used to create the records to find media attachments for
		 * @param callback
		 */
		public ScanTask(BaseActivity owner, Project project, ScanCallback callback)
		{
			this(owner, Collections.singletonList(project), callback);
		}
		
		/**
		 * @param owner
		 * @param projects projects used to create the records to find media attachments for
		 * @param callback
		 */
		public ScanTask(BaseActivity owner, List<Project> projects, ScanCallback callback)
		{
			super(owner, owner.getString(R.string.mediaScanning));
			this.owner = owner;
			this.callback = callback;

			// Populate schema->form map:
			this.schema2Form = new HashMap<Schema, Form>();
			for(Project project : projects)
				for(Form form : project.getForms())
					schema2Form.put(form.getSchema(), form);
		}

		@Override
		protected List<File> doInBackground(List<Record>... params)
		{
			List<Record> records = params[0];
			try
			{
				// Group records by form:
				Map<Form, List<Record>> recordsByForm = new HashMap<Form, List<Record>>();
				for(Record r : records)
				{
					Form form = schema2Form.get(r.getSchema());
					if(form == null)
						continue;
					List<Record> formRecs;
					if(!recordsByForm.containsKey(form))
						recordsByForm.put(form, formRecs = new ArrayList<Record>());
					else
						formRecs = recordsByForm.get(form);
					formRecs.add(r);
				}
				// Scan for attachments:
				final List<File> attachments = new ArrayList<File>();		
				FileStorageProvider fileSP = owner.getCollectorApp().getFileStorageProvider();
				for(Form form : recordsByForm.keySet())
					for(Record record : recordsByForm.get(form))
						for(Field field : form.getFields())
							if(field instanceof MediaField)
							{
								MediaField mf = (MediaField) field;
								for(int i = 0; i < mf.getCount(record); i++)
								{
									File attachment = mf.getMediaFile(fileSP, record, i);
									if(attachment.exists())
										attachments.add(attachment);
								}
							}
				return attachments;
			}
			catch(Exception e)
			{
				e.printStackTrace(System.err);
				Log.d(getClass().getName(), ExceptionHelpers.getMessageAndCause(e));
				failure = e;
				return Collections.<File> emptyList();
			}
		}
		
		@Override
		protected void onPostExecute(List<File> result)
		{
			super.onPostExecute(result); // dismiss dialog
			if(failure != null)
				callback.scanFailure(failure);
			else
				callback.scanSuccess(result);
		}
		
	}
	
	public interface ScanCallback
	{
		
		public void scanSuccess(List<File> mediaFiles);
		
		public void scanFailure(Exception reason);
		
	}
	
}
