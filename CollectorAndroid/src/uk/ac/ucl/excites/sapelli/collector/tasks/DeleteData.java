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
import java.util.HashSet;
import java.util.List;
import java.util.Set;

import org.apache.commons.io.FileUtils;

import uk.ac.ucl.excites.sapelli.collector.R;
import uk.ac.ucl.excites.sapelli.collector.activities.BaseActivity;
import uk.ac.ucl.excites.sapelli.collector.model.Project;
import uk.ac.ucl.excites.sapelli.shared.util.ExceptionHelpers;
import uk.ac.ucl.excites.sapelli.storage.model.Record;
import uk.ac.ucl.excites.sapelli.storage.model.Schema;
import uk.ac.ucl.excites.sapelli.storage.queries.Order;
import uk.ac.ucl.excites.sapelli.storage.queries.RecordsQuery;
import uk.ac.ucl.excites.sapelli.storage.queries.Source;

/**
 * Helper class which implements deletion of all records associated with a particular project.
 * 
 * @author mstevens
 */
public class DeleteData implements RecordsTasks.QueryCallback, RecordsTasks.DeleteCallback, MediaTasks.ScanCallback
{
	
	private final BaseActivity owner;
	private Project project;
	
	/**
	 * @param owner
	 */
	public DeleteData(BaseActivity owner)
	{
		this.owner = owner;
	}

	public void deleteFor(Project project)
	{
		this.project = project;
		// Schemas (when list stays empty all records of any schema/project/form will be fetched):
		Set<Schema> schemata = new HashSet<Schema>();
		schemata.addAll(project.getModel().getSchemata());
		// Retrieve by query:
		new RecordsTasks.QueryTask(owner, this).execute(new RecordsQuery(Source.From(schemata), Order.UNDEFINED));
	}
	
	@SuppressWarnings("unchecked")
	@Override
	public void querySuccess(final List<Record> toDelete)
	{
		if(toDelete == null || toDelete.isEmpty())
			owner.showOKDialog(R.string.delete_records_menuitem, R.string.deleteNoRecordsFound);
		else
		{
			// Confirm deletion & delete all records:
			owner.showOKCancelDialog(
				R.string.delete_records_menuitem,
				String.format(owner.getString(R.string.deleteRecordsConfirmation), toDelete.size(), project.toString(false)),
				false,
				new Runnable()
				{
					@Override
					public void run()
					{
						new RecordsTasks.DeleteTask(owner, DeleteData.this).execute(toDelete);
					}
				},
				false);
		}
	}

	@Override
	public void queryFailure(Exception reason)
	{
		owner.showErrorDialog(String.format(owner.getString(R.string.exportQueryFailed), ExceptionHelpers.getMessageAndCause(reason)));
	}

	/* (non-Javadoc)
	 * 
	 * Called upon successful record deletion
	 * 
	 * @see uk.ac.ucl.excites.sapelli.collector.tasks.RecordsTasks.DeleteCallback#deleteSuccess()
	 */
	@SuppressWarnings("unchecked")
	@Override
	public void deleteSuccess(List<Record> deletedRecords)
	{
		// Scan for media attachments:
		new MediaTasks.ScanTask(owner, project, this).execute(deletedRecords);
	}
	
	@Override
	public void deleteFailure(Exception reason)
	{
		owner.showErrorDialog(String.format(owner.getString(R.string.exportDeleteFailureMsg), ExceptionHelpers.getMessageAndCause(reason)));
	}

	@Override
	public void scanSuccess(final List<File> attachments)
	{
		// Ask for confirmation:
		if(!attachments.isEmpty())
			owner.showOKCancelDialog(
				R.string.delete_records_menuitem,
				String.format(owner.getString(R.string.deleteAttachmentsConfirmation), attachments.size()),
				false,
				new Runnable()
				{
					@Override
					public void run()
					{	
						// Delete attachments:
						for(File attachment : attachments)
							FileUtils.deleteQuietly(attachment);
					}
				},
				false);
	}

	@Override
	public void scanFailure(Exception reason)
	{
		// do nothing
	}

}
