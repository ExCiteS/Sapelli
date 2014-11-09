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

package uk.ac.ucl.excites.sapelli.collector.ui.dialogs;

import java.io.File;
import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

import org.apache.commons.lang3.ArrayUtils;

import uk.ac.ucl.excites.sapelli.collector.R;
import uk.ac.ucl.excites.sapelli.collector.io.FileStorageProvider;
import uk.ac.ucl.excites.sapelli.collector.io.FileStorageProvider.Folder;
import uk.ac.ucl.excites.sapelli.collector.util.AsyncTaskWithWaitingDialog;
import uk.ac.ucl.excites.sapelli.shared.io.Zipper;
import uk.ac.ucl.excites.sapelli.util.Debug;
import android.app.AlertDialog;
import android.app.Dialog;
import android.content.Context;
import android.content.DialogInterface;
import android.content.Intent;
import android.net.Uri;

/**
 * @author Michalis Vitos, mstevens
 *
 */
public class BackupDialogBuilder
{

	static public final Folder[] BACKUPABLE_FOLDERS = { Folder.Attachments, Folder.Crashes, Folder.Export, Folder.Logs, Folder.Projects };
	
	static private String getFolderString(Context context, Folder folder)
	{
		switch(folder)
		{
			case Crashes:
				return context.getString(R.string.folderCrashes);
			case Export:
				return context.getString(R.string.folderExports);
			case Logs:
				return context.getString(R.string.folderLogs);
			case Attachments:
				return context.getString(R.string.folderAttachments);
			case Projects:
				return context.getString(R.string.folderProjects);
			// Not back-upable:
			case Downloads:
			case Temp:
			default:
				throw new IllegalArgumentException("This folder (" + folder.name() + ") cannot be backed-up!");
		}
	}
	
	static private boolean isFolderDefaultSelected(Folder folder)
	{
		switch(folder)
		{
			case Crashes:
			case Export:
			case Logs:
				return true;
			case Attachments:
			case Projects:
				return false;
			// Not back-upable:
			case Downloads:
			case Temp:
			default:
				throw new IllegalArgumentException("This folder (" + folder.name() + ") cannot be backed-up!");
		}
	}
	
	public static Dialog Build(final FileStorageProvider fileStorageProvider, final Context context)
	{
		// Initialise:
		final Set<Folder> foldersToExport = new HashSet<Folder>();
		List<String> checkboxItems = new ArrayList<String>();
		List<Boolean> checkedItems = new ArrayList<Boolean>();
		for(Folder folder : BACKUPABLE_FOLDERS)
		{
			checkboxItems.add(getFolderString(context, folder));
			boolean selected = isFolderDefaultSelected(folder);
			checkedItems.add(selected);
			if(selected)
				foldersToExport.add(folder);
		}
		
		// Create the dialog:
		AlertDialog.Builder builder = new AlertDialog.Builder(context);
		// Set the dialog title
		builder.setTitle(R.string.selectForBackup)
		// Specify the list array, the items to be selected by default (null for none),
		// and the listener through which to receive callbacks when items are selected
				.setMultiChoiceItems(
				// Transform checkboxItems to a CharSequence[]
				checkboxItems.toArray(new CharSequence[checkboxItems.size()]),
				// Transform checkedItems to a boolean[]
				ArrayUtils.toPrimitive(checkedItems.toArray(new Boolean[checkedItems.size()])),
				new DialogInterface.OnMultiChoiceClickListener()
				{
					@Override
					public void onClick(DialogInterface dialog, int which, boolean isChecked)
					{
						if(isChecked)
							// If the user checked the item, add it to the selected items:
							foldersToExport.add(BACKUPABLE_FOLDERS[which]);
						else
							// Else, if the item is already in the array, remove it:
							foldersToExport.remove(BACKUPABLE_FOLDERS[which]);
					}
				})
				// Set the action buttons
				.setPositiveButton(android.R.string.ok, new DialogInterface.OnClickListener()
				{
					@Override
					public void onClick(DialogInterface dialog, int id)
					{
						// Get file paths for the selected items from FileStorageProvider
						List<File> toZip = new ArrayList<File>();
						for(Folder folder : foldersToExport)
							toZip.add(fileStorageProvider.getFolder(folder, false));
						
						// Zip everything with AsyncZipper (which will also back-up the database and include it in the zip):
						new AsyncZipper(context,
										context.getString(R.string.backing_up),
										toZip,
										fileStorageProvider.getBackupFile()).execute();
							
					}
				}).setNegativeButton(android.R.string.cancel, null);
		
		// Return the dialog:
		return builder.create();
	}

	/**
	 * 
	 * @author Michalis Vitos, mstevens
	 */
	private static class AsyncZipper extends AsyncTaskWithWaitingDialog<Void, Void, Void>
	{
		private Context context;
		private List<File> sourceFiles;
		private File destZipFile;
	
		public AsyncZipper(Context context, String waitingMsg, List<File> sourceFiles, File destZipFile)
		{
			super(context, waitingMsg);
	
			this.context = context;
			this.sourceFiles = sourceFiles;
			this.destZipFile = destZipFile;
		}
	
		@Override
		protected Void doInBackground(Void... params)
		{
			try
			{
				// TODO Add database backup!
				
				new Zipper().zip(sourceFiles, destZipFile);
			}
			catch(Exception e)
			{
				Debug.e(e);
			}
			return null;
		}
	
		@Override
		protected void onPostExecute(Void result)
		{
			super.onPostExecute(result);
	
			// Show confirmation dialog (with option to share the file):
			new AlertDialog.Builder(context).setTitle(R.string.successful_backup).setMessage(context.getString(R.string.backup_in) + "\n" + destZipFile.getAbsolutePath())
					.setPositiveButton(android.R.string.ok, null)
					.setNegativeButton(R.string.share, new DialogInterface.OnClickListener()
					{
						@Override
						public void onClick(DialogInterface dialog, int which)
						{
							Intent sendIntent = new Intent();
		                    sendIntent.setAction(Intent.ACTION_SEND);
		                    sendIntent.putExtra(Intent.EXTRA_STREAM, Uri.fromFile(destZipFile));
		                    sendIntent.setType("application/zip");
		                    context.startActivity(sendIntent);
						}
					})
					.create().show();
		}
	}

	
}
