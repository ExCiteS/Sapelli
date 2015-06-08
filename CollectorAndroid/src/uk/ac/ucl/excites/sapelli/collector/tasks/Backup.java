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
import java.io.IOException;
import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

import org.apache.commons.io.FileUtils;
import org.apache.commons.lang3.ArrayUtils;

import uk.ac.ucl.excites.sapelli.collector.R;
import uk.ac.ucl.excites.sapelli.collector.activities.BaseActivity;
import uk.ac.ucl.excites.sapelli.collector.fragments.ExportFormatFragment;
import uk.ac.ucl.excites.sapelli.collector.io.FileStorageProvider;
import uk.ac.ucl.excites.sapelli.collector.io.FileStorageProvider.Folder;
import uk.ac.ucl.excites.sapelli.collector.util.AsyncTaskWithWaitingDialog;
import uk.ac.ucl.excites.sapelli.shared.db.StoreBackupper;
import uk.ac.ucl.excites.sapelli.shared.io.FileHelpers;
import uk.ac.ucl.excites.sapelli.shared.io.Zipper;
import uk.ac.ucl.excites.sapelli.shared.util.ExceptionHelpers;
import uk.ac.ucl.excites.sapelli.storage.eximport.ExportResult;
import uk.ac.ucl.excites.sapelli.storage.model.Record;
import uk.ac.ucl.excites.sapelli.storage.queries.RecordsQuery;
import uk.ac.ucl.excites.sapelli.util.Debug;
import android.annotation.SuppressLint;
import android.app.AlertDialog;
import android.app.Dialog;
import android.content.Context;
import android.content.DialogInterface;
import android.content.DialogInterface.OnDismissListener;
import android.content.Intent;
import android.net.Uri;
import android.view.ContextThemeWrapper;

/**
 * Sapelli Collector Back-up procedure 
 * 
 * @author Michalis Vitos, mstevens
 */
public class Backup implements RecordsTasks.QueryCallback, RecordsTasks.ExportCallback
{

	// STATIC -----------------------------------------------------------------
	static public final Folder[] BACKUPABLE_FOLDERS = { Folder.Attachments, Folder.Crashes, Folder.Export, Folder.Logs, Folder.Projects };
	static public final String EMPTY_FILE = ".empty";
	
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
			case DB: // (in fact this is will always included in back-up but not directly, only after DB(s) has/have been copied to a temp folder)
			default:
				throw new IllegalArgumentException("This folder (" + folder.name() + ") cannot be backed-up!");
		}
	}
	
	static private boolean isFolderDefaultSelected(Folder folder)
	{
		switch(folder)
		{
			case Attachments:
			case Crashes:
			case Export:
			case Logs:
				return true;
			case Projects:
				return false;
			// Not back-upable:
			case Downloads:
			case Temp:
			case DB: // (see comment above)
			default:
				throw new IllegalArgumentException("This folder (" + folder.name() + ") cannot be backed-up!");
		}
	}

	static public void Run(BaseActivity activity, FileStorageProvider fileStorageProvider)
	{
		// create Backup instance and start the back-up process by showing the selection diagram...
		new Backup(activity, fileStorageProvider).showSelectionDialog();
	}
	
	// DYNAMIC ----------------------------------------------------------------
	private final BaseActivity activity;
	private final FileStorageProvider fileStorageProvider;
	private final Set<Folder> foldersToExport;
	
	private List<Record> recordsToExport;
	private ExportFormatFragment frgFormat;
	private final Runnable runBackup;
	
	private Backup(BaseActivity activity, FileStorageProvider fileStorageProvider)
	{
		// Initialise:
		this.activity = activity;
		this.fileStorageProvider = fileStorageProvider;
		foldersToExport = new HashSet<Folder>();
		runBackup = new Runnable()
		{
			@Override
			public void run()
			{
				doBackup(); // go straight to back-up
			}
		};
	}
	
	/**
	 * Brings up the selection dialog (= start of back-up procedure)
	 */
	private void showSelectionDialog()
	{
		// Initialise folder selection:
		List<String> checkboxItems = new ArrayList<String>();
		List<Boolean> checkedItems = new ArrayList<Boolean>();
		for(Folder folder : BACKUPABLE_FOLDERS)
		{
			checkboxItems.add(getFolderString(activity, folder));
			boolean selected = isFolderDefaultSelected(folder);
			checkedItems.add(selected);
			if(selected)
				foldersToExport.add(folder);
		}
		
		// Get dialog builder & configure the dialog...
		new AlertDialog.Builder(new ContextThemeWrapper(activity, R.style.AppTheme))
		//	Set title:
		.setTitle(R.string.selectForBackup)
		//	Set multiple choice:0
		.setMultiChoiceItems(
			// Transform checkboxItems to a CharSequence[]
			checkboxItems.toArray(new CharSequence[checkboxItems.size()]),
			// Transform checkedItems to a boolean[] -> items to be pre-selected
			ArrayUtils.toPrimitive(checkedItems.toArray(new Boolean[checkedItems.size()])),
			// Choice click event handler:
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
		// Set OK button:
		.setPositiveButton(android.R.string.ok, new DialogInterface.OnClickListener()
		{
			@Override
			public void onClick(DialogInterface dialog, int id)
			{
				if(foldersToExport.contains(Folder.Export))
					showExportYesNoDialog(); // propose creation of a fresh full project data export
				else
					doBackup(); // go straight to back-up
			}
		})
		// Set Cancel button:
		.setNegativeButton(android.R.string.cancel, null)
		// Create & show the dialog:
		.create().show();
	}
	
	@SuppressLint("InflateParams")
	private void showExportYesNoDialog()
	{
		// Get dialog builder & configure the dialog...
		new AlertDialog.Builder(new ContextThemeWrapper(activity, R.style.AppTheme))
		//	Set title:
		.setTitle(R.string.preBackupExportTitle)
		//	Set message:
		.setMessage(R.string.preBackupExportMsg)
		// Set "Yes button:
		.setPositiveButton(R.string.preBackupExportYes, new DialogInterface.OnClickListener()
		{
			@Override
			public void onClick(DialogInterface dialog, int which)
			{
				// Query to see if there are any records to export:
				new RecordsTasks.QueryTask(activity, Backup.this).execute(RecordsQuery.ALL); // TODO filter out Collector-internal schemas
			}
		})
		// Set "No" button:
		.setNegativeButton(R.string.no, new DialogInterface.OnClickListener()
		{
			@Override
			public void onClick(DialogInterface dialog, int which)
			{	// go straight to back-up:
				doBackup();
			}
		})
		// Create & show the dialog:
		.create().show();
	}
	
	@Override
	public void querySuccess(List<Record> result)
	{
		if(result != null && !result.isEmpty())
		{
			recordsToExport = result;
			showExportFormatDialog();
		}
		else
		{
			activity.showOKDialog(	R.string.preBackupExportTitle,
									activity.getString(R.string.exportNoRecordsFound) + "\n" + activity.getString(R.string.backup_continue),
									false,
									runBackup);
		}
	}

	@Override
	public void queryFailure(Exception reason)
	{
		activity.showOKDialog(	R.string.preBackupExportTitle,
								activity.getString(R.string.exportQueryFailed, ExceptionHelpers.getMessageAndCause(reason)) + "\n" + activity.getString(R.string.backup_continue),
								false,
								runBackup);
	}
	
	@SuppressLint("InflateParams")
	private void showExportFormatDialog()
	{
		// Create the dialog
		AlertDialog.Builder builder = new AlertDialog.Builder(new ContextThemeWrapper(activity, R.style.AppTheme));
		// Set the title:
		builder.setTitle(R.string.preBackupExportTitle);
		// Set msg:
		builder.setMessage(activity.getString(R.string.preBackupExportFormatMsg, recordsToExport.size()));
		// Set UI:
		builder.setView(activity.getLayoutInflater().inflate(R.layout.dialog_prebackup_export_format, null));
		frgFormat = (ExportFormatFragment) activity.getSupportFragmentManager().findFragmentById(R.id.frgExportFormatPreBackup);
		// Set OK button:
		builder.setPositiveButton(android.R.string.ok, new Dialog.OnClickListener()
		{
			@Override
			public void onClick(DialogInterface dialog, int which)
			{
				frgFormat.runExportTask(recordsToExport, activity, fileStorageProvider.getExportFolder(true), activity.getString(R.string.backup), Backup.this);
			}
		});
		// Create the dialog
		Dialog dialog = builder.create();
		// Add onDismiss listener:
		dialog.setOnDismissListener(new OnDismissListener()
		{
			@Override
			public void onDismiss(DialogInterface dialog)
			{
				frgFormat.forget(); // necessary to avoid duplicate id error when dialog is shown again later
			}
		});
		// Show the dialog:
		dialog.show();
	}
	
	@Override
	public void exportDone(ExportResult result)
	{
		if(!result.wasSuccessful())
		{
			activity.showOKDialog(	R.string.preBackupExportTitle,
									(result.getNumberedOfExportedRecords() > 0 ?
										activity.getString(R.string.exportPartialSuccessMsg, result.getNumberedOfExportedRecords(), result.getDestination(), result.getNumberOfUnexportedRecords(), ExceptionHelpers.getMessageAndCause(result.getFailureReason())) :
										activity.getString(R.string.exportFailureMsg, result.getDestination(), ExceptionHelpers.getMessageAndCause(result.getFailureReason())))
									+ "\n" + activity.getString(R.string.backup_continue),
									false,
									runBackup);
		}
		else
			doBackup();
	}
	
	@SuppressWarnings("unchecked")
	private void doBackup()
	{
		new AsyncBackup().execute(foldersToExport);
	}
	
	private void showSuccessDialog(final File destZipFile)
	{
		// Get dialog builder & configure the dialog...
		new AlertDialog.Builder(new ContextThemeWrapper(activity, R.style.AppTheme))
		//	Set title:
		.setTitle(R.string.successful_backup)
		//	Set message:
		.setMessage(activity.getString(R.string.backup_in) + "\n" + destZipFile.getAbsolutePath())
		// Set "OK" button:
		.setPositiveButton(R.string.done, null)
		// Set "Share" button:
		.setNegativeButton(R.string.share, new DialogInterface.OnClickListener()
		{
			@Override
			public void onClick(DialogInterface dialog, int which)
			{
				Intent sendIntent = new Intent();
				sendIntent.setAction(Intent.ACTION_SEND);
				sendIntent.putExtra(Intent.EXTRA_STREAM, Uri.fromFile(destZipFile));
				sendIntent.setType("application/zip");
				activity.startActivity(sendIntent);
			}
		})
		// Create & show the dialog:
		.create().show();
	}
	
	private void showFailureDialog(Exception cause)
	{
		activity.showErrorDialog(activity.getString(R.string.backupFailDueTo, ExceptionHelpers.getMessageAndCause(cause)), false);
	}
	
	/**
	 * 
	 * @author Michalis Vitos, mstevens
	 */
	private class AsyncBackup extends AsyncTaskWithWaitingDialog<Set<Folder>, File>
	{
		
		private Exception failure = null;
	
		public AsyncBackup()
		{
			super(activity);
		}
	
		@Override
		protected File doInBackground(Set<Folder>... params)
		{
			File destZipFile = null;
			File tmpFolder = null;
			try
			{
				// Phase 1: Preparation
				publishProgress(activity.getString(R.string.backup_progress_init));
				//	Create array with file paths of the selected items as well as the Temp/DB folder:
				Set<Folder> selectedFolders = params[0];
				File[] toZip = new File[selectedFolders.size() + 1]; // +1 for tmp/DB folder!
				tmpFolder = fileStorageProvider.getTempSubFolder("Backup_" + System.currentTimeMillis());
				int z = 0;
				for(Folder folder : foldersToExport)
					toZip[z++] = getFolderFile(folder, tmpFolder); // add folders as File objects
				toZip[z] = FileHelpers.getSubDirectory(tmpFolder, Folder.DB.name(), true); // add Temp/Backup_[timestamp]/DB/ folder
				
				// Phase 2: Back-up database(s)
				publishProgress(activity.getString(R.string.backup_progress_db));
				// 	Create backups in the Temp/Backup_[timestamp]/DB/ folder and use original file names (not labeled as backups):				
				StoreBackupper.Backup(toZip[z], false, activity.getCollectorApp().getStoreHandlesForBackup());
				
				// Phase 3: Create ZIP archive
				publishProgress(activity.getString(R.string.backup_progress_zipping));
				destZipFile = fileStorageProvider.getNewBackupFile();
				Zipper.Zip(destZipFile, toZip);
			}
			catch(Exception e)
			{
				Debug.e(e);
				failure = e;
				return null;
			}
			finally
			{	// Clean-up
				FileUtils.deleteQuietly(tmpFolder);
			}
			return destZipFile;
		}
		
		private File getFolderFile(Folder folder, File tmpFolder) throws IOException
		{
			File folderFile = fileStorageProvider.getFolder(folder, false);
			if(!folderFile.exists() || !folderFile.isDirectory() || folderFile.listFiles().length == 0)
			{
				// Create matching folder in tmpFolder:
				folderFile = FileHelpers.getSubDirectory(tmpFolder, folder.name(), true); // Temp/Backup_[timestamp]/[folder]/
				// Create .empty file (to make sure folder is included in ZIP):
				(new File(folderFile, EMPTY_FILE)).createNewFile();
			}
			return folderFile;
		}
	
		@Override
		protected void onPostExecute(File destZipFile)
		{
			// Dismiss progress dialog:
			super.onPostExecute(destZipFile);
			
			// Show success or failure dialog:
			if(destZipFile != null && failure == null)
				showSuccessDialog(destZipFile);
			else
				showFailureDialog(failure);
		}
		
	}
	
}
