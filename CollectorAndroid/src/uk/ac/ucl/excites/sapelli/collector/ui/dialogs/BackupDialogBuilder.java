/**
 * 
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
import uk.ac.ucl.excites.sapelli.collector.util.AsyncZipper;
import android.app.AlertDialog;
import android.app.Dialog;
import android.content.Context;
import android.content.DialogInterface;
import android.widget.Toast;

/**
 * @author Michalis Vitos, mstevens
 *
 */
public class BackupDialogBuilder
{

	static public final Folder[] BACK_UP_ABLE_FOLDERS = { Folder.Attachments, Folder.Crashes, Folder.Export, Folder.Logs, Folder.Projects };
	
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
		for(Folder folder : BACK_UP_ABLE_FOLDERS)
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
							foldersToExport.add(Folder.values()[which]);
						else
							// Else, if the item is already in the array, remove it:
							foldersToExport.remove(Folder.values()[which]);
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
							toZip.add(fileStorageProvider.getSapelliFolder(folder, false));
						// TODO Add database

						// Call an AsyncZipper only if there are selected items
						if(!toZip.isEmpty())
							new AsyncZipper(context,
											context.getString(R.string.exporting_data),
											toZip,
											fileStorageProvider.getBackupLocation().getAbsolutePath()).execute();
						else
							Toast.makeText(context, R.string.select_at_least_one_folder_to_export_data, Toast.LENGTH_LONG).show();
					}
				}).setNegativeButton(android.R.string.cancel, null);
		
		// Return the dialog:
		return builder.create();
	}
	
}
