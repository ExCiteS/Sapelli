package uk.ac.ucl.excites.sapelli.sender.dropbox;

import java.io.File;

import uk.ac.ucl.excites.collector.project.model.MediaField;
import uk.ac.ucl.excites.sapelli.sender.util.RecursiveFileObserver;
import uk.ac.ucl.excites.sapelli.util.Debug;
import uk.ac.ucl.excites.util.FileHelpers;
import android.content.Context;
import android.os.FileObserver;

import com.dropbox.sync.android.DbxAccountManager;
import com.dropbox.sync.android.DbxFile;
import com.dropbox.sync.android.DbxFileSystem;
import com.dropbox.sync.android.DbxPath;

/**
 * Class to define the Folder Sync and Upload to Dropbox
 * 
 * @author Michalis Vitos
 * 
 */
public class DropboxSync extends RecursiveFileObserver
{
	private static final int flags = FileObserver.CREATE | FileObserver.DELETE | FileObserver.MOVED_TO | FileObserver.CLOSE_WRITE;
	private static final String NOT_ALLOWED_FILE = "jpg";

	private String basePath;

	// Dropbox Variables
	private DbxAccountManager mDbxAcctMgr;
	private DbxFileSystem dbxFs;

	// TODO Scan folder for files and upload them to the Dropbox if they don't exist

	/**
	 * Constructor takes the Folder to watch as a parameter
	 * 
	 * @param path
	 */
	public DropboxSync(Context context, File folder, String basePath)
	{
		super(folder.getAbsolutePath(), flags);
		this.basePath = basePath;

		Debug.d("Set up Dropbox Observer to folder: " + folder.getAbsolutePath());

		// Setup Dropbox
		try
		{
			Dropbox mDropbox = new Dropbox(context);

			if(mDropbox.hasLinkedAccount())
			{
				// Set up Dropbox
				mDbxAcctMgr = mDropbox.getDropboxManager();
				dbxFs = DbxFileSystem.forAccount(mDbxAcctMgr.getLinkedAccount());
				Debug.d("Dropbox has been linked.");
			}
			else
			{
				mDropbox.linkAccount();
			}
		}
		catch(Exception e)
		{
			Debug.e("DropboxSync() error: ", e);
		}
	}

	@Override
	public synchronized void onEvent(int event, String path)
	{

		// Debug.d("Event: " + event + " and path: " + path);

		// Make sure the path is not null
		if(path == null)
		{
			return;
		}

		// File to upload to Dropbox
		File fileToUpload = new File(path);

		// Check what was changed to the Projects Folder and upload or delete the file
		switch(event)
		{
		// Case used for new files
		case FileObserver.CREATE:
			Debug.d("File: " + fileToUpload + " was created but no action is taken.");
			// uploadFile(fileToUpload);
			break;

		// Case used for closed files
		case FileObserver.CLOSE_WRITE:
			Debug.d("File: " + fileToUpload + " was closed.");
			uploadFile(fileToUpload);
			break;

		// Case used for photos
		case FileObserver.MOVED_TO:
			Debug.d("File: " + fileToUpload + " was moved to.");
			uploadFile(fileToUpload);
			break;

		case FileObserver.DELETE:
			deleteFile(fileToUpload);
			// Log.i(MainActivity.TAG, "File: " + fileToUpload + " was deleted.");
			break;
		}
	}

	private void uploadFile(File fileToUpload)
	{
		// Get the non Obfuscated filename and the extension
		final String nonObfuscatedFilename = MediaField.UndoExtensionObfuscation(fileToUpload.getName());
		final String fileExtension = FileHelpers.getFileExtension(nonObfuscatedFilename);

		Debug.d("The file is: " + nonObfuscatedFilename + " and its filename is: " + fileExtension);

		DbxFile dropboxFile = null;

		// TODO Change the approved list from XML
		// TODO Get the image extension from the Media Class
		// for now we do not upload images

		// Upload only approved files
		if(!fileExtension.equals(NOT_ALLOWED_FILE))
		{
			try
			{
				// Path to the Dropbox Structure where to upload the file
				// fileToUplad - base path
				final String dropboxServerPath = fileToUpload.getParent().replace(basePath, "") + File.separator + nonObfuscatedFilename;
				DbxPath dropboxPath = new DbxPath(dropboxServerPath);

				Debug.d("File to be uploaded is: " + fileToUpload.toString());
				Debug.d("Dropbox path to upload is: " + dropboxPath.toString());

				Debug.d("File " + dropboxPath.getName() + " does " + (!dbxFs.isFile(dropboxPath) ? "not" : "") + " exist on the Dropbox Server.");

				// Create the file if it does not exist
				if(!dbxFs.isFile(dropboxPath))
				{
					dropboxFile = dbxFs.create(dropboxPath);
					// Upload the file to Dropbox
					dropboxFile.writeFromExistingFile(fileToUpload, false);
				}
			}
			catch(Exception e)
			{
				Debug.e(e);
			}
			finally
			{
				if(dropboxFile != null)
					dropboxFile.close();
				Debug.d("File upload scheduled: " + fileToUpload.getName() + " Dropbox side name: " + nonObfuscatedFilename);
			}
		}
		else
		{
			Debug.d("For now will not try to upload " + nonObfuscatedFilename + " because it has the extention: " + fileExtension);
		}
	}

	private void deleteFile(File fileToDelete)
	{
		// For now, do not do anything
	};
}