package uk.ac.ucl.excites.sender.dropbox;

import java.io.File;

import uk.ac.ucl.excites.util.Debug;
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
public class DropboxSync extends FileObserver
{
	private static final int flags = FileObserver.CREATE | FileObserver.DELETE | FileObserver.MOVED_TO;
	private String absolutePath;

	// Dropbox Variables
	private DbxAccountManager mDbxAcctMgr;
	private DbxFileSystem dbxFs;

	/**
	 * Constructor takes the Folder to watch as a parameter
	 * 
	 * @param path
	 */
	public DropboxSync(Context context, File folder)
	{
		super(folder.getAbsolutePath(), flags);
		absolutePath = folder.getAbsolutePath() + File.separator;

		Debug.d("Set up Dropbox Observer to folder: " + absolutePath);

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

		Debug.d("Event: " + event + " and path: " + path);

		// Make sure the path is not null
		if(path == null)
		{
			return;
		}

		// File to upload to Dropbox
		File fileToUpload = new File(absolutePath + path);

		// Check what changed to the Projects Folder and upload or delete the file
		switch(event)
		{
		case FileObserver.CREATE:
			Debug.d("File: " + fileToUpload + " was created.");
			uploadFile(fileToUpload);
			break;

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
		DbxFile dropboxFile = null;
		try
		{
			// Path to the Dropbox Structure where to upload the file
			// TODO Add the Project's Folder etc
			DbxPath dropboxPath = new DbxPath(fileToUpload.getName());
			Debug.d("File to upload: " + fileToUpload);
			Debug.d("File does " + (!dbxFs.isFile(dropboxPath) ? "not " : "") + "exist");
			if(dbxFs.isFile(dropboxPath))
				dropboxFile = dbxFs.open(dropboxPath);
			else
				dropboxFile = dbxFs.create(dropboxPath);

			// Upload the file to Dropbox
			dropboxFile.writeFromExistingFile(fileToUpload, false);
		}
		catch(Exception e)
		{
			Debug.e(e);
		}
		finally
		{
			if(dropboxFile != null)
				dropboxFile.close();
			Debug.d("File upload scheduled: " + fileToUpload.getName());
		}
	}

	private void deleteFile(File fileToDelete)
	{
		// For now, do not do anything
	};
}