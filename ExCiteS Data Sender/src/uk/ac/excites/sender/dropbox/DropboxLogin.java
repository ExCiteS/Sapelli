package uk.ac.excites.sender.dropbox;

import uk.ac.excites.sender.Constants;
import uk.ac.excites.sender.R;
import android.app.Activity;
import android.content.Intent;
import android.os.Bundle;
import android.util.Log;

import com.dropbox.sync.android.DbxAccountManager;

public class DropboxLogin extends Activity
{
	private DbxAccountManager mDbxAcctMgr;
	private static final int REQUEST_LINK_TO_DBX = 0;

	@Override
	protected void onCreate(Bundle savedInstanceState)
	{
		super.onCreate(savedInstanceState);
		setContentView(R.layout.activity_dropbox);

		Dropbox mDropbox = new Dropbox(getApplicationContext());
		mDbxAcctMgr = mDropbox.getDropboxManager();

		// Link to Dropbox Account
		if(!mDbxAcctMgr.hasLinkedAccount())
		{
			mDbxAcctMgr.startLink(this, REQUEST_LINK_TO_DBX);
		}
	}

	@Override
	public void onActivityResult(int requestCode, int resultCode, Intent data)
	{
		if(requestCode == REQUEST_LINK_TO_DBX)
		{
			if(resultCode == Activity.RESULT_OK)
			{
				// ... Start using Dropbox files.
				Log.i(Constants.TAG, "DropboxLogin onActivityResult OK");
				finish();
			}
			else
			{
				// ... Link failed or was cancelled by the user.
				Log.i(Constants.TAG, "DropboxLogin onActivityResult not OK");
				finish();
			}
		}
		else
		{
			super.onActivityResult(requestCode, resultCode, data);
		}
	}
}
