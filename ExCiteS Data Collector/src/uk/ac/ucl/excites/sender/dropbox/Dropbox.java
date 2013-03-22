package uk.ac.ucl.excites.sender.dropbox;

import android.content.Context;
import android.content.Intent;

import com.dropbox.sync.android.DbxAccountManager;

/**
 * Dropbox Configuration
 * 
 * @author Michalis Vitos
 * 
 */
public class Dropbox
{
	private DbxAccountManager mDbxAccountManager;
	private Context mContext;
	/*
	 * HERE GOES THE ACTUAL DROPBOX API KEY AND SECRET
	 */
	private static final String appKey = "y15h8nlpkr6ezp1";
	private static final String appSecret = "wbn1bdrc5l7bdxp";

	/**
	 * Initialise the configuration
	 * 
	 * @param applicationContext
	 */
	public Dropbox(Context applicationContext)
	{
		this.mDbxAccountManager = DbxAccountManager.getInstance(applicationContext, appKey, appSecret);
		this.mContext = applicationContext;
	}

	/**
	 * Get the Dropbox Account Manager
	 * 
	 * @return an DbxAccountManager object
	 */
	public DbxAccountManager getDropboxManager()
	{
		return mDbxAccountManager;
	}

	/**
	 * Check if account is linked
	 * 
	 * @return true / false
	 */
	public boolean hasLinkedAccount()
	{
		return (mDbxAccountManager.hasLinkedAccount()) ? true : false;
	}

	/**
	 * Call the appropriate activity to link with the Dropbox Account
	 */
	public void linkAccount()
	{
		if(!mDbxAccountManager.hasLinkedAccount())
		{
			// Call the DropboxLogin Activity
			Intent mIntent = new Intent(mContext, DropboxLogin.class);
			mIntent.setFlags(Intent.FLAG_ACTIVITY_NEW_TASK);
			mContext.startActivity(mIntent);
		}
	}
}
