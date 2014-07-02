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

package uk.ac.ucl.excites.sapelli.sender.dropbox;

import uk.ac.ucl.excites.sapelli.collector.R;
import uk.ac.ucl.excites.sapelli.sender.util.Constants;
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
