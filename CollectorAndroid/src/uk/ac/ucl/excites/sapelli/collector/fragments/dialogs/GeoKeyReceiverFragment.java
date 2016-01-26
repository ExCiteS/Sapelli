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

package uk.ac.ucl.excites.sapelli.collector.fragments.dialogs;

import android.annotation.SuppressLint;
import android.app.AlertDialog;
import android.app.Dialog;
import android.content.DialogInterface;
import android.os.Bundle;
import android.support.v7.app.AppCompatActivity;
import android.view.View;
import android.widget.EditText;
import uk.ac.ucl.excites.sapelli.collector.R;
import uk.ac.ucl.excites.sapelli.collector.activities.BaseActivity;
import uk.ac.ucl.excites.sapelli.collector.activities.ProjectManagerActivity;
import uk.ac.ucl.excites.sapelli.collector.fragments.ProjectManagerFragment;
import uk.ac.ucl.excites.sapelli.collector.transmission.SendConfigurationHelpers;
import uk.ac.ucl.excites.sapelli.collector.transmission.SendConfigurationHelpers.ReceiverUpdateCallback;
import uk.ac.ucl.excites.sapelli.collector.transmission.protocol.geokey.AndroidGeoKeyClient;
import uk.ac.ucl.excites.sapelli.collector.util.AsyncTaskWithWaitingDialog;
import uk.ac.ucl.excites.sapelli.shared.util.android.DeviceControl;
import uk.ac.ucl.excites.sapelli.shared.util.android.DialogHelpers;
import uk.ac.ucl.excites.sapelli.transmission.model.transport.geokey.GeoKeyAccount;

/**
 * @author mstevens
 *
 */
public class GeoKeyReceiverFragment extends ProjectManagerFragment implements DialogInterface.OnClickListener
{
	
	// STATIC -------------------------------------------------------
	static public void ShowAddDialog(AppCompatActivity owner, ReceiverUpdateCallback callback)
	{
		new GeoKeyReceiverFragment(callback).show(owner.getSupportFragmentManager(), R.string.add + GeoKeyAccount.class.getSimpleName());
	}

	static public void ShowEditDialog(AppCompatActivity owner, ReceiverUpdateCallback callback, GeoKeyAccount editCorrespondent)
	{
		new GeoKeyReceiverFragment(callback, editCorrespondent).show(owner.getSupportFragmentManager(), R.string.edit + GeoKeyAccount.class.getSimpleName());
	}
	
	// DYNAMIC ------------------------------------------------------
	private final ReceiverUpdateCallback callback;
	
	private GeoKeyAccount editReceiver;
	
	private EditText txtGeoKeyServerURL;
	private EditText txtEmail;
	private EditText txtPassword;
	private EditText txtReceiverName;
	
	public GeoKeyReceiverFragment(ReceiverUpdateCallback callback)
	{
		this(callback, null);
	}
	
	public GeoKeyReceiverFragment(ReceiverUpdateCallback callback, GeoKeyAccount receiver)
	{
		this.callback = callback;
		this.editReceiver = receiver;
	}
	
	/* (non-Javadoc)
	 * @see uk.ac.ucl.excites.sapelli.collector.fragments.ProjectManagerFragment#getLayoutID()
	 */
	@Override
	protected Integer getLayoutID()
	{
		return R.layout.dialog_geokey_receiver;
	}
	
	public boolean isEditing()
	{
		return editReceiver != null;
	}
	
	/* (non-Javadoc)
	 * @see uk.ac.ucl.excites.sapelli.collector.fragments.ProjectManagerFragment#setupUI(android.view.View)
	 */
	@Override
	protected void setupUI(View rootLayout)
	{
		txtGeoKeyServerURL = (EditText) rootLayout.findViewById(R.id.txtGeoKeyServerURL);
		txtGeoKeyServerURL.setTypeface(ProjectManagerActivity.FONT_SANS_SERIF_CONDENSED);
		txtEmail = (EditText) rootLayout.findViewById(R.id.txtEmail);
		txtPassword = (EditText) rootLayout.findViewById(R.id.txtPassword);
		txtReceiverName = (EditText) rootLayout.findViewById(R.id.txtGeoKeyReceiverName);
		
		if(isEditing())
		{
			txtGeoKeyServerURL.setText(editReceiver.getUrl());
			txtEmail.setText(editReceiver.getEmail()); // hint about anonymous use
			txtPassword.setText(editReceiver.getPassword()); // TODO "click to change"
			txtReceiverName.setText(editReceiver.getName());
		}
		else
		{
			txtGeoKeyServerURL.setText(R.string.url_protocol_https);
			txtEmail.setText("");
			txtPassword.setText("");
			txtReceiverName.setText("");
		}
		// Dis/enable editing of finals:
		txtGeoKeyServerURL.setEnabled(!isEditing());
	}
	
	@SuppressLint("InflateParams")
	@Override
	public Dialog onCreateDialog(Bundle savedInstanceState)
	{
		AlertDialog.Builder builder = new AlertDialog.Builder(getOwner())
		.setIcon(SendConfigurationHelpers.getGeoKeyReceiverDrawable(true))
		.setTitle(isEditing() ? R.string.editReceiver : R.string.addReceiver)
		.setPositiveButton(android.R.string.ok, null) // listener will be set through the MakeNonDismission() call below
		.setNegativeButton(android.R.string.cancel, this);
		final AlertDialog dialog = builder.create();
		
		DialogHelpers.MakeNonDismissing(dialog, this, DialogInterface.BUTTON_POSITIVE);
		
		// Set view:
		int lrSpacingPx = getDialogLeftRightPaddingPx();
		dialog.setView(getRootLayout(), lrSpacingPx, getDialogMessageToViewSpacingPx(), lrSpacingPx, 0);
		
		return dialog;
	}

	@Override
	public void onClick(DialogInterface dialog, int which)
	{
		switch(which)
		{
			case DialogInterface.BUTTON_POSITIVE :
				save(dialog);
				break;
			case DialogInterface.BUTTON_NEGATIVE :
				break;
		}
	}
	
	/**
	 * @param dialog
	 */
	private void save(DialogInterface dialog)
	{
		// Input validation:
		boolean valid = true;
		//	URL:
		String url = txtGeoKeyServerURL.getText().toString().trim(); 
		if(url.isEmpty() || !GeoKeyAccount.URL_VALIDATOR.isValid(url))
		{
			txtGeoKeyServerURL.setBackgroundResource(R.color.red25percent);
			valid = false;
		}
		//	E-mail:
		String email = txtEmail.getText().toString().trim();
		if(email.isEmpty() || !GeoKeyAccount.EMAIL_VALIDATOR.isValid(email))
		{
			txtEmail.setBackgroundResource(R.color.red25percent);
			valid = false;
		}
		//	Password:
		String password = txtPassword.getText().toString(); 
		if(password.isEmpty())
		{
			txtPassword.setBackgroundResource(R.color.red25percent);
			valid = false;
		}
		//	Name:
		String name = txtReceiverName.getText().toString().trim();
		if(!valid)
			dialog.dismiss();
		
		// Check if we are editing...
		if(isEditing())
		{	// We are, check for changes:
			boolean changed = false;
			if(!editReceiver.getName().equals(name))
			{
				editReceiver.setName(name);
				changed = true;
			}
			if(!editReceiver.getEmail().equals(email))
			{
				editReceiver.setEmail(email);
				changed = true;
			}
			if(!editReceiver.getPassword().equals(password))
			{
				editReceiver.setPassword(password);
				changed = true;
			}
			// Check if any changes were made:
			if(!changed)
				// No changes, we are done here...
				dialog.dismiss();
		}
		
		// Verify & save account:
		verifyAndSave(dialog, isEditing() ? editReceiver : GeoKeyAccount.CreateNew(name, url, email, password));
	}
	
	static private enum CheckResult
	{
		NoInternet,
		InvalidServer,
		InvalidAccount,
		OK
	};
	
	private void verifyAndSave(final DialogInterface dialog, final GeoKeyAccount account)
	{
		final ProjectManagerActivity owner = getOwner();
		new AsyncTaskWithWaitingDialog<BaseActivity, Void, CheckResult>(owner, R.string.verifyingServerAndAccount)
		{
			@Override
			protected CheckResult runInBackground(Void... params)
			{
				if(!DeviceControl.isOnline(owner))
					return CheckResult.NoInternet;
				
				// We are online, perform actual checks:
				AndroidGeoKeyClient client = new AndroidGeoKeyClient(owner.getCollectorApp());
				
				//	Check if server is reachable and run the right GeoKey & extension versions:
				if(!client.verifyServer(account))
					return CheckResult.InvalidServer;
				
				//	Check if user can login:
				if(!client.login(account))
					return CheckResult.InvalidAccount;
				
				// All OK!:
				return CheckResult.OK;
			}

			@Override
			protected void onPostExecute(CheckResult result)
			{
				// Dismiss waiting dialog:
				super.onPostExecute(result);
				
				// Deal with result:
				switch(result)
				{
					case NoInternet:
						getOwner().showErrorDialog(R.string.connectionError, false);
						break;
					case InvalidAccount:
						// TODO
						break;
					case InvalidServer:
						// TODO
						break;
					case OK:
						// Save correspondent:
						SendConfigurationHelpers.saveCorrespondent(owner, account);
						if(callback != null)
						{
							if(!isEditing())
								callback.newReceiver(account);
							else
								callback.editedReceiver(account);
						}
						// Close dialog:
						dialog.dismiss();
						break;
				
				}
			}
		}.execute();
	}
		
}
