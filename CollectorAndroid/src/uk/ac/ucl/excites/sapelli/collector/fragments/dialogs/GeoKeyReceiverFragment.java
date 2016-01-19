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
import uk.ac.ucl.excites.sapelli.collector.activities.ProjectManagerActivity;
import uk.ac.ucl.excites.sapelli.collector.fragments.ProjectManagerFragment;
import uk.ac.ucl.excites.sapelli.collector.transmission.SendConfigurationHelpers;
import uk.ac.ucl.excites.sapelli.collector.transmission.SendConfigurationHelpers.ReceiverUpdateCallback;
import uk.ac.ucl.excites.sapelli.collector.transmission.protocol.geokey.GeoKeyTasks;
import uk.ac.ucl.excites.sapelli.collector.transmission.protocol.geokey.GeoKeyTasks.CheckResult;
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
			txtEmail.setText(editReceiver.getEmail());
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
				saveChanges(dialog);
				break;
			case DialogInterface.BUTTON_NEGATIVE :
				break;
		}
	}
	
	/**
	 * @return whether or not to dismiss the dialog
	 */
	private void saveChanges(final DialogInterface dialog)
	{
		// Input validation:
		boolean valid = true;
		//	URL:
		String url = txtGeoKeyServerURL.getText().toString(); 
		if(url.isEmpty() || !GeoKeyAccount.URL_VALIDATOR.isValid(url))
		{
			txtGeoKeyServerURL.setBackgroundResource(R.color.red25percent);
			valid = false;
		}
		//	E-mail:
		String email = txtEmail.getText().toString();
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
		String name = txtReceiverName.getText().toString();
		if(!valid)
			return;
		
		// Check if we are editing...
		if(isEditing())
		{	// We are, check if actual changes were made:
			if(	!editReceiver.getName().equals(name) ||
				!editReceiver.getUrl().equals(url) ||
				!editReceiver.getEmail().equals(email) ||
				!editReceiver.getPassword().equals(password))
			{	// The receiver has been changed...
				//	We cannot alter existing correspondents to instead we delete (or hide it), and replace it by a new one to be stored below.
				SendConfigurationHelpers.deleteCorrespondent(getOwner(), editReceiver); // TODO what with transmittable records scheduled for this receiver??
				// TODO what to do with other projects using the same receiver? --> update their schedules! (And transmittables?)				
			}
			else
				// No changes, we are done here...
				dialog.dismiss();
		}
		
		// Create correspondent:
		final GeoKeyAccount toSave = GeoKeyAccount.CreateNew(name, url, email, password);
		
		// Verify it:
		GeoKeyTasks.CheckServer(getOwner(), toSave, null /*TODO pass current project if receiver being made for a schedule*/, new GeoKeyTasks.CheckCallback()
		{
			@Override
			public void checkDone(CheckResult result)
			{
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
					case NoSuchProject:
						// TODO
						break;
					case OK:
						// Save correspondent:
						SendConfigurationHelpers.saveCorrespondent(getOwner(), toSave);
						if(callback != null)
						{
							if(!isEditing())
								callback.newReceiver(toSave);
							else
								callback.editedReceiver(toSave, editReceiver);
						}
						// Close dialog:
						dialog.dismiss();
						break;
				
				}
			}
		});

	}
		
}
