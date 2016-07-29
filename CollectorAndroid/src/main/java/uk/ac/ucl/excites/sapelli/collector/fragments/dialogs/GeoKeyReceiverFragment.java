/**
 * Sapelli data collection platform: http://sapelli.org
 * 
 * Copyright 2012-2016 University College London - ExCiteS group
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

import android.content.DialogInterface;
import android.view.View;
import android.widget.EditText;
import android.widget.Toast;
import uk.ac.ucl.excites.sapelli.collector.R;
import uk.ac.ucl.excites.sapelli.collector.activities.ProjectManagerActivity;
import uk.ac.ucl.excites.sapelli.collector.transmission.SendConfigurationHelpers;
import uk.ac.ucl.excites.sapelli.collector.transmission.SendConfigurationHelpers.ReceiverUpdateCallback;
import uk.ac.ucl.excites.sapelli.collector.transmission.protocol.geokey.AndroidGeoKeyClient;
import uk.ac.ucl.excites.sapelli.collector.util.AsyncTaskWithWaitingDialog;
import uk.ac.ucl.excites.sapelli.shared.util.URLUtils;
import uk.ac.ucl.excites.sapelli.shared.util.android.DeviceControl;
import uk.ac.ucl.excites.sapelli.transmission.model.transport.geokey.GeoKeyServer;
import uk.ac.ucl.excites.sapelli.transmission.protocol.geokey.GeoKeyClient;

/**
 * @author mstevens
 *
 */
public class GeoKeyReceiverFragment extends AbstractReceiverFragment<GeoKeyServer> implements DialogInterface.OnClickListener
{
	
	// STATIC -------------------------------------------------------
	static public void ShowAddDialog(ProjectManagerActivity owner, ReceiverUpdateCallback callback)
	{
		new GeoKeyReceiverFragment(callback).show(owner.getSupportFragmentManager(), R.string.add + GeoKeyServer.class.getSimpleName());
	}

	static public void ShowEditDialog(ProjectManagerActivity owner, ReceiverUpdateCallback callback, GeoKeyServer editCorrespondent)
	{
		new GeoKeyReceiverFragment(callback, editCorrespondent).show(owner.getSupportFragmentManager(), R.string.edit + GeoKeyServer.class.getSimpleName());
	}
	
	// DYNAMIC ------------------------------------------------------
	private EditText txtGeoKeyServerURL;
	private EditText txtEmail;
	private EditText txtPassword;
	private EditText txtReceiverName;
	
	public GeoKeyReceiverFragment(ReceiverUpdateCallback callback)
	{
		this(callback, null);
	}
	
	public GeoKeyReceiverFragment(ReceiverUpdateCallback callback, GeoKeyServer receiver)
	{
		super(callback, receiver);
	}
	
	/* (non-Javadoc)
	 * @see uk.ac.ucl.excites.sapelli.collector.fragments.ProjectManagerFragment#getLayoutID()
	 */
	@Override
	protected Integer getLayoutID()
	{
		return R.layout.dialog_geokey_receiver;
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
			txtEmail.setText(editReceiver.getUserEmail()); // hint about anonymous use
			txtPassword.setText(editReceiver.getUserPassword()); // TODO "click to change"
			txtReceiverName.setText(editReceiver.getName());
		}
		else
		{
			txtGeoKeyServerURL.setText(URLUtils.PROTOCOL_HTTPS);
			txtEmail.setText("");
			txtPassword.setText("");
			txtReceiverName.setText("");
		}
		// Dis/enable editing of finals:
		txtGeoKeyServerURL.setEnabled(!isEditing());
	}
	
	@Override
	protected int getIconId()
	{
		return SendConfigurationHelpers.getGeoKeyReceiverDrawable(true);
	}

	/**
	 * @param dialog
	 * @param toSave
	 */
	@Override
	protected void validateAndSave(final DialogInterface dialog, GeoKeyServer toSave)
	{
		final ProjectManagerActivity activity = getOwner();
		if(activity == null)
		{
			dialog.dismiss();
			return;
		}
		
		if(toSave == null)
		{
			// Input validation:
			boolean valid = true;
			//	URL:
			String url = txtGeoKeyServerURL.getText().toString().trim(); 
			if(url.isEmpty() || !GeoKeyServer.URL_VALIDATOR.isValid(url))
			{
				txtGeoKeyServerURL.setBackgroundResource(R.color.red25percent);
				valid = false;
			}
			//	E-mail:
			String email = txtEmail.getText().toString().trim();
			if(!email.isEmpty() && !GeoKeyServer.EMAIL_VALIDATOR.isValid(email))
			{
				txtEmail.setBackgroundResource(R.color.red25percent);
				valid = false;
			}
			//	Password:
			String password = txtPassword.getText().toString(); 
			if(!email.isEmpty() && password.isEmpty())
			{
				txtPassword.setBackgroundResource(R.color.red25percent);
				valid = false;
			}
			//	Name:
			String name = txtReceiverName.getText().toString().trim();
			if(!valid)
				return; // !!!
			
			// Are we editing?
			boolean credetialsChanged = false;
			boolean changed = false;
			if(isEditing())
			{	// We are, check for changes:
				if(!editReceiver.getName().equals(name))
				{
					editReceiver.setName(name);
					changed = true;
				}
				if(!editReceiver.getUserEmail().equals(email) || !editReceiver.getUserPassword().equals(password)) 
				{
					editReceiver.setUserCredentials(email, password);
					credetialsChanged = changed = true;
				}
				// Check if any changes were made:
				if(!changed)
				{	// No changes, we are done here...
					dialog.dismiss();
					return; // !!!
				}
			}
			
			// Get account to save:
			toSave = isEditing() ? editReceiver : GeoKeyServer.CreateNew(name, url, email, password);
			
			// Verify by contacting server if needed...
			if(!isEditing() || credetialsChanged)
			{
				final GeoKeyServer toVerify = toSave;
				new VerifierTask(activity, new VerifierCallback()
				{
					@Override
					public void done(VerificationResult result)
					{
						switch(result)
						{
							case NoInternet:
								getOwner().showErrorDialog(R.string.connectionError, false);
								break;
							case InvalidServer:
								getOwner().showErrorDialog(activity.getString(R.string.geokeyInvalidServer, toVerify.getUrl(), GeoKeyClient.MINIMAL_GEOKEY_VERSION, GeoKeyClient.MINIMAL_GEOKEY_SAPELLI_VERSION), false);
								txtGeoKeyServerURL.setBackgroundResource(R.color.red25percent);
								break;
							case InvalidAccount:
								getOwner().showErrorDialog(R.string.geokeyInvalidAccount, false);
								txtEmail.setBackgroundResource(R.color.red25percent);
								txtPassword.setBackgroundResource(R.color.red25percent);
								break;
							case OK:
								Toast.makeText(activity, R.string.verificationSuccessful, Toast.LENGTH_LONG).show();
								validateAndSave(dialog, toVerify);
								break;
						case Aborted:
						default:
							break;
						}
					}
				}).execute(toVerify);
				return; // !!!
			}
		}
		
		// Save account & dismiss dialog:
		doSave(toSave);
		dialog.dismiss();
	}
	
	static private enum VerificationResult
	{
		Aborted,
		NoInternet,
		InvalidServer,
		InvalidAccount,
		OK
	};
	
	private class VerifierTask extends AsyncTaskWithWaitingDialog<ProjectManagerActivity, GeoKeyServer, VerificationResult>
	{
		
		private final VerifierCallback verificationCallback;
		
		public VerifierTask(ProjectManagerActivity activity, VerifierCallback callback)
		{
			super(activity, R.string.verifyingServerAndAccount);
			this.verificationCallback = callback;
		}
		
		@Override
		protected VerificationResult runInBackground(GeoKeyServer... params)
		{
			ProjectManagerActivity activity = getContext();
			if(activity == null)
				return null;
			
			if(!DeviceControl.isOnline(activity))
				return VerificationResult.NoInternet;

			// We are online, perform actual checks:
			AndroidGeoKeyClient client = new AndroidGeoKeyClient(activity.getCollectorApp());

			GeoKeyServer server = params[0];
			
			//	Check if server is reachable and running the right GeoKey & extension versions:
			if(!client.verifyServer(server))
				return VerificationResult.InvalidServer;
			
			//	Check if user can login:
			if(!client.connectAndLogin(server, false /*already verified*/))
				return VerificationResult.InvalidAccount;
			
			// All OK!:
			return VerificationResult.OK;
		}

		@Override
		protected void onPostExecute(VerificationResult result)
		{
			// Dismiss waiting dialog:
			super.onPostExecute(result);
			
			if(result == null)
				result = VerificationResult.Aborted;
			verificationCallback.done(result);
		}
	}
	
	private interface VerifierCallback
	{
		
		public void done(VerificationResult result);
		
	}

}
