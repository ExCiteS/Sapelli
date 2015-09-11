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

package uk.ac.ucl.excites.sapelli.collector.fragments;

import com.google.i18n.phonenumbers.Phonenumber.PhoneNumber;

import android.annotation.SuppressLint;
import android.app.AlertDialog;
import android.app.Dialog;
import android.content.DialogInterface;
import android.os.Bundle;
import android.support.v7.app.AppCompatActivity;
import android.util.Log;
import android.view.View;
import android.widget.CheckBox;
import android.widget.EditText;
import uk.ac.ucl.excites.sapelli.collector.R;
import uk.ac.ucl.excites.sapelli.shared.db.StoreHandle.StoreUser;
import uk.ac.ucl.excites.sapelli.shared.util.ExceptionHelpers;
import uk.ac.ucl.excites.sapelli.shared.util.android.DeviceControl;
import uk.ac.ucl.excites.sapelli.shared.util.android.DialogHelpers;
import uk.ac.ucl.excites.sapelli.transmission.db.TransmissionStore;
import uk.ac.ucl.excites.sapelli.transmission.model.Transmission.Type;
import uk.ac.ucl.excites.sapelli.transmission.model.transport.sms.SMSCorrespondent;

/**
 * @author mstevens
 *
 */
public class SMSReceiverFragment extends ProjectManagerFragment implements DialogInterface.OnClickListener, StoreUser
{
	
	// STATIC -------------------------------------------------------
	static public void ShowAddDialog(AppCompatActivity owner)
	{
		new SMSReceiverFragment().show(owner.getSupportFragmentManager(), R.string.add + SMSCorrespondent.class.getSimpleName());
	}

	static public void ShowEditDialog(AppCompatActivity owner, SMSCorrespondent editCorrespondent)
	{
		new SMSReceiverFragment(editCorrespondent).show(owner.getSupportFragmentManager(), R.string.edit + SMSCorrespondent.class.getSimpleName());
	}
	
	// DYNAMIC ------------------------------------------------------
	private SMSCorrespondent editReceiver;
	
	private EditText txtReceiverName;
	private EditText txtReceiverPhoneNumber;
	private CheckBox chkBinarySMS;
	
	public SMSReceiverFragment()
	{
		this(null);
	}
	
	public SMSReceiverFragment(SMSCorrespondent editCorrespondent)
	{
		this.editReceiver = editCorrespondent;
	}
	
	/* (non-Javadoc)
	 * @see uk.ac.ucl.excites.sapelli.collector.fragments.ProjectManagerFragment#getLayoutID()
	 */
	@Override
	protected Integer getLayoutID()
	{
		return R.layout.fragment_sms_receceiver;
	}
	
	/* (non-Javadoc)
	 * @see uk.ac.ucl.excites.sapelli.collector.fragments.ProjectManagerFragment#setupUI(android.view.View)
	 */
	@Override
	protected void setupUI(View rootLayout)
	{
		txtReceiverName = (EditText) rootLayout.findViewById(R.id.txtReceiverName);
		txtReceiverPhoneNumber = (EditText) rootLayout.findViewById(R.id.txtReceiverPhoneNumber);
		chkBinarySMS = (CheckBox) rootLayout.findViewById(R.id.chkBinarySMS);
		
		if(editReceiver != null)
		{
			txtReceiverName.setText(editReceiver.getName());
			txtReceiverPhoneNumber.setText(editReceiver.getPhoneNumberInternational());
			chkBinarySMS.setChecked(editReceiver.getTransmissionType() == Type.BINARY_SMS);
		}
		else if(DeviceControl.getSimCountryISOCode(getOwner()) != null)
		{
			String countyCode = "+" + SMSCorrespondent.findCountryCode(DeviceControl.getSimCountryISOCode(getOwner()));
			txtReceiverPhoneNumber.setText(countyCode);
			txtReceiverPhoneNumber.setSelection(countyCode.length());
		}
	}
	
	@SuppressLint("InflateParams")
	@Override
	public Dialog onCreateDialog(Bundle savedInstanceState)
	{
		AlertDialog.Builder builder = new AlertDialog.Builder(getOwner())
		.setIcon(R.drawable.ic_sms_black_36dp)
		.setTitle(editReceiver == null ? R.string.addReceiver : R.string.editReceiver)
		.setPositiveButton(android.R.string.ok, null) // listener will be set through the MakeNonDismission() call below
		.setNegativeButton(android.R.string.cancel, this);
		final AlertDialog dialog = builder.create();
		
		DialogHelpers.MakeNonDismissing(dialog, this, DialogInterface.BUTTON_POSITIVE);
		
		// Set view:
		View rootLayout = getActivity().getLayoutInflater().inflate(getLayoutID(), null);
		setupUI(rootLayout);
		int lrSpacingPx = getDialogLeftRightPaddingPx();
		dialog.setView(rootLayout, lrSpacingPx, getDialogMessageToViewSpacingPx(), lrSpacingPx, 0);

		return dialog;
	}

	@Override
	public void onClick(DialogInterface dialog, int which)
	{
		switch(which)
		{
			case DialogInterface.BUTTON_POSITIVE :
				if(saveChanges())
					dialog.dismiss();
				break;
			case DialogInterface.BUTTON_NEGATIVE :
				// TODO callback?
				break;
		}
	}
	
	/**
	 * @return whether or not to dismiss the dialog
	 */
	private boolean saveChanges()
	{
		// Input validation:
		//	Name:
		String name = txtReceiverName.getText().toString();
		if(name.isEmpty())
		{
			getOwner().showErrorDialog(R.string.emptyReceiverName);
			txtReceiverName.requestFocus();
			return false;
		}
		//	PhoneNumber:
		PhoneNumber phoneNumber = null;
		try
		{
			phoneNumber = SMSCorrespondent.toPhoneNumber(txtReceiverPhoneNumber.getText().toString(), DeviceControl.getSimCountryISOCode(getOwner()));
		}
		catch(Exception e)
		{
			getOwner().showErrorDialog(R.string.invalidPhoneNumber);
			txtReceiverPhoneNumber.requestFocus();
			return false;
		}
		//	Mode:
		boolean binarySMS = chkBinarySMS.isChecked();
		
		if(editReceiver != null) // We are editing... 
		{	// Check if actual changes were made:
			if(	!editReceiver.getName().equals(name) ||
				!editReceiver.getPhoneNumber().equals(phoneNumber) ||
				(editReceiver.getTransmissionType() == Type.BINARY_SMS) != binarySMS)
			{
				editReceiver.markAsUserDeleted();
				store(editReceiver);
				// TODO what to do with other projects using the same receiver?
				editReceiver = null; // ensure we don't use it below
			}
			else
				return true; // no changes made! (TODO callback?)
		}
		
		// Save new/edited correspondent:
		SMSCorrespondent newReceiver = new SMSCorrespondent(name, phoneNumber, binarySMS);
		store(newReceiver);
		
		// TODO callback
		
		return true;
	}
	
	private void store(SMSCorrespondent correspondent)
	{
		try
		{
			// Get RecordStore instance:
			TransmissionStore tStore = getOwner().getCollectorClient().transmissionStoreHandle.getStore(this);
			
			// Store/update correspondent:
			tStore.store(correspondent);
		}
		catch(Exception e)
		{
			Log.d(getClass().getSimpleName(), ExceptionHelpers.getMessageAndCause(e));
		}
		finally
		{
			getOwner().getCollectorClient().transmissionStoreHandle.doneUsing(this);
		}
	}
	
}
