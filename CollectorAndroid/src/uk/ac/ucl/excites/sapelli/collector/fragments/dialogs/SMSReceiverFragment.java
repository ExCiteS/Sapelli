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

import com.google.i18n.phonenumbers.Phonenumber.PhoneNumber;

import android.annotation.SuppressLint;
import android.app.AlertDialog;
import android.app.Dialog;
import android.content.DialogInterface;
import android.os.Bundle;
import android.support.v7.app.AppCompatActivity;
import android.view.View;
import android.widget.CheckBox;
import android.widget.CompoundButton;
import android.widget.CompoundButton.OnCheckedChangeListener;
import android.widget.EditText;
import uk.ac.ucl.excites.sapelli.collector.R;
import uk.ac.ucl.excites.sapelli.collector.fragments.ProjectManagerFragment;
import uk.ac.ucl.excites.sapelli.collector.transmission.SendConfigurationHelpers;
import uk.ac.ucl.excites.sapelli.collector.transmission.SendConfigurationHelpers.ReceiverUpdateCallback;
import uk.ac.ucl.excites.sapelli.shared.util.android.DeviceControl;
import uk.ac.ucl.excites.sapelli.shared.util.android.DialogHelpers;
import uk.ac.ucl.excites.sapelli.transmission.model.Transmission.Type;
import uk.ac.ucl.excites.sapelli.transmission.model.transport.sms.SMSCorrespondent;

/**
 * @author mstevens
 *
 */
public class SMSReceiverFragment extends ProjectManagerFragment implements DialogInterface.OnClickListener
{
	
	// STATIC -------------------------------------------------------
	static public void ShowAddDialog(AppCompatActivity owner, ReceiverUpdateCallback callback)
	{
		new SMSReceiverFragment(callback).show(owner.getSupportFragmentManager(), R.string.add + SMSCorrespondent.class.getSimpleName());
	}

	static public void ShowEditDialog(AppCompatActivity owner, ReceiverUpdateCallback callback, SMSCorrespondent editCorrespondent)
	{
		new SMSReceiverFragment(callback, editCorrespondent).show(owner.getSupportFragmentManager(), R.string.edit + SMSCorrespondent.class.getSimpleName());
	}
	
	// DYNAMIC ------------------------------------------------------
	private final ReceiverUpdateCallback callback;
	
	private SMSCorrespondent editReceiver;
	
	private EditText txtReceiverName;
	private EditText txtReceiverPhoneNumber;
	private CheckBox chkBinarySMS;
	
	public SMSReceiverFragment(ReceiverUpdateCallback callback)
	{
		this(callback, null);
	}
	
	public SMSReceiverFragment(ReceiverUpdateCallback callback, SMSCorrespondent receiver)
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
		return R.layout.dialog_sms_receiver;
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
		txtReceiverName = (EditText) rootLayout.findViewById(R.id.txtSMSReceiverName);
		txtReceiverPhoneNumber = (EditText) rootLayout.findViewById(R.id.txtReceiverPhoneNumber);
		chkBinarySMS = (CheckBox) rootLayout.findViewById(R.id.chkBinarySMS);
		
		if(isEditing())
		{
			txtReceiverName.setText(editReceiver.getName());
			txtReceiverPhoneNumber.setText(editReceiver.getPhoneNumberInternational());
			chkBinarySMS.setChecked(editReceiver.getTransmissionType() == Type.BINARY_SMS);
		}
		else if(DeviceControl.getSimCountryISOCode(getOwner()) != null)
		{
			txtReceiverName.setText("");
			String countyCode = "+" + SMSCorrespondent.findCountryCode(DeviceControl.getSimCountryISOCode(getOwner()));
			txtReceiverPhoneNumber.setText(countyCode);
			txtReceiverPhoneNumber.setSelection(countyCode.length());
			chkBinarySMS.setChecked(SMSCorrespondent.DEFAULT_BINARY_SMS);
		}
	}
	
	@SuppressLint("InflateParams")
	@Override
	public Dialog onCreateDialog(Bundle savedInstanceState)
	{
		AlertDialog.Builder builder = new AlertDialog.Builder(getOwner())
		.setIcon(SendConfigurationHelpers.getSMSReceiverDrawable(isEditing() ? editReceiver.isBinary() : SMSCorrespondent.DEFAULT_BINARY_SMS, true))
		.setTitle(isEditing() ? R.string.editReceiver : R.string.addReceiver)
		.setPositiveButton(android.R.string.ok, null) // listener will be set through the MakeNonDismission() call below
		.setNegativeButton(android.R.string.cancel, this);
		final AlertDialog dialog = builder.create();
		
		DialogHelpers.MakeNonDismissing(dialog, this, DialogInterface.BUTTON_POSITIVE);
		
		// Set view:
		int lrSpacingPx = getDialogLeftRightPaddingPx();
		View layout = getRootLayout();
		dialog.setView(layout, lrSpacingPx, getDialogMessageToViewSpacingPx(), lrSpacingPx, 0);
		
		// Switch dialog icon based on chkBinarySMS state:
		((CheckBox) layout.findViewById(R.id.chkBinarySMS)).setOnCheckedChangeListener(new OnCheckedChangeListener()
		{
			@Override
			public void onCheckedChanged(CompoundButton buttonView, boolean isChecked)
			{
				dialog.setIcon(SendConfigurationHelpers.getSMSReceiverDrawable(isChecked, true));
			}
		});
		
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
		
		// Check if we are editing...
		if(isEditing())
		{	// We are, check if actual changes were made:
			if(	!editReceiver.getName().equals(name) ||
				!editReceiver.getPhoneNumber().equals(phoneNumber) ||
				(editReceiver.getTransmissionType() == Type.BINARY_SMS) != binarySMS)
			{	// The receiver has been changed...
				//	We cannot alter existing correspondents to instead we delete (or hide it), and replace it by a new one to be stored below.
				SendConfigurationHelpers.deleteCorrespondent(getOwner(), editReceiver); // TODO what with transmittable records scheduled for this receiver??
				// TODO what to do with other projects using the same receiver? --> update their schedules! (And transmittables?)				
			}
			else
				// No changes, we are done here...
				return true;
		}
		
		// Save correspondent:
		SMSCorrespondent toSave = new SMSCorrespondent(name, phoneNumber, binarySMS);
		SendConfigurationHelpers.saveCorrespondent(getOwner(), toSave);
		if(callback != null)
		{
			if(!isEditing())
				callback.newReceiver(toSave);
			else
				callback.editedReceiver(toSave, editReceiver);
		}
		
		return true;
	}
		
}
