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

import com.google.i18n.phonenumbers.Phonenumber.PhoneNumber;

import android.annotation.SuppressLint;
import android.app.AlertDialog;
import android.content.DialogInterface;
import android.os.Bundle;
import android.view.View;
import android.widget.CheckBox;
import android.widget.CompoundButton;
import android.widget.CompoundButton.OnCheckedChangeListener;
import android.widget.EditText;
import uk.ac.ucl.excites.sapelli.collector.R;
import uk.ac.ucl.excites.sapelli.collector.activities.ProjectManagerActivity;
import uk.ac.ucl.excites.sapelli.collector.transmission.SendConfigurationHelpers;
import uk.ac.ucl.excites.sapelli.collector.transmission.SendConfigurationHelpers.ReceiverUpdateCallback;
import uk.ac.ucl.excites.sapelli.shared.util.android.DeviceControl;
import uk.ac.ucl.excites.sapelli.transmission.model.Transmission.Type;
import uk.ac.ucl.excites.sapelli.transmission.model.transport.sms.SMSCorrespondent;

/**
 * @author mstevens
 *
 */
public class SMSReceiverFragment extends AbstractReceiverFragment<SMSCorrespondent> implements DialogInterface.OnClickListener
{
	
	// STATIC -------------------------------------------------------
	static public void ShowAddDialog(ProjectManagerActivity owner, ReceiverUpdateCallback callback)
	{
		new SMSReceiverFragment(callback).show(owner.getSupportFragmentManager(), R.string.add + SMSCorrespondent.class.getSimpleName());
	}

	static public void ShowEditDialog(ProjectManagerActivity owner, ReceiverUpdateCallback callback, SMSCorrespondent editCorrespondent)
	{
		new SMSReceiverFragment(callback, editCorrespondent).show(owner.getSupportFragmentManager(), R.string.edit + SMSCorrespondent.class.getSimpleName());
	}
	
	// DYNAMIC ------------------------------------------------------
	private EditText txtReceiverName;
	private EditText txtReceiverPhoneNumber;
	private CheckBox chkBinarySMS;
	
	public SMSReceiverFragment(ReceiverUpdateCallback callback)
	{
		this(callback, null);
	}
	
	public SMSReceiverFragment(ReceiverUpdateCallback callback, SMSCorrespondent receiver)
	{
		super(callback, receiver);
	}
	
	/* (non-Javadoc)
	 * @see uk.ac.ucl.excites.sapelli.collector.fragments.ProjectManagerFragment#getLayoutID()
	 */
	@Override
	protected Integer getLayoutID()
	{
		return R.layout.dialog_sms_receiver;
	}
	
	/* (non-Javadoc)
	 * @see uk.ac.ucl.excites.sapelli.collector.fragments.ProjectManagerFragment#setupUI(uk.ac.ucl.excites.sapelli.collector.activities.ProjectManagerActivity, android.view.View)
	 */
	@Override
	protected void setupUI(final ProjectManagerActivity owner, final View rootLayout)
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
		else
		{
			txtReceiverName.setText("");
			if(DeviceControl.getSimCountryISOCode(owner) != null)
			{
				String countyCode = "+" + SMSCorrespondent.findCountryCode(DeviceControl.getSimCountryISOCode(owner));
				txtReceiverPhoneNumber.setText(countyCode);
				txtReceiverPhoneNumber.setSelection(countyCode.length());
			}
			else
				txtReceiverPhoneNumber.setText("");
			chkBinarySMS.setChecked(SMSCorrespondent.DEFAULT_BINARY_SMS);
		}
		
		// Dis/enable editing of finals (only the name can be edited):
		txtReceiverPhoneNumber.setEnabled(!isEditing());
		chkBinarySMS.setEnabled(!isEditing());
	}
	
	@SuppressLint("InflateParams")
	@Override
	public AlertDialog onCreateDialog(Bundle savedInstanceState)
	{
		final AlertDialog dialog = super.onCreateDialog(savedInstanceState);
		
		// Switch dialog icon based on chkBinarySMS state:
		chkBinarySMS.setOnCheckedChangeListener(new OnCheckedChangeListener()
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
	protected int getIconId()
	{
		return SendConfigurationHelpers.getSMSReceiverDrawable(isEditing() ? editReceiver.isBinary() : SMSCorrespondent.DEFAULT_BINARY_SMS, true);
	}

	@Override
	protected void validateAndSave(DialogInterface dialog, SMSCorrespondent toSave)
	{
		if(toSave == null)
		{
			// Input validation:
			//	Name:
			String name = txtReceiverName.getText().toString();
			if(name.isEmpty())
			{
				getOwner().showErrorDialog(R.string.emptyReceiverName);
				txtReceiverName.requestFocus();
				return;
			}
			//	PhoneNumber:
			PhoneNumber phoneNumber = null;
			if(!isEditing())
			{
				try
				{
					phoneNumber = SMSCorrespondent.toPhoneNumber(txtReceiverPhoneNumber.getText().toString(), DeviceControl.getSimCountryISOCode(getOwner()));
				}
				catch(Exception e)
				{
					getOwner().showErrorDialog(R.string.invalidPhoneNumber);
					txtReceiverPhoneNumber.requestFocus();
					return;
				}
			}
			//	Mode:
			boolean binarySMS = chkBinarySMS.isChecked();
		
			// Check if we are editing...
			if(isEditing())
			{	// We are, check if actual changes were made (only name can be changed):
				if(!editReceiver.getName().equals(name))
					// Update name:
					editReceiver.setName(name);
				else
				{	// No changes, we are done here...
					dialog.dismiss();
					return;
				}
			}
			
			// set/create SMSCorrespondent to save:
			toSave = isEditing() ? editReceiver : new SMSCorrespondent(name, phoneNumber, binarySMS);
		}
		
		// Save correspondent & dismiss dialog:
		doSave(toSave);
		dialog.dismiss();
	}

}
