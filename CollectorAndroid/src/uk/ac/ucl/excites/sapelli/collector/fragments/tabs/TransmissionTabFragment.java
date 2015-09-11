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

package uk.ac.ucl.excites.sapelli.collector.fragments.tabs;

import android.content.Context;
import android.os.Bundle;
import android.support.v7.widget.SwitchCompat;
import android.util.Log;
import android.view.View;
import android.view.View.OnClickListener;
import android.widget.AdapterView;
import android.widget.AdapterView.OnItemSelectedListener;
import android.widget.ArrayAdapter;
import android.widget.Button;
import android.widget.EditText;
import android.widget.LinearLayout;
import android.widget.Spinner;
import uk.ac.ucl.excites.sapelli.collector.R;
import uk.ac.ucl.excites.sapelli.collector.fragments.ProjectManagerTabFragment;
import uk.ac.ucl.excites.sapelli.collector.fragments.SMSReceiverFragment;
import uk.ac.ucl.excites.sapelli.collector.model.Project;
import uk.ac.ucl.excites.sapelli.collector.transmission.SendingSchedule;
import uk.ac.ucl.excites.sapelli.shared.db.StoreHandle.StoreUser;
import uk.ac.ucl.excites.sapelli.shared.db.exceptions.DBException;
import uk.ac.ucl.excites.sapelli.shared.util.android.AdvancedSpinnerAdapter;
import uk.ac.ucl.excites.sapelli.transmission.db.TransmissionStore;
import uk.ac.ucl.excites.sapelli.transmission.model.Correspondent;
import uk.ac.ucl.excites.sapelli.transmission.model.transport.sms.SMSCorrespondent;

/**
 * Fragment that defines the project manager layout per project (tabs)
 * 
 * @author Julia, mstevens
 */
public class TransmissionTabFragment extends ProjectManagerTabFragment implements OnClickListener, OnItemSelectedListener, StoreUser
{

	// Views
	//	Sending...
	private LinearLayout sendHeader;	
	private SwitchCompat switchSend;
	private LinearLayout sendSettings;
	private Spinner spinReceiver;
	private Button btnAddReceiver;
	private Button btnEditReceiver;
	private Button btnDeleteReceiver;
	private EditText txtSendIntervalMin;
	//	Receiving...
	private LinearLayout receiveHeader;
	private SwitchCompat switchReceive;
	private LinearLayout receiveSettings;
	
	// Adapter:
	private ArrayAdapter<Correspondent> spinReceiverAdapter;
	
	// DOA:
	private TransmissionStore transmissionStore;
	
	// Model:
	private SendingSchedule sendingSchedule;

	@Override
	public void onCreate(Bundle savedInstanceState)
	{
		super.onCreate(savedInstanceState);
		try
		{
			// Get transmission store object:
			transmissionStore = getOwner().getCollectorClient().transmissionStoreHandle.getStore(this);
		}
		catch(Exception e)
		{
			Log.e(getClass().getSimpleName(), "Could not get TransmissionStore object", e);
			throw new IllegalStateException(e);
		}
	}

	@Override
	protected Integer getLayoutID()
	{
		return R.layout.tab_transmission;
	}

	@Override
	protected void setupUI(View rootLayout)
	{
		// Sending config...
		sendHeader = (LinearLayout) rootLayout.findViewById(R.id.sendHeader);
		switchSend = (SwitchCompat) rootLayout.findViewById(R.id.switchSend);
		switchSend.setOnClickListener(this);
		sendSettings = (LinearLayout) rootLayout.findViewById(R.id.sendSettings);
		spinReceiver = (Spinner) rootLayout.findViewById(R.id.spinSendReceiver);
		spinReceiver.setOnItemSelectedListener(this);
		btnAddReceiver = (Button) rootLayout.findViewById(R.id.btnAddReceiver);
		btnAddReceiver.setOnClickListener(this);
		btnEditReceiver = (Button) rootLayout.findViewById(R.id.btnEditReceiver);
		btnEditReceiver.setOnClickListener(this);
		btnDeleteReceiver = (Button) rootLayout.findViewById(R.id.btnDeleteReceiver);
		btnDeleteReceiver.setOnClickListener(this);
		txtSendIntervalMin = (EditText) rootLayout.findViewById(R.id.txtSendIntervalMin);
		
		// Receiving config...
		receiveHeader = (LinearLayout) rootLayout.findViewById(R.id.receiveHeader);
		switchReceive = (SwitchCompat) rootLayout.findViewById(R.id.switchReceive);
		switchReceive.setOnClickListener(this);
		receiveSettings = (LinearLayout) rootLayout.findViewById(R.id.receiveSettings);
	}
	
	private void toggleConfigGroup(View clickview, View expandview, boolean show)
	{
		expandview.setVisibility(show ? View.VISIBLE : View.GONE);
		clickview.setBackgroundResource(show ? R.layout.drop_shadow_top : R.layout.drop_shadow);
	}

	@Override
	public void onResume()
	{
		super.onResume();
		update();
	}
	
	private void update()
	{
		Project project = getProject(false);
		if(project == null)
			return;
		
		try
		{
			// Get current schedule:
			sendingSchedule = getOwner().getProjectStore().retrieveSendScheduleForProject(project, transmissionStore);
		}
		catch(DBException e)
		{
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
		
		if(sendingSchedule == null)
			sendingSchedule = new SendingSchedule(project, false);
		
		// Update sending config UI parts:
		switchSend.setChecked(sendingSchedule.isEnabled());
		toggleConfigGroup(sendHeader, sendSettings, sendingSchedule.isEnabled());
		if(sendingSchedule.isEnabled() || true) // TODO hack hack
		{
			// Load receivers:
			spinReceiverAdapter =
				new AdvancedSpinnerAdapter<Correspondent>(getOwner(), getOwner().getString(R.string.lstPleaseSelect), "None", transmissionStore.retrieveCorrespondents(false))
				{
					@Override
					protected Integer getItemDrawableResourceId(int position, Correspondent receiver)
					{
						if(receiver == null)
							return null;
						//else:
						ReceiverDrawableProvider provider = new ReceiverDrawableProvider();
						receiver.handle(provider);
						return provider.drawableResourceId;
					}
				};
			spinReceiver.setAdapter(spinReceiverAdapter);

			// Select current receiver:
			if(sendingSchedule.getReceiver() != null)
				spinReceiver.setSelection(spinReceiverAdapter.getPosition(sendingSchedule.getReceiver()));
			
			// Set interval:
			txtSendIntervalMin.setText(Float.valueOf((float) sendingSchedule.getTransmitIntervalS() / 60.0f).toString()); 
		}
	}
	
	private Correspondent getReceiver()
	{
		return spinReceiverAdapter.getItem(spinReceiver.getSelectedItemPosition());
	}
	
	@Override
	public void onClick(View v)
	{
		switch(v.getId())
		{
			case R.id.switchSend :
				toggleConfigGroup(sendHeader, sendSettings, switchSend.isChecked());
				break;
			case R.id.switchReceive :
				toggleConfigGroup(receiveHeader, receiveSettings, switchReceive.isChecked());
				break;
			case R.id.btnAddReceiver :
				SMSReceiverFragment.ShowAddDialog(getOwner());
				break;
			case R.id.btnEditReceiver :
				if(getReceiver() != null)
					getReceiver().handle(new Correspondent.Handler()
					{
						@Override
						public void handle(SMSCorrespondent smsCorrespondent)
						{
							SMSReceiverFragment.ShowEditDialog(getOwner(), smsCorrespondent);
						}
					});
				break;
			case R.id.btnDeleteReceiver :
				if(getReceiver() != null)
				{
					// TODO
				}
				break;
		}
	}
	
	@Override
	public void onItemSelected(AdapterView<?> parent, View view, int position, long id)
	{
		btnEditReceiver.setEnabled(spinReceiverAdapter.getItem(position) != null);
		btnDeleteReceiver.setEnabled(spinReceiverAdapter.getItem(position) != null);
	}

	@Override
	public void onNothingSelected(AdapterView<?> parent)
	{
		// TODO Auto-generated method stub
		
	}
	
	public void saveSettings()
	{
		//TODO detect actual changes
		
		sendingSchedule.setEnabled(switchSend.isChecked());
		
		sendingSchedule.setReceiver(getReceiver());
			
		//sendingSchedule.setTransmitIntervalS(transmitIntervalS)
		
		// TODO input validation
		
		// Store new/updated schedule:
		getOwner().getProjectStore().storeSendSchedule(sendingSchedule, transmissionStore);
		
		// TODO schedule (and unschedule old?)
		//Log.d(getClass().getSimpleName(), "Starting alarm scheduler...");
		//DataSendingSchedulingService.ScheduleAll(getOwner().getApplicationContext());
		
	}
	
	@Override
	public void onDestroy()
	{
		// clean up:
		getOwner().getCollectorClient().transmissionStoreHandle.doneUsing(this);
		// super:
		super.onDestroy();
	}

	@Override
	public String getTabTitle(Context context)
	{
		return context.getString(R.string.tab_transmission);
	}
	
	private class ReceiverDrawableProvider implements Correspondent.Handler
	{
		
		Integer drawableResourceId = null;
		
		@Override
		public void handle(SMSCorrespondent smsCorrespondent)
		{
			drawableResourceId = R.drawable.ic_sms_black_24dp;
		}
		
	}
	
}
