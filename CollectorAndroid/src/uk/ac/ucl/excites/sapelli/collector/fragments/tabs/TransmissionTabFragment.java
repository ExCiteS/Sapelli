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

import uk.ac.ucl.excites.sapelli.collector.CollectorApp;
import uk.ac.ucl.excites.sapelli.collector.R;
import uk.ac.ucl.excites.sapelli.collector.fragments.ProjectManagerTabFragment;
import uk.ac.ucl.excites.sapelli.collector.model.Project;
import uk.ac.ucl.excites.sapelli.collector.services.DataSendingSchedulingService;
import uk.ac.ucl.excites.sapelli.collector.transmission.SendingSchedule;
import uk.ac.ucl.excites.sapelli.shared.db.StoreHandle.StoreUser;
import uk.ac.ucl.excites.sapelli.shared.db.exceptions.DBException;
import uk.ac.ucl.excites.sapelli.transmission.db.TransmissionStore;
import uk.ac.ucl.excites.sapelli.transmission.model.Correspondent;
import uk.ac.ucl.excites.sapelli.transmission.model.transport.sms.SMSCorrespondent;

import java.util.List;

import android.content.Context;
import android.os.Bundle;
import android.util.Log;
import android.view.View;
import android.view.View.OnClickListener;
import android.view.ViewGroup;
import android.widget.AdapterView;
import android.widget.AdapterView.OnItemSelectedListener;
import android.widget.ArrayAdapter;
import android.widget.CheckBox;
import android.widget.ImageView;
import android.widget.LinearLayout;
import android.widget.Spinner;
import android.widget.SpinnerAdapter;
import android.widget.TextView;

/**
 * Fragment that defines the project manager layout per project (tabs)
 * 
 * @author Julia, mstevens
 */
public class TransmissionTabFragment extends ProjectManagerTabFragment implements OnClickListener, StoreUser
{

	// Views
	private LinearLayout sendDataHeader;
	private LinearLayout sendDataView;
	private LinearLayout receiveDataHeader;
	private TextView receiveDataView;
	private CheckBox checkSend;
	private CheckBox checkReceive;
	private ImageView expandSend;
	private ImageView expandReceive;
	private Spinner spinSendReceiver;
	private ReceiverAdapter spinSendReceiverAdapter;
	
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
			transmissionStore = getOwner().getCollectorApp().collectorClient.transmissionStoreHandle.getStore(this);
		}
		catch(DBException dbE)
		{
			Log.e(getClass().getSimpleName(), "Could not get TransmissionStore object", dbE);
			throw new IllegalStateException(dbE);
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
		sendDataHeader = (LinearLayout) rootLayout.findViewById(R.id.sendHeader);
		sendDataView = (LinearLayout) rootLayout.findViewById(R.id.sendSettings);
		sendDataHeader.setOnClickListener(this);

		receiveDataHeader = (LinearLayout) rootLayout.findViewById(R.id.receiveHeader);
		receiveDataView = (TextView) rootLayout.findViewById(R.id.receiveSettings);
		receiveDataHeader.setOnClickListener(this);

		checkSend = (CheckBox) rootLayout.findViewById(R.id.checkSend);
		checkSend.setOnClickListener(this);
		checkReceive = (CheckBox) rootLayout.findViewById(R.id.checkReceive);
		checkReceive.setOnClickListener(this);

		// on old Android versions the label overlaps the checkbox
		if(android.os.Build.VERSION.SDK_INT < android.os.Build.VERSION_CODES.JELLY_BEAN_MR1)
		{
			addChbxPadding(checkSend);
			addChbxPadding(checkReceive);
		}

		expandSend = (ImageView) rootLayout.findViewById(R.id.expandSend);
		expandReceive = (ImageView) rootLayout.findViewById(R.id.expandReceive);
		
		spinSendReceiver = (Spinner) rootLayout.findViewById(R.id.spinSendReceiver);
	}

	/**
	 * Handles layout changes when views are expanded
	 * 
	 * @param clickview
	 * @param expandview
	 */
	private void toggleView(View clickview, View expandview)
	{
		expandview.setVisibility(expandview.isShown() ? View.GONE : View.VISIBLE);
		clickview.setBackgroundResource(expandview.isShown() ? R.layout.drop_shadow_top : R.layout.drop_shadow);
		((ImageView) ((ViewGroup) clickview).getChildAt(1)).setImageResource(expandview.isShown() ? R.drawable.ic_action_collapse : R.drawable.ic_action_expand);
	}

	/**
	 * Makes view non-expandable
	 * 
	 * @param clickview
	 * @param expandview
	 */
	private void disableToggle(View clickview, View expandview)
	{
		expandview.setVisibility(View.GONE);
		clickview.setBackgroundResource(R.layout.drop_shadow);
		((ImageView) ((ViewGroup) clickview).getChildAt(1)).setVisibility(View.GONE); // TODO hackish
	}

	@Override
	public void onClick(View v)
	{
		switch(v.getId())
		{
			case R.id.sendHeader:
				toggleView(sendDataHeader, sendDataView);
				break;
			case R.id.receiveHeader:
				toggleView(receiveDataHeader, receiveDataView);
				break;
			case R.id.checkSend:
				if(checkSend.isChecked())
				{
					expandSend.setVisibility(View.VISIBLE);
					toggleView(sendDataHeader, sendDataView);
				}
				else
				{
					disableToggle(sendDataHeader, sendDataView);
				}
				break;
			case R.id.checkReceive:
				if(checkReceive.isChecked())
				{
					expandReceive.setVisibility(View.VISIBLE);
					toggleView(receiveDataHeader, receiveDataView);
				}
				else
				{
					disableToggle(receiveDataHeader, receiveDataView);
				}
				break;
		}
	}

	/**
	 * @param checkbox
	 * @see http://stackoverflow.com/questions/4037795/android-spacing-between-checkbox-and-text
	 */
	private void addChbxPadding(CheckBox checkbox)
	{
		checkbox.setPadding(checkbox.getPaddingLeft() + (int) (30.0f * this.getResources().getDisplayMetrics().density + 0.5f), checkbox.getPaddingTop(), checkbox.getPaddingRight(), checkbox.getPaddingBottom());
	}

	@Override
	public void onResume()
	{
		super.onResume();
		
		if(checkSend.isChecked())
			((ImageView) ((ViewGroup) sendDataHeader).getChildAt(1)).setVisibility(View.VISIBLE);
		if(checkReceive.isChecked())
			((ImageView) ((ViewGroup) receiveDataHeader).getChildAt(1)).setVisibility(View.VISIBLE);
		
		Project project = getProject(false);
		if(project == null)
			return;
		
		try
		{
			// Get current schedule:
			sendingSchedule = getOwner().getProjectStore().retrieveSendScheduleForProject(project, transmissionStore);
			if(sendingSchedule == null)
				sendingSchedule = new SendingSchedule(project, false);
			
			// Update sending config UI parts:
			checkSend.setEnabled(sendingSchedule.isEnabled());
			
			
			// once project loading done, store a dummy schedule:
//			Correspondent receiver = new SMSCorrespondent("Matthias Belgium", "+32486170492", false);
//			TransmissionStore tStore = ((CollectorApp)this.getApplication()).collectorClient.transmissionStoreHandle.getStore(this);
//			tStore.store(receiver);
//			if(projectStore.retrieveSendScheduleForProject(currentProject, tStore) == null)
//				projectStore.storeSendSchedule(new SendingSchedule(currentProject, true).setReceiver(receiver).setTransmitIntervalS(60 /*1min*/), tStore);
			
			spinSendReceiverAdapter = new ReceiverAdapter(getOwner(), transmissionStore.retrieveCorrespondents(false));
			spinSendReceiver.setAdapter(spinSendReceiverAdapter);
			
			// Select current receiver
			if(sendingSchedule.getReceiver() != null)
				spinSendReceiver.setSelection(spinSendReceiverAdapter.getPosition(sendingSchedule.getReceiver()));
			spinSendReceiver.setOnItemSelectedListener(new OnItemSelectedListener()
			{

				@Override
				public void onItemSelected(AdapterView<?> parent, View view, int position, long id)
				{
					saveSettings();
				}

				@Override
				public void onNothingSelected(AdapterView<?> parent)
				{
					// TODO Auto-generated method stub
					
				}
			});
			
		}
		catch(DBException e)
		{
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
		
	}
	
	public void saveSettings()
	{
		//TODO detect actual changes
		
		sendingSchedule.setEnabled(checkSend.isChecked());
		sendingSchedule.setReceiver(spinSendReceiverAdapter.getItem(spinSendReceiver.getSelectedItemPosition()));
		//sendingSchedule.setTransmitIntervalS(transmitIntervalS)
		
		// Store new/updated schedule:
		getOwner().getProjectStore().storeSendSchedule(sendingSchedule, transmissionStore);
		
		//Log.d(getClass().getSimpleName(), "Starting alarm scheduler...");
		//DataSendingSchedulingService.ScheduleAll(getOwner().getApplicationContext());

		
	}
	
	@Override
	public void onDestroy()
	{
		// clean up:
		getOwner().getCollectorApp().collectorClient.transmissionStoreHandle.doneUsing(this);
		// super:
		super.onDestroy();
	}

	@Override
	public String getTabTitle(Context context)
	{
		return context.getString(R.string.tab_transmission);
	}
	
	private class ReceiverAdapter extends ArrayAdapter<Correspondent>
	{

		public ReceiverAdapter(Context context, List<Correspondent> receivers)
		{
			super(context, android.R.layout.simple_spinner_item, receivers.toArray(new Correspondent[receivers.size()]));
		}
		
	}
	
}
