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

import java.util.List;

import android.content.Context;
import android.os.Bundle;
import android.support.v7.widget.SwitchCompat;
import android.util.Log;
import android.view.ContextMenu;
import android.view.ContextMenu.ContextMenuInfo;
import android.view.Menu;
import android.view.MenuItem;
import android.view.View;
import android.view.View.OnClickListener;
import android.view.ViewGroup;
import android.widget.AdapterView;
import android.widget.Button;
import android.widget.CompoundButton;
import android.widget.CompoundButton.OnCheckedChangeListener;
import android.widget.LinearLayout;
import android.widget.ListView;
import android.widget.TextView;
import uk.ac.ucl.excites.sapelli.collector.R;
import uk.ac.ucl.excites.sapelli.collector.fragments.ProjectManagerTabFragment;
import uk.ac.ucl.excites.sapelli.collector.fragments.SendScheduleFragment;
import uk.ac.ucl.excites.sapelli.collector.model.Project;
import uk.ac.ucl.excites.sapelli.collector.transmission.SendConfigurationHelpers;
import uk.ac.ucl.excites.sapelli.collector.transmission.SendSchedule;
import uk.ac.ucl.excites.sapelli.shared.util.TransactionalStringBuilder;
import uk.ac.ucl.excites.sapelli.shared.util.android.AdvancedSpinnerAdapter;
import uk.ac.ucl.excites.sapelli.shared.util.android.DeviceControl;
import uk.ac.ucl.excites.sapelli.transmission.model.Correspondent;
import uk.ac.ucl.excites.sapelli.transmission.model.transport.sms.SMSCorrespondent;

/**
 * Fragment that defines the project manager layout per project (tabs)
 * 
 * @author Julia, mstevens
 */
public class TransmissionTabFragment extends ProjectManagerTabFragment implements OnClickListener
{

	public static final float SEC_IN_MIN = 60.0f;
	
	// Views
	//	Sending...
	private LinearLayout sendHeader;	
	private SwitchCompat switchSend;
	private LinearLayout sendSettings;
	private ListView listSchedules;
	private Button btnAddSchedule;
	//	Receiving...
	private LinearLayout receiveHeader;
	private SwitchCompat switchReceive;
	private LinearLayout receiveSettings;
	
	// Adapter:
	private SendScheduleAdapter listScheduleAdapter;
	
	@Override
	public void onCreate(Bundle savedInstanceState)
	{
		super.onCreate(savedInstanceState);
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
		listSchedules = (ListView) rootLayout.findViewById(R.id.listSendSchedules);
		btnAddSchedule = (Button) rootLayout.findViewById(R.id.btnAddSchedule);
		btnAddSchedule.setOnClickListener(this);

		// Receiving config...
		receiveHeader = (LinearLayout) rootLayout.findViewById(R.id.receiveHeader);
		switchReceive = (SwitchCompat) rootLayout.findViewById(R.id.switchReceive);
		switchReceive.setOnClickListener(this);
		receiveSettings = (LinearLayout) rootLayout.findViewById(R.id.receiveSettings);
	}
	
	private void toggleConfigGroup(boolean send, boolean enabled)
	{
		// Switch state:
		(send ? switchSend : switchReceive).setChecked(enabled);
		// Header background:
		(send ? sendHeader : receiveHeader).setBackgroundResource(enabled ? R.drawable.drop_shadow_top : R.drawable.drop_shadow);
		// Settings pane:
		(send ? sendSettings : receiveSettings).setVisibility(enabled ? View.VISIBLE : View.GONE);
	}
	
	@Override
	protected void refresh(Project project)
	{
		if(project == null)
			return;
		
		// Update sending config UI parts:
		// 	Get schedules for project:
		List<SendSchedule> schedules = getSchedules();
		//	Populate list:
		listScheduleAdapter = new SendScheduleAdapter(listSchedules.getContext(), schedules); 
		listSchedules.setAdapter(listScheduleAdapter);
		registerForContextMenu(listSchedules);
		
		//	Set sending switch:
		boolean sendingEnabled = false;
		for(SendSchedule schedule : schedules)
			if(schedule.isEnabled())
			{
				sendingEnabled = true;
				break;
			}
		toggleConfigGroup(true, sendingEnabled);
	}
	
	@Override
	public void onClick(View v)
	{
		switch(v.getId())
		{
			case R.id.switchSend :
				toggleConfigGroup(true, switchSend.isChecked());
				if(switchSend.isChecked())
				{
					// If there is no schedule make one...
					if(listScheduleAdapter.isEmpty())
						SendScheduleFragment.ShowAddDialog(this);
				}
				else
					// Disable all schedules:
					disableSending();
				break;
			case R.id.switchReceive :
				toggleConfigGroup(false, switchReceive.isChecked());
				break;
			case R.id.btnAddSchedule :
				SendScheduleFragment.ShowAddDialog(this);
				break;
		}
	}
	
	@Override
	public void onCreateContextMenu(ContextMenu menu, View v, ContextMenuInfo menuInfo)
	{
		if(v.getId() == R.id.listSendSchedules)
		{
			SendSchedule schedule = listScheduleAdapter.getItem(((AdapterView.AdapterContextMenuInfo) menuInfo).position);
			menu.setHeaderTitle(getString(R.string.schedule) + " for " + schedule.getReceiver().getName());
			// Menu items: 
			int[] menuItemTitles = new int[] { R.string.editEtc, R.string.delete };
			for(int m = 0; m < menuItemTitles.length; m++)
				menu.add(Menu.NONE, menuItemTitles[m], m, menuItemTitles[m]);
		}
	}
	
	@Override
	public boolean onContextItemSelected(MenuItem item)
	{
		SendSchedule schedule = listScheduleAdapter.getItem(((AdapterView.AdapterContextMenuInfo) item.getMenuInfo()).position);		
		switch(item.getItemId())
		{
			case R.string.editEtc :
				SendScheduleFragment.ShowEditDialog(this, schedule); // TODO callback?
				break;
			case R.string.delete :
				delete(schedule);
				break;
		}
		
		return true;
	}
	
	public void disableSending()
	{
		if(listScheduleAdapter == null)
			return;
		boolean changed = false;
		for(int s = 0; s < listScheduleAdapter.getCount(); s++)
		{
			SendSchedule schedule = listScheduleAdapter.getItem(s);
			if(schedule != null && schedule.isEnabled())
			{
				schedule.setEnabled(false);
				save(schedule, false);
				changed = true;
			}
		}
		if(changed)
		{
			listScheduleAdapter.notifyDataSetChanged();
			SendConfigurationHelpers.reschedule(getOwner(), getProject(true));
		}
	}
	
	public List<SendSchedule> getSchedules()
	{
		return SendConfigurationHelpers.getSchedulesForProject(getOwner(), getProject(true));
	}
	
	private void save(SendSchedule schedule, boolean reschedule)
	{
		SendConfigurationHelpers.saveSchedule(getOwner(), schedule, true);
	}
	
	public void saveEdited(SendSchedule schedule)
	{
		// Store the new schedule:
		save(schedule, true);
	
		// Update the list:
		listScheduleAdapter.notifyDataSetChanged();
	}
	
	/**
	 * @param schedule the new SendSchedule, or null if creation of new schedule was cancelled
	 */
	public void addNew(SendSchedule schedule)
	{
		if(schedule != null)
		{
			// Store the new schedule:
			save(schedule, true);
		
			// Add it to the list:
			listScheduleAdapter.add(schedule);
			
			// TODO msg to tell user to enable the schedule?
		}
		
		// Open/close sending pane:
		toggleConfigGroup(true, !listScheduleAdapter.isEmpty());
	}
	
	public void delete(SendSchedule schedule)
	{
		try
		{
			SendConfigurationHelpers.deleteSchedule(getOwner(), schedule);
			
			// Update UI:
			refresh();
		}
		catch(Exception e)
		{
			Log.e(getClass().getSimpleName(), "Error upon deleting sendSchedule", e);
		}
	}

	@Override
	public String getTabTitle(Context context)
	{
		return context.getString(R.string.tab_transmission);
	}

	static public class ReceiverDrawableProvider implements Correspondent.Handler
	{
		
		public Integer drawableResourceId = null;
		
		@Override
		public void handle(SMSCorrespondent smsCorrespondent)
		{
			// TODO later we might differentiate based on smsCorrespondent.isBinary()
			drawableResourceId = R.drawable.ic_sms_black_24dp;
		}
		
	}
	
	private class SendScheduleAdapter extends AdvancedSpinnerAdapter<SendSchedule>
	{
		
		public SendScheduleAdapter(Context context, List<SendSchedule> schedules)
		{
			super(context, R.layout.schedule_list_item, R.layout.schedule_list_item, R.id.lblReceiver, null, null, schedules);
		}
		
		@Override
		protected String getItemString(SendSchedule sendSchedule)
		{
			return sendSchedule.getReceiver().toString();
		}

		@Override
		protected Integer getItemDrawableResourceId(int position, SendSchedule sendSchedule)
		{
			if(sendSchedule == null)
				return null;
			//else:
			ReceiverDrawableProvider provider = new ReceiverDrawableProvider();
			sendSchedule.getReceiver().handle(provider);
			return provider.drawableResourceId;
		}

		@Override
		protected View createView(final int position, final SendSchedule sendSchedule, CharSequence itemText, Integer itemDrawableResourceId, boolean center, View convertView, final ViewGroup parent, int resource)
		{
			ViewGroup layout = (ViewGroup) super.createView(position, sendSchedule, itemText, itemDrawableResourceId, center, convertView, parent, resource);
			
			// Set-up switch:
			SwitchCompat switchEnabled = (SwitchCompat) layout.findViewById(R.id.switchEnabled);
			switchEnabled.setChecked(sendSchedule.isEnabled());
			switchEnabled.setOnCheckedChangeListener(new OnCheckedChangeListener()
			{
				@Override
				public void onCheckedChanged(CompoundButton buttonView, boolean isChecked)
				{
					if(sendSchedule.isEnabled() != isChecked)
					{
						// dis/enable schedule:
						sendSchedule.setEnabled(isChecked);
						
						// Save settings:
						save(sendSchedule, true);	
					}
				}
			});
			
			// Set settings label:
			TransactionalStringBuilder bldr = new TransactionalStringBuilder("; ");
			//	Send interval:
			bldr.append(getString(R.string.interval) + " " + Float.valueOf((float) sendSchedule.getTransmitIntervalS() / SEC_IN_MIN).toString() + " minutes");
			//	Airplane mode:
			if(DeviceControl.canSetAirplaneMode() && sendSchedule.isAirplaneModeCycling())
				bldr.append(getString(R.string.airplaneModeCycling));
			//	Heartbeat interval:
			// TODO
			//	Encrypt:
			// TODO
			((TextView) layout.findViewById(R.id.lblScheduleSettings)).setText(bldr.toString());
			
			return layout;
		}
		
	}
		
}
