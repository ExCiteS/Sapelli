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
import uk.ac.ucl.excites.sapelli.collector.fragments.dialogs.SendScheduleFragment;
import uk.ac.ucl.excites.sapelli.collector.model.MediaFile;
import uk.ac.ucl.excites.sapelli.collector.model.Project;
import uk.ac.ucl.excites.sapelli.collector.tasks.ProjectTasks;
import uk.ac.ucl.excites.sapelli.collector.transmission.SendConfigurationHelpers;
import uk.ac.ucl.excites.sapelli.collector.transmission.SendSchedule;
import uk.ac.ucl.excites.sapelli.shared.util.TransactionalStringBuilder;
import uk.ac.ucl.excites.sapelli.shared.util.android.AdvancedSpinnerAdapter;
import uk.ac.ucl.excites.sapelli.shared.util.android.DeviceControl;
import uk.ac.ucl.excites.sapelli.storage.model.Record;

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
		registerForContextMenu(listSchedules);
		btnAddSchedule = (Button) rootLayout.findViewById(R.id.btnAddSchedule);
		btnAddSchedule.setOnClickListener(this);
		registerForContextMenu(btnAddSchedule);

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
		List<SendSchedule> schedules = SendConfigurationHelpers.getSchedulesForProject(getOwner(), project);
		//	Populate list:
		listScheduleAdapter = new SendScheduleAdapter(listSchedules.getContext(), schedules); 
		listSchedules.setAdapter(listScheduleAdapter);
		//	Set sending switch (on if at least 1 schedule is enabled, off otherwise):
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
				{	// Sending is being switched on...
					if(listScheduleAdapter.isEmpty()) // if there is no schedule make one...
						SendScheduleFragment.ShowAddDialog(this);
					//else
						// TODO enable all schedules
				}
				else
				{	// Sending is being switched off:
					disableSending(); // disable all schedules
				}
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
				save(schedule);
				changed = true;
			}
		}
		if(changed)
			listScheduleAdapter.notifyDataSetChanged();
	}
	
	private void save(SendSchedule schedule)
	{
		SendConfigurationHelpers.saveSchedule(getOwner(), schedule);
	}
	
	public void saveEdited(SendSchedule schedule)
	{
		// Store the new schedule:
		save(schedule);
	
		// Update the list:
		listScheduleAdapter.notifyDataSetChanged();
	}
	
	/**
	 * @param schedule the new SendSchedule, or null if creation of new schedule was cancelled
	 */
	public void addNew(final SendSchedule schedule)
	{
		if(schedule != null)
		{
			// Automatically enable new schedule:
			schedule.setEnabled(true);
			
			// Store the new schedule:
			save(schedule);
		
			// Refresh schedules:
			listScheduleAdapter.add(schedule);
			
			// Query for currently existing project data & propose transmission: 
			ProjectTasks.RunProjectDataQueries(getOwner(), getProject(false), new ProjectTasks.ProjectDataCallback()
			{
				@Override
				public void projectDataQuerySuccess(final List<Record> records, List<MediaFile> mediaFiles)
				{
					if(!records.isEmpty())
						getOwner().showYesNoDialog(
							R.string.tab_transmission,
							getString(R.string.sendExistingData, records.size(), mediaFiles.size(), schedule.getReceiver().getName()),
							R.drawable.ic_transfer_black_36dp,
							new Runnable()
							{
								@Override
								public void run()
								{
									getOwner().getCollectorClient().scheduleSending(records, schedule.getReceiver());
								}
							},
							false, null, false);
				}

				@Override
				public void projectDataQueryFailure(Exception reason)
				{
					Log.e(TransmissionTabFragment.class.getSimpleName(), "Failed to query for records", reason);
				}
			});
		}
		else
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
	
	/**
	 * @author mstevens
	 *
	 */
	private class SendScheduleAdapter extends AdvancedSpinnerAdapter<SendSchedule> implements OnCheckedChangeListener
	{
		
		public SendScheduleAdapter(Context context, List<SendSchedule> schedules)
		{
			super(context, R.layout.schedule_list_item, 0, R.id.lblReceiver, null, null, schedules);
		}
		
		@Override
		protected CharSequence getItemString(SendSchedule sendSchedule)
		{
			return SendConfigurationHelpers.getReceiverLabelText(sendSchedule.getReceiver(), false);
		}

		@Override
		protected Integer getItemDrawableResourceId(int position, SendSchedule sendSchedule)
		{
			if(sendSchedule == null)
				return null;
			//else:
			return SendConfigurationHelpers.getReceiverDrawable(sendSchedule.getReceiver(), false);
		}

		@Override
		protected View createView(final int position, final SendSchedule sendSchedule, CharSequence itemText, Integer itemDrawableResourceId, boolean center, View convertView, final ViewGroup parent, int resource)
		{
			ViewGroup layout = (ViewGroup) super.createView(position, sendSchedule, itemText, itemDrawableResourceId, center, convertView, parent, resource);
			
			// Set-up switch:
			SwitchCompat switchEnabled = (SwitchCompat) layout.findViewById(R.id.switchEnabled);
			switchEnabled.setTag(position); // we'll use this to get the correct schedule in onCheckedChanged() below
			switchEnabled.setChecked(sendSchedule.isEnabled());
			switchEnabled.setOnCheckedChangeListener(this);
			
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
		
		@Override
		public void onCheckedChanged(CompoundButton buttonView, boolean isChecked)
		{
			try
			{
				SendSchedule sendSchedule = getItem((Integer) buttonView.getTag()); // get schedule using tag
				if(sendSchedule.isEnabled() != isChecked)
				{
					// dis/enable schedule:
					sendSchedule.setEnabled(isChecked);
					
					// Save settings:
					save(sendSchedule);	
				}
			}
			catch(Exception e)
			{
				Log.e(TransmissionTabFragment.class.getSimpleName(), "Error upon handling schedule switch change", e);
			}
		}
		
	}
		
}
