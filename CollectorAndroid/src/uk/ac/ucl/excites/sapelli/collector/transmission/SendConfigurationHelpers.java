/**
 * Sapelli data collection platform: http://sapelli.org
 * 
 * Copyright 2012-2015 University College London - ExCiteS group
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

package uk.ac.ucl.excites.sapelli.collector.transmission;

import java.util.ArrayList;
import java.util.Collections;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

import android.text.SpannableString;
import android.text.TextUtils;
import android.util.Log;
import uk.ac.ucl.excites.sapelli.collector.R;
import uk.ac.ucl.excites.sapelli.collector.activities.ProjectManagerActivity;
import uk.ac.ucl.excites.sapelli.collector.fragments.dialogs.GeoKeyReceiverFragment;
import uk.ac.ucl.excites.sapelli.collector.fragments.dialogs.SMSReceiverFragment;
import uk.ac.ucl.excites.sapelli.collector.model.Project;
import uk.ac.ucl.excites.sapelli.shared.util.ExceptionHelpers;
import uk.ac.ucl.excites.sapelli.shared.util.URLUtils;
import uk.ac.ucl.excites.sapelli.shared.util.android.CustomTypefaceSpan;
import uk.ac.ucl.excites.sapelli.transmission.db.TransmissionStore;
import uk.ac.ucl.excites.sapelli.transmission.model.Correspondent;
import uk.ac.ucl.excites.sapelli.transmission.model.transport.geokey.GeoKeyAccount;
import uk.ac.ucl.excites.sapelli.transmission.model.transport.sms.SMSCorrespondent;

/**
 * @author mstevens
 */
public final class SendConfigurationHelpers
{

	private SendConfigurationHelpers() {}
	
	/**
	 * @param activity
	 * @param schedule
	 */
	static public void saveSchedule(ProjectManagerActivity activity, SendSchedule schedule)
	{
		try
		{
			// Store schedule:
			activity.getProjectStore().storeSendSchedule(schedule);
			
			// Set or cancel alarm:
			SchedulingHelpers.ScheduleOrCancel(activity.getApplicationContext(), schedule);
		}
		catch(Exception e)
		{
			Log.e(SendConfigurationHelpers.class.getSimpleName(), "Error upon saving send schedule", e);
		}
	}
	
	/**
	 * @param activity
	 * @param schedule
	 */
	static public void deleteSchedule(ProjectManagerActivity activity, SendSchedule schedule)
	{
		try
		{
			// Delete schedule:
			activity.getProjectStore().deleteSendSchedule(schedule);
			
			// Cancel alarm:
			SchedulingHelpers.Cancel(activity.getApplicationContext(), schedule);
		}
		catch(Exception e)
		{
			Log.e(SendConfigurationHelpers.class.getSimpleName(), "Error upon deleting send schedule", e);
		}
		
	}
	
	/**
	 * TODO
	 * 
	 * @param activity
	 * @return
	 */
	static public List<Correspondent> getReceivers(ProjectManagerActivity activity)
	{
		return activity.getTransmissionStore().retrieveCorrespondents(false, false); // don't include unknown senders, nor user-deleted correspondents
	}
	
	/**
	 * @param activity
	 * @param schedule
	 * @return
	 */
	static public List<Correspondent> getSelectableCorrespondents(ProjectManagerActivity activity, SendSchedule schedule)
	{
		List<Correspondent> usedReceivers = new ArrayList<Correspondent>();
		for(SendSchedule projSched : getSchedulesForProject(activity, schedule.getProject()))
			if(projSched.getReceiver() != null)
				usedReceivers.add(projSched.getReceiver());
		List<Correspondent> selectableReceivers = new ArrayList<Correspondent>();
		for(Correspondent receiver : getReceivers(activity))
			if(receiver.equals(schedule.getReceiver()) || !usedReceivers.contains(receiver))
				selectableReceivers.add(receiver);
		return selectableReceivers;
	}
	
	static private List<SendSchedule> filterSendSchedulesWithMissingReceiver(ProjectManagerActivity activity, List<SendSchedule> schedulesToFilter)
	{
		List<SendSchedule> schedules = new ArrayList<SendSchedule>();
		for(SendSchedule schedule : schedulesToFilter)
		{
			if(SendSchedule.hasValidReceiver(schedule))
				schedules.add(schedule);
			else
				// schedule has no receiver, delete it:
				deleteSchedule(activity, schedule); // will also cancel alarms
		}
		return schedules;
	}
	
	/**
	 * @param project
	 * @return
	 */
	static public List<SendSchedule> getSchedulesForProject(ProjectManagerActivity activity, Project project)
	{
		try
		{
			return filterSendSchedulesWithMissingReceiver(activity, activity.getProjectStore().retrieveSendSchedulesForProject(project));
		}
		catch(Exception e)
		{
			Log.e(SendConfigurationHelpers.class.getSimpleName(), "Error upon querying for send schedules", e);
			return Collections.<SendSchedule> emptyList();
		}
	}
	
	/**
	 * @param activity
	 * @param receiver
	 * @return
	 */
	static public List<SendSchedule> getSchedulesForReceiver(ProjectManagerActivity activity, Correspondent receiver)
	{
		try
		{
			return filterSendSchedulesWithMissingReceiver(activity, activity.getProjectStore().retrieveSendSchedulesForReceiver(receiver));
		}
		catch(Exception e)
		{
			Log.e(SendConfigurationHelpers.class.getSimpleName(), "Error upon querying for send schedules", e);
			return Collections.<SendSchedule> emptyList();
		}
	}
	
	/**
	 * @param activity
	 * @param receiver
	 * @return
	 */
	static public Set<Project> getProjectsUsingReceiver(ProjectManagerActivity activity, Correspondent receiver)
	{
		Set<Project> projects = new HashSet<Project>();
		for(SendSchedule schedule : getSchedulesForReceiver(activity, receiver))
			projects.add(schedule.getProject());
		return projects;
	}
	
	/**
	 * @param activity
	 * @param correspondent
	 */
	static public void saveCorrespondent(ProjectManagerActivity activity, Correspondent correspondent)
	{
		try
		{	// Store/update correspondent:
			activity.getTransmissionStore().store(correspondent);
		}
		catch(Exception e)
		{
			Log.e(SendConfigurationHelpers.class.getSimpleName(), ExceptionHelpers.getMessageAndCause(e), e);
		}
	}
	
	/**
	 * @param activity
	 * @param correspondent the correspondent to delete (if null nothing will happen)
	 * @return null if nothing happened, true if correspondent was deleted, false if it was only hidden
	 */
	static public Boolean deleteCorrespondent(ProjectManagerActivity activity, Correspondent correspondent)
	{
		if(correspondent == null)
			return null;
		try
		{	
			if(!hasTransmissions(activity, correspondent))
			{	// If the correspondent doesn't have transmissions we can really delete it:
				activity.getTransmissionStore().deleteCorrespondent(correspondent); // TODO what to do with transmittable records for this receiver???
				return true;
			}
			else
			{	// Otherwise we only hide it:
				correspondent.markAsUserDeleted();
				saveCorrespondent(activity, correspondent);
				return false;
			}
		}
		catch(Exception e)
		{
			Log.e(SendConfigurationHelpers.class.getSimpleName(), ExceptionHelpers.getMessageAndCause(e), e);
			return null;
		}
	}
	
	/**
	 * @param activity
	 * @param correspondent
	 * @return whether or not the correspondent has at least 1 sent or received transmission
	 */
	static public boolean hasTransmissions(ProjectManagerActivity activity, Correspondent correspondent)
	{
		if(correspondent == null)
			return false;
		try
		{
			TransmissionStore tStore = activity.getTransmissionStore();
			return tStore != null && (!tStore.retrieveTransmissions(true, correspondent).isEmpty() || !tStore.retrieveTransmissions(false, correspondent).isEmpty());
		}
		catch(Exception e)
		{
			Log.e(SendConfigurationHelpers.class.getSimpleName(), ExceptionHelpers.getMessageAndCause(e), e);
			return false;
		}
	}
	
	/**
	 * @author mstevens
	 *
	 */
	public interface ReceiverUpdateCallback
	{
		
		public void newReceiver(Correspondent newReceiver);
		
		public void editedReceiver(Correspondent newReceiver, Correspondent oldReceiver);
		
		public void deletedReceiver(Correspondent oldReceiver);
		
	}
	
	/**
	 * @param activity
	 * @param callback
	 * @param receiver
	 */
	static public void openEditReceiverDialog(final ProjectManagerActivity activity, final ReceiverUpdateCallback callback, Correspondent receiver)
	{
		if(receiver == null)
			return;
		receiver.handle(new Correspondent.Handler()
		{
			@Override
			public void handle(SMSCorrespondent smsCorrespondent)
			{
				SMSReceiverFragment.ShowEditDialog(activity, callback, smsCorrespondent);
			}

			@Override
			public void handle(GeoKeyAccount geokeyAccount)
			{
				GeoKeyReceiverFragment.ShowEditDialog(activity, callback, geokeyAccount);
			}
		});
	}
	
	static private ReceiverDrawableProvider receiverDrawableProvider = new ReceiverDrawableProvider();
	
	/**
	 * @param receiver
	 * @param big
	 * @return
	 */
	static public int getReceiverDrawable(Correspondent receiver, boolean big)
	{
		if(receiver != null)
		{
			receiverDrawableProvider.drawableResourceId = null;
			receiverDrawableProvider.big = big;
			receiver.handle(receiverDrawableProvider);
			if(receiverDrawableProvider.drawableResourceId != null)
				return receiverDrawableProvider.drawableResourceId;
		}
		// Fall-back:
		return big ? R.drawable.ic_transfer_black_36dp : R.drawable.ic_transfer_black_24dp;
	}
	
	/**
	 * @param smsCorrespondent
	 * @param big
	 * @return
	 */
	static public int getSMSReceiverDrawable(SMSCorrespondent smsCorrespondent, boolean big)
	{
		return getSMSReceiverDrawable(smsCorrespondent.isBinary(), big);
	}
	
	/**
	 * @param binary
	 * @param big
	 * @return
	 */
	static public int getSMSReceiverDrawable(boolean binary, boolean big)
	{
		if(big)
			return binary ? R.drawable.ic_sms_bin_black_36dp : R.drawable.ic_sms_txt_black_36dp;
		else
			return binary ? R.drawable.ic_sms_bin_black_24dp : R.drawable.ic_sms_txt_black_24dp;
	}
	
	/**
	 * @param binary
	 * @param big
	 * @return
	 */
	static public int getGeoKeyReceiverDrawable(boolean big)
	{
		return big ? R.drawable.ic_web_black_36dp : R.drawable.ic_web_black_24dp;
	}
	
	/**
	 * @author mstevens
	 *
	 */
	static private class ReceiverDrawableProvider implements Correspondent.Handler
	{
		
		public Integer drawableResourceId = null;
		public boolean big;
		
		@Override
		public void handle(SMSCorrespondent smsCorrespondent)
		{
			drawableResourceId = getSMSReceiverDrawable(smsCorrespondent, big); 
		}

		@Override
		public void handle(GeoKeyAccount geokeyAccount)
		{
			drawableResourceId = getGeoKeyReceiverDrawable(big);
		}
		
	}
	
	static private ReceiverAddressStringProvider receiverAddressStringProvider = new ReceiverAddressStringProvider();
	
	static public CharSequence getReceiverLabelText(Correspondent receiver, boolean multiLine)
	{
		if(receiver != null)
		{
			receiverAddressStringProvider.addressStr = null;
			receiver.handle(receiverAddressStringProvider);
			if(receiverAddressStringProvider.addressStr != null)
			{
				SpannableString condensedAddress = new SpannableString(receiverAddressStringProvider.addressStr);
				condensedAddress.setSpan(new CustomTypefaceSpan(ProjectManagerActivity.FONT_SANS_SERIF_CONDENSED), 0, receiverAddressStringProvider.addressStr.length(), 0);
				return TextUtils.concat(receiver.getName(), multiLine ? "\n" : " ", "[", condensedAddress, "]");	
			}
		}
		// Fall-back:
		return "";
	}
	
	/**
	 * @author mstevens
	 *
	 */
	static private class ReceiverAddressStringProvider implements Correspondent.Handler
	{
		
		public String addressStr = null;
		
		@Override
		public void handle(GeoKeyAccount geokeyAccount)
		{
			addressStr =
				(geokeyAccount.hasUserDisplayName() ? geokeyAccount.getUserDisplayName() + "@" : "") +
				URLUtils.stripTrailingSlash(URLUtils.stripHTTP(geokeyAccount.getUrl()));
		}
		
		@Override
		public void handle(SMSCorrespondent smsCorrespondent)
		{
			addressStr = smsCorrespondent.getPhoneNumberInternational();
		}
		
	}
	
}
