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

import java.util.List;
import java.util.Set;

import android.annotation.SuppressLint;
import android.app.AlertDialog;
import android.app.Dialog;
import android.content.DialogInterface;
import android.os.Bundle;
import android.support.v7.app.AppCompatActivity;
import android.view.View;
import android.view.View.OnClickListener;
import android.view.ViewGroup;
import android.widget.Button;
import android.widget.ListView;
import uk.ac.ucl.excites.sapelli.collector.R;
import uk.ac.ucl.excites.sapelli.collector.activities.ProjectManagerActivity;
import uk.ac.ucl.excites.sapelli.collector.fragments.ProjectManagerFragment;
import uk.ac.ucl.excites.sapelli.collector.fragments.tabs.TransmissionTabFragment;
import uk.ac.ucl.excites.sapelli.collector.model.Project;
import uk.ac.ucl.excites.sapelli.collector.transmission.SendConfigurationHelpers;
import uk.ac.ucl.excites.sapelli.collector.transmission.SendConfigurationHelpers.ReceiverUpdateCallback;
import uk.ac.ucl.excites.sapelli.shared.util.android.AdvancedSpinnerAdapter;
import uk.ac.ucl.excites.sapelli.shared.util.android.DialogHelpers;
import uk.ac.ucl.excites.sapelli.transmission.model.Correspondent;

/**
 * @author mstevens
 *
 */
public class ManageReceiversFragment extends ProjectManagerFragment implements OnClickListener, DialogInterface.OnClickListener, ReceiverUpdateCallback 
{
	
	// STATIC -------------------------------------------------------
	static public void ShowDialog(AppCompatActivity owner, ReceiverUpdateCallback callback)
	{
		new ManageReceiversFragment(callback).show(owner.getSupportFragmentManager(), ManageReceiversFragment.class.getSimpleName());
	}
	
	// DYNAMIC ------------------------------------------------------
	private ReceiverUpdateCallback callback;
	
	private ListView listReceivers;
	private ReceiverAdapter listReceiversAdapter;
	private Button btnAddReceiver;
	
	public ManageReceiversFragment(ReceiverUpdateCallback callback)
	{
		this.callback = callback;
	}
		
	/* (non-Javadoc)
	 * @see uk.ac.ucl.excites.sapelli.collector.fragments.ProjectManagerFragment#getLayoutID()
	 */
	@Override
	protected Integer getLayoutID()
	{
		return R.layout.dialog_manage_receivers;
	}
	
	/* (non-Javadoc)
	 * @see uk.ac.ucl.excites.sapelli.collector.fragments.ProjectManagerFragment#setupUI(uk.ac.ucl.excites.sapelli.collector.activities.ProjectManagerActivity, android.view.View)
	 */
	@Override
	protected void setupUI(final ProjectManagerActivity owner, final View rootLayout)
	{
		listReceivers = (ListView) rootLayout.findViewById(R.id.listReceivers);
		updateReceivers();
		
		btnAddReceiver = (Button) rootLayout.findViewById(R.id.btnAddReceiver);
		btnAddReceiver.setOnClickListener(this);
	}
	
	private void updateReceivers()
	{
		final ProjectManagerActivity owner = getOwner();
		if(owner == null) // just in case...
			return;
		listReceiversAdapter = new ReceiverAdapter(SendConfigurationHelpers.getReceivers(owner)); 
		listReceivers.setAdapter(listReceiversAdapter);
	}
	
	@SuppressLint("InflateParams")
	@Override
	public Dialog onCreateDialog(Bundle savedInstanceState)
	{
		AlertDialog.Builder builder = new AlertDialog.Builder(getOwner())
		.setIcon(R.drawable.ic_transfer_black_36dp)
		.setTitle(R.string.manageReceivers)
		.setPositiveButton(android.R.string.ok, null); // listener will be set through the MakeNonDismission() call below
		final AlertDialog dialog = builder.create();
		
		DialogHelpers.MakeNonDismissing(dialog, this, DialogInterface.BUTTON_POSITIVE);
		
		// Set view:
		int lrSpacingPx = getDialogLeftRightPaddingPx();
		dialog.setView(getRootLayout(), lrSpacingPx, 0, lrSpacingPx, 0);
		
		return dialog;
	}
	
//	private Set<ViewGroup> receiveSwitchGroups = new HashSet<ViewGroup>();
//	
//	static private final int RECEIVE_SWITCH_IDX = 1;
//	
//	private boolean setReceiveSwitch(ViewGroup receiveSwitchGroup, boolean enabled)
//	{
//		((Checkable) receiveSwitchGroup.getChildAt(RECEIVE_SWITCH_IDX)).setChecked(enabled);
//		return enabled;
//	}
	
//	case R.id.switchReceive :
//		toggleConfigGroup(false, switchReceive.isChecked());
//		if(!switchReceive.isChecked()) // Disable receiving of all transmission types:
//			for(ViewGroup receiveSwitchGroup : receiveSwitchGroups)
//			{
//				setReceiveSwitch(receiveSwitchGroup, false);
//				getOwner().getProjectStore().setReceiving(getProject(false), (Transmission.Type) receiveSwitchGroup.getTag(), false);
//			}
//		break;
	
//	addReceiveSwitch(Transmission.Type.BINARY_SMS, (ViewGroup) rootLayout.findViewById(R.id.switchReceiveBinSMS));
//	addReceiveSwitch(Transmission.Type.TEXTUAL_SMS, (ViewGroup) rootLayout.findViewById(R.id.switchReceiveTxtSMS));

//	private void addReceiveSwitch(Transmission.Type transmissionType, ViewGroup receiveSwitch)
//	{
//		receiveSwitch.setTag(transmissionType);
//		receiveSwitch.setOnClickListener(this);
//		receiveSwitchGroups.add(receiveSwitch);
//	}
	
//	private boolean toggleReceiveSwitch(ViewGroup receiveSwitchGroup)
//	{
//		Checkable receiveSwitch = (Checkable) receiveSwitchGroup.getChildAt(RECEIVE_SWITCH_IDX); 
//		receiveSwitch.toggle();
//		return receiveSwitch.isChecked();
//	}
	
//	// Update receiving config UI parts:
//	boolean receivingEnabled = false;
//	for(ViewGroup receiveSwitchGroup : receiveSwitchGroups)
//		if(setReceiveSwitch(receiveSwitchGroup,	getOwner().getProjectStore().isReceiving(project, (Transmission.Type) receiveSwitchGroup.getTag())))
//			receivingEnabled = true;
//	toggleConfigGroup(false, receivingEnabled);
	
//	case R.id.switchReceiveBinSMS :
//	case R.id.switchReceiveTxtSMS :
//		getOwner().getProjectStore().setReceiving(
//			getProject(false),
//			(Transmission.Type) view.getTag(),
//			toggleReceiveSwitch((ViewGroup) view));
//		break;
	
	@Override
	public void onClick(DialogInterface dialog, int which)
	{
		switch(which)
		{
			case DialogInterface.BUTTON_POSITIVE :
				// TODO callback
				dialog.dismiss();
				break;
			case DialogInterface.BUTTON_NEGATIVE :
				break;
		}
	}

	@Override
	public void onClick(View v)
	{
		if(v.getId() == R.id.btnAddReceiver)
		{	// Add...
			PickReceiverTypeFragment.ShowDialog(getOwner(), this);
		}
		else if(v.getId() == R.id.btnEditReceiver || v.getId() == R.id.btnDeleteReceiver)
		{
			Correspondent receiver = listReceiversAdapter.getItem((Integer) v.getTag());
			if(receiver == null)
				return;
			Set<Project> projectsUsingReceiver = SendConfigurationHelpers.getProjectsUsingReceiver(getOwner(), receiver);
			if(!projectsUsingReceiver.isEmpty())
			{
				getOwner().showYesNoDialog(R.string.manageReceivers, R.string.delete, R.drawable.ic_transfer_black_36dp, new Runnable()
				{
					@Override
					public void run()
					{
						
					}
				},
				false,
				null,
				false);
			}
			
			if(v.getId() == R.id.btnEditReceiver)
			{	// Edit...
				SendConfigurationHelpers.openEditReceiverDialog(getOwner(), this, receiver);
			}
			else
			{	// Delete...
				// TODO confirm delete msg box
				SendConfigurationHelpers.deleteCorrespondent(getOwner(), receiver);
				deletedReceiver(receiver);
			}
		}
	}
	
	@Override
	public void newReceiver(Correspondent newReceiver)
	{
		updateReceivers();
		// Forward:
		if(callback != null)
			callback.newReceiver(newReceiver);
	}
	
	@Override
	public void editedReceiver(Correspondent editedReceiver)
	{
		updateReceivers();
		// Forward:
		if(callback != null)
			callback.editedReceiver(editedReceiver);
		// Request TransmissionTab update:
		getOwner().refreshTab(TransmissionTabFragment.class);
	}
	
	@Override
	public void deletedReceiver(Correspondent deletedReceiver)
	{
		updateReceivers();
		// Forward:
		if(callback != null)
			callback.deletedReceiver(deletedReceiver);
		// Request TransmissionTab update:		
		getOwner().refreshTab(TransmissionTabFragment.class);
	}
	
	/**
	 * @author mstevens
	 *
	 */
	private class ReceiverAdapter extends AdvancedSpinnerAdapter<Correspondent>
	{
		
		public ReceiverAdapter(List<Correspondent> receivers)
		{
			super(getOwner(), R.layout.manage_receiver_item, 0, R.id.lblReceiver, null, null, receivers);
		}
		
		
		@Override
		protected CharSequence getItemString(Correspondent receiver)
		{
			return SendConfigurationHelpers.getReceiverLabelText(receiver, true);
		}
		
		@Override
		protected Integer getItemDrawableResourceId(int position, Correspondent receiver)
		{
			return receiver == null ? null : SendConfigurationHelpers.getReceiverDrawable(receiver, false);
		}
		
		@Override
		protected View createView(final int position, final Correspondent receiver, CharSequence itemText, Integer itemDrawableResourceId, boolean center, View convertView, final ViewGroup parent, int resource)
		{
			ViewGroup layout = (ViewGroup) super.createView(position, receiver, itemText, itemDrawableResourceId, center, convertView, parent, resource);
			
			// Set on click listener:
			for(int btnId : new int[] { R.id.btnEditReceiver, R.id.btnDeleteReceiver })
			{
				View btn = layout.findViewById(btnId);
				btn.setTag(position); // we'll use this to get the correct receiver in onClick()
				btn.setOnClickListener(ManageReceiversFragment.this);
			}
			
			return layout;
		}
		
	}

}
