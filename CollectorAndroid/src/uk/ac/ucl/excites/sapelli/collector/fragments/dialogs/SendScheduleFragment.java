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

import java.util.List;

import android.annotation.SuppressLint;
import android.app.AlertDialog;
import android.app.Dialog;
import android.content.DialogInterface;
import android.graphics.Color;
import android.os.Bundle;
import android.text.Editable;
import android.text.TextWatcher;
import android.view.View;
import android.view.View.OnClickListener;
import android.view.ViewGroup;
import android.widget.AdapterView;
import android.widget.AdapterView.OnItemSelectedListener;
import android.widget.ArrayAdapter;
import android.widget.Button;
import android.widget.CheckBox;
import android.widget.EditText;
import android.widget.Spinner;
import uk.ac.ucl.excites.sapelli.collector.R;
import uk.ac.ucl.excites.sapelli.collector.activities.ProjectManagerActivity;
import uk.ac.ucl.excites.sapelli.collector.fragments.ProjectManagerFragment;
import uk.ac.ucl.excites.sapelli.collector.fragments.tabs.TransmissionTabFragment;
import uk.ac.ucl.excites.sapelli.collector.transmission.SendConfigurationHelpers;
import uk.ac.ucl.excites.sapelli.collector.transmission.SendConfigurationHelpers.ReceiverUpdateCallback;
import uk.ac.ucl.excites.sapelli.collector.transmission.SendSchedule;
import uk.ac.ucl.excites.sapelli.shared.util.Objects;
import uk.ac.ucl.excites.sapelli.shared.util.android.AdvancedSpinnerAdapter;
import uk.ac.ucl.excites.sapelli.shared.util.android.DeviceControl;
import uk.ac.ucl.excites.sapelli.shared.util.android.DialogHelpers;
import uk.ac.ucl.excites.sapelli.transmission.model.Correspondent;

/**
 * Fragment that defines the project manager layout per project (tabs)
 * 
 * @author mstevens
 */
public class SendScheduleFragment extends ProjectManagerFragment implements OnClickListener, DialogInterface.OnClickListener, OnItemSelectedListener, ReceiverUpdateCallback
{

	// STATIC -------------------------------------------------------
	static public void ShowAddDialog(TransmissionTabFragment transmissionTab)
	{
		ProjectManagerActivity activity = transmissionTab.getOwner();
		ShowDialog(transmissionTab, new SendSchedule(activity.getCurrentProject(true), null, false), false);
	}

	static public void ShowEditDialog(TransmissionTabFragment transmissionTab, SendSchedule editSchedule)
	{
		ShowDialog(transmissionTab, editSchedule, true);
	}
	
	static private void ShowDialog(final TransmissionTabFragment transmissionTab, final SendSchedule schedule, final boolean editing)
	{
		ProjectManagerActivity activity = transmissionTab.getOwner();
		
		List<Correspondent> selectableReceivers = SendConfigurationHelpers.getSelectableCorrespondents(activity, schedule); 
		if(selectableReceivers.isEmpty())
		{	// this can/should only happen when creating a SendSchedule (not when editing one)
			createNewReceiver(activity, new ReceiverUpdateCallback()
			{
				@Override
				public void newReceiver(Correspondent newReceiver)
				{
					schedule.setReceiver(newReceiver);
					ShowDialog(transmissionTab, schedule, editing);
				}
				
				@Override
				public void editedReceiver(Correspondent newReceiver, Correspondent oldReceiver) {}

				@Override
				public void deletedReceiver(Correspondent oldReceiver) {}
			});
		}
		else
		{	// We have at least one selectable receiver, open the dialog:
			new SendScheduleFragment(transmissionTab, selectableReceivers, schedule, editing).show(activity.getSupportFragmentManager(), (editing ? R.string.edit : R.string.add) + SendScheduleFragment.class.getSimpleName());
		}
	}
	
	static private void createNewReceiver(ProjectManagerActivity activity, ReceiverUpdateCallback callback)
	{
		PickReceiverTypeFragment.ShowDialog(activity, callback);
	}
	
	// DYNAMIC ------------------------------------------------------
	private TransmissionTabFragment transmissionTab;
	
	// Views
	private ViewGroup groupReceiver;
	private Spinner spinReceiver;
	private Button btnNewReceiver;
	private Button btnEditReceiver;
	private Button btnDeleteReceiver;
	private ViewGroup groupInterval;
	private EditText txtSendIntervalMin;
	private CheckBox checkAirplaneModeCycle;
	
	// Adapter:
	private ArrayAdapter<Correspondent> spinReceiverAdapter;
	
	// Model:
	private List<Correspondent> selectableReceivers;
	private SendSchedule schedule;
	private final boolean editing;
	
	private SendScheduleFragment(TransmissionTabFragment transmissionTab, List<Correspondent> selectableReceivers, SendSchedule schedule, boolean editing)
	{
		this.transmissionTab = transmissionTab;
		this.selectableReceivers = selectableReceivers;
		this.schedule = schedule;
		this.editing = editing;
	} 
	
	@Override
	protected Integer getLayoutID()
	{
		return R.layout.dialog_send_schedule;
	}
	
	@Override
	protected void setupUI(View rootLayout)
	{
		groupReceiver = (ViewGroup) rootLayout.findViewById(R.id.groupReceiver);
		spinReceiver = (Spinner) rootLayout.findViewById(R.id.spinSendReceiver);
		spinReceiver.setOnItemSelectedListener(this);
		updateReceivers(false); // !!!
		
		btnNewReceiver = (Button) rootLayout.findViewById(R.id.btnNewScheduleReceiver);
		btnNewReceiver.setOnClickListener(this);
		
		btnEditReceiver = (Button) rootLayout.findViewById(R.id.btnEditScheduleReceiver);
		btnEditReceiver.setOnClickListener(this);
		btnEditReceiver.setEnabled(false);
		
		btnDeleteReceiver = (Button) rootLayout.findViewById(R.id.btnDeleteScheduleReceiver);
		btnDeleteReceiver.setOnClickListener(this);
		btnDeleteReceiver.setEnabled(false);
		
		groupInterval = (ViewGroup) rootLayout.findViewById(R.id.groupInterval);
		txtSendIntervalMin = (EditText) rootLayout.findViewById(R.id.txtSendIntervalMin);
		txtSendIntervalMin.setText(Float.valueOf((float) schedule.getTransmitIntervalS() / TransmissionTabFragment.SEC_IN_MIN).toString());
		txtSendIntervalMin.addTextChangedListener(new TextWatcher()
		{
			public void afterTextChanged(Editable editable)
			{
				if(editable.length() == 0)
					return;
				else
					groupInterval.setBackgroundColor(Color.TRANSPARENT);
			}

			public void beforeTextChanged(CharSequence s, int start, int count, int after) {}

			public void onTextChanged(CharSequence s, int start, int before, int count) {}
		});
		
		checkAirplaneModeCycle = (CheckBox) rootLayout.findViewById(R.id.checkAirplaneModeCycle);
		checkAirplaneModeCycle.setChecked(schedule.isAirplaneModeCycling());
		checkAirplaneModeCycle.setVisibility(DeviceControl.canSetAirplaneMode() ? View.VISIBLE : View.GONE);
	}
	
	private void updateReceivers(boolean requery)
	{
		if(requery)
			selectableReceivers = SendConfigurationHelpers.getSelectableCorrespondents(getOwner(), schedule);
		
		// Adapter:
		spinReceiverAdapter = new AdvancedSpinnerAdapter<Correspondent>(	getOwner(),
																			R.layout.pick_receiver_item,
																			R.layout.receiver_dropdown_item,
																			AdvancedSpinnerAdapter.TEXTVIEW_RESOURCE_ID_WHOLE_LAYOUT,
																			getString(R.string.lstPleaseSelect),
																			null,
																			selectableReceivers)
		{
			@Override
			protected CharSequence getItemString(Correspondent receiver)
			{
				return SendConfigurationHelpers.getReceiverLabelText(receiver, false);
			}
	
			@Override
			protected Integer getItemDrawableResourceId(int position, Correspondent receiver)
			{
				return receiver == null ? null : SendConfigurationHelpers.getReceiverDrawable(receiver, false);
			}
		};
		spinReceiver.setAdapter(spinReceiverAdapter);
		
		// Select current/"none" receiver:
		spinReceiver.setSelection(spinReceiverAdapter.getPosition(schedule.getReceiver()));
	}
	
	@SuppressLint("InflateParams")
	@Override
	public Dialog onCreateDialog(Bundle savedInstanceState)
	{
		AlertDialog.Builder builder = new AlertDialog.Builder(getOwner())
		.setIcon(R.drawable.ic_transfer_black_36dp)
		.setTitle(editing ? R.string.editSchedule : R.string.addSchedule)
		.setPositiveButton(android.R.string.ok, null) // listener will be set through the MakeNonDismission() call below
		.setNegativeButton(android.R.string.cancel, this);
		final AlertDialog dialog = builder.create();
		
		DialogHelpers.MakeNonDismissing(dialog, this, DialogInterface.BUTTON_POSITIVE);
		
		// Set view:
		int lrSpacingPx = getDialogLeftRightPaddingPx();
		dialog.setView(getRootLayout(), lrSpacingPx, getDialogMessageToViewSpacingPx(), lrSpacingPx, 0);
		
		return dialog;
	}
	
	@Override
	public void onItemSelected(AdapterView<?> parent, View view, int position, long id)
	{
		boolean selected = getReceiver() != null;
		if(selected)
			groupReceiver.setBackgroundColor(Color.TRANSPARENT);
		
		// Update buttons:
		btnEditReceiver.setEnabled(selected);
		btnDeleteReceiver.setEnabled(selected);
	}

	private Correspondent getReceiver()
	{
		return spinReceiverAdapter.getItem(spinReceiver.getSelectedItemPosition());
	}
	
	@Override
	public void onClick(View v)
	{
		Correspondent receiver = getReceiver();
		switch(v.getId())
		{
			case R.id.btnNewScheduleReceiver :
				createNewReceiver(getOwner(), this);
				break;
			case R.id.btnEditScheduleReceiver :
				SendConfigurationHelpers.openEditReceiverDialog(getOwner(), this, receiver);
				break;
			case R.id.btnDeleteScheduleReceiver :
				// TODO
				break;
		}
	}
	
	@Override
	public void newReceiver(Correspondent newReceiver)
	{
		schedule.setReceiver(newReceiver);
		updateReceivers(true);
	}
	
	@Override
	public void editedReceiver(Correspondent newReceiver, Correspondent oldReceiver)
	{
		if(schedule.getReceiver().equals(oldReceiver))
			schedule.setReceiver(newReceiver);
		updateReceivers(true);
	}
	
	@Override
	public void deletedReceiver(Correspondent oldReceiver)
	{
		editedReceiver(null, oldReceiver);
	}
	
	private boolean save()
	{
		// Input validation & change detection:
		boolean valid = true;
		boolean changed = false;
		//	Receiver:
		Correspondent selectedReceiver = getReceiver();
		if(selectedReceiver == null)
		{
			groupReceiver.setBackgroundResource(R.color.red25percent);
			valid = false;
		}
		else if(!Objects.equals(schedule.getReceiver(), selectedReceiver))
		{
			schedule.setReceiver(selectedReceiver);
			changed = true;
		}
		//	Interval:
		try
		{
			int intervalS = (int) (Float.valueOf(txtSendIntervalMin.getText().toString()) * TransmissionTabFragment.SEC_IN_MIN);
			if(intervalS != schedule.getTransmitIntervalS())
			{
				schedule.setTransmitIntervalS(intervalS);
				changed = true;
			}
		}
		catch(Exception parsingError)
		{
			groupInterval.setBackgroundResource(R.color.red25percent);
			valid = false;
		}
		//	AirplaneModeCycle:
		if(checkAirplaneModeCycle.getVisibility() == View.VISIBLE && schedule.isAirplaneModeCycling() != checkAirplaneModeCycle.isChecked())
		{
			schedule.setAirplaneModeCycling(checkAirplaneModeCycle.isChecked());
			changed = true;
		}
		
		// Report to transmission tab:
		if(valid && (!editing || changed))
		{
			if(editing)
				transmissionTab.saveEdited(schedule);
			else
				transmissionTab.addNew(schedule);
		}
		return valid;
	}
	
	@Override
	public void onClick(DialogInterface dialog, int which)
	{
		switch(which)
		{
			case DialogInterface.BUTTON_POSITIVE :
				if(save())
					dialog.dismiss();
				break;
			case DialogInterface.BUTTON_NEGATIVE :
				if(!editing)
					transmissionTab.addNew(null); // signals adding new schedule was cancelled
				break;
		}
	}
	
	@Override
	public void onNothingSelected(AdapterView<?> parent)
	{
		// does nothing
	}

}
