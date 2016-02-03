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

import java.util.Collections;
import java.util.List;

import android.annotation.SuppressLint;
import android.annotation.TargetApi;
import android.app.AlertDialog;
import android.app.Dialog;
import android.content.DialogInterface;
import android.graphics.Color;
import android.os.Build;
import android.os.Bundle;
import android.text.Editable;
import android.text.TextWatcher;
import android.util.Log;
import android.view.View;
import android.view.View.OnClickListener;
import android.view.ViewGroup;
import android.widget.AdapterView;
import android.widget.AdapterView.OnItemSelectedListener;
import android.widget.Button;
import android.widget.CheckBox;
import android.widget.EditText;
import android.widget.Spinner;
import android.widget.Toast;
import uk.ac.ucl.excites.sapelli.collector.R;
import uk.ac.ucl.excites.sapelli.collector.activities.ProjectManagerActivity;
import uk.ac.ucl.excites.sapelli.collector.fragments.ProjectManagerFragment;
import uk.ac.ucl.excites.sapelli.collector.fragments.tabs.TransmissionTabFragment;
import uk.ac.ucl.excites.sapelli.collector.transmission.SchedulingHelpers;
import uk.ac.ucl.excites.sapelli.collector.transmission.SendConfigurationHelpers;
import uk.ac.ucl.excites.sapelli.collector.transmission.SendConfigurationHelpers.ReceiverUpdateCallback;
import uk.ac.ucl.excites.sapelli.collector.transmission.SendSchedule;
import uk.ac.ucl.excites.sapelli.collector.transmission.control.AndroidTransmissionController;
import uk.ac.ucl.excites.sapelli.collector.util.AsyncTaskWithWaitingDialog;
import uk.ac.ucl.excites.sapelli.shared.util.TimeUtils;
import uk.ac.ucl.excites.sapelli.shared.util.android.AdvancedSpinnerAdapter;
import uk.ac.ucl.excites.sapelli.shared.util.android.DeviceControl;
import uk.ac.ucl.excites.sapelli.shared.util.android.DialogHelpers;
import uk.ac.ucl.excites.sapelli.transmission.control.TransmissionController;
import uk.ac.ucl.excites.sapelli.transmission.control.TransmissionController.ModelQueryStatus;
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
					if(newReceiver != null)
						ShowDialog(transmissionTab, Collections.singletonList(newReceiver), newReceiver, schedule, editing);
					else
						transmissionTab.addNew(null); // signal that adding schedule is cancelled
				}
				
				@Override
				public void editedReceiver(Correspondent editedReceiver) {}

				@Override
				public void deletedReceiver(Correspondent deleteReceiver) {}
			});
		}
		else
		{	// We have at least one selectable receiver, open the dialog:
			ShowDialog(transmissionTab, selectableReceivers, editing ? schedule.getReceiver() : null, schedule, editing);
		}
	}
	
	static private void ShowDialog(final TransmissionTabFragment transmissionTab, List<Correspondent> selectableReceivers, Correspondent preselectedReceiver, final SendSchedule schedule, final boolean editing)
	{
		new SendScheduleFragment(transmissionTab, selectableReceivers, preselectedReceiver, schedule, editing)
			.show(transmissionTab.getOwner().getSupportFragmentManager(), (editing ? R.string.edit : R.string.add) + SendScheduleFragment.class.getSimpleName());
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
	private AdvancedSpinnerAdapter<Correspondent> spinReceiverAdapter;
	
	// Model:
	private List<Correspondent> selectableReceivers;
	private final Correspondent preselectedReceiver;
	private SendSchedule schedule;
	private final boolean editing;
	
	private SendScheduleFragment(TransmissionTabFragment transmissionTab, List<Correspondent> selectableReceivers, Correspondent receiverToSelect, SendSchedule schedule, boolean editing)
	{
		this.transmissionTab = transmissionTab;
		this.selectableReceivers = selectableReceivers;
		this.preselectedReceiver = receiverToSelect;
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
		updateReceivers(false, preselectedReceiver); // !!!
		
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
		txtSendIntervalMin.setText(Float.valueOf((float) schedule.getTransmitIntervalS() / (float) TimeUtils.SEC_IN_MIN).toString());
		txtSendIntervalMin.setSelection(txtSendIntervalMin.getText().length());
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
	
	private void updateReceivers(boolean requery, Correspondent receiverToSelect)
	{
		// Refresh receive list if needed:
		if(requery)
			selectableReceivers = SendConfigurationHelpers.getSelectableCorrespondents(getOwner(), schedule);

		// Create new adapter if needed:
		if(requery || spinReceiverAdapter == null)
		{
			// Adapter:
			spinReceiverAdapter = new AdvancedSpinnerAdapter<Correspondent>(getOwner(),
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
		}
		
		// Select current/"none" receiver:
		spinReceiver.setSelection(spinReceiverAdapter.getPosition(receiverToSelect));
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

	@Override
	public void onNothingSelected(AdapterView<?> parent)
	{
		// does nothing
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
			case R.id.btnNewScheduleReceiver :
				createNewReceiver(getOwner(), this);
				break;
			case R.id.btnEditScheduleReceiver :
				Correspondent r = getReceiver();
				SendConfigurationHelpers.openEditReceiverDialog(getOwner(), this, r);
				break;
			case R.id.btnDeleteScheduleReceiver :
				// TODO warning about effects to other projects
				SendConfigurationHelpers.deleteCorrespondent(getOwner(), getReceiver());
				updateReceivers(true, null);
				break;
		}
	}
	
	@Override
	public void newReceiver(Correspondent newReceiver)
	{
		if(newReceiver != null)
			updateReceivers(true, newReceiver);
	}
	
	@Override
	public void editedReceiver(Correspondent editedReceiver)
	{
		updateReceivers(true, editedReceiver);
	}
	
	@Override
	public void deletedReceiver(Correspondent deletedReceiver)
	{
		updateReceivers(true, null);
	}
	
	@Override
	public void onClick(DialogInterface dialog, int which)
	{
		switch(which)
		{
			case DialogInterface.BUTTON_POSITIVE :
				save(dialog, false);
				break;
			case DialogInterface.BUTTON_NEGATIVE :
				transmissionTab.addNew(null); // signals adding/editing schedule was cancelled 
				break;
		}
	}
	
	/**
	 * @param dialog
	 * @param receiverQueryDone
	 */
	private void save(final DialogInterface dialog, boolean receiverQueryDone)
	{
		final ProjectManagerActivity activity = getOwner();
		if(activity == null)
		{
			dialog.dismiss();
			return;
		}
		
		// Input validation & change detection:
		boolean valid = true;
		boolean changed = false;
		
		//	Receiver:
		final Correspondent selectedReceiver = getReceiver();
		if(selectedReceiver == null)
		{
			groupReceiver.setBackgroundResource(R.color.red25percent);
			valid = false;
		}
		else if(receiverQueryDone)
		{	// Change receiver on schedule:
			schedule.setReceiver(selectedReceiver);
			changed = true;
		}
		else if(!selectedReceiver.equals(schedule.getReceiver()))
		{
			// Check if we need to do a model query:
			boolean requiresModelQuery = schedule.getReceiver() == null || !schedule.getReceiver().canBeSwappedWithoutNewModelQuery(selectedReceiver);
			
			if(requiresModelQuery)
			{
				// Contact receiver to check if it has this project:
				new ProjectQueryTask(activity, new ProjectQueryTaskCallback()
				{
					@Override
					public void done(ModelQueryStatus mqs)
					{
						if(mqs == null)
							mqs = ModelQueryStatus.Pending;
						switch(mqs)
						{
							case Pending:
								showModelQueryErrorMsg(activity, selectedReceiver, dialog, R.string.pendingProjectQuery);
								break;
							case ModelAbsent:
								showModelQueryErrorMsg(activity, selectedReceiver, dialog, R.string.projectAbsentOnReceiver);
								break;
							case ModelAccessUnauthorised:
								showModelQueryErrorMsg(activity, selectedReceiver, dialog, R.string.projectUnauthorised);
								break;
							case ModelPresent:
								Toast.makeText(
									activity,
									activity.getString(R.string.projectPresentOnReceiver, selectedReceiver.getName(), schedule.getProject().toString(false)),
									Toast.LENGTH_LONG).show();
								// Recursive call to save():
								save(dialog, true);
								break;
						}
					}
				}).execute(selectedReceiver);
			}
			else
			{
				// Pretend model query was done (it hasn't but it was not needed):
				save(dialog, true);	
			}
			return; // !!!
		}
		
		//	Interval:
		try
		{
			int intervalS = (int) (Float.valueOf(txtSendIntervalMin.getText().toString()) * (float) TimeUtils.SEC_IN_MIN);
			if(intervalS != schedule.getTransmitIntervalS())
			{
				schedule.setTransmitIntervalS(SchedulingHelpers.getEffectiveAlarmIntervalSeconds(intervalS));
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
		
		if(valid)
			dialog.dismiss();
	}
	
	private void showModelQueryErrorMsg(final ProjectManagerActivity activity, final Correspondent selectedReceiver, final DialogInterface dialog, int msgID)
	{
		activity.showYesNoDialog(
				R.string.schedule,
				activity.getString(
					msgID,
					selectedReceiver.getName(),
					schedule.getProject().toString(false)),
				new Runnable()
				{
					@Override
					public void run()
					{
						try
						{
							groupReceiver.setBackgroundResource(R.color.amber25percent);
						}
						catch(Exception ignore) {}
						// Recursive call to save():
						save(dialog, true);
					}
				}, false,
				new Runnable()
				{
					@Override
					public void run()
					{
						updateReceivers(false, null);
					}
				}, false);
	}
	
	private class ProjectQueryTask extends AsyncTaskWithWaitingDialog<ProjectManagerActivity, Correspondent, ModelQueryStatus>
	{
		
		static final int CYCLE_SLEEP_MS = 5 * 1000; 
		static final int TIMEOUT_CYCLES = 60; // 60 * 5s = 5 minutes

		private final ProjectQueryTaskCallback callback;
		
		public ProjectQueryTask(ProjectManagerActivity activity, ProjectQueryTaskCallback callback)
		{
			super(activity, true /*cancelable*/);
			this.callback = callback;
		}

		@Override
		protected ModelQueryStatus runInBackground(Correspondent... params)
		{
			ProjectManagerActivity activity = getContext();
			if(activity == null)
				return null;
			
			ModelQueryStatus mqs = ModelQueryStatus.Pending;
			try
			{
				Correspondent receiver = params[0];
				
				// Set progress msg:
				publishProgress(activity.getString(R.string.queryingReceiverAboutProject, schedule.getProject().toString(false), (CYCLE_SLEEP_MS * TIMEOUT_CYCLES / 1000 / 60)));
				
				// Get transmission controller:
				TransmissionController tc = new AndroidTransmissionController(activity.getCollectorApp());
				
				// Query receiver for model (i.e. project):
				int mqID = tc.sendModelQuery(activity.getCurrentProject(true).getModel(), receiver);
				
				// Check for status of request:
				int cycles = 0;
				while((mqs = tc.getModelQueryStatus(mqID)) == ModelQueryStatus.Pending && cycles < TIMEOUT_CYCLES)
				{
					Thread.sleep(CYCLE_SLEEP_MS);
					cycles++;
				}
			}
			catch(InterruptedException ignore) {}
			catch(Exception e)
			{
				Log.e(getClass().getSimpleName(), "Error during querying receiver for project", e);
			}
			return mqs;
		}
		
		@Override
		protected void onPostExecute(ModelQueryStatus result)
		{
			super.onPostExecute(result); // dismiss dialog
			callback.done(result);
		}
		
		@TargetApi(Build.VERSION_CODES.HONEYCOMB)
		@Override
		protected void onCancelled(ModelQueryStatus result)
		{
			super.onCancelled(result); // dismiss dialog
			callback.done(result);
		}

		@Override
		protected void onCancelled()
		{
			super.onCancelled(); // dismiss dialog
			callback.done(null);
		}
		
	}
	
	private interface ProjectQueryTaskCallback
	{
		
		public void done(ModelQueryStatus mqs);
		
	}

}
