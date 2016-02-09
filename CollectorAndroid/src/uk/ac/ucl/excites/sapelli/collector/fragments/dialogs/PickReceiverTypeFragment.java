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

import android.annotation.SuppressLint;
import android.app.AlertDialog;
import android.app.Dialog;
import android.content.DialogInterface;
import android.os.Bundle;
import android.view.View;
import android.view.View.OnClickListener;
import uk.ac.ucl.excites.sapelli.collector.R;
import uk.ac.ucl.excites.sapelli.collector.activities.ProjectManagerActivity;
import uk.ac.ucl.excites.sapelli.collector.fragments.ProjectManagerFragment;
import uk.ac.ucl.excites.sapelli.collector.transmission.SendConfigurationHelpers.ReceiverUpdateCallback;

/**
 * @author mstevens
 *
 */
public class PickReceiverTypeFragment extends ProjectManagerFragment implements OnClickListener, DialogInterface.OnClickListener
{

	// STATIC -------------------------------------------------------
	static public void ShowDialog(ProjectManagerActivity owner, ReceiverUpdateCallback callback)
	{
		new PickReceiverTypeFragment(callback).show(owner.getSupportFragmentManager(), PickReceiverTypeFragment.class.getSimpleName());
	}
	
	// DYNAMIC ------------------------------------------------------
	private final ReceiverUpdateCallback callback;
	
	public PickReceiverTypeFragment(ReceiverUpdateCallback callback)
	{
		this.callback = callback;
	}
	
	/* (non-Javadoc)
	 * @see uk.ac.ucl.excites.sapelli.collector.fragments.ProjectManagerFragment#getLayoutID()
	 */
	@Override
	protected Integer getLayoutID()
	{
		return R.layout.dialog_pick_receiver_type;
	}

	/* (non-Javadoc)
	 * @see uk.ac.ucl.excites.sapelli.collector.fragments.ProjectManagerFragment#setupUI(android.view.View)
	 */
	@Override
	protected void setupUI(View rootLayout)
	{
		rootLayout.findViewById(R.id.btnSMSReceiver).setOnClickListener(this);
		rootLayout.findViewById(R.id.btnGeoKeyReceiver).setOnClickListener(this);
	}

	@SuppressLint("InflateParams")
	@Override
	public Dialog onCreateDialog(Bundle savedInstanceState)
	{
		AlertDialog.Builder builder = new AlertDialog.Builder(getOwner())
		.setIcon(R.drawable.ic_transfer_black_36dp)
		.setTitle(R.string.chooseReceiverType)
		.setNegativeButton(android.R.string.cancel, this);
		AlertDialog dialog = builder.create();
		
		// Set view:
		int lrSpacingPx = getDialogLeftRightPaddingPx();
		setDialogView(dialog, lrSpacingPx, 0, lrSpacingPx, 0);

		return dialog;
	}

	@Override
	public void onClick(View v)
	{
		switch(v.getId())
		{
			case R.id.btnSMSReceiver:
				getDialog().dismiss();
				SMSReceiverFragment.ShowAddDialog(getOwner(), callback);
				break;
			case R.id.btnGeoKeyReceiver:
				getDialog().dismiss();
				GeoKeyReceiverFragment.ShowAddDialog(getOwner(), callback);
				break;
		}
	}
	
	@Override
	public void onClick(DialogInterface dialog, int which)
	{
		if(which == DialogInterface.BUTTON_NEGATIVE)
			dialog.cancel();
	}

	@Override
	public void onCancel(DialogInterface dialog)
	{
		super.onCancel(dialog);
		callback.newReceiver(null); // signal that adding new receiver was cancelled
	}
	
}
