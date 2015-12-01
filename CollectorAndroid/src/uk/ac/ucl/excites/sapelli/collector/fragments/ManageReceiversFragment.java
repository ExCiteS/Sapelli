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

package uk.ac.ucl.excites.sapelli.collector.fragments;

import android.annotation.SuppressLint;
import android.app.AlertDialog;
import android.app.Dialog;
import android.content.DialogInterface;
import android.os.Bundle;
import android.support.v7.app.AppCompatActivity;
import android.view.View;
import android.view.View.OnClickListener;
import android.widget.AdapterView;
import android.widget.AdapterView.OnItemClickListener;
import android.widget.Button;
import android.widget.ListView;
import uk.ac.ucl.excites.sapelli.collector.R;
import uk.ac.ucl.excites.sapelli.collector.transmission.SendConfigurationHelpers;
import uk.ac.ucl.excites.sapelli.collector.transmission.SendConfigurationHelpers.ReceiverUpdateCallback;
import uk.ac.ucl.excites.sapelli.collector.ui.adapters.ReceiverAdapter;
import uk.ac.ucl.excites.sapelli.shared.util.android.DialogHelpers;
import uk.ac.ucl.excites.sapelli.transmission.model.Correspondent;

/**
 * @author mstevens
 *
 */
public class ManageReceiversFragment extends ProjectManagerFragment implements OnItemClickListener, OnClickListener, DialogInterface.OnClickListener, ReceiverUpdateCallback 
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
	private Button btnEditReceiver;
	private Button btnDeleteReceiver;
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
	 * @see uk.ac.ucl.excites.sapelli.collector.fragments.ProjectManagerFragment#setupUI(android.view.View)
	 */
	@Override
	protected void setupUI(View rootLayout)
	{
		listReceivers = (ListView) rootLayout.findViewById(R.id.listReceivers);
		listReceivers.setOnItemClickListener(this);
		updateReceivers();
		
		btnEditReceiver = (Button) rootLayout.findViewById(R.id.btnEditReceiver);
		btnEditReceiver.setOnClickListener(this);
		btnEditReceiver.setEnabled(false);
		btnDeleteReceiver = (Button) rootLayout.findViewById(R.id.btnDeleteReceiver);
		btnDeleteReceiver.setOnClickListener(this);
		btnDeleteReceiver.setEnabled(false);
		btnAddReceiver = (Button) rootLayout.findViewById(R.id.btnAddReceiver);
		btnAddReceiver.setOnClickListener(this);
	}
	
	private void updateReceivers()
	{
		listReceiversAdapter = new ReceiverAdapter(getOwner(), SendConfigurationHelpers.getReceivers(getOwner()), false);
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
		Correspondent receiver = getReceiver();
		switch(v.getId())
		{
			case R.id.btnEditReceiver :
				// TODO warn about projects that use the receiver
				SendConfigurationHelpers.openEditReceiverDialog(getOwner(), this, receiver);
				// TODO request TransmissionTab update
				break;
			case R.id.btnDeleteReceiver :
				// TODO warn about projects that use the receiver
				if(SendConfigurationHelpers.deleteCorrespondent(getOwner(), receiver) != null)
					deletedReceiver(receiver);				
				getOwner().refreshAllTabs();
				break;
			case R.id.btnAddReceiver :
				PickReceiverTypeFragment.ShowDialog(getOwner(), this);
				break;
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
	public void editedReceiver(Correspondent newReceiver, Correspondent oldReceiver)
	{
		updateReceivers();
		// Forward:
		if(callback != null)
			callback.editedReceiver(newReceiver, oldReceiver);
	}
	
	@Override
	public void deletedReceiver(Correspondent oldReceiver)
	{
		updateReceivers();
		// Forward:
		if(callback != null)
			callback.deletedReceiver(oldReceiver);
	}
	
	private Correspondent getReceiver()
	{
		return listReceiversAdapter.getItem(listReceivers.getCheckedItemPosition());
	}

	@Override
	public void onItemClick(AdapterView<?> parent, View view, int position, long id)
	{
		boolean selected = getReceiver() != null;
		btnEditReceiver.setEnabled(selected);
		btnDeleteReceiver.setEnabled(selected);
	}

}
