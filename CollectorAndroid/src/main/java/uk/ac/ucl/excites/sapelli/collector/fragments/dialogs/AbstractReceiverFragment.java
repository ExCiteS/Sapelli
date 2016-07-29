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
import android.content.DialogInterface;
import android.os.Bundle;
import uk.ac.ucl.excites.sapelli.collector.R;
import uk.ac.ucl.excites.sapelli.collector.fragments.ProjectManagerFragment;
import uk.ac.ucl.excites.sapelli.collector.transmission.SendConfigurationHelpers;
import uk.ac.ucl.excites.sapelli.collector.transmission.SendConfigurationHelpers.ReceiverUpdateCallback;
import uk.ac.ucl.excites.sapelli.shared.util.android.DialogHelpers;
import uk.ac.ucl.excites.sapelli.transmission.model.Correspondent;

/**
 * Abstract super class for DialogFragments which create or edit particular Correspondent types.  
 * 
 * @author mstevens
 */
public abstract class AbstractReceiverFragment<C extends Correspondent> extends ProjectManagerFragment implements DialogInterface.OnClickListener
{
	
	// DYNAMIC ------------------------------------------------------
	protected final ReceiverUpdateCallback callback;
	
	protected C editReceiver;
	
	public AbstractReceiverFragment(ReceiverUpdateCallback callback)
	{
		this(callback, null);
	}
	
	public AbstractReceiverFragment(ReceiverUpdateCallback callback, C receiver)
	{
		this.callback = callback;
		this.editReceiver = receiver;
	}
	
	public boolean isEditing()
	{
		return editReceiver != null;
	}
		
	@SuppressLint("InflateParams")
	@Override
	public AlertDialog onCreateDialog(Bundle savedInstanceState)
	{
		AlertDialog.Builder builder = new AlertDialog.Builder(getOwner())
		.setIcon(getIconId())
		.setTitle(isEditing() ? R.string.editReceiver : R.string.addReceiver)
		.setPositiveButton(android.R.string.ok, null) // listener will be set through the MakeNonDismission() call below
		.setNegativeButton(android.R.string.cancel, this);
		final AlertDialog dialog = builder.create();
		
		DialogHelpers.MakeNonDismissing(dialog, this, DialogInterface.BUTTON_POSITIVE);
		
		// Set view:
		int lrSpacingPx = getDialogLeftRightPaddingPx();
		dialog.setView(getRootLayout(), lrSpacingPx, getDialogMessageToViewSpacingPx(), lrSpacingPx, 0);
		
		return dialog;
	}
	
	protected abstract int getIconId();
	
	@Override
	public void onClick(DialogInterface dialog, int which)
	{
		switch(which)
		{
			case DialogInterface.BUTTON_POSITIVE :
				validateAndSave(dialog, null);
				break;
			case DialogInterface.BUTTON_NEGATIVE :
				dialog.cancel();
				break;
		}
	}
	
	@Override
	public void onCancel(DialogInterface dialog)
	{
		super.onCancel(dialog);
		if(!isEditing())
			callback.newReceiver(null); // signal that adding new receiver was cancelled
	}
	
	/**
	 * @param dialog
	 * @param toSave
	 */
	protected abstract void validateAndSave(final DialogInterface dialog, C toSave);
	
	protected void doSave(C toSave)
	{
		SendConfigurationHelpers.saveCorrespondent(getOwner(), toSave);
		if(callback != null)
		{
			if(!isEditing())
				callback.newReceiver(toSave);
			else
				callback.editedReceiver(editReceiver);
		}
	}
			
}
