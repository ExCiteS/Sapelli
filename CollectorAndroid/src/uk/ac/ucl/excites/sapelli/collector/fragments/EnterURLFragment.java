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

import android.app.AlertDialog;
import android.app.Dialog;
import android.content.DialogInterface;
import android.os.Bundle;
import android.widget.EditText;
import uk.ac.ucl.excites.sapelli.collector.R;

/**
 * @author mstevens
 */
public class EnterURLFragment extends ProjectManagerFragment
{

	@Override
	public Dialog onCreateDialog(Bundle savedInstanceState)
	{
		AlertDialog.Builder bldr = new AlertDialog.Builder(getOwner());
		bldr.setTitle(R.string.enter_url_title);
		bldr.setIcon(R.drawable.ic_cloud_download_black_36dp);
		bldr.setMessage(R.string.enterURLMsg);
		bldr.setNegativeButton(android.R.string.cancel, null);
		AlertDialog dialog = bldr.create();
		
		// Set view:
		final EditText txtURL = new EditText(getActivity());
		txtURL.setTextAppearance(getOwner(), android.R.style.TextAppearance_Small);
		int lrSpacingPx = getDialogLeftRightPaddingPx();
		dialog.setView(txtURL, lrSpacingPx, getDialogMessageToViewSpacingPx(), lrSpacingPx, 0);
		
		// Set OK button:
		dialog.setButton(DialogInterface.BUTTON_POSITIVE, getOwner().getString(android.R.string.ok), new DialogInterface.OnClickListener()
		{
			public void onClick(DialogInterface dialog, int whichButton)
			{
				getOwner().loadProject(txtURL.getText().toString());
			}
		});
		
		return dialog;
	}

	@Override
	protected Integer getLayoutID()
	{
		return null;
	}
	
}
