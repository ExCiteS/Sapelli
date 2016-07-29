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

package uk.ac.ucl.excites.sapelli.shared.util.android;

import android.app.AlertDialog;
import android.content.DialogInterface;
import android.view.View;
import android.widget.Button;

/**
 * @author mstevens
 *
 */
public final class DialogHelpers
{

	/**
	 * Makes buttons on an AlertDialog non-dismissing (i.e. clicking them won't close the dialog).
	 * 
	 * @param dialog
	 * @param onClickListener listener to which to direct clicks
	 * @param whichButtons the buttons which need to be made non-dismissing, they must already have been set on the AlertDialog and are specified here as as DialogInterface button identifier(s), e.g.: {@link DialogInterface#BUTTON_POSITIVE}, {@link DialogInterface#BUTTON_NEGATIVE}, ..., for
	 * 
	 *  @see http://stackoverflow.com/a/7626339/1084488
	 */
	static public void MakeNonDismissing(final AlertDialog dialog, final DialogInterface.OnClickListener onClickListener, final int... whichButtons)
	{
		dialog.setOnShowListener(new DialogInterface.OnShowListener()
		{
			@Override
			public void onShow(final DialogInterface dialogInterface)
			{
				for(final int whichButton : whichButtons)
				{
					Button button = dialog.getButton(whichButton);
					if(button != null)
						// Replace dismissing onClickListener with custom one
						button.setOnClickListener(new View.OnClickListener()
						{
							@Override
							public void onClick(View v)
							{
								// Re-route to onClickListener:
								onClickListener.onClick(dialogInterface, whichButton);
							}
						});
				}
			}
		});
	}
	
	private DialogHelpers()
	{
		// should never be instantiated
	}
	
}
