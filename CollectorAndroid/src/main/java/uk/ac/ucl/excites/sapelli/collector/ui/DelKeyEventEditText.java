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

package uk.ac.ucl.excites.sapelli.collector.ui;

import android.content.Context;
import android.util.AttributeSet;
import android.view.KeyEvent;
import android.view.inputmethod.EditorInfo;
import android.view.inputmethod.InputConnection;
import android.view.inputmethod.InputConnectionWrapper;
import android.widget.EditText;

/**
 * EditText subclass which makes listening for the DEL key work
 * on recent Android versions.
 * 
 * @see http://stackoverflow.com/a/16551670/1084488
 * 
 * @author mstevens
 */
public class DelKeyEventEditText extends EditText
{

	public DelKeyEventEditText(Context context, AttributeSet attrs, int defStyle)
	{
		super(context, attrs, defStyle);
	}

	public DelKeyEventEditText(Context context, AttributeSet attrs)
	{
		super(context, attrs);
	}

	public DelKeyEventEditText(Context context)
	{
		super(context);
	}

	@Override
	public InputConnection onCreateInputConnection(EditorInfo outAttrs)
	{
		return new CustomInputConnection(super.onCreateInputConnection(outAttrs), true);
	}

	private class CustomInputConnection extends InputConnectionWrapper
	{

		public CustomInputConnection(InputConnection target, boolean mutable)
		{
			super(target, mutable);
		}

		/*@Override
		public boolean sendKeyEvent(KeyEvent event)
		{
			if(event.getAction() == KeyEvent.ACTION_DOWN && event.getKeyCode() == KeyEvent.KEYCODE_DEL)
			{
				// Un-comment if you wish to cancel the backspace:
				//return false;
			}
			return super.sendKeyEvent(event);
		}*/

		@Override
		public boolean deleteSurroundingText(int beforeLength, int afterLength)
		{
			// In recent Android versions, deleteSurroundingText(1, 0) will be called for backspace!
			if(beforeLength == 1 && afterLength == 0)
			{
				// backspace
				return	sendKeyEvent(new KeyEvent(KeyEvent.ACTION_DOWN, KeyEvent.KEYCODE_DEL)) &&
						sendKeyEvent(new KeyEvent(KeyEvent.ACTION_UP, KeyEvent.KEYCODE_DEL));
			}

			return super.deleteSurroundingText(beforeLength, afterLength);
		}

	}

}