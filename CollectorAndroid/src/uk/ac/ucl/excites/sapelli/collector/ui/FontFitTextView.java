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

package uk.ac.ucl.excites.sapelli.collector.ui;

import android.content.Context;
import android.graphics.Paint;
import android.util.AttributeSet;
import android.util.TypedValue;
import android.widget.TextView;

/**
 * Taken from <a href="http://stackoverflow.com/a/7875656/1084488">http://stackoverflow.com/a/7875656/1084488</a>
 * 
 * @author mstevens
 *
 */
public class FontFitTextView extends TextView
{

	private Paint testPaint;
	
	public FontFitTextView(Context context)
	{
		super(context);
		initialise();
	}

	public FontFitTextView(Context context, AttributeSet attrs)
	{
		super(context, attrs);
		initialise();
	}

	private void initialise()
	{
		testPaint = new Paint();
		testPaint.set(this.getPaint());
		// max size defaults to the initially specified text size unless it is too small
	}

	/**
	 * Resize the font so the specified text fits in the text box assuming the text box is the specified width.
	 */
	private void refitText(String text, int textWidth)
	{
		if(textWidth <= 0)
			return;
		int targetWidth = textWidth - this.getPaddingLeft() - this.getPaddingRight();
		float hi = 100;
		float lo = 2;
		final float threshold = 0.5f; // How close we have to be

		testPaint.set(this.getPaint());

		while((hi - lo) > threshold)
		{
			float size = (hi + lo) / 2;
			testPaint.setTextSize(size);
			if(testPaint.measureText(text) >= targetWidth)
				hi = size; // too big
			else
				lo = size; // too small
		}
		// Use lo so that we undershoot rather than overshoot
		this.setTextSize(TypedValue.COMPLEX_UNIT_PX, lo);
	}

	@Override
	protected void onMeasure(int widthMeasureSpec, int heightMeasureSpec)
	{
		super.onMeasure(widthMeasureSpec, heightMeasureSpec);
		int parentWidth = MeasureSpec.getSize(widthMeasureSpec);
		int height = getMeasuredHeight();
		refitText(this.getText().toString(), parentWidth);
		this.setMeasuredDimension(parentWidth, height);
	}

	@Override
	protected void onTextChanged(final CharSequence text, final int start, final int before, final int after)
	{
		refitText(text.toString(), this.getWidth());
	}

	@Override
	protected void onSizeChanged(int w, int h, int oldw, int oldh)
	{
		if(w != oldw)
		{
			refitText(this.getText().toString(), w);
		}
	}

}