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
import android.graphics.Rect;
import android.util.AttributeSet;
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
		this.setIncludeFontPadding(false); //slightly reduce vertical padding on TextView (may need to re-enable for other langs due to accents)
		initialise();
	}

	private void initialise()
	{
		testPaint = new Paint();
		testPaint.set(this.getPaint());
		// max size defaults to the initially specified text size unless it is too small
	}


	/**
	 * Compute maximum font size which makes the specified text fit in the box defined by the textWidth and textHeight parameters.
	 */
	private float refitText(String text, int textWidth, int textHeight)
	{
		if(textWidth <= 0 || textHeight <= 0)
			return this.getTextSize();
		int targetWidth = textWidth - this.getPaddingLeft() - this.getPaddingRight();
		int targetHeight = textHeight - this.getPaddingBottom() - this.getPaddingTop();
		float hi = 100;
		float lo = 2;
		final float threshold = 0.5f; // How close we have to be
		testPaint.set(this.getPaint());
		Rect boundsRect = new Rect(); // Store measured bounds in a new Rect

		while((hi - lo) > threshold)
		{
			float size = (hi + lo) / 2;
			testPaint.setTextSize(size);
			testPaint.getTextBounds(text, 0, text.length(), boundsRect); //measure bounds and store in boundsRect
			int additionalSpacing = testPaint.getFontMetricsInt(null); //TextView always adds a line spacing's worth of padding above and below text
			//TODO allow for changed line spacing? - this assumes default only
			//Note: getTextBounds not used to measure text width; see http://www.stackoverflow.com/questions/7549182/
			if(testPaint.measureText(text) >= targetWidth 
					|| boundsRect.height()+2*additionalSpacing >= targetHeight)
				hi = size; // too big
			else
				lo = size; // too small
		}
		// Use lo so that we undershoot rather than overshoot
		return lo;
	}


	@Override
	protected void onMeasure(int widthMeasureSpec, int heightMeasureSpec)
	{
		super.onMeasure(widthMeasureSpec, heightMeasureSpec);
		int parentWidth = MeasureSpec.getSize(widthMeasureSpec);
		int parentHeight = MeasureSpec.getSize(heightMeasureSpec);
		refitText(this.getText().toString(), parentWidth, parentHeight);
		this.setMeasuredDimension(parentWidth, parentHeight);
	}

	@Override
	protected void onTextChanged(final CharSequence text, final int start, final int before, final int after)
	{
		refitText(text.toString(), this.getWidth(), this.getHeight());
	}

	@Override
	protected void onSizeChanged(int w, int h, int oldw, int oldh)
	{
		if(w != oldw || h != oldh)
		{
			refitText(this.getText().toString(), w, h);
		}
	}

}