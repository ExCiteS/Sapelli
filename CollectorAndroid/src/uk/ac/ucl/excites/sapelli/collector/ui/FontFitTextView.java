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

import java.util.ArrayList;
import java.util.List;

import android.content.Context;
import android.graphics.Paint;
import android.util.Log;
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
	private Coordinator coordinator;
	
	public FontFitTextView(Context context)
	{
		this(context, null);
	}
	
	public FontFitTextView(Context context, Coordinator coordinator)
	{
		super(context);
		this.coordinator = coordinator;
		initialise();
	}

	private void initialise()
	{
		if(coordinator != null)
			coordinator.register(this);
		testPaint = new Paint();
		testPaint.set(this.getPaint());
		// max size defaults to the initially specified text size unless it is too small
	}

	/**
	 * Compute maximum font size which makes the specified text fits in the text box assuming the text box is the specified width.
	 */
	private float refitText(String text, int textWidth)
	{
		if(textWidth <= 0)
			return this.getTextSize();
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
		return lo;
	}

	@Override
	protected void onMeasure(int widthMeasureSpec, int heightMeasureSpec)
	{
		super.onMeasure(widthMeasureSpec, heightMeasureSpec);
		int parentWidth = MeasureSpec.getSize(widthMeasureSpec);
		int height = getMeasuredHeight(); // ???
		this.setTextSize(TypedValue.COMPLEX_UNIT_PX, refitText(this.getText().toString(), parentWidth));
		if(coordinator != null)
			coordinator.refitted(this);
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
	
	/**
	 * @author mstevens, Ben
	 *
	 */
	static public class Coordinator
	{
		
		private List<FontFitTextView> views;
		private float minMaxFontSize = Float.MAX_VALUE;
		
		public Coordinator()
		{
			this.views = new ArrayList<FontFitTextView>();
		}
		
		public void register(FontFitTextView view)
		{
			views.add(view);
		}
		
		public void refitted(FontFitTextView view)
		{
			if(view.getTextSize() < minMaxFontSize)
			{
				minMaxFontSize = view.getTextSize();
				for(FontFitTextView v : views)
					if(v != view)
						v.setTextSize(TypedValue.COMPLEX_UNIT_PX, minMaxFontSize);
			}
			else if(view.getTextSize() > minMaxFontSize)
				view.setTextSize(TypedValue.COMPLEX_UNIT_PX, minMaxFontSize);
		}
		
	}

}