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
import android.graphics.Rect;
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
	private static final float START_HI = 100F; // This does need to be this high because some text gets pretty big
	private static final float START_LO = 5F;
	private static final float THRESHOLD = 1F;  // how close we have to be to accept the size we have converged on and stop iterating

	private Paint testPaint;
	private FontSizeCoordinator fontSizeCoordinator;
	private Rect boundsRect;
	private int lastViewWidth = -1;
	private int lastViewHeight = -1;
	private float lastFontSize;
	
	public FontFitTextView(Context context)
	{
		this(context, null);
	}

	public FontFitTextView(Context context, FontSizeCoordinator fontSizeCoordinator)
	{
		super(context);
		this.fontSizeCoordinator = fontSizeCoordinator;
		
		if (fontSizeCoordinator != null)
			fontSizeCoordinator.register(this);
		
		 //slightly reduce vertical padding on TextView (may need to re-enable for other langs due to accents):
		this.setIncludeFontPadding(false);
	}

	/**
	 * Compute maximum font size which makes the specified text fit in the box defined by the textWidth and textHeight parameters.
	 */
	private void refitText(String text, int viewWidth, int viewHeight)
	{
		if(viewWidth <= 0 || viewHeight <= 0)
			// do not try to change font size if a dimension is 0
			return;
		
		if (viewWidth == lastViewWidth && viewHeight == lastViewHeight) {
			// already seen these dimensions (but may need to set text size again)
			this.setTextSize(TypedValue.COMPLEX_UNIT_PX, lastFontSize);
			return;
		}
		
		// target text width/height is the provided container width minus relevant padding
		int targetWidth = viewWidth - this.getPaddingLeft() - this.getPaddingRight();
		int targetHeight = viewHeight - this.getPaddingBottom() - this.getPaddingTop();
		// initialise max/min to some appropriate values
		float hi = (fontSizeCoordinator == null) ? START_HI : fontSizeCoordinator.minMaxFontSize;
		float lo = START_LO;

		if (testPaint == null)
			testPaint = new Paint();
		testPaint.set(this.getPaint());
		
		// Store measured bounds in a new Rect
		if (boundsRect == null)
			boundsRect = new Rect(); 

		while((hi - lo) > THRESHOLD)
		{
			float size = (hi + lo) / 2;
			testPaint.setTextSize(size);
			testPaint.getTextBounds(text, 0, text.length(), boundsRect); //measure bounds and store in boundsRect
			
			// NOTE: " + testPaint.getFontMetricsInt(null)":
			// TextView always adds a line spacing's worth of padding above and below text -- http://stackoverflow.com/questions/4768738/android-textview-remove-spacing-and-padding-on-top-and-bottom
			// Here I am cheating a bit and only making sure there is enough space for the text + the top padding
			// (i.e. the bottom padding can go off the screen and the font can be a little larger)
			
			//TODO allow for changed line spacing? - this assumes default only
			//NOTE: getTextBounds not used to measure text width; see http://www.stackoverflow.com/questions/7549182/
			if(testPaint.measureText(text) >= targetWidth || boundsRect.height() + testPaint.getFontMetricsInt(null) >= targetHeight)
				hi = size; // was too big, so reduce hi
			else
				lo = size; // was too small, so increase lo
		}
		
		lastViewWidth = viewWidth;
		lastViewHeight = viewHeight;
		
		// Use lo so that we undershoot rather than overshoot

		lastFontSize = lo;
		this.setTextSize(TypedValue.COMPLEX_UNIT_PX, lo);
		
		if (fontSizeCoordinator != null)
			fontSizeCoordinator.refitted(this);
	}


	@Override
	protected void onMeasure(int widthMeasureSpec, int heightMeasureSpec)
	{
		super.onMeasure(widthMeasureSpec, heightMeasureSpec);
		refitText(this.getText().toString(), MeasureSpec.getSize(widthMeasureSpec), MeasureSpec.getSize(heightMeasureSpec));
	}

	@Override
	protected void onTextChanged(final CharSequence text, final int start, final int before, final int after)
	{
		refitText(text.toString(), this.getWidth(), this.getHeight());
		// invalidate "cached" font size:
		lastViewWidth = -1;
		lastViewHeight = -1;
	}

	@Override
	protected void onSizeChanged(int w, int h, int oldw, int oldh)
	{
		if(w != oldw || h != oldh)
			refitText(this.getText().toString(), w, h);
	}
	
	
	/**
	 * @author mstevens, Ben
	 *
	 */
	static public class FontSizeCoordinator
	{
		
		private List<FontFitTextView> views;
		private float minMaxFontSize = START_HI; // keep track of the smallest font size of all of the views we're tracking
		
		public FontSizeCoordinator()
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
				// this TextView has a font size smaller than the others, so update them all
				minMaxFontSize = view.getTextSize();
				for(FontFitTextView v : views)
					if(v != view) {
						v.setTextSize(TypedValue.COMPLEX_UNIT_PX, minMaxFontSize);
						v.lastFontSize = minMaxFontSize;
					}
			}
			else if(view.getTextSize() > minMaxFontSize) {
				// this view's font size must be reduced to match that of all the others
				view.setTextSize(TypedValue.COMPLEX_UNIT_PX, minMaxFontSize);
				view.lastFontSize = minMaxFontSize;
			}
		}
		
	}

}