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

import android.content.Context;
import android.graphics.Paint;
import android.graphics.Rect;
import android.util.TypedValue;
import android.widget.TextView;

/**
 * Adapted from <a href="http://stackoverflow.com/a/7875656/1084488">http://stackoverflow.com/a/7875656/1084488</a>
 * 
 * @author mstevens, benelliott
 */
public class FontFitTextView extends TextView
{

	/**
	 * Maximum font size which we will ever use in this view. This does need to be this high because some text gets pretty big.
	 */
	private static final float MAX_TEXT_SIZE = 150.0f; //

	/**
	 * Minimum font size which we will ever use in this view.
	 */
	private static final float MIN_TEXT_SIZE = 5.0f;

	/**
	 * How close we have to be to accept the size we have converged on and stop iterating.
	 */
	private static final float THRESHOLD = 0.5f;

	private Paint testPaint;
	private TextSizeCoordinator coordinator;
	private int coordinatorSlot;
	private Rect boundsRect;
	private int lastTargetWidth = -1;
	private int lastTargetHeight = -1;

	/**
	 * @param context
	 */
	public FontFitTextView(Context context)
	{
		this(context, null, -1);
	}

	/**
	 * @param context
	 * @param coordinator
	 */
	public FontFitTextView(Context context, TextSizeCoordinator coordinator)
	{
		this(context, coordinator, -1);
	}

	/**
	 * @param context
	 * @param coordinator
	 * @param coordinatorSlot
	 */
	public FontFitTextView(Context context, TextSizeCoordinator coordinator, int coordinatorSlot)
	{
		super(context);

		this.coordinator = coordinator;
		if(coordinator != null)
			this.coordinatorSlot = coordinatorSlot >= 0 ? coordinatorSlot : coordinator.claimSlot();

		// disable font padding (we deal with interline spacing ourselves):
		this.setIncludeFontPadding(false);
	}

	/**
	 * Computes maximum font size which makes the text fit in the bounding box defined by the viewWidth and viewHeight parameters.
	 * This will ensure that the resulting text size will accommodate multiline text.
	 * <br>
	 * We use 2 distinct ways of measuring text:
	 * <ul>
	 * 	<li>
	 * 		{@link Paint#getTextBounds(String, int, int, Rect)}:
	 * 		<br>
	 * 	  	This computes both width & height but unfortunately underestimates both.
	 * 			<ul>
	 * 				<li>
	 * 					The underestimation of height is caused by TextView always adding a line spacing's
	 * 		  			worth of padding above and below the text, as explained here: http://stackoverflow.com/questions/4768738
	 * 					<br>
	 * 		 			We solve this by adding {@link Paint#getFontMetricsInt()} to the height
	 * 				</li>
	 * 				<li>
	 * 					The underestimation of width has a more low-level cause, as explained here: http://stackoverflow.com/a/7579469/1084488
	 * 					<br>
	 * 		  			We solve this by instead relying on...
	 * 				</li>
	 * 			</ul>
	 * 	</li>
	 * 	<li>
	 * 		{@link Paint#measureText(String)}:
	 * 		<br>
	 * 		this does compute the width in a reliable way
	 * 	</li>
	 * </ul>
	 * @param viewWidth
	 * @param viewHeight
	 * @param force
	 */
	private void fitText(int viewWidth, int viewHeight, boolean force)
	{
		// Avoid unnecessary (re)fitting: 0 or negative view dimensions
		if(viewWidth <= 0 || viewHeight <= 0)
			return;

		// Avoid unnecessary (re)fitting: text is empty
		if(this.getText().length() < 1)
			return;
		
		
		// Compute the target text width/height as the provided container width minus relevant padding
		int targetWidth = viewWidth - this.getPaddingLeft() - this.getPaddingRight();
		int targetHeight = viewHeight - this.getPaddingBottom() - this.getPaddingTop();

		// Avoid unnecessary (re)fitting: no forcing and target dimensions are the same as last time
		if(!force && targetWidth == lastTargetWidth && targetHeight == lastTargetHeight)
			return;

		// At this point we know a new size computation will take place...
		//Log.d("FFTV", "fitText: IS REFITTING, text: " + this.getText() + ", hash: " + hashCode() + " w=" + viewWidth + ", h=" + viewHeight + ", forced: " + force);

		// Initialise lo & hi bounds:
		float lo = MIN_TEXT_SIZE;
		float hi = (coordinator == null) ? MAX_TEXT_SIZE : coordinator.getCoordinatedTextSize();
		
		// get an array of all the lines in the text:
		String[] lines = this.getText().toString().split("\n");

		// Get a Rect to store measured bounds:
		if(boundsRect == null)
			boundsRect = new Rect();
		
		// Get/update testPaint:
		if(testPaint == null)
			testPaint = new Paint();
		testPaint.set(this.getPaint());

		// Fitting loop (binary search):
		while((hi - lo) > THRESHOLD)
		{
			float size = (hi + lo) / 2;
			testPaint.setTextSize(size);

			// keep track of maximum width across all lines of text:
			float maxWidth = 0;
			// keep track of cumulative height across all lines of text:
			float cumulHeight = -testPaint.getFontMetrics(null); // initialise to (- 1 * line spacing) so we can just add one line spacing for each line (should really be n-1)
			
			for (String line : lines) {
				// measure bounds for each line of text:
				testPaint.getTextBounds(line, 0, line.length(), boundsRect);
				// max width is max(previous max width, this line width)
				maxWidth = Math.max(maxWidth, testPaint.measureText(line));
				// add line height and interline spacing to cumulative height
				cumulHeight += boundsRect.height() + testPaint.getFontMetrics(null);
			}			
			
			// see if our text fits
			if(maxWidth >= targetWidth || cumulHeight >= targetHeight)
				hi = size; // was too big, so reduce hi
			else
				lo = size; // was too small, so increase lo
		}

		// Remember target dimensions:
		lastTargetWidth = targetWidth;
		lastTargetHeight = targetHeight;

		// We set lo as the new textSize, so that we undershoot rather than overshoot:
		if(coordinator != null)
			// Let the coordinator apply the textSize at the appropriate time:
			coordinator.setMaxTextSize(this, lo);
		else if(this.getTextSize() != lo)
			// Apply now:
			this.setTextSize(TypedValue.COMPLEX_UNIT_PX, lo);
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see android.widget.TextView#onMeasure(int, int)
	 */
	@Override
	protected void onMeasure(int widthMeasureSpec, int heightMeasureSpec)
	{
		//Log.d("FFTV", "onMeasure: " + this.getText() + ", hash: " + hashCode() + " w=" + MeasureSpec.getSize(widthMeasureSpec) + ", h=" + MeasureSpec.getSize(heightMeasureSpec));
		
		// Super call:
		super.onMeasure(widthMeasureSpec, heightMeasureSpec); // do not remove(!), nor move below fitText() (we get more "requestLayout() improperly called" warnings in that case)
		
		// Request (re)fit: but don't force it (by passing false) such that fitText() can determine itself whether it is necessary
		fitText(MeasureSpec.getSize(widthMeasureSpec), MeasureSpec.getSize(heightMeasureSpec), false);
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see android.widget.TextView#onTextChanged(java.lang.CharSequence, int, int, int)
	 */
	@Override
	protected void onTextChanged(final CharSequence text, final int start, final int before, final int after)
	{
		//Log.d("FFTV", "onTextChanged: text: " + text + ", this.getText(): " + this.getText() + ", hash: " + hashCode() + " w=" + this.getWidth() + ", h=" + this.getHeight());
		
		// Request (re)fit: we force it (by passing true) because it is surely needed due to the changed text
		fitText(this.getWidth(), this.getHeight(), true);
		
		// Note: no need for a super call as super implementation is empty
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see android.view.View#onSizeChanged(int, int, int, int)
	 */
	@Override
	protected void onSizeChanged(int w, int h, int oldw, int oldh)
	{
		//Log.d("FFTV", "onSizeChanged: " + this.getText() + ", hash: " + hashCode() + ", changed:" + (w != oldw || h != oldh) + ", old-w=" + oldw + ", old-h=" + oldh + ", w=" + w + ", h=" + h);
		
		if(w != oldw || h != oldh)
			// Request (re)fit: but don't force it (by passing false) such that fitText() can determine itself whether it is really necessary
			fitText(w, h, false);
		
		// Note: no need for a super call as super implementation is empty
	}

	/**
	 * Helper class to coordinate text sizes across multiple FontFitTextView instances
	 * 
	 * @author mstevens, benelliott
	 */
	static public class TextSizeCoordinator
	{

		private ArrayList<FontFitTextView> views = new ArrayList<FontFitTextView>();
		private ArrayList<Float> intendedTextSizes = new ArrayList<Float>(); // init as empty array!


		/**
		 * Requests that the coordinator reserve a 'slot' for this view so that it will update its font size at some point.
		 * @return
		 */
		public int claimSlot()
		{
			int slot = views.size();
			intendedTextSizes.add(MAX_TEXT_SIZE); //TODO check
			//Log.d("FFTV", "Coordinator.claimSlot: number of slots: " + intendedTextSizes.length);
			return slot;
		}

		/**
		 * Requests that the controller record the provided maximum text size for the provided view, and then coordinates font sizes across all views.
		 * @param view - the view whose max text size is being recorded
		 * @param maxTextSize - the max text size to record
		 */
		public void setMaxTextSize(FontFitTextView view, float maxTextSize)
		{
			// Just in case:
			if(view.coordinatorSlot < 0 || view.coordinatorSlot >= intendedTextSizes.size())
				throw new IllegalArgumentException("Invalid or unclaimed coordinator slot (" + view.coordinatorSlot + "), make sure to claim a slot for each coordinated view.");

			// Register the view:
			views.add(view);

			// If the intended text size is different than before...
			if(intendedTextSizes.get(view.coordinatorSlot) != maxTextSize) // TODO round/floor to whole pixels?
			{
				intendedTextSizes.set(view.coordinatorSlot, maxTextSize);
				// TODO wait until all fitted? (causes more "requestLayout() improperly called" warnings in some case)
				coordinate();
			}
		}

		/**
		 * @return the largest font size that appreciates the maximum font size constraints of all views registered to the controller.
		 */
		public float getCoordinatedTextSize()
		{
			//Log.d("FFTV", "Intended sizes: " + Arrays.toString(intendedTextSizes));
			float min = MAX_TEXT_SIZE; // keep track of the smallest font size of all of the views we're tracking
			for(int v = 0; v < views.size(); v++)
				if(views.get(v) != null && intendedTextSizes.get(v) < min)
					min = intendedTextSizes.get(v);
			return min;
		}

		/**
		 * Applies the coordinated font size to all registered views.
		 */
		public void coordinate()
		{
			float coordinatedTextSize = getCoordinatedTextSize();
			//Log.d("FFTV", "Applying coordinated text size: " + coordinatedTextSize);
			for(FontFitTextView v : views)
			{
				// Log.d("FFTV", "Applying to: " + v.getText());
				if(v != null && v.getTextSize() != coordinatedTextSize) // TODO use minimum difference instead of !=? E.g. Math.abs(v.getTextSize() - coordinatedTextSize) > 1.0 (or some other value)
					v.setTextSize(TypedValue.COMPLEX_UNIT_PX, coordinatedTextSize);
			}
		}

	}

}