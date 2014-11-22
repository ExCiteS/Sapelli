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

import java.lang.reflect.Field;
import java.util.Arrays;
import android.annotation.TargetApi;
import android.content.Context;
import android.graphics.Paint;
import android.graphics.Rect;
import android.os.Build;
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

		// slightly reduce vertical padding on TextView (may need to re-enable for other langs due to accents):
		this.setIncludeFontPadding(false); // TODO re-evaluate whether this is a good idea (what if we do have accepts to show?)
	}
	
	/* (non-Javadoc)
	 * @see android.widget.TextView#setIncludeFontPadding(boolean)
	 */
	@Override
	public void setIncludeFontPadding(boolean includepad)
	{
		if(includepad != getIncludeFontPadding())
		{	// Force reset of fitted test size:
			lastTargetWidth = -1;
			lastTargetHeight = -1;
		}
		// Super call:
		super.setIncludeFontPadding(includepad);
	}

	/**
	 * @see android.widget.TextView#getIncludeFontPadding()
	 * @see android.widget.TextView#setIncludeFontPadding(boolean)
	 * @see http://grepcode.com/file/repository.grepcode.com/java/ext/com.google.android/android/4.0.4_r2.1/android/widget/TextView.java#6433
	 */
	@TargetApi(Build.VERSION_CODES.JELLY_BEAN)
	@Override
	public boolean getIncludeFontPadding()
	{
		if(Build.VERSION.SDK_INT >= Build.VERSION_CODES.JELLY_BEAN /* 16 */)
			return super.getIncludeFontPadding();
		else
		{	// Force our way with reflection:
			try
			{
				Field includePadField = TextView.class.getDeclaredField("mIncludePad"); // may throw NoSuchFieldException
				includePadField.setAccessible(true);
				boolean includePad = includePadField.getBoolean(this); // may throw IllegalAccessException
				includePadField.setAccessible(false);
				return includePad;
			}
			catch(Exception e)
			{
				e.printStackTrace(System.err);
				return true; // return the default
			}
		}
	}

	/**
	 * Computes maximum font size which makes the text fit in the bounding box defined by the viewWidth and viewHeight parameters.
	 * 
	 * We use 2 distinct ways of measuring text:
	 * 	- {@link Paint#getTextBounds(String, int, int, Rect)}:
	 * 	  This computes both width & height but unfortunately underestimates both.
	 * 		* The underestimation of height is caused by TextView always adding a line spacing's
	 * 		  worth of padding above and below the text, as explained here: http://stackoverflow.com/questions/4768738
	 * 		  We solve this by adding {@link Paint#getFontMetricsInt()} to the height
	 * 		* The underestimation of width has a more low-level cause, as explained here: http://stackoverflow.com/a/7579469/1084488
	 * 		  We solve this by instead relying on...
	 * 	- {@link Paint#measureText(String)}: this does compute the width in a reliable way
	 * 
	 * @param viewWidth
	 * @param viewHeight
	 * @param force
	 */
	private void fitText(int viewWidth, int viewHeight, boolean force)
	{
		// Avoid unnecessary (re)fitting: 0 or negative view dimensions
		if(viewWidth <= 0 || viewHeight <= 0)
			return;

		// Get text to measure:
		String text = this.getText().toString();

		// Avoid unnecessary (re)fitting: text is empty
		if(text.isEmpty())
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

			// Measure bounds and store in boundsRect:
			testPaint.getTextBounds(text, 0, text.length(), boundsRect);

			// NOTE: Here I am cheating a bit and only making sure there is enough space for the text + the top padding
			// (i.e. the bottom padding can go off the screen and the font can be a little larger)
			// TODO why cheat, can't we just add 2 * testPaint.getFontMetricsInt(null) ? --> probably not a good idea because it seems we already reserve more space than needed
			// TODO why do we use testPaint.getFontMetricsInt(null) and not testPaint.getFontMetrics(null) ?
			// TODO what about testPaint.getFontSpacing() shouldn't we use that instead?
			// TODO allow for changed line spacing? - this assumes default only
			
			if(testPaint.measureText(text) >= targetWidth || boundsRect.height() + (this.getIncludeFontPadding() ? testPaint.getFontMetricsInt(null) : 0) >= targetHeight)
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

		private FontFitTextView[] views = new FontFitTextView[0]; // init as empty array!
		private float[] intendedTextSizes = new float[0]; // init as empty array!

		public int claimSlot()
		{
			int slot = views.length;
			views = Arrays.copyOf(views, slot + 1);
			intendedTextSizes = Arrays.copyOf(intendedTextSizes, slot + 1);
			intendedTextSizes[slot] = MAX_TEXT_SIZE;
			//Log.d("FFTV", "Coordinator.claimSlot: number of slots: " + intendedTextSizes.length);
			return slot;
		}

		public void setMaxTextSize(FontFitTextView view, float maxTextSize)
		{
			// Just in case:
			if(view.coordinatorSlot < 0 || view.coordinatorSlot >= intendedTextSizes.length)
				throw new IllegalArgumentException("Invalid or unclaimed coordinator slot (" + view.coordinatorSlot + "), make sure to claim a slot for each coordinated view.");

			// Register the view:
			views[view.coordinatorSlot] = view;

			// If the intended text size is different than before...
			if(intendedTextSizes[view.coordinatorSlot] != maxTextSize) // TODO round/floor to whole pixels?
			{
				intendedTextSizes[view.coordinatorSlot] = maxTextSize;
				// TODO wait until all fitted? (causes more "requestLayout() improperly called" warnings in some case)
				coordinate();
			}
		}

		public float getCoordinatedTextSize()
		{
			//Log.d("FFTV", "Intended sizes: " + Arrays.toString(intendedTextSizes));
			float min = MAX_TEXT_SIZE; // keep track of the smallest font size of all of the views we're tracking
			for(int v = 0; v < views.length; v++)
				if(views[v] != null && intendedTextSizes[v] < min)
					min = intendedTextSizes[v];
			return min;
		}

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