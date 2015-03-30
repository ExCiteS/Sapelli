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
import android.graphics.Color;
import android.graphics.Paint;
import android.graphics.Paint.Style;
import android.util.Log;

/**
 * A View that displays its own (Canvas) size (width & height in number of pixels).
 * The displayed size applies to the inner bounding box (i.e. not counting padding).
 * 
 * The font size scales automatically according to the the available space and can be coordinated
 * across multiple {@link MeasureView}s or {@link TextFitView}s using a {@link TextSizeCoordinator}.
 * 
 * @author mstevens
 */
public class MeasureView extends TextFitView
{

	static private final String TAG = "SizeDisplayView";
	
	private Paint borderPaint;
	
	public MeasureView(Context context)
	{
		this(context, null, UNASSIGNED_SLOT);
	}

	/**
	 * @param context
	 * @param coordinator
	 */
	public MeasureView(Context context, TextSizeCoordinator coordinator)
	{
		this(context, coordinator, UNASSIGNED_SLOT);
	}
	
	/**
	 * @param context
	 * @param coordinator
	 * @param coordinatorSlot
	 */
	public MeasureView(Context context, TextSizeCoordinator coordinator, int coordinatorSlot)
	{
		super(context, coordinator, coordinatorSlot);
		
		// Initialise border paint:
		borderPaint = new Paint();
		borderPaint.setAntiAlias(true);
		borderPaint.setColor(Color.BLUE);
		borderPaint.setStyle(Style.STROKE);
		borderPaint.setStrokeWidth(1);
	}
	
	@Override
	public void setText(String text)
	{
		Log.d(TAG, "Ignoring external setText() call.");
	}

	/* (non-Javadoc)
	 * @see uk.ac.ucl.excites.sapelli.collector.ui.TextFitView#onLayout(boolean, int, int, int, int)
	 */
	@Override
	public void onLayout(boolean changed, int left, int top, int right, int bottom)
	{
		// Text becomes W/H dimensions:
		int contentWidth = this.getWidth() - this.getPaddingLeft() - this.getPaddingRight();
		int contentHeight = this.getHeight() - this.getPaddingBottom() - this.getPaddingTop();
		super.setText("W: " + contentWidth + "px\nH:" + contentHeight + "px"); // will also call fitText() like super.onLayout() would
	}

}
