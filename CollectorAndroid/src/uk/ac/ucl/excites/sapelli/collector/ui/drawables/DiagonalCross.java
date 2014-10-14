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

package uk.ac.ucl.excites.sapelli.collector.ui.drawables;

import android.graphics.Canvas;
import android.graphics.Color;
import android.graphics.ColorFilter;
import android.graphics.Paint;
import android.graphics.PixelFormat;
import android.graphics.Rect;
import android.graphics.drawable.Drawable;

/**
 * Draws a diagonal cross across the canvas
 * 
 * @author mstevens
 */
public class DiagonalCross extends Drawable
{

	static public final int DEFAULT_COLOUR = Color.BLACK;
	
	private int colour;
	private Paint linePaint;
	
	/**
	 * @param colour
	 * @param lineWidthPx the stroke width of the lines the cross consists of (in pixels)
	 */
	public DiagonalCross(int colour, float lineWidthPx, Paint.Cap lineEnds)
	{
		if(lineWidthPx < 0.0f)
			throw new IllegalArgumentException("Invalid line width (" + lineWidthPx + " px).");
		this.colour = colour;
		
		// Paint:
		linePaint = new Paint(Paint.ANTI_ALIAS_FLAG);
		linePaint.setStyle(Paint.Style.FILL_AND_STROKE);
		linePaint.setAntiAlias(true);
		linePaint.setColor(colour);
		linePaint.setStrokeWidth(lineWidthPx);
		linePaint.setStrokeCap(lineEnds);		
	}
	
	@Override
	public void draw(Canvas canvas)
	{
		// Canvas bounding box:
		Rect canvasBounds = getBounds();
		
		// Draw the cross:
		canvas.drawLine(canvasBounds.left, canvasBounds.top, canvasBounds.right, canvasBounds.bottom, linePaint);
		canvas.drawLine(canvasBounds.left, canvasBounds.bottom, canvasBounds.right, canvasBounds.top, linePaint);
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see android.graphics.drawable.Drawable#getOpacity()
	 */
	@Override
	public int getOpacity()
	{
		return PixelFormat.UNKNOWN;
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see android.graphics.drawable.Drawable#setAlpha(int)
	 */
	@Override
	public void setAlpha(int alpha)
	{	//if the colour already had an alpha value it is overwritten:
		this.colour = Color.argb(alpha, Color.red(this.colour), Color.green(this.colour), Color.blue(this.colour));
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see android.graphics.drawable.Drawable#setColorFilter(android.graphics.ColorFilter)
	 */
	@Override
	public void setColorFilter(ColorFilter cf)
	{
		//Not supported
	}

}
