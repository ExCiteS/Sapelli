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

package uk.ac.ucl.excites.sapelli.collector.ui.drawables;

import android.graphics.Canvas;
import android.graphics.Color;
import android.graphics.ColorFilter;
import android.graphics.Paint;
import android.graphics.Path;
import android.graphics.PixelFormat;
import android.graphics.Rect;
import android.graphics.drawable.Drawable;

/**
 * Draws a square saltire cross, which fits exactly in the bounds of the canvas
 * 
 * @author mstevens
 */
public class SaltireCross extends Drawable
{

	static public final int DEFAULT_COLOUR = Color.BLACK;
	static public final float DEFAULT_THICKNESS = 0.2f;
	static public final float MAX_THICKNESS = 0.25f;
	
	private int colour;
	private float thickness;
	private Paint crossPaint;
	
	/**
	 * @param colour
	 * @param thickness  a value between zero and {@value #MAX_THICKNESS}
	 */
	public SaltireCross(int color, float thickness)
	{
		if(thickness < 0.0f || thickness > MAX_THICKNESS)
			throw new IllegalArgumentException("Invalid thickness (" + thickness + "), value should be within [0.0, " + MAX_THICKNESS + "].");
		this.colour = color;
		this.thickness = thickness;
		
		// SaltireCross paint:
		crossPaint = new Paint(Paint.ANTI_ALIAS_FLAG);
		crossPaint.setStrokeWidth(1);
		crossPaint.setAntiAlias(true);
		crossPaint.setStyle(Paint.Style.FILL_AND_STROKE);
		crossPaint.setColor(colour);
	}
	
	public SaltireCross(int color)
	{
		this(color, DEFAULT_THICKNESS);
	}
	
	public SaltireCross(float thickness)
	{
		this(DEFAULT_COLOUR, thickness);
	}
	
	public SaltireCross()
	{
		this(DEFAULT_COLOUR, DEFAULT_THICKNESS);
	}
	
	@Override
	public void draw(Canvas canvas)
	{
		// Canvas bounding box:
		Rect canvasBounds = getBounds();
		
		// SaltireCross bounding box:
		float side = Math.min(canvasBounds.width(), canvasBounds.height());
		Rect crossBounds = new Rect(canvasBounds.left + (int) ((canvasBounds.width() - side) / 2.0f),
									canvasBounds.top + (int) ((canvasBounds.height() - side) / 2.0f),
									canvasBounds.right - (int) ((canvasBounds.width() - side) / 2.0f),
									canvasBounds.bottom - (int) ((canvasBounds.height() - side) / 2.0f));
		
		/*//Highlight bounds (for debugging only):
		Paint boundsPaint = new Paint(Paint.ANTI_ALIAS_FLAG);
		boundsPaint.setStrokeWidth(1);
		boundsPaint.setAntiAlias(true);
		boundsPaint.setStyle(Paint.Style.STROKE);
		boundsPaint.setColor(Color.RED);
		canvas.drawRect(canvasBounds, boundsPaint);
		canvas.drawRect(crossBounds, boundsPaint);*/
		
		// Draw the cross:
		float x1 = side * thickness;	//x1 = horizontal distance between 45� corner of triangle and nearest crossBounds corner
		float y1 = (side / 2.0f) - x1;	//y1 = altitude of triangle
		Path cross = new Path();
		cross.moveTo(crossBounds.left + x1, crossBounds.bottom);			// 1
		cross.lineTo(crossBounds.exactCenterX(), crossBounds.bottom - y1);	// 2
		cross.lineTo(crossBounds.right - x1, crossBounds.bottom);			// 3
		cross.lineTo(crossBounds.right, crossBounds.bottom - x1);			// 4
		cross.lineTo(crossBounds.right - y1, crossBounds.exactCenterY()); 	// 5
		cross.lineTo(crossBounds.right, crossBounds.top + x1);				// 6
		cross.lineTo(crossBounds.right - x1, crossBounds.top);				// 7
		cross.lineTo(crossBounds.exactCenterX(), crossBounds.top + y1);		// 8
		cross.lineTo(crossBounds.left + x1, crossBounds.top);				// 9
		cross.lineTo(crossBounds.left, crossBounds.top + x1); 				// 10
		cross.lineTo(crossBounds.left + y1, crossBounds.exactCenterY()); 	// 11
		cross.lineTo(crossBounds.left, crossBounds.bottom - x1); 			// 12
		cross.close();
		canvas.drawPath(cross, crossPaint);
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
