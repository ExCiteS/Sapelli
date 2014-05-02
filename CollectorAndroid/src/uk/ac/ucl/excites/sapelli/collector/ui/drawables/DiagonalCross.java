/**
 * 
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
	private float lineWidth;
	
	/**
	 * @param colour
	 * @param lineWidthPx the stroke width of the lines the cross consists of (in pixels)
	 */
	public DiagonalCross(int color, float lineWidthPx)
	{
		if(lineWidthPx < 0.0f)
			throw new IllegalArgumentException("Invalid line width (" + lineWidthPx + " px).");
		this.colour = color;
		this.lineWidth = lineWidthPx;
	}
	
	@Override
	public void draw(Canvas canvas)
	{
		// Canvas bounding box:
		Rect canvasBounds = getBounds();

		// Paint:
		Paint crossPaint = new Paint(Paint.ANTI_ALIAS_FLAG);
		crossPaint.setStrokeWidth(lineWidth);
		crossPaint.setAntiAlias(true);
		crossPaint.setStyle(Paint.Style.FILL_AND_STROKE);
		crossPaint.setColor(colour);
		
		// Draw the cross:
		canvas.drawLine(canvasBounds.left, canvasBounds.top, canvasBounds.right, canvasBounds.bottom, crossPaint);
		canvas.drawLine(canvasBounds.left, canvasBounds.bottom, canvasBounds.right, canvasBounds.top, crossPaint);
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
