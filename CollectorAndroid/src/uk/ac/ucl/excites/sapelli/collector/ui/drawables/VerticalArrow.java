/**
 * 
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
 * Draws a vertical arrow (pointing up or down), which fits exactly in the vertical bounds of the canvas
 * 
 * @author mstevens
 */
public class VerticalArrow extends Drawable
{
	
	static public final int DEFAULT_COLOUR = Color.BLACK;
	static public final boolean DEFAULT_FILL_WIDTH = false;

	private int colour;
	private boolean upPointing = true;
	
	public VerticalArrow(boolean upPointing)
	{
		this(DEFAULT_COLOUR, upPointing);
	}
	
	public VerticalArrow(int colour, boolean upPointing)
	{
		this.colour = colour;
		this.upPointing = upPointing;
	}
	
	@Override
	public void draw(Canvas canvas)
	{
		// Canvas bounding box:
		Rect bounds = getBounds();
		
		// Arrow paint:
		Paint arrowPaint = new Paint(Paint.ANTI_ALIAS_FLAG);
		arrowPaint.setStrokeWidth(1);
		arrowPaint.setAntiAlias(true);
		arrowPaint.setStyle(Paint.Style.FILL_AND_STROKE);
		arrowPaint.setColor(colour);
		
		// The arrow head is an equilateral triangle with altitude equal to the half the height of the bounding box
		float altitude = bounds.height() / 2.0f;
		//float side = 2.0f * altitude / (float) Math.sqrt(3.0);
		float halfSide = altitude / (float) Math.sqrt(3.0);
		float quarterSide = halfSide / 2.0f;

		// Draw arrow:
		Path arrow = new Path();
		arrow.moveTo(bounds.exactCenterX(), upPointing ? bounds.top : bounds.bottom);
		arrow.lineTo(bounds.exactCenterX() + halfSide, upPointing ? bounds.top + altitude : bounds.bottom - altitude);
		arrow.lineTo(bounds.exactCenterX() + quarterSide, upPointing ? bounds.top + altitude : bounds.bottom - altitude);
		arrow.lineTo(bounds.exactCenterX() + quarterSide, upPointing ? bounds.bottom : bounds.top);
		arrow.lineTo(bounds.exactCenterX() - quarterSide, upPointing ? bounds.bottom : bounds.top);
		arrow.lineTo(bounds.exactCenterX() - quarterSide, upPointing ? bounds.top + altitude : bounds.bottom - altitude);
		arrow.lineTo(bounds.exactCenterX() - halfSide, upPointing ? bounds.top + altitude : bounds.bottom - altitude);
		arrow.close();
		canvas.drawPath(arrow, arrowPaint);
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
