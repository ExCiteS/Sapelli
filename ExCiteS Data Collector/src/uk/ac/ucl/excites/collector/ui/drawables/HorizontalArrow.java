/**
 * 
 */
package uk.ac.ucl.excites.collector.ui.drawables;

import android.graphics.Canvas;
import android.graphics.Color;
import android.graphics.ColorFilter;
import android.graphics.Paint;
import android.graphics.Path;
import android.graphics.PixelFormat;
import android.graphics.Rect;
import android.graphics.drawable.Drawable;

/**
 * Draws a horizontal arrow (pointing left or right), which fits exactly in the bounds of the canvas
 * 
 * @author mstevens
 */
public class HorizontalArrow extends Drawable
{
	
	static public final int DEFAULT_COLOUR = Color.BLACK;

	private int colour;
	private boolean leftPointing = true;
	
	public HorizontalArrow(int colour, boolean leftPointing)
	{
		this.colour = colour;
		this.leftPointing = leftPointing;
	}
	
	public HorizontalArrow(boolean leftPointing)
	{
		this(DEFAULT_COLOUR, false);
	}
	
	@Override
	public void draw(Canvas canvas)
	{
		// Canvas bounding box:
		Rect bounds = getBounds();
		float quarterHeight = (bounds.height() / 4.0f);
		
		//Log.d("Arrow", "Bound: " + bounds.left + ", " + bounds.bottom + ", " + bounds.right + ", " + bounds.top);
		
		/*// Highlight bounds (for debugging only):
		Paint boundsPaint = new Paint(Paint.ANTI_ALIAS_FLAG);
		boundsPaint.setStrokeWidth(1);
		boundsPaint.setAntiAlias(true);
		boundsPaint.setStyle(Paint.Style.STROKE);
		boundsPaint.setColor(Color.RED);
		canvas.drawRect(bounds, boundsPaint);*/
		
		// Arrow paint:
		Paint arrowPaint = new Paint(Paint.ANTI_ALIAS_FLAG);
		arrowPaint.setStrokeWidth(1);
		arrowPaint.setAntiAlias(true);
		arrowPaint.setStyle(Paint.Style.FILL_AND_STROKE);
		arrowPaint.setColor(colour);
		
		// Altitude of the equilateral triangle which forms the arrow head:
		float altitude = (float) (Math.sqrt(3.0) * bounds.height() / 2.0);
		
		// Find the middle of the bounds
		float middle = bounds.width() / 2;

		// Draw arrow:
		Path arrow = new Path();
		arrow.moveTo(leftPointing ? middle - altitude : middle + altitude, bounds.exactCenterY());
		arrow.lineTo(middle, bounds.top);
		arrow.lineTo(middle, bounds.top + quarterHeight);
		arrow.lineTo(leftPointing ? middle + altitude : middle - altitude, bounds.top + quarterHeight);
		arrow.lineTo(leftPointing ? middle + altitude : middle - altitude, bounds.bottom - quarterHeight);
		arrow.lineTo(middle, bounds.bottom - quarterHeight);
		arrow.lineTo(middle, bounds.bottom);
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
