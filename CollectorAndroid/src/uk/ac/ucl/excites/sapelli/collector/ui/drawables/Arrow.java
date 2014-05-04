/**
 * 
 */
package uk.ac.ucl.excites.sapelli.collector.ui.drawables;

import android.graphics.Color;
import android.graphics.ColorFilter;
import android.graphics.Paint;
import android.graphics.PixelFormat;
import android.graphics.drawable.Drawable;

/**
 * Abstract super class for {@link HorizontalArrow} & {@link VerticalArrow}.
 * 
 * @author mstevens
 */
public abstract class Arrow extends Drawable
{
	
	static public final int DEFAULT_COLOUR = Color.BLACK;
	static public final boolean DEFAULT_FILL_WIDTH = false;

	private int colour;
	protected Paint arrowPaint;
	
	public Arrow(int colour)
	{
		this.colour = colour;
		
		// Arrow paint:
		arrowPaint = new Paint(Paint.ANTI_ALIAS_FLAG);
		arrowPaint.setStrokeWidth(1);
		arrowPaint.setAntiAlias(true);
		arrowPaint.setStyle(Paint.Style.FILL_AND_STROKE);
		arrowPaint.setColor(colour);
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
