/**
 * 
 */
package uk.ac.ucl.excites.sapelli.collector.ui.drawables;

import android.graphics.Canvas;
import android.graphics.Path;
import android.graphics.Rect;

/**
 * Draws a horizontal arrow (pointing left or right), which fits exactly in the vertical, and optionally also the horizontal bounds of the canvas
 * 
 * @author mstevens, Michalis Vitos
 */
public class HorizontalArrow extends Arrow
{
	
	static public final boolean DEFAULT_FILL_WIDTH = false;

	private boolean leftPointing;
	private boolean fillWidth = DEFAULT_FILL_WIDTH;
	
	public HorizontalArrow(boolean leftPointing)
	{
		this(DEFAULT_COLOUR, leftPointing, DEFAULT_FILL_WIDTH);
	}
	
	public HorizontalArrow(int colour, boolean leftPointing)
	{
		this(colour, leftPointing, DEFAULT_FILL_WIDTH);
	}
	
	public HorizontalArrow(int colour, boolean leftPointing, boolean fillWidth)
	{
		super(colour);
		this.leftPointing = leftPointing;
		this.fillWidth = fillWidth;
	}
	
	@Override
	public void draw(Canvas canvas)
	{
		// Canvas bounding box:
		Rect bounds = getBounds();
		float quarterHeight = bounds.height() / 4.0f;
		
		//Log.d("Arrow", "Bound: " + bounds.left + ", " + bounds.bottom + ", " + bounds.right + ", " + bounds.top);
		
		/*// Highlight bounds (for debugging only):
		Paint boundsPaint = new Paint(Paint.ANTI_ALIAS_FLAG);
		boundsPaint.setStrokeWidth(1);
		boundsPaint.setAntiAlias(true);
		boundsPaint.setStyle(Paint.Style.STROKE);
		boundsPaint.setColor(Color.RED);
		canvas.drawRect(bounds, boundsPaint);*/
		
		// Altitude of the equilateral triangle which forms the arrow head:
		float altitude = (float) (Math.sqrt(3.0) * bounds.height() / 2.0f);

		// Draw arrow:
		Path arrow = new Path();
		if(fillWidth)
		{
			arrow.moveTo(leftPointing ? bounds.left : bounds.right, bounds.exactCenterY());
			arrow.lineTo(leftPointing ? bounds.left + altitude : bounds.right - altitude, bounds.top);
			arrow.lineTo(leftPointing ? bounds.left + altitude : bounds.right - altitude, bounds.top + quarterHeight);
			arrow.lineTo(leftPointing ? bounds.right : bounds.left, bounds.top + quarterHeight);
			arrow.lineTo(leftPointing ? bounds.right : bounds.left, bounds.bottom - quarterHeight);
			arrow.lineTo(leftPointing ? bounds.left + altitude : bounds.right - altitude, bounds.bottom - quarterHeight);
			arrow.lineTo(leftPointing ? bounds.left + altitude : bounds.right - altitude, bounds.bottom);
		}
		else
		{
			float middle = bounds.width() / 2.0f; // find the middle of the bounds
			arrow.moveTo(leftPointing ? middle - altitude : middle + altitude, bounds.exactCenterY());
			arrow.lineTo(middle, bounds.top);
			arrow.lineTo(middle, bounds.top + quarterHeight);
			arrow.lineTo(leftPointing ? middle + altitude : middle - altitude, bounds.top + quarterHeight);
			arrow.lineTo(leftPointing ? middle + altitude : middle - altitude, bounds.bottom - quarterHeight);
			arrow.lineTo(middle, bounds.bottom - quarterHeight);
			arrow.lineTo(middle, bounds.bottom);
		}
		arrow.close();
		canvas.drawPath(arrow, arrowPaint);
	}

}
