/**
 * 
 */
package uk.ac.ucl.excites.sapelli.collector.ui.drawables;

import android.graphics.Canvas;
import android.graphics.Path;
import android.graphics.Rect;

/**
 * Draws a vertical arrow (pointing up or down), which fits exactly in the vertical bounds of the canvas
 * 
 * @author mstevens
 */
public class VerticalArrow extends Arrow
{
	
	private boolean upPointing = true;
	
	public VerticalArrow(boolean upPointing)
	{
		this(DEFAULT_COLOUR, upPointing);
	}
	
	public VerticalArrow(int colour, boolean upPointing)
	{
		super(colour);
		this.upPointing = upPointing;
	}
	
	@Override
	public void draw(Canvas canvas)
	{
		// Canvas bounding box:
		Rect bounds = getBounds();
		
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

}
