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
