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
import android.graphics.Paint;
import android.graphics.drawable.ShapeDrawable;
import android.graphics.drawable.shapes.Shape;

public class TwoToneShapeDrawable extends ShapeDrawable
{
	private final Paint fillpaint, strokepaint;

	public TwoToneShapeDrawable(Shape s, int fillColor, int strokeColor, int strokeWidthDip)
	{
		super(s);
		fillpaint = new Paint(this.getPaint());
		fillpaint.setColor(fillColor);
		strokepaint = new Paint(fillpaint);
		strokepaint.setStyle(Paint.Style.STROKE);
		strokepaint.setStrokeWidth(strokeWidthDip);
		strokepaint.setColor(strokeColor);
	}

	@Override
	protected void onDraw(Shape shape, Canvas canvas, Paint paint)
	{
		shape.draw(canvas, fillpaint);
		shape.draw(canvas, strokepaint);
	}
	
}
