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
