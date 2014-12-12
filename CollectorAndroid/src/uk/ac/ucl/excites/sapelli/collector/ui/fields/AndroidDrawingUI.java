package uk.ac.ucl.excites.sapelli.collector.ui.fields;

import java.io.File;

import uk.ac.ucl.excites.sapelli.collector.R;
import uk.ac.ucl.excites.sapelli.collector.control.CollectorController;
import uk.ac.ucl.excites.sapelli.collector.model.Field;
import uk.ac.ucl.excites.sapelli.collector.model.fields.DrawingField;
import uk.ac.ucl.excites.sapelli.collector.ui.CollectorView;
import uk.ac.ucl.excites.sapelli.collector.ui.items.FileImageItem;
import uk.ac.ucl.excites.sapelli.collector.ui.items.ImageItem;
import uk.ac.ucl.excites.sapelli.collector.ui.items.Item;
import uk.ac.ucl.excites.sapelli.collector.ui.items.ResourceImageItem;
import uk.ac.ucl.excites.sapelli.collector.util.ColourHelpers;
import uk.ac.ucl.excites.sapelli.shared.io.FileHelpers;
import android.content.Context;
import android.graphics.Bitmap;
import android.graphics.BitmapFactory;
import android.graphics.Canvas;
import android.graphics.Color;
import android.graphics.Paint;
import android.graphics.Path;
import android.net.Uri;
import android.view.MotionEvent;
import android.view.View;
import android.widget.ImageView;
import android.widget.ImageView.ScaleType;

public class AndroidDrawingUI extends AndroidMediaUI<DrawingField>
{
	
	public static final int DEFAULT_BACKGROUND_COLOR = Color.LTGRAY;
	public static final int DEFAULT_STROKE_COLOR = Color.BLACK;
	public static final float DEFAULT_STROKE_WIDTH = 8f;


	private DrawingView drawingSurface;

	public AndroidDrawingUI(DrawingField field, CollectorController controller, CollectorView collectorUI)
	{
		super(field, controller, collectorUI);
		// TODO Auto-generated constructor stub
	}

	@Override
	protected boolean onCapture()
	{
		// TODO grab view contents and save it
		return true;
	}

	@Override
	protected void onDiscard()
	{
		// nothing to do
	}

	@Override
	protected View getCaptureContent(Context context)
	{
		
		if (drawingSurface == null)
			drawingSurface = new DrawingView(context, DEFAULT_BACKGROUND_COLOR, BitmapFactory.decodeResource(context.getResources(), R.drawable.edit), DEFAULT_STROKE_COLOR, DEFAULT_STROKE_WIDTH); // TODO config



		return drawingSurface;
	}

	@Override
	protected View getReviewContent(Context context, File mediaFile)
	{
		// add an ImageView to the review UI:
		ImageView reviewView = new ImageView(context);
		reviewView.setScaleType(ScaleType.FIT_CENTER);
		// set the ImageView to the provided drawing file:
		reviewView.setImageURI(Uri.fromFile(mediaFile));
		return reviewView;
	}

	@Override
	protected Item getItemFromFile(File file)
	{
		return new FileImageItem(file);
	}

	@Override
	protected Item generateCaptureButton(Context context)
	{
		ImageItem captureButton = null;
		File captureImgFile = controller.getFileStorageProvider().getProjectImageFile(controller.getProject(), field.getCaptureButtonImageRelativePath());
		if(FileHelpers.isReadableFile(captureImgFile))
			// return a custom drawing capture button if it exists
			captureButton = new FileImageItem(captureImgFile);
		else
			// otherwise just use the default resource (a tick)
			captureButton = new ResourceImageItem(context.getResources(), R.drawable.button_tick_svg);
		captureButton.setBackgroundColor(ColourHelpers.ParseColour(field.getBackgroundColor(), Field.DEFAULT_BACKGROUND_COLOR));
		return captureButton;
	}

	/**
	 * TODO
	 * inspired by http://www.vogella.com/tutorials/AndroidTouch/article.html
	 * 
	 * @author benelliott
	 */
	private class DrawingView extends View
	{
		private Paint paint;
		private Path path;
		private boolean downClicked; // need this for accessibility
		private int backgroundColor;
		private Bitmap backgroundImg;
		private float backgroundLeft;
		private float backgroundTop;

		public DrawingView(Context context, int backgroundColor, Bitmap backgroundImg, int strokeColor, float strokeWidth)
		{
			super(context);
			
			this.backgroundColor = backgroundColor;
			this.backgroundImg = backgroundImg;

			paint = new Paint();
			paint.setAntiAlias(true);
			paint.setStrokeWidth(strokeWidth); //TODO to constants ...
			paint.setColor(strokeColor);
			paint.setStyle(Paint.Style.STROKE);
			paint.setStrokeJoin(Paint.Join.ROUND);

			path = new Path();
		}

		@Override
		protected void onLayout(boolean changed, int left, int top, int right, int bottom)
		{
			if (changed)
			{
				backgroundLeft = 0.5f * (right - left - backgroundImg.getWidth());
				backgroundTop = 0.5f * (bottom - top - backgroundImg.getHeight());
			}
		};

		@Override
		public void onDraw(Canvas canvas)
		{
			canvas.drawColor(backgroundColor);
			canvas.drawBitmap(backgroundImg, backgroundLeft, backgroundTop, paint);
			canvas.drawPath(path, paint);
		}

		@Override
		public boolean onTouchEvent(MotionEvent event)
		{
			float x = event.getX();
			float y = event.getY();
			switch(event.getAction())
			{
			case MotionEvent.ACTION_DOWN:
				path.moveTo(x, y);
				downClicked = true;
				return true;
			case MotionEvent.ACTION_MOVE:
				path.lineTo(x, y);
				break;
			case MotionEvent.ACTION_UP:
				// nothing to do
				if(downClicked)
				{
					// execute click as this event is the "up" from the click:
					downClicked = false;
					performClick();
				}
				break;
			default:
				return false;
			}

			invalidate();
			return true;
		}

		@Override
		public boolean performClick()
		{
			super.performClick();
			// do nothing -- lint moans if this is not overriden though
			return true;
		}
	}
}
