package uk.ac.ucl.excites.sapelli.collector.ui.fields;

import java.io.File;
import java.io.FileOutputStream;

import uk.ac.ucl.excites.sapelli.collector.R;
import uk.ac.ucl.excites.sapelli.collector.control.CollectorController;
import uk.ac.ucl.excites.sapelli.collector.control.Controller.LeaveRule;
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
import android.graphics.Bitmap.CompressFormat;
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

/**
 * A UI for Drawing fields, which allow for the capture of "finger drawings" created on the touchscreen.
 * 
 * @author benelliott
 *
 */
public class AndroidDrawingUI extends AndroidMediaUI<DrawingField>
{
	public static final Bitmap.CompressFormat DRAWING_OUTPUT_FORMAT = CompressFormat.PNG; // TODO link to DrawingField?
	public static final int DRAWING_OUTPUT_QUALITY = 0; // PNG will ignore compression factor anyway

	private DrawingView drawingView;

	public AndroidDrawingUI(DrawingField field, CollectorController controller, CollectorView collectorUI)
	{
		super(field, controller, collectorUI);
	}

	@Override
	protected boolean onCapture()
	{
		try
		{
			// get a new file into which to store the capture:
			captureFile = field.getNewAttachmentFile(controller.getFileStorageProvider(),controller.getCurrentRecord());
			// get a bitmap of the current drawing:
			Bitmap drawing = drawingView.captureDrawingBitmap();
			// compress the bitmap into a PNG and save it into the file:
			FileOutputStream fos;
			fos = new FileOutputStream(captureFile);
			drawing.compress(DRAWING_OUTPUT_FORMAT, DRAWING_OUTPUT_QUALITY, fos);
			// close the file output stream and attach the media file to the field:
			fos.close();
			attachMedia(captureFile);
			// proceed to the review screen or the next field:
			if(field.isShowReview())
				controller.goToCurrent(LeaveRule.UNCONDITIONAL_WITH_STORAGE);
			else
				controller.goForward(true);
		}
		catch(Exception e)
		{
			e.printStackTrace();
		}
		// allow subsequent clicks:
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
		// note views aren't cached in media UIs so create one each time
		drawingView = new DrawingView(context);
		return drawingView;
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
	 * A view that interprets "drag" gestures as the user "painting" the view with their finger.
	 * <br>
	 * Inspired by http://www.vogella.com/tutorials/AndroidTouch/article.html
	 * 
	 * @author benelliott
	 */
	private class DrawingView extends View
	{
		private Paint paint;
		private Path path; // the path the user has painted
		private boolean downClicked; // need this for accessibility
		private int backgroundColor;
		private Bitmap backgroundImg;
		private float backgroundLeft; // left x-coordinate of background image (for centering)
		private float backgroundTop; // top y-coordinate of background image (for centering)

		public DrawingView(Context context)
		{
			super(context);
			// create the background image:
			File backgroundImgFile = controller.getFileStorageProvider().getProjectImageFile(controller.getProject(), field.getBackgroundImageRelativePath());
			if(!FileHelpers.isReadableFile(backgroundImgFile) || (backgroundImg = BitmapFactory.decodeFile(backgroundImgFile.getAbsolutePath())) == null)
				// failed to use custom background image so use default:
				backgroundImg = BitmapFactory.decodeResource(context.getResources(),R.drawable.edit);

			backgroundColor = Color.parseColor(field.getBackgroundColor());
			
			paint = new Paint();
			paint.setAntiAlias(true); // looks pixellated otherwise
			paint.setStrokeWidth(field.getStrokeWidth());
			paint.setColor(Color.parseColor(field.getStrokeColor()));
			paint.setStyle(Paint.Style.STROKE); // as opposed to Fill etc
			paint.setStrokeJoin(Paint.Join.ROUND);

			path = new Path();
		}

		@Override
		protected void onLayout(boolean changed, int left, int top, int right, int bottom)
		{
			if (changed)
			{	// calculate where the background image has to be in order for it to be centred:
				backgroundLeft = 0.5f * (right - left - backgroundImg.getWidth()); // left coord is (view width - image width) / 2
				backgroundTop = 0.5f * (bottom - top - backgroundImg.getHeight()); // top coord is (view height - image height) / 2 
			}
		};

		@Override
		public void onDraw(Canvas canvas)
		{
			canvas.drawColor(backgroundColor); // draw background colour
			canvas.drawBitmap(backgroundImg, backgroundLeft, backgroundTop, paint); // draw background image
			canvas.drawPath(path, paint); // draw the user's cumulative path
		}

		@Override
		public boolean onTouchEvent(MotionEvent event)
		{
			float x = event.getX();
			float y = event.getY();
			switch(event.getAction())
			{
			case MotionEvent.ACTION_DOWN:
				// start drawing from where the user has put their finger down
				path.moveTo(x, y);
				downClicked = true; // a click has effectively been started
				return true;
			case MotionEvent.ACTION_MOVE:
				// continue drawing path up to the current finger position
				path.lineTo(x, y);
				break;
			case MotionEvent.ACTION_UP:
				if(downClicked)
				{
					// execute click as this event is the "up" from the click:
					downClicked = false;
					performClick();
				}
				// nothing else to do with "up" events
				break;
			default:
				return false;
			}
			// ensure this view will be redrawn:
			invalidate();
			return true;
		}

		@Override
		public boolean performClick()
		{
			super.performClick();
			// do nothing -- lint moans if this method is not overridden though
			return true;
		}
		
		/**
		 * @return a Bitmap containing the user's drawing up until now, including the background colour (in case a custom one is set) but NOT the background image
		 */
		private Bitmap captureDrawingBitmap()
		{
			// create a bitmap to return:
			Bitmap toReturn = Bitmap.createBitmap(this.getWidth(), this.getHeight(), Bitmap.Config.ARGB_8888);
			// draw background colour and user's path on it through a canvas:
			Canvas retCanvas = new Canvas(toReturn);
			retCanvas.drawColor(backgroundColor);
			retCanvas.drawPath(path, paint);
			return toReturn;
		}
	}
}
