package uk.ac.ucl.excites.sapelli.collector.ui;

import java.util.ArrayList;

import android.annotation.SuppressLint;
import android.content.Context;
import android.graphics.Canvas;
import android.graphics.Color;
import android.graphics.Paint;
import android.graphics.Paint.FontMetrics;
import android.view.SurfaceHolder;
import android.view.SurfaceView;

/**
 * A subclass of SurfaceView which acts as a simple text container (like a stripped-down TextView) but automatically scales its text to ensure that it will
 * always fit in the View's bounds. If a FontSizeCoordinator object is used then this property will be preserved, but any other FontFitTextViews which share a 
 * Coordinator will take the largest possible size that makes the text fit in all of them.
 * <br>
 * NOTE: Since this class is replacing a TextView without recreating much of its functionality it may be a source of bugs (e.g. if using unusual scripts). Features
 * such as text gravity and alignment are not supported beyond the default (vertical and horizontal centering). 
 * 
 * @author benelliott, mstevens
 *
 */
public class FontFitSurfaceView extends SurfaceView implements SurfaceHolder.Callback
{

	/**
	 * Maximum font size which we will ever use in this view. This does need to be this high because some text gets pretty big.
	 */
	private static final float MAX_TEXT_SIZE = 150.0f;

	/**
	 * Minimum font size which we will ever use in this view.
	 */
	private static final float MIN_TEXT_SIZE = 5.0f;

	/**
	 * How close we have to be to accept the size we have converged on and stop iterating.
	 */
	private static final float THRESHOLD = 0.5f;

	private int textColor = Color.BLACK;
	private FontMetrics fontMetrics = new FontMetrics();
	private Paint paint;
	private TextSizeCoordinator coordinator;
	private int coordinatorSlot;
	private int lastTargetWidth = -1;
	private int lastTargetHeight = -1;
	private float textSizePx;
	private String[] textLines; // an array where each element represents a line of text
	private float[] leftOffsets; // array of measurements of the half-width of each line of text, to be used when centering them horizontally

	public FontFitSurfaceView(Context context)
	{
		this(context, null, -1);
	}

	public FontFitSurfaceView(Context context, TextSizeCoordinator coordinator)
	{
		this(context, coordinator, -1);
	}

	public FontFitSurfaceView(Context context, TextSizeCoordinator coordinator, int coordinatorSlot)
	{
		super(context);

		this.coordinator = coordinator;

		if(coordinator != null)
			this.coordinatorSlot = coordinatorSlot >= 0 ? coordinatorSlot : coordinator.claimSlot(this);

		this.coordinatorSlot = coordinatorSlot;

		paint = new Paint();
		paint.setAntiAlias(true); // text looks pixellated otherwise
		paint.setColor(textColor);

		getHolder().addCallback(this);
	}

	public void setText(String text)
	{
		String normalisedLineEndings = text.replace("\r", ""); // remove "CR" from line endings, leaving "LF"
		String[] lines = normalisedLineEndings.split("\n"); // split on "LF"
		this.textLines = lines;
	}

	public String[] getTextLines()
	{
		return textLines;
	}

	public void setTextSizePx(float textSizePx)
	{
		this.textSizePx = textSizePx;
		// set text size on paint
		paint.setTextSize(textSizePx);
		// update font metrics (to use when positioning text):
		paint.getFontMetrics(fontMetrics);
		
		// calculate new text centering offsets:
		leftOffsets = new float[textLines.length];

		for(int i = 0; i < textLines.length; i++)
			leftOffsets[i] = paint.measureText(textLines[i]) / 2;
	}

	public float getTextSizePx()
	{
		return this.textSizePx; // do not just use paint.getTextSize because the offsets may not have been calculated (that size could have been set in the iteration)
	}

	public int getTextColor()
	{
		return textColor;
	}

	public void setTextColor(int textColor)
	{
		this.textColor = textColor;
	}


	@Override
	public void onDraw(Canvas canvas)
	{
//		canvas.drawColor(backgroundColor); // wipe canvas (just in case)-- doesn't seem necessary but try this in case of bugs!
		float halfWidth = this.getWidth() / 2;
		float halfHeight = this.getHeight() / 2;
		// assume text size was set in setTextSize
		for(int i = 0; i < textLines.length; i++)
			canvas.drawText(textLines[i],
					// centre text horizontally by starting it at (half container width - half text width):
					halfWidth - leftOffsets[i],
					/*
					 * We want the text to be vertically centred as well as horizontally. Achieve this by setting each line of text's baseline y-coordinate as:
					 * 
					 * half of container height - half of total text height + height of a line of text * the position of this line - the room needed for
					 * descending chars (which go beneath the baseline)
					 * 
					 * = halfHeight - text.length * drawPaint.getFontMetrics(null) / 2 + (i + 1) * drawPaint.getFontMetrics(null) - fontMetrics.descent, or...
					 */
					halfHeight + (i + 1 - ((float) textLines.length) / 2) * paint.getFontMetrics(null) - fontMetrics.descent,
					paint);
	}

	@Override
	public void surfaceCreated(SurfaceHolder holder)
	{
		// when surface is created, do a text measurement
		fitText(this.getWidth(), this.getHeight(), false);
		// then draw the text...
		doDraw(holder);
	}

	@Override
	public void surfaceChanged(SurfaceHolder holder, int format, int width, int height)
	{
		/// when surface is changed, do a text measurement
		fitText(this.getWidth(), this.getHeight(), false);
		// measure text because dimens may have changed
		doDraw(holder);
	}

	@Override
	public void surfaceDestroyed(SurfaceHolder arg0)
	{
		// nothing to do (?)
	}

	@SuppressLint("WrongCall")
	private void doDraw(SurfaceHolder holder)
	{
		Canvas c = null;
		try
		{
			c = holder.lockCanvas(null);
			synchronized(holder)
			{
				this.onDraw(c);
			}
		}
		finally
		{
			if(c != null)
				holder.unlockCanvasAndPost(c);
		}
	}

	/**
	 * Computes the maximum font size which makes the TextView's (possibly multi-line) text fit in the bounding box defined by the viewWidth and viewHeight
	 * parameters, always within the range ({@link #MIN_TEXT_SIZE}, {@link #MAX_TEXT_SIZE}) but taking into account any predetermined font size constraints that
	 * come from the font size coordinator (if present).
	 * 
	 * @param viewWidth the width of the View into which the text will be drawn (including padding)
	 * @param viewHeight the height of the View into which the text will be drawn (including padding)
	 * @param force whether or not to force a recalculation of font size (this could otherwise be avoided
	 */
	private void fitText(int viewWidth, int viewHeight, boolean force)
	{
		// Avoid unnecessary (re)fitting: 0 or negative view dimensions
		if(viewWidth <= 0 || viewHeight <= 0)
			return;

		// Avoid unnecessary (re)fitting: text is empty
		if(textLines.length == 0 || textLines.length == 1 && textLines[0].isEmpty())
			return;

		// Compute the target text width/height as the provided container width minus relevant padding
		int targetWidth = viewWidth - this.getPaddingLeft() - this.getPaddingRight();
		int targetHeight = viewHeight - this.getPaddingBottom() - this.getPaddingTop();

		// Avoid unnecessary (re)fitting: no forcing and target dimensions are the same as last time
		if(!force && targetWidth == lastTargetWidth && targetHeight == lastTargetHeight)
			return;

		// At this point we know a new size computation will take place...

//		Log.d("FFTV", "fitText: IS REFITTING, text: " + this.getTextLines() + ", hash: " + hashCode() + " w=" + viewWidth + ", h=" + viewHeight + ", forced: " + force);

		// Initialise lo & hi bounds:
		float lo = MIN_TEXT_SIZE;
		float hi = (coordinator == null) ? MAX_TEXT_SIZE : coordinator.getCoordinatedTextSize();
		
		// see if we can use the "hi" instead of iterating (if "hi" fits then iterating will only get us back to a value marginally smaller than "hi"):
		float textSize = hi;

		// only iterate if "hi" was too big:
		if(!textFits(textSize, targetWidth, targetHeight))
		{
			// Fitting loop (binary search):
			while((hi - lo) > THRESHOLD)
			{
				textSize = (hi + lo) / 2;

				// see if our text fits
				if(textFits(textSize, targetWidth, targetHeight))
					lo = textSize; // was too small, so increase lo
				else
					hi = textSize; // was too big, so reduce hi
			}

			// We set lo as the new textSize, so that we undershoot rather than overshoot:
			textSize = lo;
		}

//		Log.d("FFTV", "final textSize for slot " + coordinatorSlot + ": " + textSize);

		// Remember target dimensions:
		lastTargetWidth = targetWidth;
		lastTargetHeight = targetHeight;

		if(coordinator != null)
			// Let the coordinator apply the textSize at the appropriate time:
			coordinator.setMaxTextSize(this, textSize);
		else if(this.getTextSizePx() != textSize)
			// Apply now:
			this.setTextSizePx(textSize);
	}

	/**
	 * Computes whether or not the provided (possibly multi-line) text will fit within a bounding box defined by the supplied parameters, using the supplied
	 * text (font) size.
	 * 
	 * @param textSize
	 *            the text/font size to use
	 * @param targetWidth
	 *            the width of the containing box
	 * @param targetHeight
	 *            the height of the containing box
	 * @return whether or not the text will fit into the containing box with the provided text size
	 */
	private boolean textFits(float textSize, float targetWidth, float targetHeight)
	{
		paint.setTextSize(textSize);

		float width = 0;
		for(String line : textLines)
			// measure bounds for each line of text:
			width = Math.max(width, paint.measureText(line)); // max width is max(previous max width, this line width)
		
		// determine height by doing no. lines * interline spacing (= font height + spacing between bottom of one line and top of another)
		// NOTE: do not use Paint#getTextBounds because this ignores any spacing above or below the character, which will be included by the
		// TextView regardless (e.g. the bounds height of "." would be very low as it excludes the space above the character)
		return(width <= targetWidth && textLines.length * paint.getFontMetrics(null) <= targetHeight);

	}

	/**
	 * Helper class to coordinate text sizes across multiple FontFitTextView instances
	 * 
	 * @author mstevens, benelliott
	 */
	static public class TextSizeCoordinator
	{

		private ArrayList<FontFitSurfaceView> views = new ArrayList<FontFitSurfaceView>();
		private ArrayList<Float> intendedTextSizes = new ArrayList<Float>();

		/**
		 * Requests that the coordinator reserve a 'slot' for this view so that it will update its font size at some point.
		 * 
		 * @return the claimed slot
		 */
		public int claimSlot()
		{
			return claimSlot(null);
		}

		/**
		 * @param view
		 * @return the claimed slot
		 */
		public int claimSlot(FontFitSurfaceView view)
		{
			views.add(view);
			intendedTextSizes.add(MAX_TEXT_SIZE);
			// Log.d("FFTV", "Coordinator.claimSlot: number of slots: " + views.size());
			return views.size() - 1;
		}

		/**
		 * Requests that the controller record the provided maximum text size for the provided view, and then coordinates font sizes across all views.
		 * 
		 * @param view
		 *            - the view whose max text size is being recorded
		 * @param maxTextSize
		 *            - the max text size to record
		 */
		public void setMaxTextSize(FontFitSurfaceView view, float maxTextSize)
		{
			// Just in case:
			if(view.coordinatorSlot < 0 || view.coordinatorSlot >= views.size())
				throw new IllegalArgumentException("Invalid or unclaimed coordinator slot (" + view.coordinatorSlot+ "), make sure to claim a slot for each coordinated view.");

			// Register the view:
			views.set(view.coordinatorSlot, view);

			intendedTextSizes.set(view.coordinatorSlot, maxTextSize);
			// always coordinate regardless of whether or not the size has changed because it may now be a new view object
			// (e.g. if restarting a form):

			// TODO wait until all fitted? (causes more "requestLayout() improperly called" warnings in some case)
			coordinate();
		}

		/**
		 * @return the largest font size that appreciates the maximum font size constraints of all views registered to the controller.
		 */
		public float getCoordinatedTextSize()
		{
			float min = MAX_TEXT_SIZE; // keep track of the smallest font size of all of the views we're tracking
			for(int v = 0; v < views.size(); v++)
				if(views.get(v) != null && intendedTextSizes.get(v) < min)
					min = intendedTextSizes.get(v);
			return min;
		}

		/**
		 * Applies the coordinated font size to all registered views.
		 */
		public void coordinate()
		{
			float coordinatedTextSize = getCoordinatedTextSize();
//			Log.d("FFTV", "Applying coordinated text size: " + coordinatedTextSize);
			for(FontFitSurfaceView v : views)
			{
//				if (v!= null) Log.d("FFTV", "Applying to: " + v.getTextLines());
				if(v != null && v.getTextSizePx() != coordinatedTextSize) // TODO use minimum difference instead of !=? E.g. Math.abs(v.getTextSize() - coordinatedTextSize) > 1.0 (or some other value)
					v.setTextSizePx(coordinatedTextSize);
			}
		}

	}

}
