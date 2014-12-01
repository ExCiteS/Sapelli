package uk.ac.ucl.excites.sapelli.collector.ui;

import java.util.ArrayList;

import android.content.Context;
import android.graphics.Canvas;
import android.graphics.Color;
import android.text.Layout;
import android.text.StaticLayout;
import android.text.TextPaint;
import android.view.View;

/**
 * A custom View which acts as a simple text container (like a stripped-down TextView) but automatically scales its text to ensure that it will
 * always fit in the View's bounds. If a FontSizeCoordinator object is used then this property will be preserved, but any other FontFitViews which share a 
 * Coordinator will take the largest possible size that makes the text fit in all of them.
 * <br>
 * NOTE: Since this class is replacing a TextView without recreating much of its functionality it may be a source of bugs (e.g. if using unusual scripts).
 * 
 * @author benelliott, mstevens
 *
 */
public class FontFitView extends View
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

	private String text;
	private TextSizeCoordinator coordinator;
	private int coordinatorSlot;
	private TextPaint paint;
	private int textColor = Color.BLACK;
	private float textSizePx;
	private int lastTargetWidth = -1;
	private int lastTargetHeight = -1;
	private StaticLayout layout;
	private float heightOffset; //offset used to vertically centre text
	// layout properties:
	private Layout.Alignment alignment = Layout.Alignment.ALIGN_CENTER;
	private float spacingMult = 1.0f; // amount by which the text height is multiplied to get line height
	private float spacingAdd = 0.0f; // amount of leading to add to each line
	/*
	 * "includePad" needed for StaticLayout constructor but completely undocumented in Android :) Think it decides whether or not to include padding in metrics
	 * (a bit like setIncludeFontPadding in TextView) so we want it to be TRUE to ensure there is enough room for accented characters:
	 */
	private boolean includePad = true; 
	
	public FontFitView(Context context)
	{
		this(context, null, -1);
	}

	public FontFitView(Context context, TextSizeCoordinator coordinator)
	{
		this(context, coordinator, -1);
	}

	public FontFitView(Context context, TextSizeCoordinator coordinator, int coordinatorSlot)
	{
		super(context);

		this.coordinator = coordinator;

		if(coordinator != null)
			this.coordinatorSlot = coordinatorSlot >= 0 ? coordinatorSlot : coordinator.claimSlot(this);

		this.coordinatorSlot = coordinatorSlot;

		paint = new TextPaint();
		paint.setAntiAlias(true); // text looks pixellated otherwise
		paint.setColor(textColor);

	}

	public void setTextSizePx(float textSizePx)
	{
		// update text size field (checked elsewhere)
		this.textSizePx = textSizePx;
		// set text size on paint
		paint.setTextSize(textSizePx);
		// create a new static layout for this new text size:
		layout = new StaticLayout(text, paint, (int) lastTargetWidth, alignment, spacingMult, spacingAdd, includePad);
		// calculate new height offset for vertical centering -- want to start drawing text at height (container height - text height) / 2 :
		this.heightOffset = 0.5f * (this.getHeight() - layout.getLineBottom(layout.getLineCount() - 1));
		// ensure that this view will be redrawn:
		this.invalidate();
	}

	@Override
	public void onLayout(boolean changed, int left, int top, int right, int bottom)
	{
		fitText(this.getWidth(), this.getHeight(), false);
	}

	@Override
	public void onDraw(Canvas canvas)
	{
		// canvas.drawColor(backgroundColor); // wipe canvas (just in case)-- doesn't seem necessary but try this in case of bugs!
		// shift canvas upwards so that the text will be vertically centred:
		canvas.translate(0, heightOffset);
		// draw text:
		layout.draw(canvas);
		// undo translate:
		canvas.restore();
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
		if(text == null || text.isEmpty())
			return;

		// Compute the target text width/height as the provided container width minus relevant padding
		int targetWidth = viewWidth - this.getPaddingLeft() - this.getPaddingRight();
		int targetHeight = viewHeight - this.getPaddingBottom() - this.getPaddingTop();

		// Avoid unnecessary (re)fitting: no forcing and target dimensions are the same as last time
		if(!force && targetWidth == lastTargetWidth && targetHeight == lastTargetHeight)
			return;

		// At this point we know a new size computation will take place...

		// Log.d("FFTV", "fitText: IS REFITTING, text: " + this.getText() + ", hash: " + hashCode() + " w=" + viewWidth + ", h=" + viewHeight + ", forced: " + force);

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

		// Log.d("FFTV", "final textSize for slot " + coordinatorSlot + ": " + textSize);

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

		layout = new StaticLayout(text, paint, (int) targetWidth, alignment, spacingMult, spacingAdd, includePad);
		// Log.d("FFTV", "text: "+text+" text height: "+layout.getHeight()+" target: "+targetHeight+" fits: "+(layout.getWidth() <= targetWidth && layout.getHeight() <= targetHeight));
		return(layout.getHeight() <= targetHeight); // only check on height as width is already set
	}
	
	public void setSpacingMult(float spacingMult)
	{
		this.spacingMult = spacingMult;
		layout = new StaticLayout(text, paint, (int) lastTargetWidth, this.alignment, this.spacingMult, this.spacingAdd, this.includePad);
		this.invalidate();
	}
	
	public float getSpacingMult()
	{
		return spacingMult;
	}
	
	
	public void setSpacingAdd(float spacingAdd)
	{
		this.spacingAdd = spacingAdd;
		layout = new StaticLayout(text, paint, (int) lastTargetWidth, this.alignment, this.spacingMult, this.spacingAdd, this.includePad);
		this.invalidate();
	}
	
	public float getSpacingAdd()
	{
		return spacingAdd;
	}

	public void setIncludePad(boolean includePad)
	{
		this.includePad = includePad;
		layout = new StaticLayout(text, paint, (int) lastTargetWidth, this.alignment, this.spacingMult, this.spacingAdd, this.includePad);
		this.invalidate();
	}

	public boolean isIncludePad()
	{
		return includePad;
	}
	
	public void setAlignment(Layout.Alignment alignment)
	{
		this.alignment = alignment;
		layout = new StaticLayout(text, paint, (int) lastTargetWidth, this.alignment, this.spacingMult, this.spacingAdd, this.includePad);
		this.invalidate();
	}

	public Layout.Alignment getAlignment()
	{
		return alignment;
	}

	public void setText(String text)
	{
		this.text = text;
	}

	public String getText()
	{
		return text;
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

	/**
	 * Helper class to coordinate text sizes across multiple FontFitView instances
	 * 
	 * @author mstevens, benelliott
	 */
	static public class TextSizeCoordinator
	{

		private ArrayList<FontFitView> views = new ArrayList<FontFitView>();
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
		public int claimSlot(FontFitView view)
		{
			views.add(view);
			intendedTextSizes.add(MAX_TEXT_SIZE);
//			Log.d("FFTV", "Coordinator.claimSlot: number of slots: " + views.size());
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
		public void setMaxTextSize(FontFitView view, float maxTextSize)
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
			// Log.d("FFTV", "Applying coordinated text size: " + coordinatedTextSize);
			for(FontFitView v : views)
			{
				// if (v!= null) Log.d("FFTV", "Applying to: " + v.getTextLines());
				if(v != null && v.getTextSizePx() != coordinatedTextSize) // TODO use minimum difference instead of !=? E.g. Math.abs(v.getTextSize() - coordinatedTextSize) > 1.0 (or some other value)
					v.setTextSizePx(coordinatedTextSize);
			}
		}

	}

}
