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
 */
public class TextFitView extends View
{

	// STATICS ------------------------------------------------------
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
	
	/**
	 * The default text color is black
	 */
	public static final int DEFAULT_TEXT_COLOR = Color.BLACK; 

	// DYNAMICS ------------------------------------------------------
	/**
	 *  Horizontal text alignment setting -- default is centred.
	 */
	private Layout.Alignment horizontalAlignment = Layout.Alignment.ALIGN_CENTER;
	
	/**
	 * Amount by which the text height is multiplied to get line height
	 */
	private float spacingMult = 1.0f;
	
	/**
	 * Amount of leading to add to each line
	 */
	private float spacingAdd = 0.0f;
	
	/**
	 * "includePad" needed for StaticLayout constructor but completely undocumented in Android :) Experiments have suggested that the value of this variable does not affect
	 *  the amount of spacing between lines but does affect the amount of padding at the top and bottom of an entire piece of text.
	 */
	private boolean includePad = true;
	
	private final TextSizeCoordinator coordinator;
	private final int coordinatorSlot;
	private final TextPaint paint;
	
	private String text = "";
	private String[] textLines = new String[]{ text };
	private float textSizePx = MAX_TEXT_SIZE;
	
	private int lastTargetWidth = -1;
	private int lastTargetHeight = -1;
	private StaticLayout layout;
	
	/**
	 * @param context
	 */
	public TextFitView(Context context)
	{
		this(context, null, -1);
	}

	/**
	 * @param context
	 * @param coordinator
	 */
	public TextFitView(Context context, TextSizeCoordinator coordinator)
	{
		this(context, coordinator, -1);
	}

	/**
	 * @param context
	 * @param coordinator
	 * @param coordinatorSlot
	 */
	public TextFitView(Context context, TextSizeCoordinator coordinator, int coordinatorSlot)
	{
		super(context);
		
		this.coordinator = coordinator;
		// Set or claim coordinator slot:
		this.coordinatorSlot = coordinator != null ? (coordinatorSlot >= 0 ? coordinatorSlot : coordinator.claimSlot(this)) : -1;
		
		// Initialise paint:
		paint = new TextPaint();
		paint.setAntiAlias(true); // text looks pixellated otherwise
		paint.setColor(DEFAULT_TEXT_COLOR);
		paint.setTextSize(textSizePx);
	}

	/**
	 * @param textSizePx
	 * @param applyNow
	 */
	private void setTextSizePx(float textSizePx, boolean applyNow)
	{
		// update text size field (checked elsewhere)
		this.textSizePx = textSizePx;
		
		if(applyNow && paint.getTextSize() != textSizePx)
		{
			paint.setTextSize(textSizePx);
			updateLayout();
		}
	}
	
	/**
	 * Update layout instance & invalidate view
	 */
	private void updateLayout()
	{
		// New layout instance (DynamicLayout is intended for text that changes frequently -- e.g. in an EditText, constantly updating as the user types)
		layout = new StaticLayout(text, paint, this.getWidth() - this.getPaddingLeft() - this.getPaddingRight(), horizontalAlignment, spacingMult, spacingAdd, includePad);
		
		// Ensure that the view will be redrawn:
		this.invalidate();
	}

	@Override
	public void onDraw(Canvas canvas)
	{
		//Log.d("TFV", "Font metrics float: "+paint.getFontMetrics(null) * textLines.length +" font metrics int: "+paint.getFontMetricsInt(null)*textLines.length+" layout height: "+layout.getHeight()+" layout line bottom: "+layout.getLineBottom(layout.getLineCount() - 1)+" pad top: "+layout.getTopPadding()+" pad bottom: "+layout.getBottomPadding()+" attempt: "+(paint.getFontMetricsInt(null)*textLines.length - layout.getTopPadding() + layout.getBottomPadding()));
		
		//canvas.drawColor(backgroundColor); // wipe canvas (just in case)-- doesn't seem necessary but try this in case of bugs!
		
		// Just in case (do not remove!):
		if(layout == null)
			updateLayout();
		
		// Calculate new height offset for vertical text centering -- want to start drawing text at
		// (container height - text height) /2 = (container height - (text box height - (negative padding)) / 2 = (container height - text box height + negative padding) / 2
		float heightOffset = 0.5f * (this.getHeight() - layout.getHeight() + layout.getTopPadding() /* this is negative! */);
		
		// Shift canvas upwards so that the text will be vertically centred:
		canvas.translate(0, heightOffset);
		
		// Draw text:
		layout.draw(canvas);
		
		// Undo translate:
		canvas.restore();
	}
	
	@Override
	public void onLayout(boolean changed, int left, int top, int right, int bottom)
	{
		fitText(this.getWidth(), this.getHeight(), changed); // only force text size to be recalculated if Android tells us the layout has changed (but that should cause it to recalculate anyway)		
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

		//Log.d("TFV", "fitText: IS REFITTING, text: " + this.getText() + ", hash: " + hashCode() + " w=" + viewWidth + ", h=" + viewHeight + ", forced: " + force);

		// Initialise lo & hi bounds:
		float lo = MIN_TEXT_SIZE;
		float hi = (coordinator == null) ? MAX_TEXT_SIZE : coordinator.getCoordinatedTextSize();
		
		// See if we can use the "hi" instead of iterating (if "hi" fits then iterating will only get us back to a value marginally smaller than "hi"):
		float textSize = hi;

		// Only iterate if "hi" was too big:
		if(!textFits(textSize, targetWidth, targetHeight))
		{
			// Fitting loop (binary search):
			while((hi - lo) > THRESHOLD)
			{
				textSize = (hi + lo) / 2;

				// See if our text fits
				if(textFits(textSize, targetWidth, targetHeight))
					lo = textSize; // was too small, so increase lo
				else
					hi = textSize; // was too big, so reduce hi
			}

			// We set lo as the new textSize, so that we undershoot rather than overshoot:
			textSize = lo;
		}

		//Log.d("TFV", "final textSize for slot " + coordinatorSlot + ": " + textSize);

		// Remember target dimensions:
		lastTargetWidth = targetWidth;
		lastTargetHeight = targetHeight;

		// Set new textSize:
		this.setTextSizePx(textSize, coordinator == null); // if there is a coordinate the new size is not applied immediately but at a later time determined by the coordinator

		// Inform coordinator if there is one:
		if(coordinator != null)
			coordinator.updated(this);
	}

	/**
	 * Computes (using "simulated" drawing/measuring) whether or not the (possibly multi-line) text, will fit
	 * within a bounding box defined by the supplied parameters, when rendered at the supplied text (font) size. 
	 * 
	 * @param textSize - the text/font size to use
	 * @param targetWidth - the width of the containing box
	 * @param targetHeight - the height of the containing box
	 * @return whether or not the text will fit into the containing box with the provided text size
	 */
	private boolean textFits(float textSize, float targetWidth, float targetHeight)
	{
		//Log.d("TFV", "text: "+text+" text height: "+layout.getHeight()+" target: "+targetHeight+" fits: "+(layout.getWidth() <= targetWidth && layout.getHeight() <= targetHeight));
		
		// Set the paint's text size to the value used for simulation:
		paint.setTextSize(textSize);

		// Compute maximum width across the lines:
		float width = 0;
		for(String line : textLines)
			// measure bounds for each line of text:
			width = Math.max(width, paint.measureText(line)); // max width is max(previous max width, this line width)
			// determine height by doing no. lines * interline spacing (= font height + spacing between bottom of one line and top of another)
			// NOTE: do not use Paint#getTextBounds because this ignores any spacing above or below the character, which will be included by the
			// TextView regardless (e.g. the bounds height of "." would be very low as it excludes the space above the character)
		
		// Check whether the text fits:
		boolean fits = width <= targetWidth && textLines.length * paint.getFontMetrics(null) <= targetHeight;
		
		// Reset text size on paint to the view's:
		paint.setTextSize(this.textSizePx);
		
		// Return result:
		return fits;
	}

	/**
	 * Sets the horizontal alignment setting for the text in this View.
	 * 
	 * @param alignment
	 * @see <a href="http://developer.android.com/reference/android/text/Layout.Alignment.html">http://developer.android.com/reference/android/text/Layout.Alignment.html</a>
	 */
	public void setHorizontalAlignment(Layout.Alignment alignment)
	{
		if(this.horizontalAlignment != alignment)
		{
			this.horizontalAlignment = alignment;
			updateLayout();
		}
	}

	/**
	 * Get the current horizontal alignment setting for this View.
	 * @return
	 */
	public Layout.Alignment getHorizontalAlignment()
	{
		return horizontalAlignment;
	}

	/**
	 * Set the text to be displayed by the View.
	 * "CRLF" line endings are normalised to "LF".
	 * 
	 * @param text the text to display, will be replaced by "" if null
	 */
	public void setText(String text)
	{
		// Normalise:
		if(text == null)
			// Replace null with empty string:
			text = "";
		else
			// Remove "CR" from line endings, leaving only "LF"
			text = text.replace("\r", "");
		
		// Is this really different from the current text?
		if(!this.text.equals(text))
		{
			this.text = text; // keep the un-split text around so we can paint with a StaticLayout
			this.textLines = text.split("\n"); // split on "LF" to get an array of the lines in the text for multi-line font fitting
			
			// Compute new font size:
			fitText(this.getWidth(), this.getHeight(), true);
		}
	}

	/**
	 * Get the text that is displayed by this View.
	 * @return
	 */
	public String getText()
	{
		return text;
	}

	public int getTextColor()
	{
		return paint.getColor();
	}

	public void setTextColor(int textColor)
	{
		paint.setColor(textColor);
		/* there should be no need to update the StaticLayout here,
		 * as it has a pointer to the same Paint object (and will thus
		 * draw text in the new colour) and a colour change alone won't
		 * affect any dimensions/measurements. */
	}

	/**
	 * Helper class to coordinate text sizes across multiple FontFitView instances
	 * 
	 * @author mstevens, benelliott
	 */
	static public class TextSizeCoordinator
	{

		private ArrayList<TextFitView> views = new ArrayList<TextFitView>();

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
		public int claimSlot(TextFitView view)
		{
			views.add(view);
			//Log.d("TFV", "Coordinator.claimSlot: number of slots: " + views.size());
			return views.size() - 1;
		}

		/**
		 * Requests that the controller record the provided maximum text size for the provided view, and then coordinates font sizes across all views.
		 * 
		 * @param view - the view whose max text size is being recorded
		 */
		public void updated(TextFitView view)
		{
			// Just in case:
			if(view.coordinatorSlot < 0 || view.coordinatorSlot >= views.size())
				throw new IllegalArgumentException("Invalid or unclaimed coordinator slot (" + view.coordinatorSlot+ "), make sure to claim a slot for each coordinated view.");

			// Register the view:
			views.set(view.coordinatorSlot, view);
			
			// always coordinate regardless of whether or not the size has changed because it may now be a new view object (e.g. if restarting a form):
			coordinate();
		}

		/**
		 * @return the largest font size that appreciates the maximum font size constraints of all views registered to the controller.
		 */
		public float getCoordinatedTextSize()
		{
			float min = MAX_TEXT_SIZE; // keep track of the smallest font size of all of the views we're tracking
			for(TextFitView v : views)
				if(v != null && v.textSizePx < min)
					min = v.textSizePx;
			return min;
		}

		/**
		 * Applies the coordinated font size to all registered views.
		 */
		public void coordinate()
		{
			float coordinatedTextSize = getCoordinatedTextSize();
			//Log.d("TFV", "Applying coordinated text size: " + coordinatedTextSize);
			for(TextFitView v : views)
				if(v != null)
				{
					//Log.d("TFV", "Applying to: " + v.text);
					v.setTextSizePx(coordinatedTextSize, true); // won't redraw if size is same as current one
				}
		}

	}

}
