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
public class FontFitView extends View
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
	 * TODO 
	 */
	private Layout.Alignment alignment = Layout.Alignment.ALIGN_CENTER;
	
	/**
	 * Amount by which the text height is multiplied to get line height
	 */
	private float spacingMult = 1.0f;
	
	/**
	 * Amount of leading to add to each line
	 */
	private float spacingAdd = 0.0f;
	
	/**
	 * "includePad" needed for StaticLayout constructor but completely undocumented in Android :) Think it decides whether or not to include padding in metrics
	 * (a bit like setIncludeFontPadding in TextView) so we want it to be TRUE to ensure there is enough room for accented characters:
	 */
	private boolean includePad = true;
	
	private String text = "";
	private float textSizePx = MAX_TEXT_SIZE;
	
	private TextSizeCoordinator coordinator;
	private int coordinatorSlot;
	private TextPaint paint;
	private int lastTargetWidth = -1;
	private int lastTargetHeight = -1;
	private StaticLayout layout;
	
	/**
	 * @param context
	 */
	public FontFitView(Context context)
	{
		this(context, null, -1);
	}

	/**
	 * @param context
	 * @param coordinator
	 */
	public FontFitView(Context context, TextSizeCoordinator coordinator)
	{
		this(context, coordinator, -1);
	}

	/**
	 * @param context
	 * @param coordinator
	 * @param coordinatorSlot
	 */
	public FontFitView(Context context, TextSizeCoordinator coordinator, int coordinatorSlot)
	{
		super(context);
		
		this.coordinator = coordinator;
		// Set or claim coordinator slot:
		if(coordinator != null)
			this.coordinatorSlot = coordinatorSlot >= 0 ? coordinatorSlot : coordinator.claimSlot(this);
		
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
		boolean different = this.textSizePx == textSizePx; // TODO use minimum difference instead of ==? E.g. Math.abs(v.getTextSize() - coordinatedTextSize) > 1.0 (or some other value)
		
		// update text size field (checked elsewhere)
		this.textSizePx = textSizePx;
		
		if(applyNow && different)
		{
			// set text size on paint:
			paint.setTextSize(textSizePx);
			
			updateLayout();
		}
	}
	
	/**
	 * Update layout instance & invalidate view
	 */
	private void updateLayout()
	{
		// New layout instance:
		layout = new StaticLayout(text, paint, (int) lastTargetWidth, alignment, spacingMult, spacingAdd, includePad);
		
		// TODO shouldn't we use a DynamicLayout instead? As it is clearly changing...
		
		// TODO not sure it's save to use lastTargetWidth here because it is initialised to -1 ...
		//	perhaps it's better to do 
		// TODO maybe it's better to round width nearest whole px? instead of (int) which floors
		
		// Ensure that the view will be redrawn:
		this.invalidate();
	}

	@Override
	public void onDraw(Canvas canvas)
	{
		// canvas.drawColor(backgroundColor); // wipe canvas (just in case)-- doesn't seem necessary but try this in case of bugs!
		
		// calculate new height offset for vertical text centering -- want to start drawing text at height (container height - text height) / 2 :
		float heightOffset = 0.5f * (this.getHeight() - layout.getLineBottom(layout.getLineCount() - 1));
		
		// shift canvas upwards so that the text will be vertically centred:
		canvas.translate(0, heightOffset);
		
		// draw text:
		layout.draw(canvas);
		
		// undo translate:
		canvas.restore();
	}
	
	@Override
	public void onLayout(boolean changed, int left, int top, int right, int bottom)
	{
		fitText(this.getWidth(), this.getHeight(), false); // TODO use changed instead of false?		
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

		// Log.d("FFV", "fitText: IS REFITTING, text: " + this.getText() + ", hash: " + hashCode() + " w=" + viewWidth + ", h=" + viewHeight + ", forced: " + force);

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

		// Log.d("FFV", "final textSize for slot " + coordinatorSlot + ": " + textSize);

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
	 * Computes whether or not the provided (possibly multi-line) text will fit within a
	 * bounding box defined by the supplied parameters, using the supplied text (font) size.
	 * 
	 * TODO explain why we don't do any explicit width checks
	 * 
	 * @param textSize - the text/font size to use
	 * @param targetWidth - the width of the containing box
	 * @param targetHeight - the height of the containing box
	 * @return whether or not the text will fit into the containing box with the provided text size
	 */
	private boolean textFits(float textSize, float targetWidth, float targetHeight)
	{
		paint.setTextSize(textSize);

		layout = new StaticLayout(text, paint, (int) targetWidth, alignment, spacingMult, spacingAdd, includePad);
		// Log.d("FFV", "text: "+text+" text height: "+layout.getHeight()+" target: "+targetHeight+" fits: "+(layout.getWidth() <= targetWidth && layout.getHeight() <= targetHeight));
		return layout.getHeight() <= targetHeight; // only check on height as width is already set
		
		// TODO maybe leave some commented out code for the other ways of measuring height/width of text?
	}
	
	/**
	 * TODO document (and possibly rename) this or remove the setter if it doesn't make sense for client code to ever call this
	 * 
	 * @param spacingMult
	 */
	public void setSpacingMultiplier(float spacingMult)
	{
		if(this.spacingMult != spacingMult)
		{	
			this.spacingMult = spacingMult;
			updateLayout();
		}
	}
	
	public float getSpacingMult()
	{
		return spacingMult;
	}
	
	/**
	 * TODO document (and possibly rename) this or remove the setter if it doesn't make sense for client code to ever call this
	 * 
	 * @param spacingAdd
	 */
	public void setSpacingAdd(float spacingAdd)
	{
		if(this.spacingAdd != spacingAdd)
		{
			this.spacingAdd = spacingAdd;
			updateLayout();
		}
	}
	
	public float getSpacingAdd()
	{
		return spacingAdd;
	}

	/**
	 * TODO document (and possibly rename) this or remove the setter if it doesn't make sense for client code to ever call this
	 * 
	 * @param includePad
	 */
	public void setIncludePad(boolean includePad)
	{
		if(this.includePad != includePad)
		{
			this.includePad = includePad;
			updateLayout();
		}
	}

	public boolean isIncludePad()
	{
		return includePad;
	}
	
	/**
	 * TODO document this
	 * 
	 * @param alignment
	 */
	public void setHorizontalAlignment(Layout.Alignment alignment)
	{
		if(this.alignment != alignment)
		{
			this.alignment = alignment;
			updateLayout();
		}
	}

	public Layout.Alignment getHorizontalAlignment()
	{
		return alignment;
	}

	public void setText(String text)
	{
		if(this.text != null ? !this.text.equals(text) : text != null)
		{
			this.text = text;
			fitText(this.getWidth(), this.getHeight(), true);
		}
	}

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
		if(paint.getColor() != textColor)
		{
			paint.setColor(textColor);
			updateLayout();
		}
	}

	/**
	 * Helper class to coordinate text sizes across multiple FontFitView instances
	 * 
	 * @author mstevens, benelliott
	 */
	static public class TextSizeCoordinator
	{

		private ArrayList<FontFitView> views = new ArrayList<FontFitView>();

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
			//Log.d("FFV", "Coordinator.claimSlot: number of slots: " + views.size());
			return views.size() - 1;
		}

		/**
		 * Requests that the controller record the provided maximum text size for the provided view, and then coordinates font sizes across all views.
		 * 
		 * @param view - the view whose max text size is being recorded
		 */
		public void updated(FontFitView view)
		{
			// Just in case:
			if(view.coordinatorSlot < 0 || view.coordinatorSlot >= views.size())
				throw new IllegalArgumentException("Invalid or unclaimed coordinator slot (" + view.coordinatorSlot+ "), make sure to claim a slot for each coordinated view.");

			// Register the view:
			views.set(view.coordinatorSlot, view);
			
			// always coordinate regardless of whether or not the size has changed because it may now be a new view object (e.g. if restarting a form):
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
				if(views.get(v) != null && views.get(v).textSizePx < min)
					min = views.get(v).textSizePx;
			return min;
		}

		/**
		 * Applies the coordinated font size to all registered views.
		 */
		public void coordinate()
		{
			float coordinatedTextSize = getCoordinatedTextSize();
			// Log.d("FFV", "Applying coordinated text size: " + coordinatedTextSize);
			for(FontFitView v : views)
				if(v != null)
				{
					//Log.d("FFV", "Applying to: " + v.text);
					v.setTextSizePx(coordinatedTextSize, true); // won't redraw if size is same as current one
				}
		}

	}

}
