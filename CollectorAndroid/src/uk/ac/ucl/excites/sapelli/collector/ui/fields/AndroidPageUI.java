package uk.ac.ucl.excites.sapelli.collector.ui.fields;

import uk.ac.ucl.excites.sapelli.collector.control.CollectorController;
import uk.ac.ucl.excites.sapelli.collector.control.Controller.FormSession.Mode;
import uk.ac.ucl.excites.sapelli.collector.model.CollectorRecord;
import uk.ac.ucl.excites.sapelli.collector.model.fields.Page;
import uk.ac.ucl.excites.sapelli.collector.ui.CollectorView;
import uk.ac.ucl.excites.sapelli.collector.ui.FieldUI;
import uk.ac.ucl.excites.sapelli.collector.util.ViewHelpers;
import android.annotation.SuppressLint;
import android.content.Context;
import android.graphics.Color;
import android.graphics.Paint;
import android.graphics.Paint.Style;
import android.graphics.drawable.Drawable;
import android.graphics.drawable.ShapeDrawable;
import android.graphics.drawable.shapes.RectShape;
import android.view.View;
import android.view.ViewGroup;
import android.widget.LinearLayout;
import android.widget.LinearLayout.LayoutParams;
import android.widget.ScrollView;

/**
 * @author mstevens, Michalis Vitos
 * 
 */
@SuppressLint("ViewConstructor")
public class AndroidPageUI extends PageUI<View, CollectorView>
{

	static public final float CHILD_BOTTOM_MARGIN_DIP = 5.0f;
	static private final float WRAPPER_PADDING_DIP = 2.0f;
	
	private ScrollView view;
	private LayoutParams childLayoutParams;
	private int wrapperPaddingPx;
	private LayoutParams wrapperLayoutParams;
	
	public AndroidPageUI(Page page, CollectorController controller, CollectorView collectorView)
	{
		super(page, controller, collectorView);
		
		childLayoutParams = new LayoutParams((ViewGroup.MarginLayoutParams) CollectorView.FULL_WIDTH_LAYOUTPARAMS);
		childLayoutParams.setMargins(0, 0, 0, collectorView.convertDipToPx(CHILD_BOTTOM_MARGIN_DIP));
		
		wrapperPaddingPx = collectorView.convertDipToPx(WRAPPER_PADDING_DIP);
		wrapperLayoutParams = new LayoutParams((ViewGroup.MarginLayoutParams) CollectorView.FULL_WIDTH_LAYOUTPARAMS);
		wrapperLayoutParams.setMargins(0, 0, 0, 0);
	}
	
	@Override
	protected void markValidity(FieldUI<?, View, CollectorView> fieldUI, boolean valid)
	{
		if(view == null)
			return; // (this should never happen)
		markValidity((LinearLayout) ((LinearLayout) view.getChildAt(0)).getChildAt(fieldUIs.indexOf(fieldUI)), valid);
	}
	
	private void markValidity(LinearLayout wrappedView, boolean valid)
	{
		Drawable background = null;
		if(!valid)
		{	// Draw red border:
			background = new ShapeDrawable(new RectShape());
			Paint paint = ((ShapeDrawable) background).getPaint();
			paint.setColor(Color.RED);
			paint.setStyle(Style.STROKE);
			paint.setStrokeWidth(5.0f);
		}
		ViewHelpers.setViewBackground(wrappedView, background); // passing null will remove the background
	}
	
	@Override
	public ScrollView getPlatformView(boolean onPage, CollectorRecord record, boolean newRecord)
	{
		if(onPage)
			throw new IllegalStateException("Pages cannot be nested!");
		
		Context context = collectorUI.getContext();
		
		// Create or recycle view: 
		LinearLayout container = null;
		if(view == null)
		{
			// Scroll view:
			view = new ScrollView(context);
			
			// Container (LinearLayout with vertical orientation):
			container = new LinearLayout(context);
			container.setOrientation(LinearLayout.VERTICAL);
			view.addView(container);
		}
		else
			container = (LinearLayout) view.getChildAt(0);
		
		// (Re)add updated views for contained fields:
		int fIndex = 0;
		for(FieldUI<?, View, CollectorView> fUI : fieldUIs)
		{
			LinearLayout currentWrappedView = (LinearLayout) container.getChildAt(fIndex); // may be null
			View newView = fUI.getPlatformView(true, record); // the actual view object returned may be recycled but its state will be updated to reflect current record
			
			// Replace current (wrapped) view:
			if(newView != unwrapView(currentWrappedView)) // unwrapView(null) will return null
			{	// not the same view so remove current:
				if(currentWrappedView != null)
					container.removeViewAt(fIndex); // removes wrapped view
				// Current becomes wrapped newView:
				currentWrappedView = wrapView(newView); // wrapView(null) will return null
				// Add if not null:
				if(currentWrappedView != null) // just in case
					// Add to ourselves:
					container.addView(currentWrappedView, fIndex, childLayoutParams);
			}
			
			if(currentWrappedView == null) // just in case
				continue;
			
			// Deal with showOnCreate/Edit:
			if(	(controller.getCurrentFormMode() == Mode.CREATE && !fUI.getField().isShowOnCreate()) ||
				(controller.getCurrentFormMode() == Mode.EDIT && !fUI.getField().isShowOnEdit()))
			{
				currentWrappedView.setVisibility(View.GONE);
				controller.addLogLine("HIDING", fUI.getField().getID());
			}
			else
				currentWrappedView.setVisibility(View.VISIBLE);
			
			// Mark as valid (initial status):
			if(newRecord)
				markValidity(currentWrappedView, true);
			
			fIndex++;
		}
		
		return view;
	}
	
	private LinearLayout wrapView(View fieldUIView)
	{
		if(fieldUIView == null)
			return null;
		final LinearLayout wrapper = new LinearLayout(collectorUI.getContext());
		wrapper.setPadding(wrapperPaddingPx, wrapperPaddingPx, wrapperPaddingPx, wrapperPaddingPx);
		wrapper.setLayoutParams(wrapperLayoutParams);
		wrapper.addView(fieldUIView);
		return wrapper;
	}
	
	private View unwrapView(LinearLayout wrapper)
	{
		if(wrapper == null)
			return null;
		return wrapper.getChildAt(0);
	}

}
