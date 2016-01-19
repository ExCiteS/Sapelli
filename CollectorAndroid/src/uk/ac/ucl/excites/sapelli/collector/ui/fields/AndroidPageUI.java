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

package uk.ac.ucl.excites.sapelli.collector.ui.fields;

import uk.ac.ucl.excites.sapelli.collector.control.AndroidCollectorController;
import uk.ac.ucl.excites.sapelli.collector.model.fields.Page;
import uk.ac.ucl.excites.sapelli.collector.ui.CollectorView;
import uk.ac.ucl.excites.sapelli.shared.util.android.ViewHelpers;
import uk.ac.ucl.excites.sapelli.storage.model.Record;
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
	
	private PageView view;
	private LayoutParams childLayoutParams;
	private int wrapperPaddingPx;
	private LayoutParams wrapperLayoutParams;
	
	public AndroidPageUI(Page page, AndroidCollectorController controller, CollectorView collectorView)
	{
		super(page, controller, collectorView);
		
		childLayoutParams = new LayoutParams((ViewGroup.MarginLayoutParams) CollectorView.FULL_WIDTH_LAYOUTPARAMS);
		childLayoutParams.setMargins(0, 0, 0, collectorView.convertDipToPx(CHILD_BOTTOM_MARGIN_DIP));
		
		wrapperPaddingPx = collectorView.convertDipToPx(WRAPPER_PADDING_DIP);
		wrapperLayoutParams = new LayoutParams((ViewGroup.MarginLayoutParams) CollectorView.FULL_WIDTH_LAYOUTPARAMS);
		wrapperLayoutParams.setMargins(0, 0, 0, 0);
	}
	
	@Override
	protected PageView getPlatformView(boolean onPage, boolean enabled, Record record, boolean newRecord)
	{
		if(onPage)
			throw new IllegalStateException("Pages cannot be nested!");
		
		// Create or recycle view:
		if(view == null)
		{
			view = new PageView(collectorUI.getContext());
			newRecord = true; // force update of new view
		}
		
		// (Re)add updated views for contained fields:
		view.update(record, newRecord);

		// Return the view:
		return view;
	}
	
	@Override
	protected void markValidity(final FieldUI<?, View, CollectorView> fieldUI, final boolean valid)
	{
		if(view == null)
			return; // (this should never happen)
		
		// Update UI (from the main/UI thread):
		collectorUI.activity.runOnUiThread(new Runnable()
		{
			@Override
			public void run()
			{
				view.markValidity(fieldUI, valid);
			}
		});
	}
	
	/**
	 * PageView class
	 * 
	 * @author mstevens
	 */
	private class PageView extends ScrollView
	{
		
		private final LinearLayout container;
		
		/**
		 * @param context
		 */
		public PageView(Context context)
		{
			super(context);
			
			// Container (LinearLayout with vertical orientation):
			container = new LinearLayout(context);
			container.setOrientation(LinearLayout.VERTICAL);
			addView(container);
		}
		
		public void update(Record record, boolean newRecord)
		{
			int fIndex = 0;
			for(FieldUI<?, View, CollectorView> fUI : fieldUIs)
			{
				LinearLayout currentWrappedView = (LinearLayout) container.getChildAt(fIndex); // may be null
				View newView = fUI.showField(); // the actual view object returned may be recycled but its state will be updated to reflect current record
				
				// Replace current (wrapped) view:
				if(newView != unwrapView(currentWrappedView)) // Note: unwrapView(null) will return null
				{	// not the same view so remove current:
					if(currentWrappedView != null)
						container.removeViewAt(fIndex); // remove wrapped view
					// Current becomes wrapped newView:
					currentWrappedView = wrapView(newView); // Note: wrapView(null) will return null
					// Add if not null:
					if(currentWrappedView != null) // just in case
						// Add to ourselves:
						container.addView(currentWrappedView, fIndex, childLayoutParams);
				}
				
				if(currentWrappedView == null) // just in case
					continue;
				
				// Deal with showOnCreate/Edit:
				if(!controller.isFieldToBeShown(fUI.field))
				{	// Note: even disabled fieldUIs are added to the container (but then hidden), this is to ensure that fieldUIs.indexOf(fieldUI) can always be used as a child index within the container
					currentWrappedView.setVisibility(View.GONE);
					controller.addLogLine("HIDING", fUI.field.id);
				}
				else
					currentWrappedView.setVisibility(View.VISIBLE);
				// Note: non-editable fields are shown in edit mode but will be be disabled (grayed-out)
				
				// Mark as valid (initial status):
				if(newRecord)
					markValidity(currentWrappedView, true);
				
				fIndex++;
			}
		}
		
		public void markValidity(FieldUI<?, View, CollectorView> fieldUI, boolean valid)
		{
			markValidity((LinearLayout) container.getChildAt(fieldUIs.indexOf(fieldUI)), valid);
		}
		
		private void markValidity(LinearLayout wrappedView, boolean valid)
		{
			if(wrappedView == null)
				return; // this may happen when isValid() or clearInvalidity() are called too early
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
		
		private LinearLayout wrapView(View fieldUIView)
		{
			if(fieldUIView == null)
				return null;
			LinearLayout wrapper = new LinearLayout(getContext());
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

//		/**
//		 * In principle something like this should be done, but it is currently commented out because
//		 * it causes problems in practise. Some of the field UIs may need to be made more robust in terms
//		 * of event handling and enabling/disabling logic, and then we can try this again.  
//		 * 
//		 * @see android.view.View#setEnabled(boolean)
//		 */
//		@Override
//		public void setEnabled(boolean enabled)
//		{
//			super.setEnabled(enabled);
//			container.setEnabled(enabled);
//			for(int c = 0; c < container.getChildCount(); c++)
//				unwrapView((LinearLayout) container.getChildAt(c)).setEnabled(enabled);
//		}
		
	}

}
