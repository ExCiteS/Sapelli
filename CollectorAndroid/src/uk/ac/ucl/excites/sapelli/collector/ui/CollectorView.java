package uk.ac.ucl.excites.sapelli.collector.ui;

import java.util.HashMap;

import uk.ac.ucl.excites.sapelli.collector.activities.CollectorActivity;
import uk.ac.ucl.excites.sapelli.collector.control.ProjectController;
import uk.ac.ucl.excites.sapelli.collector.project.model.fields.AudioField;
import uk.ac.ucl.excites.sapelli.collector.project.model.fields.ButtonField;
import uk.ac.ucl.excites.sapelli.collector.project.model.fields.CheckBoxField;
import uk.ac.ucl.excites.sapelli.collector.project.model.fields.ChoiceField;
import uk.ac.ucl.excites.sapelli.collector.project.model.fields.EditTextField;
import uk.ac.ucl.excites.sapelli.collector.project.model.fields.Field;
import uk.ac.ucl.excites.sapelli.collector.project.model.fields.LabelField;
import uk.ac.ucl.excites.sapelli.collector.project.model.fields.LocationField;
import uk.ac.ucl.excites.sapelli.collector.project.model.fields.Page;
import uk.ac.ucl.excites.sapelli.collector.project.model.fields.PhotoField;
import uk.ac.ucl.excites.sapelli.collector.project.model.fields.lists.MultiListField;
import uk.ac.ucl.excites.sapelli.collector.project.ui.CollectorUI;
import uk.ac.ucl.excites.sapelli.collector.project.ui.FieldUI;
import uk.ac.ucl.excites.sapelli.collector.ui.fieldviews.AudioView;
import uk.ac.ucl.excites.sapelli.collector.ui.fieldviews.ButtonView;
import uk.ac.ucl.excites.sapelli.collector.ui.fieldviews.CameraView;
import uk.ac.ucl.excites.sapelli.collector.ui.fieldviews.CheckBoxView;
import uk.ac.ucl.excites.sapelli.collector.ui.fieldviews.ChoiceView;
import uk.ac.ucl.excites.sapelli.collector.ui.fieldviews.EditTextView;
import uk.ac.ucl.excites.sapelli.collector.ui.fieldviews.LabelView;
import uk.ac.ucl.excites.sapelli.collector.ui.fieldviews.MultiListView;
import uk.ac.ucl.excites.sapelli.collector.ui.fieldviews.PageView;
import uk.ac.ucl.excites.sapelli.collector.ui.fieldviews.WaitingView;
import uk.ac.ucl.excites.sapelli.collector.util.ScreenMetrics;
import android.annotation.SuppressLint;
import android.graphics.Color;
import android.view.View;
import android.view.WindowManager;
import android.widget.LinearLayout;

/**
 * The GUI of the CollectorActivity
 * 
 * @author mstevens
 *
 */
@SuppressLint("ViewConstructor")
public class CollectorView extends LinearLayout implements CollectorUI
{
	
	static private final int BUTTONS_VIEW_ID = 0;
	static private final int FIELD_VIEW_ID = 1;

	// Spacing (in dip) between UI elements:
	static public final float SPACING_DIP = 8.0f;
	
	private CollectorActivity activity;
	private ProjectController controller;
	
	// UI elements:
	private ControlsView controlsView;
	private FieldUI fieldUI;
	private HashMap<Field, FieldUI> viewCache;
	
	public CollectorView(CollectorActivity activity)
	{
		super(activity);
		this.activity = activity;
		this.viewCache = new HashMap<Field, FieldUI>();
		
		// Root layout (= this):
		this.setOrientation(LinearLayout.VERTICAL);
		this.setLayoutParams(new LayoutParams(LayoutParams.MATCH_PARENT, LayoutParams.MATCH_PARENT));
		this.setBackgroundColor(Color.BLACK);
		
		// Set-up controlsView:
		controlsView = new ControlsView(activity, this);
		controlsView.setId(BUTTONS_VIEW_ID);
		this.addView(controlsView, new LayoutParams(LayoutParams.MATCH_PARENT, LayoutParams.WRAP_CONTENT));
	}

	/**
	 * Set the field view and removes any previous one from the screen, this is called from the ProjectController (but only for fields that have a UI representation)
	 * 
	 * @param field
	 */
	public void setField(Field field)
	{		
		// avoid layout shift (known Android bug when using full screen)
		activity.getWindow().setFlags(WindowManager.LayoutParams.FLAG_FULLSCREEN, WindowManager.LayoutParams.FLAG_FULLSCREEN);
		
		// Briefly disable the buttons:
		controlsView.disable();
		
		// Update buttons
		controlsView.update(controller);
		
		// Remove current view if there is one and it does not represent the same field:
		if(fieldUI != null && fieldUI.getField() != field)
		{
			fieldUI.cancel(); // to stop audio recording, close camera, ...
			this.removeView((View) fieldUI); // throw away the old fieldField
			fieldUI = null;
		}
		
		// Recycle cached view or create new one
		if(fieldUI == null)
		{
			FieldUI cachedView = viewCache.get(field);
			if(cachedView != null)
				this.fieldUI = cachedView; // Reuse cached view instance if possible:
			else
			{
				this.fieldUI = field.createUI(this);
				viewCache.put(field, fieldUI); // cache the view for later reuse
				((View) fieldUI).setId(FIELD_VIEW_ID);
			}
			// Add the view:
			this.addView((View) fieldUI, new LayoutParams(LayoutParams.MATCH_PARENT, LayoutParams.MATCH_PARENT));
		}

		// Update & (re-)enable the view (even if new!):
		fieldUI.update(controller.getCurrentRecord());
		((View) fieldUI).setEnabled(true);
			
		// Re-enable the buttons
		controlsView.enable();
	}

	/**
	 * @param controller the controller to set
	 */
	public void setController(ProjectController controller)
	{
		this.controller = controller;
	}

	@Override
	public FieldUI createChoiceUI(ChoiceField cf)
	{
		return new ChoiceView(activity, this, controller, cf);
	}

	@Override
	public FieldUI createPhotoUI(PhotoField pf)
	{
		return new CameraView(activity, this, controller, pf);
	}

	@Override
	public FieldUI createAudioUI(AudioField af)
	{
		return new AudioView(activity, this, controller, af);
	}
	
	@Override
	public FieldUI createLocationUI(LocationField lf)
	{
		return new WaitingView(activity, controller, lf);
	}

	@Override
	public FieldUI createLabelUI(LabelField lf)
	{
		return new LabelView(activity, lf);
	}
	
	@Override
	public FieldUI createButtonUI(ButtonField bf)
	{
		return new ButtonView(activity, bf);
	}
	
	@Override
	public FieldUI createTextFieldUI(EditTextField tf)
	{
		return new EditTextView(activity, controller, tf);
	}
	
	@Override
	public FieldUI createCheckBoxFieldUI(CheckBoxField cbf)
	{
		return new CheckBoxView(activity, controller, cbf);
	}
	
	@Override
	public FieldUI createMultiListUI(MultiListField mlf)
	{
		return new MultiListView(activity, mlf);
	}

	@Override
	public FieldUI createButtonFieldUI(ButtonField bf)
	{
		return new ButtonView(activity, bf);
	}
	
	@Override
	public FieldUI createPageUI(Page page)
	{
		return new PageView(activity, this, page);
	}
	
	public void cancelCurrentField()
	{
		if(fieldUI != null)
			fieldUI.cancel();
	}
	
	/**
	 * Removes the view corresponding to the given field from the cache, ensuring a new view will be constructed next time the field is entered
	 * 
	 * @param field
	 */
	public void invalidateView(Field field)
	{
		viewCache.remove(field);
	}
	
	/**
	 * Propagate enable/disable to children
	 * 
	 * @see android.view.View#setEnabled(boolean)
	 */
	@Override
	public void setEnabled(boolean enabled)
	{
		super.setEnabled(enabled);
		controlsView.setEnabled(enabled);
		if(fieldUI != null)
			((View) fieldUI).setEnabled(enabled);
	}
	
	/*
	 * UI element dimensions examples:
	 * 
	 * 	 # Samsung Galaxy Xcover(1):
	 * 
	 * 		Display specs:
	 * 	 	 - Resolution: 320px (w) * 480px (h)
	 *  	 - Size: 3.6"
	 *  	 - Pixel density: 158ppi
	 *  	 - Android-reported display density: 160dpi (scale factor 1) ==> 1dip = 1px
	 *  
	 *  	UI element sizes:
	 *  	 - button height: 60px
	 * 		 - item spacing: 8px
	 * 		 - ChoiceView item padding: 2px
	 * 		 - on a screen with buttons and 2 columns * 3 rows of items:
	 * 		 	- picker item outer area: 156px (w) * 132px (h)
	 * 		 	- picker item inner area (padded all round): 152px (w) * 128px (h)
	 * 		
	 * 		Note:
	 * 			For the AP & OIFLEG projects we used images (for 2 * 3 item screens) of 155 px * 135 px
	 * 			on the Xcover1. While this which is slightly too big, automatic scaling solves the problem
	 * 			without (much) visual quality degradation.
	 * 
	 * 	 # Samsung Galaxy Xcover2:
	 * 
	 * 		Display specs:
	 * 	 	 - Resolution: 480px (w) * 800px (h)
	 *  	 - Size: 4.0"
	 *  	 - Pixel density: 233ppi
	 *  	 - Android-reported display density: 240dpi (scale factor 1.5) ==> 1dip = 1.5px
	 *  
	 *  	UI element sizes:
	 *  	 - button height: 90px
	 * 		 - item spacing: 12px
	 * 		 - ChoiceView item padding: 3px
	 * 		 - on a screen with buttons and 2 columns * 3 rows of items:
	 * 		 	- picker item outer area: 234px (w) * 224px (h)
	 * 		 	- picker item inner area (padded all round): 228px (w) * 218px (h)
	 * 
	 */
	
	public int getSpacingPx()
	{
		return ScreenMetrics.ConvertDipToPx(activity, SPACING_DIP);
	}
	
	public int getIconWidthPx(int numCols)
	{
		int widthPx = (ScreenMetrics.GetScreenWidth(activity) - ((numCols - 1) * getSpacingPx())) / numCols;
		return Math.max(widthPx, 0); //avoid negative pixel counts
	}
	
	public int getIconHeightPx(int numRows, boolean buttonsShowing)
	{
		int heightPx = (ScreenMetrics.GetScreenHeight(activity) - (buttonsShowing ? (controlsView.getButtonHeightPx() + getSpacingPx()) : 0) - ((numRows - 1) * getSpacingPx())) / numRows;
		return Math.max(heightPx, 0); //avoid negative pixel counts
	}

}
