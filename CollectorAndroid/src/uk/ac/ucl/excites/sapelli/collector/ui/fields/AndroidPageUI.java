package uk.ac.ucl.excites.sapelli.collector.ui.fields;

import uk.ac.ucl.excites.sapelli.collector.control.CollectorController;
import uk.ac.ucl.excites.sapelli.collector.control.Controller.FormSession.Mode;
import uk.ac.ucl.excites.sapelli.collector.model.CollectorRecord;
import uk.ac.ucl.excites.sapelli.collector.model.fields.Page;
import uk.ac.ucl.excites.sapelli.collector.ui.CollectorView;
import uk.ac.ucl.excites.sapelli.collector.ui.FieldUI;
import uk.ac.ucl.excites.sapelli.collector.ui.fields.PageUI;
import uk.ac.ucl.excites.sapelli.collector.util.ScreenMetrics;
import android.annotation.SuppressLint;
import android.content.Context;
import android.view.View;
import android.widget.LinearLayout;
import android.widget.ScrollView;

/**
 * @author mstevens, Michalis Vitos
 * 
 */
@SuppressLint("ViewConstructor")
public class AndroidPageUI extends PageUI<View>
{

	static public final int CHILD_PADDING = 10;
	
	public AndroidPageUI(Page page, CollectorController controller, CollectorView collectorView)
	{
		super(page, controller, collectorView);
	}
	
	@Override
	public ScrollView getPlatformView(boolean onPage, CollectorRecord record)
	{
		if(onPage)
			throw new IllegalStateException("Pages cannot be nested!");
		
		Context context = ((CollectorView) collectorUI).getContext();
		
		// Note: we never reuse the view and container objects (for now)
		ScrollView view = new ScrollView(context);
		
		// Container (LinearLayout with vertical orientation):
		LinearLayout container = new LinearLayout(context);
		container.setOrientation(LinearLayout.VERTICAL);
		view.addView(container);
		
		int paddingPx = ScreenMetrics.ConvertDipToPx(context, CHILD_PADDING); // create CollectorView/UI method for this
		for(FieldUI<?, View> fUI : fieldUIs)
		{
			if(	(controller.getCurrentFormMode() == Mode.CREATE && !fUI.getField().isShowOnCreate()) ||
				(controller.getCurrentFormMode() == Mode.EDIT && !fUI.getField().isShowOnEdit()))
				controller.addLogLine("HIDING", fUI.getField().getID());
			else
			{
				View child = (View) fUI.getPlatformView(true, record);
				child.setPadding(0, 0, 0, paddingPx);
				container.addView(child);
			}
		}
		
		return view;
	}

}
