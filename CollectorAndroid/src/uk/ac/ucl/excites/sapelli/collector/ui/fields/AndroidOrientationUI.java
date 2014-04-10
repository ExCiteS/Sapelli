package uk.ac.ucl.excites.sapelli.collector.ui.fields;

import uk.ac.ucl.excites.sapelli.collector.control.Controller;
import uk.ac.ucl.excites.sapelli.collector.model.fields.OrientationField;
import uk.ac.ucl.excites.sapelli.collector.ui.CollectorView;
import uk.ac.ucl.excites.sapelli.storage.model.Record;
import android.view.View;
import android.view.View.OnClickListener;
import android.widget.Button;

/**
 * @author Julia, mstevens
 *
 */
public class AndroidOrientationUI extends OrientationUI<View, CollectorView>
{

	private Button pageView;
	
	public AndroidOrientationUI(OrientationField field, Controller controller, CollectorView collectorUI)
	{
		super(field, controller, collectorUI);
	}
	
	@Override
	protected View getPlatformView(boolean onPage, Record record, boolean newRecord)
	{
		//TODO editable
		if(onPage)
		{
			if(pageView == null)
			{
				pageView = new Button(collectorUI.getContext());
				pageView.setText(field.getCaption());
				// TODO some kind of icon/image would be nice (an arrow?)
				pageView.setOnClickListener(new OnClickListener()
				{
					@Override
					public void onClick(View v)
					{
						controller.goTo(field, true); // go to field and leave page without validation
					}
				});
			}
			return pageView;
		}
		else
		{
			return null; // for now OrientationFields don't have a UI
		}
		
	}

}
