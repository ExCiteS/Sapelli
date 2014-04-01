package uk.ac.ucl.excites.sapelli.collector.ui.fields;

import uk.ac.ucl.excites.sapelli.collector.control.Controller;
import uk.ac.ucl.excites.sapelli.collector.model.CollectorRecord;
import uk.ac.ucl.excites.sapelli.collector.model.fields.OrientationField;
import uk.ac.ucl.excites.sapelli.collector.ui.CollectorUI;
import uk.ac.ucl.excites.sapelli.collector.ui.CollectorView;
import android.view.View;
import android.view.View.OnClickListener;
import android.widget.Button;

/**
 * @author Julia, mstevens
 *
 */
public class AndroidOrientationUI extends OrientationUI<View>
{

	private Button pageView;
	
	public AndroidOrientationUI(OrientationField field, Controller controller, CollectorUI<View> collectorUI)
	{
		super(field, controller, collectorUI);
	}
	
	@Override
	public View getPlatformView(boolean onPage, CollectorRecord record)
	{
		if(onPage)
		{
			if(pageView == null)
			{
				pageView = new Button(((CollectorView) collectorUI).getContext());
				pageView.setText(field.getLabel());
				// TODO some kind of icon/image would be nice (an arrow?)
				pageView.setOnClickListener(new OnClickListener()
				{
					@Override
					public void onClick(View v)
					{
						controller.goToFromPage(field);
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
