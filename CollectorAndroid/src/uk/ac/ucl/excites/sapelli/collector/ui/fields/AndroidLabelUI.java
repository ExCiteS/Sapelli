package uk.ac.ucl.excites.sapelli.collector.ui.fields;

import uk.ac.ucl.excites.sapelli.collector.control.CollectorController;
import uk.ac.ucl.excites.sapelli.collector.model.CollectorRecord;
import uk.ac.ucl.excites.sapelli.collector.model.fields.LabelField;
import uk.ac.ucl.excites.sapelli.collector.ui.CollectorView;
import android.view.View;
import android.widget.TextView;


/**
 * @author mstevens
 *
 */
public class AndroidLabelUI extends LabelUI<View>
{
	
	private TextView label;
	
	public AndroidLabelUI(LabelField labelField, CollectorController controller, CollectorView collectorView)
	{
		super(labelField, controller, collectorView);
	}

	@Override
	public TextView getPlatformView(boolean onPage, CollectorRecord record)
	{
		if(label == null)
		{
			label = new TextView(((CollectorView) collectorUI).getContext());
			label.setText(field.getLabel());
			//TODO bold? larger font?
		}
		return label;
	}

}
