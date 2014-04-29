package uk.ac.ucl.excites.sapelli.collector.ui.fields;

import uk.ac.ucl.excites.sapelli.collector.control.CollectorController;
import uk.ac.ucl.excites.sapelli.collector.model.fields.LabelField;
import uk.ac.ucl.excites.sapelli.collector.ui.CollectorView;
import uk.ac.ucl.excites.sapelli.storage.model.Record;
import android.util.TypedValue;
import android.view.Gravity;
import android.view.View;
import android.widget.TextView;


/**
 * @author mstevens
 *
 */
public class AndroidLabelUI extends LabelUI<View, CollectorView>
{
	
	private TextView label;
	
	public AndroidLabelUI(LabelField labelField, CollectorController controller, CollectorView collectorView)
	{
		super(labelField, controller, collectorView);
	}

	@Override
	protected TextView getPlatformView(boolean onPage, boolean enabled, Record record, boolean newRecord)
	{
		if(label == null)
		{
			label = new TextView(collectorUI.getContext());
			label.setLayoutParams(CollectorView.FULL_WIDTH_LAYOUTPARAMS);
			
			// Alignment:
			label.setGravity(field.isCentered() ? Gravity.CENTER_HORIZONTAL : label.getGravity()); //TODO right alignment?
			
			// Font size:
			label.setText(field.getCaption());
			label.setTextSize(TypedValue.COMPLEX_UNIT_PX, label.getTextSize() * field.getTextSizeScale());
			
			//TODO bold? italic?
		}
		return label;
	}

}
