/**
 * 
 */
package uk.ac.ucl.excites.sapelli.collector.ui.fields;

import uk.ac.ucl.excites.sapelli.collector.control.CollectorController;
import uk.ac.ucl.excites.sapelli.collector.model.CollectorRecord;
import uk.ac.ucl.excites.sapelli.collector.model.fields.CheckBoxField;
import uk.ac.ucl.excites.sapelli.collector.ui.CollectorView;
import uk.ac.ucl.excites.sapelli.collector.ui.fields.CheckBoxUI;
import uk.ac.ucl.excites.sapelli.storage.model.columns.BooleanColumn;
import android.content.Context;
import android.view.View;
import android.widget.CheckBox;
import android.widget.LinearLayout;

/**
 * @author Julia, mstevens
 * 
 * TODO why are we wrapping the checkbox in a linear layout??
 */
public class AndroidCheckBoxUI extends CheckBoxUI<View>
{
	
	private LinearLayout view;

	public AndroidCheckBoxUI(CheckBoxField checkBox, CollectorController controller, CollectorView collectorView)
	{
		super(checkBox, controller, collectorView);
	}

	@Override
	protected boolean getValue()
	{
		return ((CheckBox) view.getChildAt(0)).isChecked();
	}
	
	@Override
	public LinearLayout getPlatformView(boolean onPage, CollectorRecord record)
	{
		CheckBox chbx = null;
		if(view == null)
		{
			Context context = ((CollectorView) collectorUI).getContext();
			// Create views:
			view = new LinearLayout(context);
			view.setOrientation(LinearLayout.HORIZONTAL);
			chbx = new CheckBox(context);
			chbx.setText(field.getLabel());
			view.addView(chbx);
		}
		else
			chbx = (CheckBox) view.getChildAt(0);
		
		// Update checkbox state:
		BooleanColumn col = (BooleanColumn) field.getColumn();
		if(record.isValueSet(col))
			chbx.setChecked(col.retrieveValue(record));
		else
			chbx.setChecked(field.getInitialValue());
		
		return view;
	}
	
}
