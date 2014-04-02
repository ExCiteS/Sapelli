/**
 * 
 */
package uk.ac.ucl.excites.sapelli.collector.ui.fields;

import uk.ac.ucl.excites.sapelli.collector.control.CollectorController;
import uk.ac.ucl.excites.sapelli.collector.model.CollectorRecord;
import uk.ac.ucl.excites.sapelli.collector.model.fields.CheckBoxField;
import uk.ac.ucl.excites.sapelli.collector.ui.CollectorView;
import uk.ac.ucl.excites.sapelli.storage.model.columns.BooleanColumn;
import android.view.View;
import android.widget.CheckBox;
import android.widget.LinearLayout.LayoutParams;

/**
 * @author Julia, mstevens
 * 
 */
public class AndroidCheckBoxUI extends CheckBoxUI<View>
{

	static private final float NEGATIVE_TOP_MARGIN_DIP = -5.5f;
	static private final float NEGATIVE_BOTTOM_MARGIN_DIP = -4.0f;
	
	private CheckBox chbx;
	private int negativeTopMarginPx;
	private int negativeBottomMarginPx;
	
	public AndroidCheckBoxUI(CheckBoxField checkBox, CollectorController controller, CollectorView collectorView)
	{
		super(checkBox, controller, collectorView);
		negativeTopMarginPx = collectorView.convertDipToPx(NEGATIVE_TOP_MARGIN_DIP);
		negativeBottomMarginPx = collectorView.convertDipToPx(NEGATIVE_BOTTOM_MARGIN_DIP);
	}

	@Override
	protected boolean getValue()
	{
		return chbx.isChecked();
	}
	
	@Override
	public CheckBox getPlatformView(boolean onPage, CollectorRecord record)
	{
		if(chbx == null)
		{
			chbx = new CheckBox(((CollectorView) collectorUI).getContext());
			LayoutParams lp = new LayoutParams(LayoutParams.WRAP_CONTENT, LayoutParams.WRAP_CONTENT);
			lp.setMargins(0, negativeTopMarginPx, 0, negativeBottomMarginPx); // otherwise checkbox has too much margin on top & bottom (at least on Nexus4) // TODO test on XCover1 & 2)
			chbx.setLayoutParams(lp);
			chbx.setText(field.getLabel());
		}
		
		// Update checkbox state:
		BooleanColumn col = (BooleanColumn) field.getColumn();
		if(record.isValueSet(col))
			chbx.setChecked(col.retrieveValue(record));
		else
			chbx.setChecked(field.getInitialValue());
		
		return chbx;
	}
	
}
