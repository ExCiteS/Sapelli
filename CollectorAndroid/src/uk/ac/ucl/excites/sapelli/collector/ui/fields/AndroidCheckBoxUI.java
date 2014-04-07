/**
 * 
 */
package uk.ac.ucl.excites.sapelli.collector.ui.fields;

import uk.ac.ucl.excites.sapelli.collector.control.CollectorController;
import uk.ac.ucl.excites.sapelli.collector.control.Controller.FormSession.Mode;
import uk.ac.ucl.excites.sapelli.collector.model.CollectorRecord;
import uk.ac.ucl.excites.sapelli.collector.model.fields.CheckBoxField;
import uk.ac.ucl.excites.sapelli.collector.ui.CollectorView;
import uk.ac.ucl.excites.sapelli.storage.model.columns.BooleanColumn;
import android.view.View;
import android.view.View.OnFocusChangeListener;
import android.widget.CheckBox;
import android.widget.LinearLayout.LayoutParams;

/**
 * @author Julia, mstevens
 * 
 */
public class AndroidCheckBoxUI extends CheckBoxUI<View, CollectorView>
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
	public CheckBox getPlatformView(boolean onPage, CollectorRecord record, boolean newRecord)
	{
		if(chbx == null)
		{
			chbx = new CheckBox(collectorUI.getContext());
			LayoutParams lp = new LayoutParams(LayoutParams.WRAP_CONTENT, LayoutParams.WRAP_CONTENT);
			lp.setMargins(0, negativeTopMarginPx, 0, negativeBottomMarginPx); // otherwise checkbox has too much margin on top & bottom (at least on Nexus4) // TODO test on XCover1 & 2)
			chbx.setLayoutParams(lp);
			chbx.setText(field.getLabel());
			
			// Make other fields lose focus, make keyboard disappear, and simulate clicking with onFocusChange:
			chbx.setFocusable(true);
			chbx.setFocusableInTouchMode(true);
			chbx.setOnFocusChangeListener(new OnFocusChangeListener()
			{
				@Override
				public void onFocusChange(View v, boolean hasFocus)
				{
					if(hasFocus)
					{
						// Hide keyboard if it is currently shown:
						collectorUI.hideKeyboard();
						
						// Lose focus again:
						v.clearFocus();

						// Swap state (to simulate a click):						
						CheckBox chbx = (CheckBox) v;
						chbx.setChecked(!chbx.isChecked());
					}
				}
			});
			
			newRecord = true; // force update of new view
		}
		
		// Update checkbox state:
		if(newRecord)
		{
			BooleanColumn col = (BooleanColumn) field.getColumn();
			if(record.isValueSet(col))
				chbx.setChecked(col.retrieveValue(record));
			else
				chbx.setChecked(field.getInitialValue());
		}
		chbx.setEnabled(controller.getCurrentFormMode() != Mode.EDIT || field.isEditable()); // disable when in edit mode and field is not editable, otherwise enable
		return chbx;
	}
	
}
