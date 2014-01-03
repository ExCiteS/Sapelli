/**
 * 
 */
package uk.ac.ucl.excites.collector.ui.fieldviews;

import uk.ac.ucl.excites.collector.project.model.CheckBoxField;
import uk.ac.ucl.excites.collector.project.model.Field;
import uk.ac.ucl.excites.collector.project.ui.FieldUI;
import android.content.Context;
import android.widget.CheckBox;
import android.widget.LinearLayout;
import android.widget.TextView;

/**
 * @author Julia
 *
 */
public class CheckBoxView extends LinearLayout implements FieldUI{

	private CheckBoxField field;
	private CheckBox chbx;

	public CheckBoxView(Context context, CheckBoxField field) {
		super(context);
		this.field = field;
		
		setOrientation(LinearLayout.VERTICAL);
		LayoutParams fullWidth = new LayoutParams(LayoutParams.MATCH_PARENT, LayoutParams.WRAP_CONTENT);
		
		TextView label = new TextView(context);
		label.setText(field.getLabel());
		label.setLayoutParams(fullWidth);
		addView(label);
		
		chbx = new CheckBox(context);
		chbx.setChecked(field.getValue());
		addView(chbx);
	}
	
	// TODO: not called yet
	public void setValue(){
		field.setValue(chbx.isChecked());
	}
	
	@Override
	public Field getField() {
		// TODO Auto-generated method stub
		return field;
	}
	

	@Override
	public void update() {
		// TODO Auto-generated method stub
		
	}

	@Override
	public void cancel() {
		// TODO Auto-generated method stub
		
	}

}
