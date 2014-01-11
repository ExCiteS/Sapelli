/**
 * 
 */
package uk.ac.ucl.excites.collector.ui.fieldviews;

import uk.ac.ucl.excites.collector.project.model.fields.CheckBoxField;
import uk.ac.ucl.excites.collector.project.model.fields.Field;
import uk.ac.ucl.excites.collector.project.ui.FieldUI;
import android.annotation.SuppressLint;
import android.content.Context;
import android.widget.CheckBox;
import android.widget.LinearLayout;
import android.widget.TextView;

/**
 * @author Julia
 * 
 */
@SuppressLint("ViewConstructor")
public class CheckBoxView extends LinearLayout implements FieldUI
{

	private CheckBoxField field;
	private CheckBox chbx;

	public CheckBoxView(Context context, CheckBoxField field)
	{
		super(context);
		this.field = field;

		setOrientation(LinearLayout.HORIZONTAL);
		chbx = new CheckBox(context);
		chbx.setChecked(field.getValue());
		addView(chbx);

		TextView label = new TextView(context);
		label.setText(field.getLabel());
		addView(label);

	}

	// TODO: not called yet
	public void setValue()
	{
		field.setValue(chbx.isChecked());
	}

	@Override
	public Field getField()
	{
		// TODO Auto-generated method stub
		return field;
	}

	@Override
	public void update()
	{
		// TODO Auto-generated method stub

	}

	@Override
	public void cancel()
	{
		// TODO Auto-generated method stub

	}

}
