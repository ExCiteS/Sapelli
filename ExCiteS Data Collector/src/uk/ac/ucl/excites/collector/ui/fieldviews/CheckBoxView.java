/**
 * 
 */
package uk.ac.ucl.excites.collector.ui.fieldviews;

import uk.ac.ucl.excites.collector.project.model.fields.CheckBoxField;
import uk.ac.ucl.excites.collector.project.model.fields.Field;
import uk.ac.ucl.excites.collector.project.ui.FieldUI;
import uk.ac.ucl.excites.storage.model.Record;
import android.annotation.SuppressLint;
import android.content.Context;
import android.widget.CheckBox;
import android.widget.LinearLayout;

/**
 * @author Julia, mstevens
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
		chbx.setText(field.getLabel());
		chbx.setChecked(field.getValue());
		addView(chbx);
	}

	@Override
	public Field getField()
	{
		return field;
	}

	@Override
	public void update(Record record)
	{
		// TODO
	}

	@Override
	public void cancel()
	{
		// does nothing
	}

	@Override
	public boolean isValid(Record record)
	{
		// TODO Auto-generated method stub
		return false;
	}

	@Override
	public void storeValue(Record record)
	{
		// TODO Auto-generated method stub
		
	}

}
