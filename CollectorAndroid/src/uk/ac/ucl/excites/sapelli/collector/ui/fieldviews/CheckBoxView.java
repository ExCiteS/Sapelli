/**
 * 
 */
package uk.ac.ucl.excites.sapelli.collector.ui.fieldviews;

import uk.ac.ucl.excites.sapelli.collector.control.Controller;
import uk.ac.ucl.excites.sapelli.collector.model.Field;
import uk.ac.ucl.excites.sapelli.collector.model.fields.CheckBoxField;
import uk.ac.ucl.excites.sapelli.collector.ui.FieldUI;
import uk.ac.ucl.excites.sapelli.storage.model.Record;
import uk.ac.ucl.excites.sapelli.storage.model.columns.BooleanColumn;
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

	private Controller controller;
	private CheckBoxField field;
	private CheckBox chbx;

	public CheckBoxView(Context context, Controller controller, CheckBoxField field)
	{
		super(context);
		this.controller = controller;
		this.field = field;

		setOrientation(LinearLayout.HORIZONTAL);
		chbx = new CheckBox(context);
		chbx.setText(field.getLabel());
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
		BooleanColumn col = getColumn();
		if(col.retrieveValue(record) != null)
			chbx.setChecked(col.retrieveValue(record));
		else
			chbx.setChecked(field.getInitialValue());
	}

	@Override
	public void cancel()
	{
		// does nothing
	}

	@Override
	public boolean isValid(Record record)
	{
		return true; // nothing to check
	}

	@Override
	public void storeValue(Record record)
	{
		if(isValid(record) && !field.isNoColumn())
			getColumn().storeValue(record, chbx.isChecked());
	}
	
	private BooleanColumn getColumn()
	{
		return (BooleanColumn) controller.getCurrentForm().getColumnFor(field);
	}

}
