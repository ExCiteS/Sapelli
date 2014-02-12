package uk.ac.ucl.excites.collector.ui.fieldviews;

import uk.ac.ucl.excites.collector.ProjectController;
import uk.ac.ucl.excites.collector.project.model.fields.EditTextField;
import uk.ac.ucl.excites.collector.project.model.fields.Field;
import uk.ac.ucl.excites.collector.project.ui.FieldUI;
import uk.ac.ucl.excites.storage.model.Record;
import uk.ac.ucl.excites.storage.model.StringColumn;
import android.annotation.SuppressLint;
import android.content.Context;
import android.graphics.Color;
import android.text.Editable;
import android.text.TextWatcher;
import android.widget.EditText;
import android.widget.LinearLayout;
import android.widget.TextView;

/**
 * @author Julia, mstevens
 * 
 */
@SuppressLint("ViewConstructor")
public class EditTextView extends LinearLayout implements FieldUI
{
	
	static private final LayoutParams FULL_WIDTH = new LayoutParams(LayoutParams.MATCH_PARENT, LayoutParams.WRAP_CONTENT);

	private ProjectController controller;
	private EditTextField field;
	private EditText editText;
	private TextView errorMsg;

	public EditTextView(final Context context, final ProjectController controller, final EditTextField field)
	{
		super(context);
		this.controller = controller;
		this.field = field;

		setOrientation(LinearLayout.VERTICAL);

		// Label:
		TextView label = new TextView(context);
		label.setText(field.getLabel());
		label.setLayoutParams(FULL_WIDTH);
		addView(label);

		// Textbox:
		editText = new EditText(context);
		editText.setLayoutParams(FULL_WIDTH);
		if(field.isMultiline() == true)
			editText.setSingleLine(false);
		else
			editText.setSingleLine(true);
		addView(editText);

		// Error msg:
		errorMsg = new TextView(context);
		errorMsg.setLayoutParams(FULL_WIDTH);
		errorMsg.setTextColor(Color.RED);
		errorMsg.setVisibility(GONE);
		addView(errorMsg);

		editText.addTextChangedListener(new TextWatcher()
		{
			
			@Override
			final public void afterTextChanged(Editable s)
			{
				isValid(controller.getCurrentRecord());
			}

			@Override
			final public void beforeTextChanged(CharSequence s, int start, int count, int after) { /* Don't care */ }

			@Override
			final public void onTextChanged(CharSequence s, int start, int before, int count) { /* Don't care */ }
		});
	}
	
	/*
	 * (non-Javadoc)
	 * 
	 * @see uk.ac.ucl.excites.collector.project.ui.FieldUI#getField()
	 */
	@Override
	public Field getField()
	{
		return field;
	}
	
	@Override
	public void update(Record record)
	{
		StringColumn col = getColumn();
		if(col.retrieveValue(record) != null)
			editText.setText(col.retrieveValue(record));
		else
			editText.setText(field.getInitialValue());
	}
	
	/*
	 * (non-Javadoc)
	 * 
	 * @see uk.ac.ucl.excites.collector.project.ui.FieldUI#cancel()
	 */
	@Override
	public void cancel()
	{
		// does nothing
	}

	private StringColumn getColumn()
	{
		return (StringColumn) controller.getCurrentForm().getColumnFor(field);
	}
	
	@Override
	public boolean isValid(Record record)
	{
		String text = editText.getText().toString();
		boolean valid = true;
		// Too short:
		if(text.length() < field.getMinLength())
		{
			errorMsg.setText("Minimum length of " + field.getMinLength() + " characters not reached."); //TODO multilang
			valid = false;
		}
		// Too long:
		else if(text.length() > field.getMaxLength())
		{
			errorMsg.setText("Maximum length of " + field.getMaxLength() + " characters exceeded."); //TODO multilang
			valid = false;
		}
		// OK:
		if(valid)
			errorMsg.setText("");

		errorMsg.setVisibility(valid ? GONE : VISIBLE);
		return valid;
	}

	public void storeValue(Record record)
	{
		if(isValid(record) && !field.isNoColumn())
			getColumn().storeValue(record, editText.getText().toString());
	}

}
