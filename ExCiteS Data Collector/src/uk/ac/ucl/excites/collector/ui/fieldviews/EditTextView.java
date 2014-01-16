package uk.ac.ucl.excites.collector.ui.fieldviews;

import uk.ac.ucl.excites.collector.project.model.fields.Field;
import uk.ac.ucl.excites.collector.project.model.fields.EditTextField;
import uk.ac.ucl.excites.collector.project.ui.FieldUI;
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

	private EditTextField field;
	private EditText editText;
	private TextView errorMsg;

	public EditTextView(final Context context, final EditTextField field)
	{
		super(context);
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
		editText.setText(field.getInitialValue());
		if(field.isMultiline() == true)
			editText.setSingleLine(false);
		else
			editText.setSingleLine(true);
		addView(editText);

		// Error msg:
		errorMsg = new TextView(context);
		errorMsg.setLayoutParams(FULL_WIDTH);
		errorMsg.setTextColor(Color.RED);
		addView(errorMsg);

		editText.addTextChangedListener(new TextValidator(editText));
	}

	// for now called from TextChangedListener (later on forward??)
	// TODO: Check width
	public void setText(boolean valid)
	{
		if(valid)
			field.setText(editText.getText().toString());
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

	/*
	 * (non-Javadoc)
	 * 
	 * @see uk.ac.ucl.excites.collector.project.ui.FieldUI#update()
	 */
	@Override
	public void update()
	{
		// does nothing
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

	public class TextValidator implements TextWatcher
	{
		private TextView textView;

		public TextValidator(TextView textView)
		{
			this.textView = textView;
		}
		
		@Override
		final public void afterTextChanged(Editable s)
		{
			String text = textView.getText().toString();
			// Too short:
			if(text.length() < field.getMinLength())
			{
				setText(false);
				errorMsg.setText("Minimum length of " + field.getMinLength() + " characters not reached."); //TODO multilang
				// TODO disable forward
			}
			// To long:
			else if(text.length() > field.getMaxLength())
			{
				setText(false);
				errorMsg.setText("Maximum length of " + field.getMaxLength() + " characters exceeded."); //TODO multilang
				// TODO disable forward
			}
			// OK:
			else
			{
				errorMsg.setText("");
				setText(true);
				// TODO enable forward
			}
		}

		@Override
		final public void beforeTextChanged(CharSequence s, int start, int count, int after)
		{ /* Don't care */
		}

		@Override
		final public void onTextChanged(CharSequence s, int start, int before, int count)
		{ /* Don't care */
		}
	}

}
