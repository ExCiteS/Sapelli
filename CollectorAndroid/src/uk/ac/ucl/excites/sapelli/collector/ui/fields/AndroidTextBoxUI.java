package uk.ac.ucl.excites.sapelli.collector.ui.fields;

import uk.ac.ucl.excites.sapelli.collector.control.CollectorController;
import uk.ac.ucl.excites.sapelli.collector.control.Controller.FormSession.Mode;
import uk.ac.ucl.excites.sapelli.collector.model.CollectorRecord;
import uk.ac.ucl.excites.sapelli.collector.model.fields.TextBoxField;
import uk.ac.ucl.excites.sapelli.collector.model.fields.TextBoxField.Content;
import uk.ac.ucl.excites.sapelli.collector.ui.CollectorView;
import android.content.Context;
import android.graphics.Color;
import android.text.Editable;
import android.text.InputFilter;
import android.text.InputType;
import android.text.TextWatcher;
import android.util.TypedValue;
import android.view.Gravity;
import android.view.View;
import android.view.View.OnFocusChangeListener;
import android.widget.EditText;
import android.widget.LinearLayout;
import android.widget.TextView;

/**
 * @author Julia, mstevens
 * 
 */
public class AndroidTextBoxUI extends TextBoxUI<View, CollectorView>
{
	
	private TextBoxView view;

	public AndroidTextBoxUI(TextBoxField textBox, CollectorController controller, CollectorView collectorView)
	{
		super(textBox, controller, collectorView);
	}
	
	@Override
	protected String getValue()
	{
		if(view == null)
			return null; // this shouldn't happen
		return view.getText();
	}
	
	@Override
	public View getPlatformView(boolean onPage, CollectorRecord record, boolean newRecord)
	{
		// Create view if needed:
		if(view == null)
		{
			view = new TextBoxView(collectorUI.getContext());
			newRecord = true; // force update of new view
		}
		
		// Update view:
		if(newRecord)
		{
			//	Clear error:
			view.clearError();
			
			//	Set default or current value:
			view.setWatchText(false);
			String value = retrieveValue(record);
			view.setText(value != null ? value : field.getInitialValue());
			view.setWatchText(true);
		}
		view.setEnabled(controller.getCurrentFormMode() != Mode.EDIT || field.isEditable()); // disable when in edit mode and field is not editable, otherwise enable
		
		// Return view:
		return view;
	}

	@Override
	protected void setValidationError(String errorDescr)
	{
		if(view != null)
			view.setError(errorDescr);
	}

	@Override
	protected void clearValidationError()
	{
		if(view != null)
			view.clearError();
	}
	
	public class TextBoxView extends LinearLayout implements OnFocusChangeListener, TextWatcher
	{

		private EditText editText;
		private TextView errorMsg;
		private boolean watchText = true;
		
		/**
		 * @param context
		 * 
		 */
		public TextBoxView(Context context)
		{
			super(context);

			setOrientation(LinearLayout.VERTICAL);
			setLayoutParams(CollectorView.FULL_WIDTH_LAYOUTPARAMS);

			// Label:
			TextView label = new TextView(context);
			label.setText(field.getCaption());
			label.setLayoutParams(CollectorView.FULL_WIDTH_LAYOUTPARAMS);
			addView(label);

			// Textbox:
			editText = new EditText(context);
			editText.setLayoutParams(CollectorView.FULL_WIDTH_LAYOUTPARAMS);
			//	Multiline (as specified as part of input type);
			editText.setSingleLine(!field.isMultiline());
			//	Limit input length to specified maximum:
			editText.setFilters(new InputFilter[] { new InputFilter.LengthFilter(field.getMaxLength()) });
			//	Set input type:
			editText.setInputType(getInputType());
			//	Event handlers:
			editText.setOnFocusChangeListener(this);
			editText.addTextChangedListener(this);
			//	Add the textbox:
			addView(editText);

			// Error msg:
			errorMsg = new TextView(context);
			errorMsg.setLayoutParams(CollectorView.FULL_WIDTH_LAYOUTPARAMS);
			errorMsg.setTextColor(Color.RED);
			errorMsg.setGravity(Gravity.RIGHT);
			errorMsg.setTextSize(TypedValue.COMPLEX_UNIT_PX, errorMsg.getTextSize() * 0.9f);
			errorMsg.setVisibility(GONE);
			addView(errorMsg);
		}
		
		public String getText()
		{
			return editText.getText().toString();
		}
		
		public void setText(String txt)
		{
			editText.setText(txt);
		}
		
		public void setError(String error)
		{
			errorMsg.setText(error);
			errorMsg.setVisibility(VISIBLE);
		}
		
		public void clearError()
		{
			errorMsg.setText("");
			errorMsg.setVisibility(GONE);
		}

		/**
		 * @param watchText the watchText to set
		 */
		public void setWatchText(boolean watchText)
		{
			this.watchText = watchText;
		}
		
		@Override
		public void setEnabled(boolean enabled)
		{
			super.setEnabled(enabled);
			editText.setEnabled(enabled);
		}
		
		@Override
		public void onFocusChange(View v, boolean hasFocus)
		{
			if(!hasFocus)
				isValidInformPage(controller.getCurrentRecord()); // will call isValid() but via the containing page such that the red box can (dis)appear, if the field is not a page isValid() is called directly
		}
		
		@Override
		final public void afterTextChanged(Editable s)
		{
			/* Don't care */
		}

		@Override
		final public void beforeTextChanged(CharSequence s, int start, int count, int after)
		{
			if(watchText)
			{
				clearPageInvalidMark(); // the user is currently typing, so don't annoy him/her with the red box
				clearError();
			}
		}

		@Override
		final public void onTextChanged(CharSequence s, int start, int before, int count)
		{
			/* Don't care */
		}
		
	}
	
	/**
	 * @return
	 * 
	 * @see <a href="http://developer.android.com/reference/android/text/InputType.html">InputType</a>
	 * @see <a href="http://developer.android.com/reference/android/widget/TextView.html#attr_android:inputType">TextView inputType</a>
	 */
	private int getInputType()
	{
		int inputType;
		
		// Input Class:
		switch(field.getContent())
		{
			case text :
			case email :
			case password :
			default :
				// Text class:
				inputType = InputType.TYPE_CLASS_TEXT; break;
			case phonenumber :
				// Phone class:
				inputType = InputType.TYPE_CLASS_PHONE; break;
			case unsignedint :
			case signedint :
			case unsignedlong :
			case signedlong :
			case unsignedfloat :
			case signedfloat :
			case unsigneddouble :
			case signeddouble :
				// Number class:
				inputType = InputType.TYPE_CLASS_NUMBER; break;
		}
		
		// Variations:
		switch(field.getContent())
		{
			case text :
			default :
				inputType |= InputType.TYPE_TEXT_VARIATION_NORMAL; break;
			case email :
				inputType |= InputType.TYPE_TEXT_VARIATION_EMAIL_ADDRESS; break;
			case password :
				inputType |= InputType.TYPE_TEXT_VARIATION_PASSWORD; break;
			case phonenumber :
				break;
			case unsignedint :
			case unsignedlong :
				break;
			case signedint :
			case signedlong :
				inputType |= InputType.TYPE_NUMBER_FLAG_SIGNED; break;
			case unsignedfloat :
			case unsigneddouble :
				inputType |= InputType.TYPE_NUMBER_FLAG_DECIMAL; break;
			case signedfloat :
			case signeddouble :
				inputType |= InputType.TYPE_NUMBER_FLAG_SIGNED | InputType.TYPE_NUMBER_FLAG_DECIMAL; break;
		}
		
		// Only for content=text:
		if(field.getContent() == Content.text)
		{
			//	Automatic capitalisation:
			switch(field.getCapitalisation())
			{
				case none :
				default :
					break;
				case all :
					inputType |= InputType.TYPE_TEXT_FLAG_CAP_CHARACTERS; break;
				case words :
					inputType |= InputType.TYPE_TEXT_FLAG_CAP_WORDS; break;
				case sentences :
					inputType |= InputType.TYPE_TEXT_FLAG_CAP_SENTENCES; break;
			}
			
			//	Multi-line:
			if(field.isMultiline())
				inputType |= InputType.TYPE_TEXT_FLAG_MULTI_LINE;	
		}
		
		return inputType;
	}
	
}
