package uk.ac.ucl.excites.collector.ui.fieldviews;

import uk.ac.ucl.excites.collector.project.model.Field;
import uk.ac.ucl.excites.collector.project.model.EditTextField;
import uk.ac.ucl.excites.collector.project.ui.FieldUI;
import android.content.Context;
import android.graphics.Color;
import android.text.Editable;
import android.text.TextWatcher;
import android.widget.EditText;
import android.widget.LinearLayout;
import android.widget.TextView;

/**
 * @author Julia
 *
 */
public class EditTextView extends LinearLayout implements FieldUI {

	private EditTextField field;
	private EditText editText;
	
	
	public EditTextView(final Context context, final EditTextField field) {
		super(context);
		this.field = field;
		
		setOrientation(LinearLayout.VERTICAL);
		LayoutParams fullWidth = new LayoutParams(LayoutParams.MATCH_PARENT, LayoutParams.WRAP_CONTENT);
		
		TextView label = new TextView(context);
		label.setText(field.getLabel());
		label.setLayoutParams(fullWidth);
		addView(label);
		
		editText = new EditText(context);
		editText.setLayoutParams(fullWidth);
		editText.setText(field.getInitialValue());
		if(field.isMultiline() == true)
			editText.setSingleLine(false);
		else
			editText.setSingleLine(true);
		addView(editText);
		
		final TextView errorMsg = new TextView(context);
		errorMsg.setLayoutParams(fullWidth);
		errorMsg.setTextColor(Color.RED);
		addView(errorMsg);
		
			

		editText.addTextChangedListener(new TextValidator(editText) {
		    @Override
		    public void validate(TextView textView, String text) {
		       if(text.length()<field.getMinLength()){
		    	   setText(false);
		    	   errorMsg.setText("Minimum length of " + field.getMinLength() + " characters not reached.");
		    		 // TODO disable forward
		       }
		       else if(text.length()>field.getMaxLength()){
		    	   setText(false);
		    	   errorMsg.setText("Maximum length of " + field.getMaxLength() + " characters exceeded.");
			    	 
			    	 // TODO disable forward
		       }else{
		    	   errorMsg.setText("");
		    	   setText(true);
		    	   // TODO enable forward
		       }
		       
		    }
		});
		

	}


	// for now called from TextChangedListener (later on forward??)
	// TODO: Check whet
	public void setText(boolean valid){
		if(valid)
		field.setText(editText.getText().toString());
	}



	/* (non-Javadoc)
	 * @see uk.ac.ucl.excites.collector.project.ui.FieldUI#getField()
	 */
	@Override
	public Field getField() {
		return field;
	}

	/* (non-Javadoc)
	 * @see uk.ac.ucl.excites.collector.project.ui.FieldUI#update()
	 */
	@Override
	public void update() {
		// does nothing
	}

	/* (non-Javadoc)
	 * @see uk.ac.ucl.excites.collector.project.ui.FieldUI#cancel()
	 */
	@Override
	public void cancel() {
		// does nothing
	}
	
	
	public abstract class TextValidator implements TextWatcher {
	    private TextView textView;

	    public TextValidator(TextView textView) {
	        this.textView = textView;
	    }

	    public abstract void validate(TextView textView, String text);

	    @Override
	    final public void afterTextChanged(Editable s) {
	        String text = textView.getText().toString();
	        validate(textView, text);
	    }

	    @Override
	    final public void beforeTextChanged(CharSequence s, int start, int count, int after) { /* Don't care */ }

	    @Override
	    final public void onTextChanged(CharSequence s, int start, int before, int count) { /* Don't care */ }
	}

}
