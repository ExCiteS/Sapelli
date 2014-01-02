/**
 * 
 */
package uk.ac.ucl.excites.collector.project.model;

import java.nio.charset.Charset;

import org.apache.commons.codec.CharEncoding;

import uk.ac.ucl.excites.collector.project.model.Field.Optionalness;
import uk.ac.ucl.excites.collector.project.ui.CollectorUI;
import uk.ac.ucl.excites.collector.project.ui.Controller;
import uk.ac.ucl.excites.collector.project.ui.FieldUI;
import uk.ac.ucl.excites.storage.model.Column;
import uk.ac.ucl.excites.storage.model.IntegerColumn;
import uk.ac.ucl.excites.storage.model.LocationColumn;
import uk.ac.ucl.excites.storage.model.Record;
import uk.ac.ucl.excites.storage.model.StringColumn;

/**
 * @author Julia
 * 
 */
public class EditTextField extends Field {

	static public final String ID_PREFIX = "txt";
	private String label;
	private String text;

	// Defaults
	public static final int DEFAULT_MIN_LENGTH = 0; // minimum length of 0 if
													// not set
	public static final int DEFAULT_MAX_LENGTH = 100; // maximum length of 100
														// if not set
	public static final boolean DEFAULT_MULTILINE = false; // single-line by
															// default
	public static final String DEFAULT_INITIAL_VALUE = ""; // no initial value
															// by default

	// Dynamics
	private int maxLength;
	private int minLength;
	private boolean multiline;
	private String initValue;

	/**
	 * @param form
	 * @param id
	 * @parap label
	 */
	public EditTextField(Form form, String id, String label) {
		super(form, (id == null || id.isEmpty() ? ID_PREFIX
				+ label.trim().replaceAll("\\s+", "_") : id));
		this.label = label;

		maxLength = DEFAULT_MAX_LENGTH;
		minLength = DEFAULT_MIN_LENGTH;
		multiline = DEFAULT_MULTILINE;
		initValue = DEFAULT_INITIAL_VALUE;
	}

	/**
	 * @return the label
	 */
	public String getLabel() {
		return label;
	}

	/**
	 * @return the maxLength
	 */
	public int getMaxLength() {
		return maxLength;
	}

	/**
	 * @return the minLength
	 */
	public int getMinLength() {
		return minLength;
	}

	/**
	 * @return the multiline
	 */
	public boolean isMultiline() {
		return multiline;
	}

	/**
	 * @return the initValue
	 */
	public String getInitialValue() {
		return initValue;
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see uk.ac.ucl.excites.collector.project.model.Field#createColumn()
	 */
	@Override
	protected Column<?> createColumn() {
		return new StringColumn(id, optional != Optionalness.NEVER,
				getMaxLength()); // String encoding? define charset??
	}

	public void storeValue(Record record)
	{
		if(!isNoColumn())
			((StringColumn) form.getColumnFor(this)).storeValue(record, text); 
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see
	 * uk.ac.ucl.excites.collector.project.model.Field#enter(uk.ac.ucl.excites
	 * .collector.project.ui.Controller)
	 */
	@Override
	public boolean enter(Controller controller) {
		return controller.enterTextField(this);
	}
	
	@Override
	public FieldUI createUI(CollectorUI collectorUI) {
		return collectorUI.createTextFieldUI(this);
	}

	public void setMaxLength(int maxLength) {
		this.maxLength = maxLength;
	}

	public void setMinLength(int minLength) {
		this.minLength = minLength;

	}

	public void setMultiline(boolean multiline) {
		this.multiline = multiline;

	}

	public void setInitialValue(String initValue) {
		this.initValue = initValue;

	}
	
	public void setText(String text) {
		this.text = text;
	}

}
