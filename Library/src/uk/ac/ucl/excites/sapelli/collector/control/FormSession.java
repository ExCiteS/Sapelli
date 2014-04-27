package uk.ac.ucl.excites.sapelli.collector.control;

import java.io.File;
import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Set;
import java.util.Stack;

import uk.ac.ucl.excites.sapelli.collector.control.Controller.FormMode;
import uk.ac.ucl.excites.sapelli.collector.model.Field;
import uk.ac.ucl.excites.sapelli.collector.model.FieldParameters;
import uk.ac.ucl.excites.sapelli.collector.model.Form;
import uk.ac.ucl.excites.sapelli.storage.model.Record;

/**
 * Helper class which holds all state variables needed to manage an open "form session"
 * 
 * @author mstevens
 */
public class FormSession
{
	
	static public FormSession Create(Form form, long deviceIDHash)
	{
		return new FormSession(form, FormMode.CREATE, form.isProducesRecords() ? form.newRecord(deviceIDHash) : null);
	}
	
	static public FormSession Edit(Form form, Record record)
	{
		return new FormSession(form, FormMode.EDIT, record);
	}
	
	//Dynamic //TODO make more fields private or final(?)
	protected final Form form;
	protected final FormMode mode;
	protected Record record;
	private Stack<FieldWithArguments> fieldAndArgumentHistory;
	private FieldWithArguments currFieldAndArguments = null;
	private boolean currFieldDisplayed = false;
	protected Set<Field> tempDisabledFields;
	protected List<File> mediaAttachments;	
	protected long startTime;
	
	/**
	 * @param form
	 * @param mode
	 * @param record
	 */
	private FormSession(Form form, FormMode mode, Record record)
	{
		if(form == null)
			throw new NullPointerException("Form cannot be null!");
		if(record == null && form.isProducesRecords())
			throw new NullPointerException("Record cannot be null because this is a record-producing form!");	
		this.form = form;
		this.mode = mode;
		this.record = record;
		this.fieldAndArgumentHistory = new Stack<FieldWithArguments>();
		this.tempDisabledFields = new HashSet<Field>();
		this.mediaAttachments = new ArrayList<File>();
		this.startTime = System.currentTimeMillis();
	}
	
	public FieldWithArguments getPrevious(boolean forBackMove)
	{
		for(int f = fieldAndArgumentHistory.size() - 1; f >= 0; f--)
		{	// loop through stack from top to bottom:
			FieldWithArguments fwA = fieldAndArgumentHistory.get(f);
			if(!forBackMove || !fwA.field.isSkipOnBack())
				return fwA;
		}
		return null;
	}
	
	public boolean canGoBack()
	{
		return getPrevious(true) != null;
	}
	
	/**
	 * Called from {@link Controller#goTo(FieldWithArguments, boolean)}
	 * 
	 * @param nextFieldAndArguments
	 */
	public void setCurrent(FieldWithArguments nextFieldAndArguments)
	{
		// Check if we already visited the next field (i.e. either because we are handling a goBack(), or a jump to a field that was visited before):
		boolean alreadyVisited = false;
		for(FieldWithArguments fwA : fieldAndArgumentHistory) // Note: stack iterates from bottom up
			if(fwA.field == nextFieldAndArguments.field)
				alreadyVisited = true;
		if(alreadyVisited)
			// Revert if already visited (remove previous visit and all fields "above"/since it)
			while(fieldAndArgumentHistory.pop().field != nextFieldAndArguments.field) {}
		else if(atField() && nextFieldAndArguments.field != currFieldAndArguments.field)
			// If the next field was not already visited *and* we are currently at a field *and* that field is not the same as the next one...
			fieldAndArgumentHistory.push(currFieldAndArguments); // then add the current field to the history.
		
		// 	Next becomes the (new) current...
		this.currFieldAndArguments = nextFieldAndArguments;
	}
	
	/**
	 * @return the currFieldDisplayed
	 */
	public boolean isCurrentFieldDisplayed()
	{
		return currFieldDisplayed;
	}

	/**
	 * @param currFieldDisplayed the currFieldDisplayed to set
	 */
	public void setCurrentFieldDisplayed(boolean currFieldDisplayed)
	{
		this.currFieldDisplayed = currFieldDisplayed;
	}

	public FieldWithArguments getCurrent()
	{
		return currFieldAndArguments;
	}
	
	public Field getCurrentField()
	{
		return currFieldAndArguments != null ? currFieldAndArguments.field : null;
	}
	
	public FieldParameters getCurrentFieldArguments()
	{
		return currFieldAndArguments != null ? currFieldAndArguments.arguments : null;
	}
	
	public boolean atField()
	{
		return currFieldAndArguments != null;
	}
	
}
