/**
 * 
 */
package uk.ac.ucl.excites.sapelli.collector.project.model.fields;

import java.io.File;
import java.util.ArrayList;
import java.util.List;

import uk.ac.ucl.excites.sapelli.collector.project.model.Form;
import uk.ac.ucl.excites.sapelli.collector.project.model.Project;
import uk.ac.ucl.excites.sapelli.collector.project.ui.CollectorUI;
import uk.ac.ucl.excites.sapelli.collector.project.ui.Controller;
import uk.ac.ucl.excites.sapelli.collector.project.ui.FieldUI;
import uk.ac.ucl.excites.sapelli.storage.model.Column;

/**
 * Field that represents relationship with another Form
 * 
 * @author mstevens
 */
public class Relationship extends Field
{

	//STATICS -------------------------------------------------------
	static public enum Type
	{
		/**
		 * A "jump" link between forms.
		 * <br/><br/>
		 * The relationship between the forms is purely "navigational" and there is no stored association between their records.
		 * In this case the Relation field merely provides a "passage way" through which navigation to the other form is possible.
		 * An "intra-form" jump to the relation field will automatically result in a subsequent "inter-form" jump to the {@code relatedForm}. 
		 */
		LINK,
		
		/**
		 * A 1:1 relationship between this form (holder of of the Relationship object) and another {@code relatedForm}.
		 * The consequence is that a new record of the {@code relatedForm} will be created for every instance of this form (unless the relationship is optional). 
		 */
		ONE_TO_ONE,
		
		/**
		 * A N:1 relationship between this form (holder of of the Relationship object) and another {@code relatedForm}.
		 * This means multiple records (N) of this form can relate (or better: "belong") to a single record (1) of the {@code relatedForm}. 
		 */
		MANY_TO_ONE,
		
		/**
		 * A N:M relationship between this form (holder of of the Relationship object) and another {@code relatedForm}.
		 * This means multiple records (N) of this form can relate to (or better: "have and belong to") multiple records (M) of the {@code relatedForm}.
		 * Currently not implemented.
		 * TODO Support for N:M cardinality (will require some kind of "cross table")
		 */
		MANY_TO_MANY
		
	}
	
	static public final boolean DEFAULT_HOLD_FOREIGN_RECORD = false;
	
	// Dynamics------------------------------------------------------
	private Form relatedForm;
	private Type type;
	private boolean holdForeignRecord;

	/**
	 * @param form
	 * @param id
	 */
	public Relationship(Form form, String id, Type type)
	{
		super(form, id);
		if(type == Type.MANY_TO_MANY)
			throw new IllegalArgumentException("Many-to-many relationships are not yet implemented.");
		this.type = type; 
		noColumn = (type == Type.LINK ? true : false);
	}
	
	public void setNoColumn(boolean noColumn)
	{
		throw new UnsupportedOperationException("setNoColumn is unsupported on Relation fields. Whether or not they have columns is soley determined by their type.");
	}
	
	public void setRelatedForm(Form relatedForm)
	{
		if(relatedForm == form)
			throw new IllegalArgumentException("A form cannot be related to itself!"); //TODO why not? e.g. person-person relationship
		this.relatedForm = relatedForm;
	}

	/**
	 * @return the relatedForm
	 */
	public Form getRelatedForm()
	{
		return relatedForm;
	}

	/**
	 * @return the type
	 */
	public Type getType()
	{
		return type;
	}

	/**
	 * @return the holdForeignRecord
	 */
	public boolean isHoldForeignRecord()
	{
		return holdForeignRecord;
	}

	/**
	 * @param holdForeignRecord the holdForeignRecord to set
	 */
	public void setHoldForeignRecord(boolean holdForeignRecord)
	{
		this.holdForeignRecord = holdForeignRecord;
	}

	/* (non-Javadoc)
	 * @see uk.ac.ucl.excites.collector.project.model.Field#createColumns()
	 */
	@Override
	protected List<Column<?>> createColumns()
	{
		List<Column<?>> list = new ArrayList<Column<?>>();
		//TODO add columns
		return list;
	}
	
	/* (non-Javadoc)
	 * @see uk.ac.ucl.excites.collector.project.model.Field#createColumn()
	 */
	@Override
	protected Column<?> createColumn()
	{
		throw new UnsupportedOperationException("Relationship fields require multiple columns, call createColumns() instead.");
	}

	/* (non-Javadoc)
	 * @see uk.ac.ucl.excites.collector.project.model.Field#getFiles(uk.ac.ucl.excites.collector.project.model.Project)
	 */
	@Override
	public List<File> getFiles(Project project)
	{
		//TODO link button image?
		return null;
	}

	/* (non-Javadoc)
	 * @see uk.ac.ucl.excites.collector.project.model.Field#enter(uk.ac.ucl.excites.collector.project.ui.Controller)
	 */
	@Override
	public boolean enter(Controller controller)
	{
		// TODO Auto-generated method stub
		return false;
	}

	/* (non-Javadoc)
	 * @see uk.ac.ucl.excites.collector.project.model.Field#createUI(uk.ac.ucl.excites.collector.project.ui.CollectorUI)
	 */
	@Override
	public FieldUI createUI(CollectorUI collectorUI)
	{
		// TODO Auto-generated method stub
		return null;
	}

}
