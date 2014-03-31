/**
 * 
 */
package uk.ac.ucl.excites.sapelli.collector.model.fields;

import java.io.File;
import java.util.List;

import uk.ac.ucl.excites.sapelli.collector.control.Controller;
import uk.ac.ucl.excites.sapelli.collector.model.Form;
import uk.ac.ucl.excites.sapelli.collector.model.Project;
import uk.ac.ucl.excites.sapelli.storage.model.Column;
import uk.ac.ucl.excites.sapelli.storage.model.columns.ForeignKeyColumn;
import uk.ac.ucl.excites.sapelli.storage.queries.ExtremeValueRecordQuery;
import uk.ac.ucl.excites.sapelli.storage.queries.NullRecordQuery;
import uk.ac.ucl.excites.sapelli.storage.queries.RecordsQuery;
import uk.ac.ucl.excites.sapelli.storage.queries.SingleRecordQuery;
import uk.ac.ucl.excites.sapelli.storage.queries.constraints.AndConstraint;
import uk.ac.ucl.excites.sapelli.storage.queries.constraints.Constraint;

/**
 * Field that represents relationship with another Form
 * 
 * @author mstevens
 */
public class Relationship extends UILessField
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
		//ONE_TO_ONE,
		
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
		//MANY_TO_MANY
		
	}
	
	static public final boolean DEFAULT_HOLD_FOREIGN_RECORD = false;
	
	// Dynamics------------------------------------------------------
	private Form relatedForm;
	private Type type;
	private boolean holdForeignRecord;
	private AndConstraint constraints;

	/**
	 * @param form
	 * @param id
	 */
	public Relationship(Form form, String id, Type type)
	{
		super(form, id);
		/*if(type == Type.ONE_TO_ONE)
			throw new IllegalArgumentException("One-to-one relationships are not yet implemented."); //TODO implemented One-to-one relationships (still needs XML syntax)
		if(type == Type.MANY_TO_MANY)
			throw new IllegalArgumentException("Many-to-many relationships are not yet implemented."); //TODO implemented Many-to-many relationships (still needs XML syntax) */
		this.type = type;
		noColumn = (type == Type.LINK);
		constraints = new AndConstraint();
	}
	
	public void setNoColumn(boolean noColumn)
	{
		throw new UnsupportedOperationException("setNoColumn is unsupported on Relation fields. Whether or not they have columns is soley determined by their type.");
	}
	
	public void setRelatedForm(Form relatedForm)
	{
		if(relatedForm == form)
			throw new IllegalArgumentException("A form cannot be related to itself!"); //TODO why not? e.g. person-person relationship
		if(type == Type.MANY_TO_ONE && !relatedForm.isProducesRecords())
		{
			type = Type.LINK;
			noColumn = true;
			form.addWarning("Related form does not produce records, changed <BelongsTo> to <Link>");
		}
		this.relatedForm = relatedForm;
	}
	
	public void addConstraint(Constraint constraint)
	{
		constraints.addConstraint(constraint);
	}

	/**
	 * @return the constraints
	 */
	public AndConstraint getConstraints()
	{
		return constraints;
	}
	
	/**
	 * Returns a SingleRecordQuery that can be used to find (or verify)
	 * the "held" record (the most recent record that meets the constraints).
	 * 
	 * If this relationship does not hold on to foreign records a dummy query
	 * will be returns which always returns a null record upon execution.
	 * 
	 * TODO check for deviceID / source:local/remote ...
	 * 
	 * @return
	 */
	public SingleRecordQuery getHeldRecordQuery()
	{
		if(!holdForeignRecord)
			return new NullRecordQuery();
		return ExtremeValueRecordQuery.Max(Form.COLUMN_TIMESTAMP_START, new RecordsQuery(relatedForm.getSchema(), constraints));
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
	 * @see uk.ac.ucl.excites.collector.project.model.Field#createColumn()
	 */
	@Override
	protected Column<?> createColumn()
	{	
		if(noColumn)
			return null; // just in case (LinksTo)
		else
			return new ForeignKeyColumn(id, relatedForm.getSchema(), (optional != Optionalness.NEVER)); // (BelongsTo)
	}
	
	@Override
	public ForeignKeyColumn getColumn()
	{
		return (ForeignKeyColumn) super.getColumn();
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
	 * @see uk.ac.ucl.excites.sapelli.collector.model.Field#enter(uk.ac.ucl.excites.sapelli.collector.control.Controller, boolean)
	 */
	@Override
	public boolean enter(Controller controller, boolean withPage)
	{
		if(!withPage)
			switch(type)
			{
				case LINK:
					return controller.enterLinksTo(this);
				//case ONE_TO_ONE:
				//	return false; //TODO
				case MANY_TO_ONE:
					return controller.enterBelongsTo(this);
				//case MANY_TO_MANY:
				//	return false; //TODO
				default :
					return false;
			}
		return true;
	}

}
