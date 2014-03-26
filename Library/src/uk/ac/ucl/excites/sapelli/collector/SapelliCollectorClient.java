/**
 * 
 */
package uk.ac.ucl.excites.sapelli.collector;

import java.util.HashSet;
import java.util.Set;

import uk.ac.ucl.excites.sapelli.collector.db.ProjectStore;
import uk.ac.ucl.excites.sapelli.collector.model.CollectorRecord;
import uk.ac.ucl.excites.sapelli.collector.model.Form;
import uk.ac.ucl.excites.sapelli.collector.model.Project;
import uk.ac.ucl.excites.sapelli.storage.model.Column;
import uk.ac.ucl.excites.sapelli.storage.model.Record;
import uk.ac.ucl.excites.sapelli.storage.model.Schema;
import uk.ac.ucl.excites.sapelli.transmission.Settings;
import uk.ac.ucl.excites.sapelli.transmission.TransmissionClient;

/**
 * @author mstevens
 *
 */
public class SapelliCollectorClient implements TransmissionClient
{
	
	// STATICS-------------------------------------------------------
	static public long GetSchemaID(Form form)
	{
		return GetSchemaID(form.getProject().getHash(), form.getIndex());
	}
	
	static public long GetSchemaID(long projectHash, int formIndex)
	{
		return	(projectHash << Form.FORM_INDEX_SIZE) +	// Project hash takes up first 32 bits
				formIndex;								// Form index takes up next 4 bits
	}
	
	static public long GetProjectHash(long schemaID)
	{
		return schemaID >> Form.FORM_INDEX_SIZE;
	}
	
	static public int GetFormIndex(long schemaID)
	{
		return (int) (schemaID % (1 << Form.FORM_INDEX_SIZE));
	}
		
	// DYNAMICS------------------------------------------------------
	private ProjectStore projectStore;
	
	public SapelliCollectorClient(ProjectStore projectStore)
	{
		this.projectStore = projectStore;
	}
	
	@Override
	public Schema getSchema(long schemaID)
	{
		Project p = projectStore.retrieveProject(GetProjectHash(schemaID));
		if(p != null)
			return p.getForm(GetFormIndex(schemaID)).getSchema();
		else
			return null;
	}
	
	@Override
	public Schema getSchemaV1(int schemaID, int schemaVersion)
	{
		Project p = projectStore.retrieveV1Project(schemaID, schemaVersion);
		if(p != null)
			return p.getForm(0).getSchema(); // return schema of the first (and assumed only) form
		else
			return null;
	}
	
	@Override
	public Record getNewRecord(Schema schema)
	{
		Project proj = projectStore.retrieveProject(GetProjectHash(schema.getID()));
		if(proj != null)
		{
			Form frm = proj.getForm(GetFormIndex(schema.getID()));
			if(frm != null)
			{
				if(frm.isProducesRecords())
					return new CollectorRecord(frm); // CollectorRecord instance
				else
					return null;
			}
		}
		return new Record(schema); // plain Record instance
	}

	/* (non-Javadoc)
	 * @see uk.ac.ucl.excites.transmission.TransmissionClient#getSettingsFor(uk.ac.ucl.excites.storage.model.Schema)
	 */
	@Override
	public Settings getSettingsFor(Schema schema)
	{
		/*TODO FIX THIS
		 * This is buggy/hacky! Because schema's can be shared by multiple forms (and no schema ID/version duplicates are allowed)
		 * we cannot safely determine transmission settings based on the schema id/version.
		 */
//		List<Form> forms = dao.retrieveForms(schema.getID(), schema.getVersion());
//		if(!forms.isEmpty())
//		{
//			if(forms.get(0)/*HACK!*/.getProject() != null)
//				return forms.get(0).getProject().getTransmissionSettings();
//			else
//				return null;
//		}
//		else
//		{
//			return null;
//		}
		return null;
	}

	/* (non-Javadoc)
	 * @see uk.ac.ucl.excites.transmission.TransmissionClient#getFactoredOutColumnsFor(uk.ac.ucl.excites.storage.model.Schema)
	 */
	@Override
	public Set<Column<?>> getFactoredOutColumnsFor(Schema schema)
	{
		Set<Column<?>> columns = new HashSet<Column<?>>();
		columns.add(Form.COLUMN_DEVICE_ID);
		return columns;
	}

}
