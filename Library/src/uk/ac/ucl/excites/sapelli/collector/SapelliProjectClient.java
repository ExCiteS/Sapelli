/**
 * 
 */
package uk.ac.ucl.excites.sapelli.collector;

import java.util.HashSet;
import java.util.Set;

import uk.ac.ucl.excites.sapelli.collector.database.DataAccess;
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
public class SapelliProjectClient implements TransmissionClient
{

	private DataAccess dao;
	
	public SapelliProjectClient(DataAccess dao)
	{
		this.dao = dao;
	}
	
	@Override
	public Schema getSchema(long usageID, int usageSubID)
	{
		Project p = dao.retrieveProject(usageID); //usageID = Project#hash
		if(p != null)
			return p.getForm(usageSubID).getSchema(); //usageSubID = Form#index
		else
			return null;
	}
	
	@Override
	public Schema getSchemaV1(int schemaID, int schemaVersion)
	{
		Project p = dao.retrieveV1Project(schemaID, schemaVersion);
		if(p != null)
			return p.getForm(0).getSchema(); // return schema of the first (and assumed only) form
		else
			return null;
	}
	
	@Override
	public Record getNewRecord(Schema schema)
	{
		Project proj = dao.retrieveProject(schema.getUsageID()); //usageID = Project#hash
		if(proj != null)
		{
			Form frm = proj.getForm(schema.getUsageSubID()); //usageSubID = Form#index
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
		columns.add(schema.getColumn(Form.COLUMN_DEVICE_ID));
		return columns;
	}

}
