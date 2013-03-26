/**
 * 
 */
package uk.ac.ucl.excites.collector.project.db;

import java.util.HashSet;
import java.util.Set;

import uk.ac.ucl.excites.collector.project.model.Form;
import uk.ac.ucl.excites.storage.model.Column;
import uk.ac.ucl.excites.storage.model.Schema;
import uk.ac.ucl.excites.transmission.ModelProvider;
import uk.ac.ucl.excites.transmission.Settings;

/**
 * @author mstevens
 *
 */
public class ProjectModelProvider implements ModelProvider
{

	private DataAccess dao;
	
	public ProjectModelProvider(DataAccess dao)
	{
		this.dao = dao;
	}
	
	/* (non-Javadoc)
	 * @see uk.ac.ucl.excites.transmission.ModelProvider#getSchema(int, int)
	 */
	@Override
	public Schema getSchema(int id, int version)
	{
		boolean open = dao.isOpen();
		try
		{
			if(!open)
				dao.openDB();
			return dao.retrieveSchema(id, version);
		}
		finally
		{
			if(!open)
				dao.closeDB();
		}
	}

	/* (non-Javadoc)
	 * @see uk.ac.ucl.excites.transmission.ModelProvider#getSettingsFor(uk.ac.ucl.excites.storage.model.Schema)
	 */
	@Override
	public Settings getSettingsFor(Schema schema)
	{
		boolean open = dao.isOpen();
		try
		{
			if(!open)
				dao.openDB();
			Form form = dao.retrieveForm(schema.getID(), schema.getVersion());
			if(form != null)
				if(form.getProject() != null)
					return form.getProject().getTransmissionSettings();
				else
					return null;
			else
				return null;
		}
		finally
		{
			if(!open)
				dao.closeDB();
		}
	}

	/* (non-Javadoc)
	 * @see uk.ac.ucl.excites.transmission.ModelProvider#getFactoredOutColumnsFor(uk.ac.ucl.excites.storage.model.Schema)
	 */
	@Override
	public Set<Column<?>> getFactoredOutColumnsFor(Schema schema)
	{
		Set<Column<?>> columns = new HashSet<Column<?>>();
		columns.add(schema.getColumn(Form.COLUMN_DEVICE_ID));
		return columns;
	}

}
