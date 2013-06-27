/**
 * 
 */
package uk.ac.ucl.excites.collector.database;

import uk.ac.ucl.excites.collector.project.model.Project;
import uk.ac.ucl.excites.storage.model.Record;
import uk.ac.ucl.excites.transmission.Transmission;

import com.db4o.Db4oEmbedded;
import com.db4o.ObjectContainer;
import com.db4o.config.EmbeddedConfiguration;

/**
 * @author mstevens
 *
 */
public final class DB4OConnector
{
	
	private DB4OConnector() {} //this class should not be instatiated
	
	static public ObjectContainer open(String filepath) throws Exception
	{
		// Configure the db:
		EmbeddedConfiguration dbConfig = Db4oEmbedded.newConfiguration();
		dbConfig.common().updateDepth(DataAccess.UPDATE_DEPTH);
		dbConfig.common().exceptionsOnNotStorable(true);
		dbConfig.common().objectClass(Record.class).cascadeOnActivate(true);
		dbConfig.common().objectClass(Record.class).cascadeOnUpdate(true);
		dbConfig.common().objectClass(Transmission.class).cascadeOnActivate(true);
		dbConfig.common().objectClass(Transmission.class).cascadeOnUpdate(true);
		dbConfig.common().objectClass(Project.class).cascadeOnActivate(true);
		dbConfig.common().objectClass(Project.class).cascadeOnUpdate(true);
		// Open the db:
		return Db4oEmbedded.openFile(dbConfig, filepath); // (throws various exceptions)		
	}

}
