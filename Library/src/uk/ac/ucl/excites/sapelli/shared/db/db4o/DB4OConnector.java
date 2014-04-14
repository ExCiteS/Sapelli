/**
 * 
 */
package uk.ac.ucl.excites.sapelli.shared.db.db4o;

import java.io.File;
import java.io.IOException;

import uk.ac.ucl.excites.sapelli.collector.db.db4o.DB4ODataAccess;
import uk.ac.ucl.excites.sapelli.shared.util.io.FileHelpers;

import com.db4o.Db4oEmbedded;
import com.db4o.ObjectContainer;
import com.db4o.config.EmbeddedConfiguration;

/**
 * Helper class to open conenctions to DB4O ObjectContainers
 * 
 * @author mstevens
 */
public final class DB4OConnector
{
	
	static public final String DB4O_FILE_EXTENSION = "db4o";
	
	static public File getFile(File folder, String filenameWithoutExtension) throws IOException
	{
		if(!FileHelpers.createFolder(folder))
			throw new IOException("Path does not exist and could not be created: " + folder.getAbsolutePath());
		return new File(folder.getAbsolutePath() + File.separator + filenameWithoutExtension + '.' + DB4O_FILE_EXTENSION);
	}
	
	static public ObjectContainer open(File db4oFile, Class<?>... cascadeClasses) throws Exception
	{
		// Configure the db:
		EmbeddedConfiguration dbConfig = Db4oEmbedded.newConfiguration();
		//dbConfig.file().readOnly(readOnly);
		dbConfig.common().updateDepth(DB4ODataAccess.UPDATE_DEPTH);
		dbConfig.common().exceptionsOnNotStorable(true);
		if(cascadeClasses != null)
			for(Class<?> clazz : cascadeClasses)
			{
				dbConfig.common().objectClass(clazz).cascadeOnActivate(true);
				dbConfig.common().objectClass(clazz).cascadeOnUpdate(true);
				dbConfig.common().objectClass(clazz).cascadeOnDelete(true);
			}
		// Open the db:
		return Db4oEmbedded.openFile(dbConfig, db4oFile.getAbsolutePath()); // (throws various exceptions)		
	}
	
	private DB4OConnector() {} //this class should not be instantiated

}
