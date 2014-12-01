/**
 * Sapelli data collection platform: http://sapelli.org
 * 
 * Copyright 2012-2014 University College London - ExCiteS group
 * 
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 * 
 *     http://www.apache.org/licenses/LICENSE-2.0
 * 
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and 
 * limitations under the License.
 */

package uk.ac.ucl.excites.sapelli.shared.db.db4o;

import java.io.File;
import java.io.IOException;

import uk.ac.ucl.excites.sapelli.shared.io.FileHelpers;

import com.db4o.Db4oEmbedded;
import com.db4o.ObjectContainer;
import com.db4o.config.EmbeddedConfiguration;

/**
 * Helper class to open connections to DB4O ObjectContainers
 * 
 * @author mstevens
 */
public final class DB4OConnector
{
	
	static public final int ACTIVATION_DEPTH = 40;
	static public final int UPDATE_DEPTH = 40;
	static public final String DB4O_FILE_EXTENSION = "db4o";
	
	static public File getFile(File folder, String filenameWithoutExtension) throws IOException
	{
		if(!FileHelpers.createDirectory(folder))
			throw new IOException("Path does not exist and could not be created: " + folder.getAbsolutePath());
		return new File(folder.getAbsolutePath() + File.separator + filenameWithoutExtension + '.' + DB4O_FILE_EXTENSION);
	}
	
	static public ObjectContainer open(File db4oFile, Class<?>... cascadeClasses) throws Exception
	{
		ObjectContainer objCont = null;
		try
		{
			// Configure the db:
			EmbeddedConfiguration dbConfig = Db4oEmbedded.newConfiguration();
			//dbConfig.file().readOnly(readOnly);
			dbConfig.common().updateDepth(UPDATE_DEPTH);
			dbConfig.common().exceptionsOnNotStorable(true);
			if(cascadeClasses != null)
				for(Class<?> clazz : cascadeClasses)
				{
					dbConfig.common().objectClass(clazz).cascadeOnActivate(true);
					dbConfig.common().objectClass(clazz).cascadeOnUpdate(true);
					dbConfig.common().objectClass(clazz).cascadeOnDelete(true);
				}
			// Open the db:
			objCont = Db4oEmbedded.openFile(dbConfig, db4oFile.getAbsolutePath()); // (throws various exceptions)
		}
		catch(Exception e)
		{
			System.err.println("DB4OConnector: failed to open connection to " + db4oFile.getAbsolutePath());
			throw e;
		}
		System.out.println("DB4OConnector: opened connection to " + db4oFile.getAbsolutePath());
		return objCont;
	}
	
	private DB4OConnector() {} //this class should not be instantiated

}
