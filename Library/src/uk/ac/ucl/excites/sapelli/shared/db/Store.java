package uk.ac.ucl.excites.sapelli.shared.db;

import java.io.File;

public interface Store
{

	public void finalise();
	
	public void backup(File destinationFolder) throws Exception;
	
}
