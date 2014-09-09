package uk.ac.ucl.excites.sapelli.storage.db.sql.sqlite;

public interface ISQLiteCursor
{

	public byte[] getBlob(int columnIdx);
	
	public long getLong(int columnIdx);
	
	public double getDouble(int columnIdx);
	
	public String getString(int columnIdx);
	
	public boolean isNull(int columnIdx);
	
	

}
