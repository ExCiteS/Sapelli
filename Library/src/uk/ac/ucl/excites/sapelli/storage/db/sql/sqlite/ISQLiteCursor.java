package uk.ac.ucl.excites.sapelli.storage.db.sql.sqlite;

/**
 * The methods of the interface deliberately matches the signatures of methods in {@link android.database.sqlite.SQLiteCursor}.
 * This is useful in AndroidSQLiteRecordStore, there we use a custom CursorFactor which creates instances of our own SQLiteCursor subclass
 * which implements ISQLiteCursor but doesn't have to implement the methods because they already exist in SQLiteCursor.
 * The purpose of all of this is to allow the (Android-agnostic) SQLiteColumn subclasses to call these methods on cursors.
 * 
 * @author mstevens
 *
 */
public interface ISQLiteCursor
{

	public byte[] getBlob(int columnIdx);
	
	public long getLong(int columnIdx);
	
	public double getDouble(int columnIdx);
	
	public String getString(int columnIdx);
	
	public boolean isNull(int columnIdx);

}
