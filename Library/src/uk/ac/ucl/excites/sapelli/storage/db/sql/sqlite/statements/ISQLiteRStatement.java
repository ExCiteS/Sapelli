package uk.ac.ucl.excites.sapelli.storage.db.sql.sqlite.statements;

import uk.ac.ucl.excites.sapelli.shared.db.DBException;
import uk.ac.ucl.excites.sapelli.storage.db.sql.sqlite.ISQLiteCursor;
import uk.ac.ucl.excites.sapelli.storage.db.sql.sqlite.ISQLiteStatement;

public interface ISQLiteRStatement extends ISQLiteStatement
{

	public ISQLiteCursor executeR() throws DBException;
	
}
