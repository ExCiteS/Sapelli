/**
 * 
 */
package uk.ac.ucl.excites.sapelli.transmission.db;

import uk.ac.ucl.excites.sapelli.shared.db.exceptions.DBException;
import uk.ac.ucl.excites.sapelli.storage.db.RecordStoreProvider;
import uk.ac.ucl.excites.sapelli.storage.model.Schema;
import uk.ac.ucl.excites.sapelli.storage.model.columns.ForeignKeyColumn;
import uk.ac.ucl.excites.sapelli.storage.model.columns.StringColumn;
import uk.ac.ucl.excites.sapelli.transmission.TransmissionClient;

/**
 * A store for Transmissions that have/are being sent.
 * 
 * @author mstevens
 */
public class SentTransmissionStore extends TransmissionStore
{
	
	public SentTransmissionStore(TransmissionClient client, RecordStoreProvider recordStoreProvider) throws DBException
	{
		super(client, recordStoreProvider);
	}

	@Override
	protected Schema getTransmissionSchema()
	{
		return SENT_TRANSMISSION_SCHEMA;
	}

	@Override
	protected StringColumn getCorrespondentColumn()
	{
		return SENT_TRANSMISSION_COLUMN_RECEIVER;
	}
	
	@Override
	protected Schema getTransmissionPartSchema()
	{
		return SENT_TRANSMISSION_PART_SCHEMA;
	}

	@Override
	protected ForeignKeyColumn getTransmissionPartTransmissionColumn()
	{
		return TRANSMISSION_PART_COLUMN_SENT_TRANSMISSION;
	}

}
