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
 * A store for Transmissions that have/are being received.
 * 
 * @author mstevens
 */
public class ReceivedTransmissionStore extends TransmissionStore
{
	
	public ReceivedTransmissionStore(TransmissionClient client, RecordStoreProvider recordStoreProvider) throws DBException
	{
		super(client, recordStoreProvider);
	}

	@Override
	protected Schema getTransmissionSchema()
	{
		return RECEIVED_TRANSMISSION_SCHEMA;
	}

	@Override
	protected StringColumn getCorrespondentColumn()
	{
		return RECEIVED_TRANSMISSION_COLUMN_SENDER;
	}
	
	@Override
	protected Schema getTransmissionPartSchema()
	{
		return RECEIVED_TRANSMISSION_PART_SCHEMA;
	}

	@Override
	protected ForeignKeyColumn getTransmissionPartTransmissionColumn()
	{
		return TRANSMISSION_PART_COLUMN_RECEIVED_TRANSMISSION;
	}

}
