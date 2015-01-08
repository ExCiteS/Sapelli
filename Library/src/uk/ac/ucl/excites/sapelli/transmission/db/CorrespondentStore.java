package uk.ac.ucl.excites.sapelli.transmission.db;

import java.io.File;

import uk.ac.ucl.excites.sapelli.collector.db.ProjectRecordStore;
import uk.ac.ucl.excites.sapelli.shared.db.Store;
import uk.ac.ucl.excites.sapelli.shared.db.StoreBackuper;
import uk.ac.ucl.excites.sapelli.shared.db.exceptions.DBException;
import uk.ac.ucl.excites.sapelli.storage.model.Model;
import uk.ac.ucl.excites.sapelli.storage.model.Schema;
import uk.ac.ucl.excites.sapelli.storage.model.columns.BooleanColumn;
import uk.ac.ucl.excites.sapelli.storage.model.columns.ForeignKeyColumn;
import uk.ac.ucl.excites.sapelli.storage.model.columns.IntegerColumn;
import uk.ac.ucl.excites.sapelli.storage.model.columns.StringColumn;
import uk.ac.ucl.excites.sapelli.storage.model.indexes.AutoIncrementingPrimaryKey;
import uk.ac.ucl.excites.sapelli.transmission.model.Correspondent;
import uk.ac.ucl.excites.sapelli.transmission.model.Receiver;
import uk.ac.ucl.excites.sapelli.transmission.model.Sender;

public class CorrespondentStore implements Store
{
	// TODO correspondent client?
	
	// STATICS---------------------------------------------	
	// Correspondent storage model:
	//	Model:
	static public final Model CORRESPONDENT_MANAGEMENT_MODEL = new Model(0, "CorrespondentManagement"); // TODO model id -- maybe put in transmission client
	// Schema(s) & columns:
	//	Correspondent Schema
	static final public Schema CORRESPONDENT_SCHEMA = new Schema(CORRESPONDENT_MANAGEMENT_MODEL, "Correspondent");
	static final public IntegerColumn CORRESPONDENT_COLUMN_ID = new IntegerColumn("ID", false, Correspondent.CORRESPONDENT_ID_FIELD);
	static final public StringColumn CORRESPONDENT_COLUMN_NAME = new StringColumn("Name", false, Correspondent.CORRESPONDENT_NAME_MAX_LENGTH_BYTES);
	static final public StringColumn CORRESPONDENT_COLUMN_ADDRESS = new StringColumn("PhoneNum", true, Correspondent.CORRESPONDENT_ADDRESS_MAX_LENGTH_BYTES);
	static final public StringColumn CORRESPONDENT_COLUMN_ENCRYPTION_KEY = new StringColumn("Key", false, Correspondent.CORRESPONDENT_ENCRYPTION_KEY_MAX_LENGTH_BYTES);
	//	Add columns and index to Correspondent Schema & seal it:
	static
	{
		CORRESPONDENT_SCHEMA.addColumn(CORRESPONDENT_COLUMN_ID);
		CORRESPONDENT_SCHEMA.addColumn(CORRESPONDENT_COLUMN_NAME);
		CORRESPONDENT_SCHEMA.addColumn(CORRESPONDENT_COLUMN_ADDRESS);
		CORRESPONDENT_SCHEMA.addColumn(CORRESPONDENT_COLUMN_ENCRYPTION_KEY);
		CORRESPONDENT_SCHEMA.setPrimaryKey(new AutoIncrementingPrimaryKey("IDIdx", CORRESPONDENT_COLUMN_ID));
		CORRESPONDENT_SCHEMA.seal();
	}
	// Receiver Schema
	static final public Schema RECEIVER_SCHEMA = new Schema(CORRESPONDENT_MANAGEMENT_MODEL, "Receiver");
	static final public IntegerColumn RECEIVER_COLUMN_ID = new IntegerColumn("ID", false, Receiver.RECEIVER_ID_FIELD);
	static final public ForeignKeyColumn RECEIVER_COLUMN_PROJECT_ID = new ForeignKeyColumn("ProjectID", ProjectRecordStore.PROJECT_SCHEMA, false);
	static final public ForeignKeyColumn RECEIVER_COLUMN_CORRESPONDENT_NAME = new ForeignKeyColumn("CorrespondentName", CORRESPONDENT_SCHEMA, false);
	static final public IntegerColumn RECEIVER_COLUMN_TRANSMISSION_TYPE = new IntegerColumn("TransmissionType", false, false, Integer.SIZE);
	static final public IntegerColumn RECEIVER_COLUMN_RETRANSMIT_INTERVAL = new IntegerColumn("RetransmitInterval", false, false, Receiver.RETRANSMIT_INTERVAL_SIZE_BITS);
	static final public BooleanColumn RECEIVER_COLUMN_ENCRYPT = new BooleanColumn("Encrypt", false);
	//	Add columns to Receiver Schema & seal it:
	static
	{
		RECEIVER_SCHEMA.addColumn(RECEIVER_COLUMN_ID);
		RECEIVER_SCHEMA.addColumn(RECEIVER_COLUMN_PROJECT_ID);
		RECEIVER_SCHEMA.addColumn(RECEIVER_COLUMN_CORRESPONDENT_NAME);
		RECEIVER_SCHEMA.addColumn(RECEIVER_COLUMN_TRANSMISSION_TYPE);
		RECEIVER_SCHEMA.addColumn(RECEIVER_COLUMN_RETRANSMIT_INTERVAL);
		RECEIVER_SCHEMA.addColumn(RECEIVER_COLUMN_ENCRYPT);
		RECEIVER_SCHEMA.setPrimaryKey(new AutoIncrementingPrimaryKey("IDIdx", RECEIVER_COLUMN_ID));
		RECEIVER_SCHEMA.seal();
	}
	// Sender Schema
	static final public Schema SENDER_SCHEMA = new Schema(CORRESPONDENT_MANAGEMENT_MODEL, "Receiver");
	static final public IntegerColumn SENDER_COLUMN_ID = new IntegerColumn("ID", false, Sender.SENDER_ID_FIELD);
	static final public ForeignKeyColumn SENDER_COLUMN_PROJECT_ID = new ForeignKeyColumn("ProjectID", ProjectRecordStore.PROJECT_SCHEMA, false);
	static final public ForeignKeyColumn SENDER_COLUMN_CORRESPONDENT_NAME = new ForeignKeyColumn("CorrespondentName", CORRESPONDENT_SCHEMA, false);
	static final public BooleanColumn SENDER_COLUMN_ACK = new BooleanColumn("Ack", false);
	// Add columns to Sender Schema and seal it:
	static
	{
		SENDER_SCHEMA.addColumn(SENDER_COLUMN_ID);
		SENDER_SCHEMA.addColumn(SENDER_COLUMN_PROJECT_ID);
		SENDER_SCHEMA.addColumn(SENDER_COLUMN_CORRESPONDENT_NAME);
		SENDER_SCHEMA.addColumn(SENDER_COLUMN_ACK);
		SENDER_SCHEMA.setPrimaryKey(new AutoIncrementingPrimaryKey("IDIdx", SENDER_COLUMN_ID));
		SENDER_SCHEMA.seal();
		
		// seal the model too:
		CORRESPONDENT_MANAGEMENT_MODEL.seal();
	}
	
	@Override
	public void finalise() throws DBException
	{
		// TODO Auto-generated method stub

	}

	@Override
	public void backup(StoreBackuper backuper, File destinationFolder) throws DBException
	{
		// TODO Auto-generated method stub

	}

}
