/**
 * Sapelli data collection platform: http://sapelli.org
 * 
 * Copyright 2012-2015 University College London - ExCiteS group
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

package uk.ac.ucl.excites.sapelli.transmission.protocol.geokey;

import java.io.File;
import java.util.List;
import java.util.Map;

import uk.ac.ucl.excites.sapelli.shared.db.StoreHandle.StoreOperation;
import uk.ac.ucl.excites.sapelli.shared.db.exceptions.DBException;
import uk.ac.ucl.excites.sapelli.storage.eximport.ExportResult;
import uk.ac.ucl.excites.sapelli.storage.eximport.csv.CSVRecordsExporter;
import uk.ac.ucl.excites.sapelli.storage.eximport.csv.CSVRecordsExporter.Separator;
import uk.ac.ucl.excites.sapelli.storage.model.Attachment;
import uk.ac.ucl.excites.sapelli.storage.model.Model;
import uk.ac.ucl.excites.sapelli.storage.model.Record;
import uk.ac.ucl.excites.sapelli.storage.model.Schema;
import uk.ac.ucl.excites.sapelli.storage.util.UnknownModelException;
import uk.ac.ucl.excites.sapelli.transmission.TransmissionClient;
import uk.ac.ucl.excites.sapelli.transmission.db.TransmissionStore;
import uk.ac.ucl.excites.sapelli.transmission.model.Payload;
import uk.ac.ucl.excites.sapelli.transmission.model.content.AckPayload;
import uk.ac.ucl.excites.sapelli.transmission.model.content.ModelPayload;
import uk.ac.ucl.excites.sapelli.transmission.model.content.ModelRequestPayload;
import uk.ac.ucl.excites.sapelli.transmission.model.content.RecordsPayload;
import uk.ac.ucl.excites.sapelli.transmission.model.content.ResendRequestPayload;
import uk.ac.ucl.excites.sapelli.transmission.model.transport.geokey.GeoKeyAccount;
import uk.ac.ucl.excites.sapelli.transmission.model.transport.geokey.GeoKeyTransmission;

public abstract class GeoKeyClient implements Payload.Handler
{
	
	// STATIC -----------------------------------------------------------------
	static protected final String PATH_API_GEOKEY = "api/";
	static protected final String PATH_API_GEOKEY_SAPELLI = "api/sapelli/";
	
	static protected final String MINIMAL_GEOKEY_VERSION = "0.8.1"; // TODO change this
	static protected final String MINIMAL_GEOKEY_SAPELLI_VERSION = "0.6.7"; // TODO change this
	static protected final String EXTENSION_GEOKEY_SAPELLI = "geokey_sapelli";
	
	static protected final String JSON_KEY_GEOKEY = "geokey";
	static protected final String JSON_KEY_VERSION = "version";
	static protected final String JSON_KEY_INSTALLED_EXTENSIONS = "installed_extensions";
	static protected final String JSON_KEY_LOGGED_IN = "logged_in";
	static protected final String JSON_KEY_ERROR = "error";
	static protected final String JSON_VALUE_ERROR_NO_SUCH_PROJECT = "No such project";
	static protected final String JSON_KEY_ACCESS_TOKEN = "access_token";
	static protected final String JSON_KEY_TOKEN_TYPE = "token_type";
	static protected final String JSON_KEY_GEOKEY_PROJECT_ID = "geokey_project_id";
	static protected final String JSON_KEY_USER_DISPLAY_NAME = "display_name";
	static protected final String JSON_KEY_ADDED = "added";
	static protected final String JSON_KEY_UPDATED = "updated";
	static protected final String JSON_KEY_IGNORED_DUPS = "ignored_duplicates";
	static protected final String JSON_KEY_IGNORED_NO_LOC = "ignored_no_loc";
	static protected final String JSON_KEY_OBSERVATION_ID = "observation_id";
	static protected final String JSON_KEY_NAME = "name";
	
	static protected final String PARAMETER_KEY_CVS_FILE = "csv_file";
	static protected final String PARAMETER_KEY_FILE = "file";
	static protected final String PARAMETER_KEY_NAME = JSON_KEY_NAME;
	static protected final String PARAMETER_KEY_DESC = "description";
	
	/**
	 * geokey-sapelli only deals with comma-separated CSVs.
	 */
	static protected final CSVRecordsExporter.Separator CSV_SEPARATOR = Separator.COMMA;
	
	// DYNAMIC ----------------------------------------------------------------
	protected final TransmissionClient client;
	
	public GeoKeyClient(TransmissionClient client)
	{
		this.client = client;
	}
	
	/**
	 * @param account should not be {@code null}
	 * @return whether or not log-in succeeded
	 */
	public abstract boolean login(GeoKeyAccount account);
	
	/**
	 * Log-out current user.
	 */
	public abstract void logout();

	/**
	 * @return whether or not there is a currently logged-in user
	 */
	public abstract boolean isLoggedIn();
	
	/**
	 * @return the currently used {@link GeoKeyAccount} (or {@code null})
	 */
	public abstract GeoKeyAccount getAccount();

	/**
	 * @return
	 */
	protected abstract File getTempFolder();
	
	
	/**
	 * @param gkTransmission
	 */
	public void send(GeoKeyTransmission gkTransmission)
	{
		// Register we are attempting to send the transmission now:
		gkTransmission.getSentCallback().onSent();
		
		// Log sending attempt:
		client.logInfo("Attempting sending of GeokeyTransmission (id: " + gkTransmission.getLocalID() + ")...");
		
		// Try to login on server:
		final GeoKeyAccount account = gkTransmission.getCorrespondent();
		if(account == null || !login(account))
		{
			return; // !!!
		}		
		//else: we are now logged in...
		
		// Save account (new access_token / display_name may have been set):
		client.transmissionStoreHandle.executeNoEx(new StoreOperation<TransmissionStore, DBException>()
		{
			@Override
			public void execute(TransmissionStore store) throws DBException
			{
				store.store(account);
			}
		});
					
		// Handle payload:
		try
		{
			gkTransmission.getPayload().handle(this); // expected to throw Exception if something goes wrong
		}
		catch(Exception e)
		{
			return; // !!!
		}
					
		// Mark as received if successful:
		gkTransmission.getSentCallback().onReceived();
		
		// Log success:
		client.logInfo("Successfully sent GeokeyTransmission (id: " + gkTransmission.getLocalID() + ")");
	}
	
	/**
	 * @param model
	 * @return a {@link ModelSession} object or {@code null}
	 * @throws UnknownModelException when the server knows no such model
	 */
	protected abstract ModelSession getModelSession(Model model) throws UnknownModelException;

	/* (non-Javadoc)
	 * @see uk.ac.ucl.excites.sapelli.transmission.model.Payload.Handler#handle(uk.ac.ucl.excites.sapelli.transmission.model.content.RecordsPayload)
	 */
	@Override
	public void handle(RecordsPayload recordsPayload) throws Exception
	{
		// Keep track of (full) success:
		boolean success = true;
		
		// Open session:
		ModelSession session = null;
		
		try 
		{
			session = getModelSession(recordsPayload.getModel());
		}
		catch(UnknownModelException ume)
		{
			 // TODO request model?
		}
		
		// Check session:
		if(session == null)
			success = false;
		else
		{
			// Upload records (as CSV) per Schema:
			CSVRecordsExporter exporter = new CSVRecordsExporter(getTempFolder(), CSV_SEPARATOR);
			for(Map.Entry<Schema, List<Record>> entry : recordsPayload.getRecordsBySchema().entrySet())
			{
				// Generate CSV file:
				ExportResult result = exporter.export(entry.getValue(), "for_upload_" + entry.getKey().getName());
				
				// Upload if CSV was generated:
				success &= result.wasSuccessful() && session.uploadCSV(result.getFiles().get(0), true, true);
			}
			
			// Upload attachments:
			for(Record record : recordsPayload.getRecords())
			{
				// Get files:
				List<? extends Attachment> attachments = client.getRecordAttachments(record);
				
				// Upload files:
				if(attachments != null && !attachments.isEmpty())
					success &= session.uploadAttachments(record, attachments);
			}
		}
		
		if(!success)
			throw new Exception("Failed to handle RecordsPayload without errors.");
	}

	/* (non-Javadoc)
	 * @see uk.ac.ucl.excites.sapelli.transmission.model.Payload.Handler#handle(uk.ac.ucl.excites.sapelli.transmission.model.content.ModelPayload)
	 */
	@Override
	public void handle(ModelPayload projectModelPayload) throws Exception
	{
		// TODO perhaps implement using SAP file upload (geokey-sapelli has an API end-point for that)
	}

	/* (non-Javadoc)
	 * @see uk.ac.ucl.excites.sapelli.transmission.model.Payload.Handler#handle(uk.ac.ucl.excites.sapelli.transmission.model.Payload, int)
	 */
	@Override
	public void handle(Payload customPayload, int type) throws Exception
	{
		// Does nothing (for now)
	}
	
	/* (non-Javadoc)
	 * @see uk.ac.ucl.excites.sapelli.transmission.model.Payload.Handler#handle(uk.ac.ucl.excites.sapelli.transmission.model.content.AckPayload)
	 */
	@Override
	public void handle(AckPayload ackPayload) throws Exception
	{
		// N/A
	}

	/* (non-Javadoc)
	 * @see uk.ac.ucl.excites.sapelli.transmission.model.Payload.Handler#handle(uk.ac.ucl.excites.sapelli.transmission.model.content.ResendRequestPayload)
	 */
	@Override
	public void handle(ResendRequestPayload resendRequestPayload) throws Exception
	{
		// N/A
	}

	/* (non-Javadoc)
	 * @see uk.ac.ucl.excites.sapelli.transmission.model.Payload.Handler#handle(uk.ac.ucl.excites.sapelli.transmission.model.content.ModelRequestPayload)
	 */
	@Override
	public void handle(ModelRequestPayload modelRequestPayload) throws Exception
	{
		// N/A
	}
	
	/**
	 * @author mstevens
	 */
	protected interface ModelSession
	{

		/**
		 * @param csvFile
		 * @param deleteUponSuccess
		 * @param deleteUponFailure
		 * @return whether or not uploading was successful
		 */
		public boolean uploadCSV(File csvFile, boolean deleteUponSuccess, boolean deleteUponFailure);
		
		/**
		 * @param record
		 * @param attachments
		 * @return whether or not uploading was successful
		 */
		public boolean uploadAttachments(Record record, List<? extends Attachment> attachments);
		
	}
	
}
