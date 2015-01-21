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

package uk.ac.ucl.excites.sapelli.transmission.control;

import java.io.IOException;

import org.joda.time.DateTime;

import uk.ac.ucl.excites.sapelli.collector.io.FileStorageException;
import uk.ac.ucl.excites.sapelli.collector.io.FileStorageProvider;
import uk.ac.ucl.excites.sapelli.collector.util.AndroidLogger;
import uk.ac.ucl.excites.sapelli.shared.db.exceptions.DBException;
import uk.ac.ucl.excites.sapelli.shared.util.Logger;
import uk.ac.ucl.excites.sapelli.transmission.TransmissionClient;
import uk.ac.ucl.excites.sapelli.transmission.model.transport.http.HTTPClient;
import uk.ac.ucl.excites.sapelli.transmission.model.transport.sms.SMSSender;
import uk.ac.ucl.excites.sapelli.transmission.sender.gsm.AndroidSMSSender;
import android.content.Context;

/**
 * 
 * @author benelliott, mstevens
 */
public class AndroidTransmissionController extends TransmissionController
{

	private Context context;
	private AndroidSMSSender smsSender;
	
	public AndroidTransmissionController(TransmissionClient transmissionClient, FileStorageProvider fileStorageProvider, Context context) throws DBException
	{
		super(transmissionClient, fileStorageProvider);
		this.context = context;
	}

	@Override
	public SMSSender getSMSService()
	{
		if (smsSender == null)
			smsSender = new AndroidSMSSender(context);
		return smsSender;
	}

	@Override
	public HTTPClient getHTTPClient()
	{
		// TODO create Android HTTP client
		return null;
	}

	@Override
	protected Logger createLogger(FileStorageProvider fileStorageProvider) throws FileStorageException, IOException
	{
		return new AndroidLogger(fileStorageProvider.getLogsFolder(true).getAbsolutePath(), LOG_FILENAME_PREFIX + DateTime.now().toString("yyyy-mm-dd"), false, true);
	}
}
