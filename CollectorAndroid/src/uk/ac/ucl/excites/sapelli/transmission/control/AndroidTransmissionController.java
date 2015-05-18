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

import uk.ac.ucl.excites.sapelli.collector.CollectorApp;
import uk.ac.ucl.excites.sapelli.collector.io.FileStorageException;
import uk.ac.ucl.excites.sapelli.collector.io.FileStorageProvider;
import uk.ac.ucl.excites.sapelli.shared.db.exceptions.DBException;
import uk.ac.ucl.excites.sapelli.shared.util.Logger;
import uk.ac.ucl.excites.sapelli.transmission.protocol.http.HTTPClient;
import uk.ac.ucl.excites.sapelli.transmission.protocol.sms.AndroidSMSSender;
import uk.ac.ucl.excites.sapelli.transmission.protocol.sms.SMSSender;
import uk.ac.ucl.excites.sapelli.util.AndroidLogger;

/**
 * 
 * @author benelliott, mstevens
 */
public class AndroidTransmissionController extends TransmissionController
{

	private final CollectorApp app;
	private AndroidSMSSender smsSender;
	
	public AndroidTransmissionController(CollectorApp app) throws DBException
	{
		super(app.collectorClient, app.getFileStorageProvider());
		this.app = app;
		initialise();
	}

	@Override
	public SMSSender getSMSService()
	{
		if(smsSender == null)
			smsSender = new AndroidSMSSender(this, app);
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
	
	@Override
	protected String getApplicationInfo()
	{
		return app.getBuildInfo().getNameAndVersion() + " [" + app.getBuildInfo().getExtraVersionInfo() + "]";
	}
	
}
