/**
 * Sapelli data collection platform: http://sapelli.org
 * 
 * Copyright 2012-2016 University College London - ExCiteS group
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

package uk.ac.ucl.excites.sapelli.collector.transmission.control;

import java.io.File;
import java.io.IOException;

import org.joda.time.DateTime;

import android.telephony.PhoneNumberUtils;
import uk.ac.ucl.excites.sapelli.collector.CollectorApp;
import uk.ac.ucl.excites.sapelli.collector.transmission.protocol.geokey.AndroidGeoKeyClient;
import uk.ac.ucl.excites.sapelli.shared.db.exceptions.DBException;
import uk.ac.ucl.excites.sapelli.shared.io.FileStorageException;
import uk.ac.ucl.excites.sapelli.shared.util.Logger;
import uk.ac.ucl.excites.sapelli.shared.util.android.AndroidLogger;
import uk.ac.ucl.excites.sapelli.storage.types.TimeStamp;
import uk.ac.ucl.excites.sapelli.transmission.control.TransmissionController;
import uk.ac.ucl.excites.sapelli.transmission.protocol.geokey.GeoKeyClient;

/**
 * 
 * @author benelliott, mstevens
 */
public class AndroidTransmissionController extends TransmissionController
{

	private final CollectorApp app;
	private AndroidGeoKeyClient gkClient;
	
	public AndroidTransmissionController(CollectorApp app) throws DBException
	{
		super(app.collectorClient);
		this.app = app;
		initialise();
	}

	/* (non-Javadoc)
	 * @see uk.ac.ucl.excites.sapelli.transmission.control.TransmissionController#getLogsFolder()
	 */
	@Override
	protected File getLogsFolder() throws FileStorageException
	{
		return app.getFileStorageProvider().getLogsFolder(true);
	}
	
	@Override
	protected Logger createLogger(File logsFolder) throws FileStorageException, IOException
	{
		return new AndroidLogger(logsFolder.getAbsolutePath(), LOG_FILENAME_PREFIX + DateTime.now().toString("yyyy-MM-dd"), false, true);
	}

	@Override
	public GeoKeyClient getGeoKeyClient()
	{
		if(gkClient == null)
			gkClient = new AndroidGeoKeyClient(app);
		return gkClient;
	}
	
	@Override
	protected String getApplicationInfo()
	{
		return app.getBuildInfo().getNameAndVersion() + " [" + app.getBuildInfo().getExtraVersionInfo() + "]";
	}
	
	static public boolean isValidPhoneNumber(String number)
	{
		return PhoneNumberUtils.isWellFormedSmsAddress(number);
	}
	
}
