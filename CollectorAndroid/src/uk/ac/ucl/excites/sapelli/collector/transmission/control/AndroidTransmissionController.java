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
import uk.ac.ucl.excites.sapelli.collector.transmission.protocol.sms.in.IncomingSMSReceiverService;
import uk.ac.ucl.excites.sapelli.collector.transmission.protocol.sms.out.AndroidSMSClient;
import uk.ac.ucl.excites.sapelli.shared.db.exceptions.DBException;
import uk.ac.ucl.excites.sapelli.shared.io.FileStorageException;
import uk.ac.ucl.excites.sapelli.shared.util.Logger;
import uk.ac.ucl.excites.sapelli.shared.util.android.AndroidLogger;
import uk.ac.ucl.excites.sapelli.storage.types.TimeStamp;
import uk.ac.ucl.excites.sapelli.transmission.control.TransmissionController;
import uk.ac.ucl.excites.sapelli.transmission.protocol.geokey.GeoKeyClient;
import uk.ac.ucl.excites.sapelli.transmission.protocol.sms.SMSClient;

/**
 * 
 * @author benelliott, mstevens
 */
public class AndroidTransmissionController extends TransmissionController
{

	private final CollectorApp app;
	private AndroidSMSClient smsClient;
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
		return new AndroidLogger(logsFolder.getAbsolutePath(), LOG_FILENAME_PREFIX + DateTime.now().toString("yyyy-mm-dd"), false, true);
	}

	@Override
	public SMSClient getSMSClient()
	{
		if(smsClient == null)
			smsClient = new AndroidSMSClient(app);
		return smsClient;
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

	/**
	 * @param localID local ID of an incomplete SMSTransmission
	 * @param time at which to send the request, or null (in which case no request will be scheduled)
	 */
	public synchronized void scheduleSMSResendRequest(int localID, TimeStamp time)
	{
		IncomingSMSReceiverService.ScheduleResendRequest(app, localID, time.toDateTime());
	}
	
	/**
	 * @param localID local ID of an incomplete SMSTransmission
	 */
	protected synchronized void cancelSMSResendRequest(int localID)
	{
		IncomingSMSReceiverService.CancelResendRequest(app, localID);
	}
	
	static public boolean isValidPhoneNumber(String number)
	{
		return PhoneNumberUtils.isWellFormedSmsAddress(number);
	}
	
}
