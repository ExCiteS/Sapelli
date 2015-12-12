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

package uk.ac.ucl.excites.sapelli.collector.transmission.protocol.geokey;

import java.util.List;
import java.util.Map;

import android.app.Activity;
import android.content.Context;
import uk.ac.ucl.excites.sapelli.collector.CollectorApp;
import uk.ac.ucl.excites.sapelli.collector.R;
import uk.ac.ucl.excites.sapelli.collector.model.Project;
import uk.ac.ucl.excites.sapelli.collector.transmission.protocol.geokey.GeoKeySapelliSession.ProjectSession;
import uk.ac.ucl.excites.sapelli.collector.util.AsyncTaskWithWaitingDialog;
import uk.ac.ucl.excites.sapelli.shared.util.android.DeviceControl;
import uk.ac.ucl.excites.sapelli.storage.model.Record;
import uk.ac.ucl.excites.sapelli.storage.model.Schema;
import uk.ac.ucl.excites.sapelli.transmission.model.content.RecordsPayload;
import uk.ac.ucl.excites.sapelli.transmission.model.transport.geokey.GeoKeyAccount;
import uk.ac.ucl.excites.sapelli.transmission.model.transport.geokey.GeoKeyTransmission;
import uk.ac.ucl.excites.sapelli.transmission.protocol.geokey.GeoKeyClient;

public class GeoKeySapelliClient implements GeoKeyClient
{
	
	private CollectorApp app;
	
	public GeoKeySapelliClient(CollectorApp app)
	{
		this.app = app;
	}
	
	static String addTrailingSlash(String url)
	{
		return url + (url.endsWith("/") ? "" : "/");
	}

	@Override
	public void send(GeoKeyTransmission gkTransmission)
	{
		GeoKeySapelliSession userSession = new GeoKeySapelliSession(app, gkTransmission.getCorrespondent());
		if(userSession.login())
		{
			if(gkTransmission.getPayloadType() != RecordsPayload.GetType())
				return; // should not happen (for now)
			
			RecordsPayload payload = (RecordsPayload) gkTransmission.getPayload();
			Project project = app.collectorClient.getProject(payload.getModel());
			
			ProjectSession projectSession = userSession.openProjectSession(project);
			
			if(projectSession == null)
				return;
			
			for(Map.Entry<Schema, List<Record>> entry : payload.getRecordsBySchema().entrySet())
			{
				
				//projectSession.uploadRecords(entry.getValue());
			}
			
			// TODO upload attachments
		}
	}
	
}
