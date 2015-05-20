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

package uk.ac.ucl.excites.sapelli.transmission.db;

import java.util.ArrayList;
import java.util.List;

import uk.ac.ucl.excites.sapelli.shared.db.exceptions.DBException;
import uk.ac.ucl.excites.sapelli.storage.model.Schema;
import uk.ac.ucl.excites.sapelli.storage.model.columns.ForeignKeyColumn;
import uk.ac.ucl.excites.sapelli.storage.queries.RecordsQuery;
import uk.ac.ucl.excites.sapelli.storage.queries.Source;
import uk.ac.ucl.excites.sapelli.storage.queries.constraints.EqualityConstraint;
import uk.ac.ucl.excites.sapelli.transmission.TransmissionClient;
import uk.ac.ucl.excites.sapelli.transmission.model.Transmission;
import uk.ac.ucl.excites.sapelli.transmission.model.transport.sms.SMSTransmission;

/**
 * A store for Transmissions that have been/are being received.
 * 
 * @author mstevens, benelliott
 */
public class ReceivedTransmissionStore extends TransmissionStore
{
	
	public ReceivedTransmissionStore(TransmissionClient client) throws DBException
	{
		super(client);
	}

	@Override
	protected Schema getTransmissionSchema()
	{
		return RECEIVED_TRANSMISSION_SCHEMA;
	}

	@Override
	protected ForeignKeyColumn getCorrespondentColumn()
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

	@Override
	protected Schema getCorrespondentSchema()
	{
		return SENDER_SCHEMA;
	}
	
	/**
	 * Returns a list of incomplete SMSTransmissions.
	 * 
	 * Note: this only deals with SMSTransmissions as an HTTPTransmission cannot (yet) be incomplete.
	 * 
	 * @return a list of incomplete SMSTransmissions
	 */
	public List<SMSTransmission<?>> getIncompleteSMSTransmissions()
	{
		List<SMSTransmission<?>> incompleteSMSTs = new ArrayList<SMSTransmission<?>>();
		
		// query DB for transmissions which are incomplete (have "null" as their receivedAt value):
		for(Transmission<?> t : retrieveTransmissions(new RecordsQuery(Source.From(getTransmissionSchema()), EqualityConstraint.IsNull(COLUMN_RECEIVED_AT))))
			if(t instanceof SMSTransmission)
				incompleteSMSTs.add((SMSTransmission<?>) t); // cast these transmissions as SMSTransmissions
		
		 return incompleteSMSTs;
	}

	@Override
	protected boolean isReceivingSide()
	{
		return true;
	}
	
}
