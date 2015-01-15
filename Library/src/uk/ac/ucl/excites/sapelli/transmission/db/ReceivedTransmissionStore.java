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
import uk.ac.ucl.excites.sapelli.storage.model.columns.StringColumn;
import uk.ac.ucl.excites.sapelli.storage.queries.RecordsQuery;
import uk.ac.ucl.excites.sapelli.storage.queries.Source;
import uk.ac.ucl.excites.sapelli.storage.queries.constraints.EqualityConstraint;
import uk.ac.ucl.excites.sapelli.storage.types.TimeStamp;
import uk.ac.ucl.excites.sapelli.transmission.Transmission;
import uk.ac.ucl.excites.sapelli.transmission.TransmissionClient;
import uk.ac.ucl.excites.sapelli.transmission.modes.sms.Message;
import uk.ac.ucl.excites.sapelli.transmission.modes.sms.SMSTransmission;

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

	/**
	 * Returns a list of incomplete SMSTransmissions whose constituent parts are all at least {@code minAgeMillis} old.
	 * @param minAgeMillis the minimum age for all parts in a transmission in order for that transmission to be returned (e.g. only return transmissions where all parts are at least 3000 milliseconds old)
	 * @return a list of incomplete SMSTransmissions
	 */
	public List<SMSTransmission<?>> getIncompleteSMSTransmissions(long minAgeMillis)
	{
		// query DB for transmissions which are incomplete (have "null" as their receivedAt value):
		List<Transmission> incompleteTransmissions = retrieveTransmissionsForQuery(
				new RecordsQuery(Source.From(getTransmissionSchema()), EqualityConstraint.IsNull(COLUMN_RECEIVED_AT)));
		
		// cast these transmissions as SMSTransmissions (an HTTPTransmission cannot yet be incomplete) and check if they are old enough for a resend request:
		List<SMSTransmission<?>> sufficientlyOldTransmissions = new ArrayList<SMSTransmission<?>>();

		// if a single part is too young then we will not send a resend request yet:
		TimeStamp minAge = new TimeStamp(System.currentTimeMillis() - minAgeMillis);
		
		outer: for (Transmission transmission : incompleteTransmissions)
		{ // for each incomplete transaction...
			for (Message message : ((SMSTransmission<?>) transmission).getParts())
				// for each message, check that it is old enough:
				if (message.getReceivedAt().isAfter(minAge))
					// if not then stop checking this transmission:
					continue outer;
			// if all of the messages in the transmission were old enough then add it to the list:
			sufficientlyOldTransmissions.add(((SMSTransmission<?>)transmission));
		}

		return sufficientlyOldTransmissions;
	}
}
