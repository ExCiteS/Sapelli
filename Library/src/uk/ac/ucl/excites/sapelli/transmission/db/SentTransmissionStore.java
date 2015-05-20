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

import uk.ac.ucl.excites.sapelli.shared.db.exceptions.DBException;
import uk.ac.ucl.excites.sapelli.storage.model.Schema;
import uk.ac.ucl.excites.sapelli.storage.model.columns.ForeignKeyColumn;
import uk.ac.ucl.excites.sapelli.transmission.TransmissionClient;

/**
 * A store for Transmissions that have/are being sent.
 * 
 * @author mstevens
 */
public class SentTransmissionStore extends TransmissionStore
{
	
	public SentTransmissionStore(TransmissionClient client) throws DBException
	{
		super(client);
	}

	@Override
	protected Schema getTransmissionSchema()
	{
		return SENT_TRANSMISSION_SCHEMA;
	}

	@Override
	protected ForeignKeyColumn getCorrespondentColumn()
	{
		return SENT_TRANSMISSION_COLUMN_RECEIVER;
	}
	
	@Override
	protected Schema getTransmissionPartSchema()
	{
		return SENT_TRANSMISSION_PART_SCHEMA;
	}

	@Override
	protected ForeignKeyColumn getTransmissionPartTransmissionColumn()
	{
		return TRANSMISSION_PART_COLUMN_SENT_TRANSMISSION;
	}

	@Override
	protected Schema getCorrespondentSchema()
	{
		return RECEIVER_SCHEMA;
	}

	@Override
	protected boolean isReceivingSide()
	{
		return false;
	}

}
