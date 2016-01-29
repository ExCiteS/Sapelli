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

package uk.ac.ucl.excites.sapelli.collector.util;

import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import uk.ac.ucl.excites.sapelli.collector.io.FileStorageProvider;
import uk.ac.ucl.excites.sapelli.collector.model.Field;
import uk.ac.ucl.excites.sapelli.collector.model.Form;
import uk.ac.ucl.excites.sapelli.collector.model.MediaFile;
import uk.ac.ucl.excites.sapelli.collector.model.Project;
import uk.ac.ucl.excites.sapelli.collector.model.fields.MediaField;
import uk.ac.ucl.excites.sapelli.shared.io.FileHelpers;
import uk.ac.ucl.excites.sapelli.storage.model.Record;
import uk.ac.ucl.excites.sapelli.storage.model.Schema;

/**
 * @author mstevens
 *
 */
public final class CollectorAttachmentUtils
{

	private CollectorAttachmentUtils() {}
	
	/**
	 * @param project
	 * @param record
	 * @param fsp
	 * @param excludeNonExisting whether or not to exclude non-existing MediaFiles
	 * @return
	 */
	static public List<MediaFile> getMediaFiles(Project project, Record record, FileStorageProvider fsp, boolean excludeNonExisting)
	{
		return getMediaFiles(Collections.singletonList(project), Collections.singletonList(record), fsp, excludeNonExisting);
	}
	
	/**
	 * @param projects
	 * @param records
	 * @param fsp
	 * @param excludeNonExisting whether or not to exclude non-existing MediaFiles
	 * @return
	 */
	static public List<MediaFile> getMediaFiles(List<Project> projects, List<Record> records, FileStorageProvider fsp, boolean excludeNonExisting)
	{
		// Populate schema->form map:
		final Map<Schema, Form> schema2Form = new HashMap<Schema, Form>();
		for(Project project : projects)
			for(Form form : project.getForms())
				schema2Form.put(form.getSchema(), form);
		
		// Group records by form:
		Map<Form, List<Record>> recordsByForm = new HashMap<Form, List<Record>>();
		for(Record r : records)
		{
			Form form = schema2Form.get(r.getSchema());
			if(form == null)
				continue;
			List<Record> formRecs;
			if(!recordsByForm.containsKey(form))
				recordsByForm.put(form, formRecs = new ArrayList<Record>());
			else
				formRecs = recordsByForm.get(form);
			formRecs.add(r);
		}
		// Scan for attachments:
		final List<MediaFile> attachments = new ArrayList<MediaFile>();
		for(Form form : recordsByForm.keySet())
			for(Record record : recordsByForm.get(form))
				for(Field field : form.getFields())
					if(field instanceof MediaField)
					{
						MediaField mf = (MediaField) field;
						for(int i = 0; i < mf.getAttachmentCount(record); i++)
						{
							MediaFile attachment = mf.getAttachment(fsp, record, i);
							if(attachment != null && (!excludeNonExisting || FileHelpers.isReadableFile(attachment.file)))
								attachments.add(attachment);
						}
					}
		return attachments;
	}

}
