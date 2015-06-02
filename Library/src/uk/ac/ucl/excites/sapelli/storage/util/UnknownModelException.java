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

package uk.ac.ucl.excites.sapelli.storage.util;

/**
 * @author mstevens
 *
 */
public class UnknownModelException extends Exception
{

	private static final long serialVersionUID = 2L;

	// For v2.0 Models and/or Schemata:
	private Long modelID;
	private String modelName;
	private Integer modelSchemaNumber;
	private String schemaName;
	
	// For v1.x Schemata:
	private Integer schemaID;
	private Integer schemaVersion;
	
	/**
	 * To report about a whole Model (>= v2.x) being missing, without reference to a specific Schema.
	 * For v2.x Models.
	 * 
	 * @param modelID
	 * @param modelName - may be null
	 */
	public UnknownModelException(long modelID, String modelName)
	{
		super(String.format("Unknown model (ID = %d; name = %s).", modelID, modelName != null ? modelName : "?"));
		this.modelID = modelID;
		this.modelName = modelName;
	}
	
	/**
	 * To report about a missing Schema and the Model it is part of.
	 * For v2.x Models/Schemata.
	 * 
	 * @param modelID
	 * @param modelName - may be null
	 * @param schemaNumber
	 * @param schemaName - may be null
	 */
	public UnknownModelException(long modelID, String modelName, int schemaNumber, String schemaName)
	{
		super(String.format("Unknown schema (modelID = %d; modelName = %s; schemaNumber = %d; schemaName = %s).", modelID, modelName != null ? modelName : "?", schemaNumber, schemaName != null ? schemaName : "?"));
		this.modelID = modelID;
		this.modelName = modelName;
		this.modelSchemaNumber = schemaNumber;
		this.schemaName = schemaName;
	}
	
	/**
	 * For v1.x Schemata.
	 * 
	 * @param schemaID
	 * @param schemaVersion
	 */
	public UnknownModelException(int schemaID, int schemaVersion)
	{
		super(String.format("Unknown v1.x schema (schemaID = %d; schemaVersion = %d).", schemaID, schemaVersion));
		this.schemaID = schemaID;
		this.schemaVersion = schemaVersion;
	}

	public boolean isV1xSchema()
	{
		return schemaID != null;
	}
	
	/**
	 * @return the modelID
	 */
	public Long getModelID()
	{
		return modelID;
	}

	/**
	 * @return the modelName
	 */
	public String getModelName()
	{
		return modelName;
	}

	/**
	 * @return the modelSchemaNumber
	 */
	public Integer getModelSchemaNumber()
	{
		return modelSchemaNumber;
	}

	/**
	 * @return the schemaName
	 */
	public String getSchemaName()
	{
		return schemaName;
	}
	
	/**
	 * @return the schemaID (v1.x)
	 */
	public Integer getSchemaID()
	{
		return schemaID;
	}

	/**
	 * @return the schemaVersion (v1.x)
	 */
	public Integer getSchemaVersion()
	{
		return schemaVersion;
	}

}
