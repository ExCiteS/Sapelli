package uk.ac.ucl.excites.sapelli.collector.model;

import uk.ac.ucl.excites.sapelli.shared.util.IntegerRangeMapping;
import uk.ac.ucl.excites.sapelli.shared.util.Objects;
import uk.ac.ucl.excites.sapelli.storage.model.Schema;

public class ProjectDescriptor implements Comparable<ProjectDescriptor>
{
	
	//STATICS-------------------------------------------------------------
	static public final int PROJECT_ID_SIZE = Schema.V1X_SCHEMA_ID_SIZE; // = 24 bits (kept the same was the v1.x Schema#id, for backwards compatibility)
	static public final IntegerRangeMapping PROJECT_ID_FIELD = IntegerRangeMapping.ForSize(0, PROJECT_ID_SIZE); // unsigned(!) 24bit integer (compatible with old schemaID)
	
	static public final String DEFAULT_VERSION = "0";
	
	static public final int PROJECT_FINGERPRINT_SIZE = 32; // project fingerprints are signed 32bit integer (like Java hashCodes)
	
	/**
	 * Backwards compatibility 
	 */
	static public final int PROJECT_ID_V1X_TEMP = -1;

	//DYNAMICS------------------------------------------------------------
	protected int id = Integer.MIN_VALUE; // don't init to 0 because that is an acceptable project id, nor -1 because that is used as temporary indication of a v1x project
	protected final int fingerPrint;
	protected final String name;
	protected String variant;
	protected String version;
	
	/**
	 * For backwards compatibility. 
	 * If this remains null the project is > v1.x
	 */
	protected Integer v1xSchemaVersion = null;

	/**
	 * @param id
	 * @param name
	 * @param variant
	 * @param version
	 * @param fingerPrint - hash code computed against XML (ignoring comments and whitespace; see XMLHasher) 
	 */
	public ProjectDescriptor(int id, String name, String variant, String version, int fingerPrint)
	{
		if(name == null || name.isEmpty())
			throw new IllegalArgumentException("A valid name is required");
		if(version == null || version.isEmpty())
			throw new IllegalArgumentException("A valid version is required");
		
		// Name, variant & version:
		this.name = name;
		if(variant != null && !variant.isEmpty())
			this.variant = variant;
		this.version = version;
		
		// Finger print:
		this.fingerPrint = fingerPrint; // must be set before initialise() is called!
		
		// Project id:
		if(id == PROJECT_ID_V1X_TEMP)
			//Backwards compatibility
			this.v1xSchemaVersion = -1; // make variable non-null to mark project as v1.x, the real schemaVersion value will be set from setV1XSchemaInfo(), which will in turn call initialise() to set the project id
		else
			initialise(id); // checks if it fits in field
	}

	protected void initialise(int id)
	{
		// Check & set id:
		if(this.id != id && PROJECT_ID_FIELD.inEffectiveRange(this.id)) // check is there is already a different & valid id set
			throw new IllegalStateException("Project id cannot be changed after it has been set.");
		if(PROJECT_ID_FIELD.inEffectiveRange(id))
			this.id = id;
		else
			throw new IllegalArgumentException("Invalid schema ID, valid values are " + PROJECT_ID_FIELD.getEffectiveRangeString() + ".");
	}
	
	/**
	 * @return the id
	 */
	public int getID()
	{
		return id;
	}
	
	/**
	 * @return the v1xProject
	 */
	public boolean isV1xProject()
	{
		return v1xSchemaVersion != null;
	}
	
	/**
	 * Method for backwards compatibility with v1.x projects. Passes id & version of the schema of the (assumed only) form to the project.
	 * 	- project id will be set to schema id of 1st (and assumed only) form
	 *  - schema version of that form will be stored in schemaVersion variable (to be able to parse old record export xml files)
	 * Can be used on v1.x projects only!
	 * 
	 * @param schemaID
	 * @param schemaVersion
	 */
	public void setV1XSchemaInfo(int schemaID, int schemaVersion)
	{
		if(!isV1xProject())
			throw new IllegalStateException("Only allowed for v1.x projects (created with id=PROJECT_ID_V1X_TEMP).");
		initialise(schemaID); // schemaID of first (and only) form is used as projectID
		if(Schema.V1X_SCHEMA_VERSION_FIELD.inEffectiveRange(schemaVersion))
			this.v1xSchemaVersion = schemaVersion;
		else
			throw new IllegalArgumentException("Invalid schema version, valid values are " + Schema.V1X_SCHEMA_VERSION_FIELD.getEffectiveRangeString() + ".");
	}
	
	/**
	 * @return the v1x schemaVersion, or null when the project is not a v1x project!
	 */
	public Integer getV1XSchemaVersion()
	{
		return v1xSchemaVersion;
	}

	public String getName()
	{
		return name;
	}
	
	/**
	 * @return the variant
	 */
	public String getVariant()
	{
		return variant;
	}

	public String getVersion()
	{
		return version;
	}
	
	/**
	 * @return the fingerPrint
	 */
	public int getFingerPrint()
	{
		return fingerPrint;
	}
	
	@Override
	public String toString()
	{
		return toString(false);
	}
	
	public String toString(boolean verbose)
	{
		return 	name + (variant != null ? (" " + variant) : "") + " (v" + version + ")"
				+ (verbose ? (" [id: " + id + "; fingerprint: " + fingerPrint + "]") : "");
	}
	
	public String getVariantVersionString()
	{
		return (variant != null ? variant + " " : "") + "v" + version;
	}
	
	/**
	 * @param that
	 * @return whether this and that project(description) have the same name, variant & version.
	 */
	public final boolean equalSignature(ProjectDescriptor that)
	{
		return 	this.name.equals(that.name)
				&& (this.variant == null ? that.variant == null : variant.equals(that.variant))
				&& this.version.equals(that.version);
	}
	
	/**
	 * @param that
	 * @return whether this and that project(description) are the same across all  ProjectDescriptor fields (id, name, variant, version, fingerprint & v1xSchemaVersion)
	 */
	public final boolean equalDescription(ProjectDescriptor that)
	{
		return 	this.id == that.id &&
				equalSignature(that) && // checks name, variant & version
				this.fingerPrint == that.fingerPrint &&
				Objects.equals(this.v1xSchemaVersion, that.v1xSchemaVersion);
	}
	
	@Override
	public boolean equals(Object obj)
	{
		if(this == obj)
			return true; // references to same object
		if(obj != null && obj.getClass() == ProjectDescriptor.class) // only matches ProjectDescriptors, not Projects! (needed for because equals() must be symmetric)
			return equalDescription((ProjectDescriptor) obj);
		return false;
	}
	
	@Override
	public int hashCode()
	{
		int hash = 1;
		hash = 31 * hash + id;
		hash = 31 * hash + name.hashCode();
		hash = 31 * hash + (variant == null ? 0 : variant.hashCode());
		hash = 31 * hash + version.hashCode();
		hash = 31 * hash + fingerPrint;
		hash = 31 * hash + (v1xSchemaVersion == null ? -1 : v1xSchemaVersion);
		return hash;
	}

	@Override
	public int compareTo(ProjectDescriptor that)
	{
		return this.toString(false).compareTo(that.toString(false));
	}
	
}
