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

package uk.ac.ucl.excites.sapelli.collector.model.fields;

import java.io.File;
import java.io.Serializable;
import java.util.ArrayList;
import java.util.List;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import uk.ac.ucl.excites.sapelli.collector.control.FieldVisitor;
import uk.ac.ucl.excites.sapelli.collector.io.FileStorageException;
import uk.ac.ucl.excites.sapelli.collector.io.FileStorageProvider;
import uk.ac.ucl.excites.sapelli.collector.load.parse.FormParser;
import uk.ac.ucl.excites.sapelli.collector.model.Field;
import uk.ac.ucl.excites.sapelli.collector.model.FieldParameters;
import uk.ac.ucl.excites.sapelli.collector.model.Form;
import uk.ac.ucl.excites.sapelli.shared.crypto.Hashing;
import uk.ac.ucl.excites.sapelli.shared.crypto.ROT13;
import uk.ac.ucl.excites.sapelli.shared.io.FileHelpers;
import uk.ac.ucl.excites.sapelli.shared.util.BinaryHelpers;
import uk.ac.ucl.excites.sapelli.shared.util.TimeUtils;
import uk.ac.ucl.excites.sapelli.storage.model.Record;
import uk.ac.ucl.excites.sapelli.storage.model.UnmodifiableValueSet;
import uk.ac.ucl.excites.sapelli.storage.model.ValueSet;
import uk.ac.ucl.excites.sapelli.storage.model.VirtualColumn;
import uk.ac.ucl.excites.sapelli.storage.model.columns.IntegerColumn;
import uk.ac.ucl.excites.sapelli.storage.model.columns.StringColumn;
import uk.ac.ucl.excites.sapelli.storage.model.columns.StringListColumn;

/**
 * @author mstevens, Michalis Vitos
 *
 */
public abstract class MediaField extends Field
{

	//static public final int DEFAULT_MIN = 0;
	static public final int DEFAULT_MAX = 255; //column will use 1 byte (up to 255 items)
	static public final char FILENAME_ELEMENT_SEPARATOR = '_';
	
	static public final String ID_PREFIX = "media";
	
	static public final String FILES_VIRTUAL_COLOMN_TARGET_NAME = "Files";
	static public final int MAX_FILENAME_LENGTH = 128;
	
	static private final Pattern OBFUSCATED_MEDIA_FILE_NAME_AND_EXTENSION_FORMAT = Pattern.compile("^([0-9A-F]{32})" + FILENAME_ELEMENT_SEPARATOR + "([0-9A-Z]+)$");
	
	//protected int min;
	protected boolean useNativeApp;
	protected int max;
	protected ChoiceField disableChoice;
	protected final FileNameGenerator defaultFileNameGenerator;

	/**
	 * @param form
	 * @param id the id of the field, may be null (but not recommended)
	 * @param caption the caption of the field, may be null
	 */
	public MediaField(Form form, String id, String caption)
	{
		super(form, GetID(id, form, ID_PREFIX, caption), caption);
		setMax(DEFAULT_MAX); //setMinMax(DEFAULT_MIN, DEFAULT_MAX);		
		
		defaultFileNameGenerator = new FileNameGenerator(this.id, form.isObfuscateMediaFiles(), form.isObfuscateMediaFiles(), getFileExtension());
	}
	
	public abstract String getMediaType();
	
	public String getFileExtension()
	{
		return getFileExtension(getMediaType()).toLowerCase();
	}
	
	protected abstract String getFileExtension(String mediaType);

	/**
	 * @return the min
	 */
	public int getMin()
	{
		return optional ? 0 : 1; 
	}
	
	/**
	 * @return the max
	 */
	public int getMax()
	{
		return max;
	}
	
	/**
	 * @return the useNativeApp
	 */
	public boolean isUseNativeApp()
	{
		return useNativeApp;
	}

	/**
	 * @param useNativeApp the useNativeApp to set
	 */
	public void setUseNativeApp(boolean useNativeApp)
	{
		this.useNativeApp = useNativeApp;
	}

//	/**
//	 * @param min the min to set
//	 * @param max the max to set
//	 */
//	public void setMinMax(int min, int max)
//	{
//		if(max < 1 || min < 0 || min > max)
//			throw new IllegalArgumentException("Min must be >= 0, max must be >= 1, and min <= max! Supplied values are min = " + min + "; max = " + max + ".");
//		this.min = min;
//		this.max = max;
//	}
	
	/**
	 * @param max the max to set
	 */
	public void setMax(int max)
	{
		if(max < 1)
			throw new IllegalArgumentException("Max must be >= 1, supplied value is " + max + ".");
		this.max = max;
	}

	/**
	 * @return the disableChoice
	 */
	public ChoiceField getDisableChoice()
	{
		return disableChoice;
	}

	/**
	 * @param disableChoice the disableChoice to set
	 */
	public void setDisableChoice(ChoiceField disableChoice)
	{
		this.disableChoice = disableChoice;
	}
	
	@Override
	protected IntegerColumn createColumn(String name)
	{
		boolean colOptional = form.getColumnOptionalityAdvisor().getColumnOptionality(this);
		IntegerColumn intCol = new IntegerColumn(name, colOptional, (colOptional ? 0 : 1), max);
		
		// Add virtual column which will contain the filenames of all attachments:
		intCol.addVirtualVersion(
			new StringListColumn(FILES_VIRTUAL_COLOMN_TARGET_NAME, StringColumn.ForCharacterCount("File", false, MAX_FILENAME_LENGTH), colOptional, 0, max, '[', ']', '|', null, null),
			new FileNameMapper(defaultFileNameGenerator));
		
		return intCol;
	}
	
	public int getCount(Record record)
	{
		Long currentCount = ((IntegerColumn) form.getColumnFor(this)).retrieveValue(record);
		if(currentCount == null)
			return 0;
		return currentCount.intValue();
	}
	
	public boolean isMaxReached(Record record)
	{
		return (getCount(record) >= max);
	}
	
	public void incrementCount(Record record)
	{
		int currentCount = getCount(record);	
		if(currentCount >= max)
			throw new IllegalStateException("Maximum # of attachments (" + max + ") reached.");
		((IntegerColumn) form.getColumnFor(this)).storeValue(record, ++currentCount);
	}

	public File getNewTempFile(FileStorageProvider fileStorageProvider, Record record) throws FileStorageException
	{
		return getMediaFile(fileStorageProvider, record, getCount(record));
	}
	
	public File getMediaFile(FileStorageProvider fileStorageProvider, Record record, int attachementNumber) throws FileStorageException
	{
		String filename = generateFilename(record, attachementNumber);
		String dataFolderPath = fileStorageProvider.getProjectAttachmentFolder(form.project, true).getAbsolutePath();
		return new File(dataFolderPath + File.separator + filename);		
	}
	
	/**
	 * Generates a filename for the {@code attachmentNumber}-th attachment for this {@link MediaField} and the provided {@code record}.
	 * Both the base filename and the extension will be obfuscated if the {@link FormParser#ATTRIBUTE_FORM_OBFUSCATE_MEDIA_FILES} attribute was set to {@code true},
	 * and if so the generated filenames will match {@link #OBFUSCATED_MEDIA_FILE_NAME_AND_EXTENSION_FORMAT}.
	 * 
	 * @param record
	 * @param attachmentNumber
	 * @return
	 */
	public String generateFilename(Record record, int attachmentNumber)
	{
		return defaultFileNameGenerator.generateFilename(record, attachmentNumber);
	}
	
	/**
	 * Generates a filename for the {@code attachmentNumber}-th attachment for this {@link MediaField} and the provided {@code record}.
	 * The filename will be obfuscated if {@code obfuscate} is {@code true}.
	 * 
	 * @param record
	 * @param attachmentNumber
	 * @param obfuscateFilename
	 * @param obfuscateExtension
	 * @return
	 */
	public String generateFilename(Record record, int attachmentNumber, boolean obfuscateFilename, boolean obfuscateExtension)
	{	
		return new FileNameGenerator(id, obfuscateFilename, obfuscateExtension, getFileExtension()).generateFilename(record, attachmentNumber);
	}
	
	/**
	 * Undoes the obfuscation of the extension on filenames that match the {@link #OBFUSCATED_MEDIA_FILE_NAME_AND_EXTENSION_FORMAT} pattern.
	 * Only the extension separator ('_' becomes '.') and extension (uppercase and ROT13 are undone) change, the base filename will stay obfuscated.
	 * 
	 * @param filename
	 * @see #generateFilename(Record, int, boolean, boolean)
	 * @return
	 * 
	 */
	public static String UndoExtensionObfuscation(String filename)
	{
		Matcher matcher = OBFUSCATED_MEDIA_FILE_NAME_AND_EXTENSION_FORMAT.matcher(filename);
		if(matcher.find() && matcher.groupCount() == 2)
		{	// Got match!
			/*
			 * System.out.println("Found value: " + matcher.group(0)); //entire expression
			 * System.out.println("Found value: " + matcher.group(1)); //hash part
			 * System.out.println("Found value: " + matcher.group(2)); //ROT13-ed and uppercased extension
			 */
			return matcher.group(1) /*hash*/ + '.' + ROT13.rot13NumRot5(matcher.group(2)).toLowerCase();
		}
		else
			// No match, return filename as-is:
			return filename;
	}
	
	/* (non-Javadoc)
	 * @see uk.ac.ucl.excites.sapelli.collector.model.Field#enter(uk.ac.ucl.excites.sapelli.collector.control.FieldVisitor, uk.ac.ucl.excites.sapelli.collector.model.FieldParameters, boolean)
	 */
	@Override
	public boolean enter(FieldVisitor visitor, FieldParameters arguments, boolean withPage)
	{
		return visitor.enterMediaField(this, arguments, withPage);
	}
	
	@Override
	public boolean equals(Object obj)
	{
		if(this == obj)
			return true; // references to same object
		if(obj instanceof MediaField)
		{
			MediaField that = (MediaField) obj;
			return	super.equals(that) && // Field#equals(Object)
					//this.min == that.min &&
					this.max == that.max &&
					this.useNativeApp == that.useNativeApp &&
					(this.disableChoice != null ? that.disableChoice != null && this.disableChoice.id.equals(that.disableChoice.id) : that.disableChoice == null); // do not use disableChoice itself to avoid potential endless loops!
		}
		else
			return false;
	}
	
	@Override
	public int hashCode()
	{
		int hash = super.hashCode(); // Field#hashCode()
		//hash = 31 * hash + min;
		hash = 31 * hash + max;
		hash = 31 * hash + (useNativeApp ? 0 : 1);
		hash = 31 * hash + (disableChoice == null ? 0 : disableChoice.id.hashCode()); // do not use disableChoice itself to avoid potential endless loops!
		return hash;
	}
	
	/**
	 * @author mstevens
	 */
	static protected class FileNameMapper extends VirtualColumn.ValueMapper<List<String>, Long>
	{
		
		private static final long serialVersionUID = 2L;
		
		private final FileNameGenerator fileNameGenerator;
		
		public FileNameMapper(FileNameGenerator fileNameGenerator)
		{
			this.fileNameGenerator = fileNameGenerator;
		}

		@Override
		public List<String> mapValue(Long nonNullValue, UnmodifiableValueSet<?> valueSet)
		{
			int attachmentCount = nonNullValue.intValue();
			List<String> filenames = new ArrayList<String>();
			for(int attachmentNumber = 0; attachmentNumber < attachmentCount; attachmentNumber++)
				filenames.add(fileNameGenerator.generateFilename(valueSet, attachmentNumber));
			return filenames;
		}

		@Override
		public int hashCode()
		{
			int hash = 1;
			hash = 31 * hash + getClass().getName().hashCode();
			hash = 31 * hash + fileNameGenerator.hashCode();
			return hash; 
		}
		
	}
	
	/**
	 * @author mstevens
	 */
	static protected class FileNameGenerator implements Serializable
	{
		
		private static final long serialVersionUID = 2L;
		
		private final String fieldID;
		private final boolean obfuscateFilename;
		private final boolean obfuscateExtension;
		private final String fileExtension;
		
		/**
		 * @param obfuscateFilename
		 * @param obfuscateExtension
		 */
		public FileNameGenerator(String fieldID, boolean obfuscateFilename, boolean obfuscateExtension, String fileExtension)
		{
			this.fieldID = fieldID;
			this.obfuscateFilename = obfuscateFilename;
			this.obfuscateExtension = obfuscateExtension;
			this.fileExtension = fileExtension;
		}

		/**
		 * Generates a filename for the {@code attachmentNumber}-th attachment for this {@link MediaField} and the provided {@code record}.
		 * The filename will be obfuscated if {@code obfuscate} is {@code true}.
		 * 
		 * @param record
		 * @param attachmentNumber
		 * @param obfuscateFilename
		 * @param obfuscateExtension
		 * @return
		 */
		public String generateFilename(ValueSet<?> record, int attachmentNumber)
		{	
			// Elements:
			String dateTime = TimeUtils.getTimestampForFileName(Form.GetStartTime(record, true));
			long deviceID = Form.GetDeviceID(record);
			
			// Assemble base filename
			//	Format: "FieldID_DeviceID_DateTime_AttachmentNumber"
			String filename = FileHelpers.makeValidFileName(fieldID) + FILENAME_ELEMENT_SEPARATOR + Long.toString(deviceID) + FILENAME_ELEMENT_SEPARATOR + dateTime + FILENAME_ELEMENT_SEPARATOR + '#' + Integer.toString(attachmentNumber);
			String ext = fileExtension;
			char extensionSeparator = '.';
			
			// Obfuscate filename and/or extension if necessary:
			if(obfuscateFilename)
				filename = BinaryHelpers.toHexadecimealString(Hashing.getMD5HashBytes(filename.getBytes()), 16, true); // Format: HEX(MD5(filename))
			if(obfuscateExtension)
			{
				extensionSeparator = FILENAME_ELEMENT_SEPARATOR;	// '_' instead of '.'
				ext = ROT13.rot13NumRot5(ext).toUpperCase(); 		// Format: UPPERCASE(ROT13(extension))
			}
			
			// Return fully assembled filename:
			return filename + extensionSeparator + ext;
		}
		
		@Override
		public boolean equals(Object obj)
		{
			if(this == obj)
				return true;
			if(!(obj instanceof FileNameGenerator))
				return false;
			FileNameGenerator that = (FileNameGenerator) obj;
			return	this.fieldID == that.fieldID &&
					this.obfuscateFilename == that.obfuscateFilename &&
					this.obfuscateExtension == that.obfuscateExtension &&
					this.fileExtension == that.fileExtension;
		}
		
		@Override
		public int hashCode()
		{
			int hash = 1;
			hash = 31 * hash + fieldID.hashCode();
			hash = 31 * hash + (obfuscateFilename ? 0 : 1);
			hash = 31 * hash + (obfuscateExtension ? 0 : 1);
			hash = 31 * hash + fileExtension.hashCode();
			return hash;
		}
		
	}
	
}
