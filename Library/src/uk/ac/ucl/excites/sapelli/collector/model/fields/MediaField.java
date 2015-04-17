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
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.Set;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import uk.ac.ucl.excites.sapelli.collector.control.FieldVisitor;
import uk.ac.ucl.excites.sapelli.collector.io.FileStorageException;
import uk.ac.ucl.excites.sapelli.collector.io.FileStorageProvider;
import uk.ac.ucl.excites.sapelli.collector.model.Field;
import uk.ac.ucl.excites.sapelli.collector.model.FieldParameters;
import uk.ac.ucl.excites.sapelli.collector.model.Form;
import uk.ac.ucl.excites.sapelli.shared.crypto.ROT13;
import uk.ac.ucl.excites.sapelli.shared.util.CollectionUtils;
import uk.ac.ucl.excites.sapelli.shared.util.TimeUtils;
import uk.ac.ucl.excites.sapelli.storage.model.Record;
import uk.ac.ucl.excites.sapelli.storage.model.columns.IntegerColumn;
import uk.ac.ucl.excites.sapelli.storage.model.columns.IntegerListColumn;

/**
 * @author mstevens, Michalis Vitos, benelliott
 *
 */
public abstract class MediaField extends Field
{

	// STATIC -------------------------------------------------------
	//static public final int DEFAULT_MIN = 0;
	static public final int DEFAULT_MAX = 255; //column will use 1 byte (up to 255 items)
	static public final boolean DEFAULT_SHOW_REVIEW = true;
	static public final char FILENAME_ELEMENT_SEPARATOR = '_';
	
	static public final String ID_PREFIX = "media";
	
	/**
	 * no longer used as filenames are now just ROT13-ed, but may be useful for backwards compatibility: 
	 */
	static private final Pattern HISTORIC_OBFUSCATED_MEDIA_FILE_NAME_AND_EXTENSION_FORMAT = Pattern.compile("^([0-9A-F]{32})" + FILENAME_ELEMENT_SEPARATOR + "([0-9A-Z]+)$");
	
	static public final long MAX_ATTACHMENT_CREATION_TIME_OFFSET = (long) (10 * 365.25 * 24 * 60 * 60 * 1000); // 10 years in ms
	
	// DYNAMIC ------------------------------------------------------
	protected String approveButtonImageRelativePath;
	protected String discardButtonImageRelativePath;
	
	//protected int min;
	protected boolean useNativeApp;
	protected boolean showReview;
	protected int max;
	
	/**
	 * @param form
	 * @param id the id of the field, may be null (but not recommended)
	 * @param caption the caption of the field, may be null
	 */
	public MediaField(Form form, String id, String caption)
	{
		super(form, GetID(id, form, ID_PREFIX, caption), caption);
		setMax(DEFAULT_MAX); //setMinMax(DEFAULT_MIN, DEFAULT_MAX);
		setShowReview(DEFAULT_SHOW_REVIEW);
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
	 * @param max the max to set
	 */
	public void setMax(int max)
	{
		if(max < 1)
			throw new IllegalArgumentException("Max must be >= 1, supplied value is " + max + ".");
		this.max = max;
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
	
	/**
	 * @return whether or not the review screen should be shown after media is captured.
	 */
	public boolean isShowReview()
	{
		return showReview;
	}
	
	/**
	 * Set whether or not the review screen should be shown after media is captured.
	 * @param showReview
	 */
	public void setShowReview(boolean showReview)
	{
		this.showReview = showReview;
	}

	/**
	 * @return the approveButtonImageRelativePath
	 */
	public String getApproveButtonImageRelativePath()
	{
		return approveButtonImageRelativePath;
	}

	/**
	 * @param approveButtonImageRelativePath the approveButtonImageRelativePath to set
	 */
	public void setApproveButtonImageRelativePath(String approveButtonImageRelativePath)
	{
		this.approveButtonImageRelativePath = approveButtonImageRelativePath;
	}

	/**
	 * @return the discardButtonImageRelativePath
	 */
	public String getDiscardButtonImageRelativePath()
	{
		return discardButtonImageRelativePath;
	}

	/**
	 * @param discardButtonImageRelativePath the discardButtonImageRelativePath to set
	 */
	public void setDiscardButtonImageRelativePath(String discardButtonImageRelativePath)
	{
		this.discardButtonImageRelativePath = discardButtonImageRelativePath;
	}

	@Override
	protected IntegerListColumn createColumn(String name)
	{
		boolean colOptional = form.getColumnOptionalityAdvisor().getColumnOptionality(this);
		return new IntegerListColumn(name, new IntegerColumn("creationTimeOffset", false, 0, MAX_ATTACHMENT_CREATION_TIME_OFFSET), colOptional, (colOptional ? 0 : 1), max);

	}
	
	public IntegerColumn createV1XColumn()
	{
		boolean colOptional = form.getColumnOptionalityAdvisor().getColumnOptionality(this);
		return new IntegerColumn(getColumn().getName() + "-v1x", colOptional, (colOptional ? 0 : 1), max);
	}
	
	@Override
	public IntegerListColumn getColumn()
	{
		return (IntegerListColumn) super.getColumn();
	}
	
	public List<Long> getCurrentAttachmentOffsets(Record record)
	{
		return getColumn().retrieveValue(record);
	}
	
	/**
	 * Returns the number of attachments for this field in the provided record.
	 * @param record
	 * @return the number of attachments
	 */
	public int getAttachmentCount(Record record)
	{
		List<Long> currentOffsets = getCurrentAttachmentOffsets(record);
		return currentOffsets != null ? currentOffsets.size() : 0;
	}
	
	/**
	 *  Returns whether or not the maximum number of attachments has been added to this
	 *  field in the provided record.
	 * @param record
	 * @return
	 */
	public boolean isMaxReached(Record record)
	{
		return (getAttachmentCount(record) >= max);
	}
	
	/**
	 * Add the provided attachment file to the column corresponding to this field in the provided record. <br>
	 * Note: does not actually alter the file system, only the record contents. The file is added to the file system as soon as it is requested through the
	 * {@link #getNewAttachmentFile(FileStorageProvider, Record)} method.
	 * 
	 * @param attachment - the file to attach to the record
	 * @param record - the record to attach the file to
	 */
	public void addAttachmentToRecord(File attachment, Record record)
	{
		// check if adding would exceed max no attachments for this field
		if(isMaxReached(record))
			throw new IllegalStateException("Maximum # of attachments (" + max + ") reached.");
		// retrieve creationTimeOffset from filename
		long creationTimeOffset = getCreationTimeOffsetFromFile(attachment);
		// add creationTimeOffset to column
		List<Long> offsets = getCurrentAttachmentOffsets(record);
		if(offsets == null)
			offsets = new ArrayList<Long>();
		offsets.add(creationTimeOffset);
		getColumn().storeValue(record, offsets);
	}

	/**
	 * Remove the provided attachment file from the column corresponding to this field in the provided record.
	 * <br>
	 * Note: does not actually alter the file system, only the record contents. Files are actually deleted from the 
	 * file system once the user's deletions are "confirmed" by them saving the record at the end.
	 * 
	 * @param attachment - the attachment to delete
	 * @param record - the record to delete the attachment from
	 */
	public void removeAttachmentFromRecord(File attachment, Record record)
	{
		// retrieve creationTimeOffset from filename
		long creationTimeOffset = getCreationTimeOffsetFromFile(attachment);
		// remove creationTimeOffset from column
		List<Long> offsets = getCurrentAttachmentOffsets(record);
		if(offsets != null)
		{
			if(offsets.remove(creationTimeOffset))
			{
				if(offsets.isEmpty())
					getColumn().clearValue(record); // reset value in column to null
				else
					getColumn().storeValue(record, offsets);
			}
			else
				System.err.println("Specified attachment could not be found for deletion.");
		}
	}
	
	/**
	 * Returns the creation time offset for an existing media file (the time offset in milliseconds between 
	 * the record's creation and the creation of that file), given the {@code File} object itself.
	 * @param file
	 * @return the creation time offset
	 */
	private long getCreationTimeOffsetFromFile(File file)
	{
		String deobfuscatedName = file.getName();
		if(form.isObfuscateMediaFiles()) // deobfuscate if necessary by rotating
			deobfuscatedName = ROT13.rot13NumRot5(file.getName());
		else
		{
			// remove file extension before extracting creationTimeOffset (if obfuscated, name/ext separator is _ anyway)
			int pos = deobfuscatedName.lastIndexOf(".");
			if(pos > 0)
			{
				deobfuscatedName = deobfuscatedName.substring(0, pos);
			}
		}

		// creationTimeOffset is the fourth part of the filename:
		String[] parts = deobfuscatedName.split(Character.toString(FILENAME_ELEMENT_SEPARATOR));
		try
		{
			return Long.parseLong(parts[3]);
		}
		catch(Exception e)
		{
			throw new IllegalStateException("Attachment filename did not have the expected syntax: " + deobfuscatedName);
		}
	}
	
	/**
	 * Creates a new file in which to store media with a filename determined by the field ID, record start time and
	 * time offset at which this file was requested from that start time.
	 * @param fileStorageProvider - an object that provides information on the location of the attachments
	 * @param record - the record with which the new file should be associated
	 * @return the new file
	 * @throws FileStorageException
	 */
	public File getNewAttachmentFile(FileStorageProvider fileStorageProvider, Record record) throws FileStorageException
	{
		long creationTimeOffset = System.currentTimeMillis() - form.getStartTime(record, true).getMsSinceEpoch();
		String filename = generateFilename(record, creationTimeOffset);
		return new File(fileStorageProvider.getProjectAttachmentFolder(form.project, true), filename);
	}
	
	/**
	 * Returns a list of the files attached to this field and record.
	 * @param fileStorageProvider
	 * @param record
	 * @return the list of attachments
	 */
	public List<File> getAttachments(FileStorageProvider fileStorageProvider, Record record)
	{
		List<Long> offsets = getCurrentAttachmentOffsets(record);
		if(offsets == null || offsets.isEmpty())
			return Collections.<File> emptyList(); // return an empty list
		// Construct list of files:	
		List<File> files = new ArrayList<File>();
		for(Long offset : offsets)
			CollectionUtils.addIgnoreNull(files, getAttachmentFromOffset(fileStorageProvider, record, offset));
		return files;
	}
	
	public File getAttachment(FileStorageProvider fileStorageProvider, Record record, int index)
	{
		List<Long> offsets = getCurrentAttachmentOffsets(record);
		if(offsets != null && index >= 0 && index < offsets.size())
			return getAttachmentFromOffset(fileStorageProvider, record, offsets.get(index));
		else
			return null;
	}
	
	private File getAttachmentFromOffset(FileStorageProvider fileStorageProvider, Record record, Long offset)
	{
		if(offset == null)
			return null;
		// Locate corresponding file:
		File file = new File(fileStorageProvider.getProjectAttachmentFolder(form.getProject(), false), generateFilename(record, offset));
		if(file.exists())
			return file;
		else
			return null;
	}
	
	/**
	 * Returns the most recently attached file.
	 * @param fileStorageProvider
	 * @param record
	 * @return
	 */
	public File getLastAttachment(FileStorageProvider fileStorageProvider, Record record)
	{
		List<Long> offsets = getCurrentAttachmentOffsets(record);
		if(offsets == null || offsets.size() < 1)
			return null;
		String dir = fileStorageProvider.getProjectAttachmentFolder(form.getProject(), true).getAbsolutePath();
		String filename = generateFilename(record, offsets.get(offsets.size() - 1)); // get final offset from list
		return new File(dir, filename);
	}
	
	/**
	 * Generates a new filename for the next media attachment for this field. If obfuscation is enabled,
	 * the entire filename is obfuscated using ROT13.
	 * 
	 * @param record
	 * @param creationTimeOffset
	 * @return the generated filename
	 */
	public String generateFilename(Record record, long creationTimeOffset)
	{
		return generateFilename(record, creationTimeOffset, form.isObfuscateMediaFiles());
	}
	
	/**
	 * Generates a filename for a new attachment for this {@link MediaField} and the provided {@code record}.
	 * The filename will be obfuscated if {@code obfuscate} is {@code true}.
	 * 
	 * @param record
	 * @param obfuscate
	 * @return the generated filename
	 */
	public String generateFilename(Record record, long creationTimeOffset, boolean obfuscate)
	{	
		// Elements:
		String dateTime = TimeUtils.getTimestampForFileName(form.getStartTime(record, true));
		long deviceID = form.getDeviceID(record);
		
		// Assemble base filename
		// Format: "FieldID_DeviceID_DateTime_CreationTimeOffset"
		String filename = this.getID() + FILENAME_ELEMENT_SEPARATOR + Long.toString(deviceID) + FILENAME_ELEMENT_SEPARATOR + dateTime + FILENAME_ELEMENT_SEPARATOR + creationTimeOffset;
		String extension = getFileExtension();
		char extensionSeparator = '.';
		
		// Obfuscate filename if necessary:
		if(obfuscate)
		{
			// TODO - remove custom extension separator and rotate all at once?
			filename = ROT13.rot13NumRot5(filename);
			extensionSeparator = FILENAME_ELEMENT_SEPARATOR; // '_' instead of '.'
			extension = ROT13.rot13NumRot5(extension).toUpperCase(); // Format: UPPERCASE(ROT13(extension))
		}
		return filename + extensionSeparator + extension;
	}
	
	/**
	 * Undoes the obfuscation of the extension on filenames that match the {@link #HISTORIC_OBFUSCATED_MEDIA_FILE_NAME_AND_EXTENSION_FORMAT} pattern.
	 * 
	 * @param filename 
	 * @return the deobfuscated extension if the input matched the pattern, else the input filename
	 */
	public static String undoHistoricExtensionObfuscation(String filename)
	{
		Matcher matcher = HISTORIC_OBFUSCATED_MEDIA_FILE_NAME_AND_EXTENSION_FORMAT.matcher(filename);
		if(matcher.find() && matcher.groupCount() == 2)
		{	// Got match!
			/*
			 * System.out.println("Found value: " + matcher.group(0)); //entire expression
			 * System.out.println("Found value: " + matcher.group(1)); //hash part
			 * System.out.println("Found value: " + matcher.group(2)); //ROT13-ed and uppercased extension
			 */
			return ROT13.rot13NumRot5(matcher.group(1)) + '.' + ROT13.rot13NumRot5(matcher.group(2)).toLowerCase();
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
	public void addFiles(Set<File> filesSet, FileStorageProvider fileStorageProvider)
	{
		super.addFiles(filesSet, fileStorageProvider); // !!!
		CollectionUtils.addIgnoreNull(filesSet, fileStorageProvider.getProjectImageFile(form.project, discardButtonImageRelativePath));
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
					this.showReview == that.showReview &&
					(this.discardButtonImageRelativePath != null ? this.discardButtonImageRelativePath.equals(that.discardButtonImageRelativePath) : that.discardButtonImageRelativePath == null);
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
		hash = 31 * hash + (showReview ? 0 : 1);
		hash = 31 * hash + (discardButtonImageRelativePath == null ? 0 : discardButtonImageRelativePath.hashCode());
		return hash;
	}
	
}
