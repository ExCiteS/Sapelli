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

package uk.ac.ucl.excites.sapelli.collector.model.fields;

import java.io.File;
import java.io.Serializable;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.Set;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import uk.ac.ucl.excites.sapelli.collector.control.FieldVisitor;
import uk.ac.ucl.excites.sapelli.collector.io.FileStorageProvider;
import uk.ac.ucl.excites.sapelli.collector.model.Field;
import uk.ac.ucl.excites.sapelli.collector.model.FieldParameters;
import uk.ac.ucl.excites.sapelli.collector.model.Form;
import uk.ac.ucl.excites.sapelli.collector.model.MediaFile;
import uk.ac.ucl.excites.sapelli.shared.crypto.Hashing;
import uk.ac.ucl.excites.sapelli.shared.crypto.ROT13;
import uk.ac.ucl.excites.sapelli.shared.io.FileHelpers;
import uk.ac.ucl.excites.sapelli.shared.io.FileStorageException;
import uk.ac.ucl.excites.sapelli.shared.util.BinaryHelpers;
import uk.ac.ucl.excites.sapelli.shared.util.CollectionUtils;
import uk.ac.ucl.excites.sapelli.shared.util.Objects;
import uk.ac.ucl.excites.sapelli.storage.model.ListColumn;
import uk.ac.ucl.excites.sapelli.storage.model.Record;
import uk.ac.ucl.excites.sapelli.storage.model.UnmodifiableValueSet;
import uk.ac.ucl.excites.sapelli.storage.model.ValueSet;
import uk.ac.ucl.excites.sapelli.storage.model.VirtualColumn;
import uk.ac.ucl.excites.sapelli.storage.model.columns.IntegerColumn;
import uk.ac.ucl.excites.sapelli.storage.model.columns.IntegerListColumn;
import uk.ac.ucl.excites.sapelli.storage.model.columns.StringColumn;
import uk.ac.ucl.excites.sapelli.storage.model.columns.StringListColumn;
import uk.ac.ucl.excites.sapelli.storage.types.TimeStampColumn;
import uk.ac.ucl.excites.sapelli.storage.util.TimeStampUtils;

/**
 * MediaField base class.
 * 
 * Note:
 * 	What we call the "V1X" column in here is somewhat confusingly named given that this way
 * 	of storing information about media attachments was used up to and including v2.0 Beta 16.
 * 
 * @author mstevens, Michalis Vitos, benelliott
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
	 * Up to 10 years after the StartTime of the Record
	 */
	static public final long MAX_ATTACHMENT_CREATION_TIME_OFFSET = (long) (10 * 365.25 * 24 * 60 * 60 * 1000); // 10 years in ms
	
	static private final StringListColumn GetVirtualFileNamesColumn(boolean optional, int max)
	{
		return new StringListColumn("Files", StringColumn.ForCharacterCount("File", false, MAX_FILENAME_LENGTH), optional, 0, max, ListColumn.DEFAULT_SERIALISATION_DELIMITER, ':');
	}
	
	static public final int MAX_FILENAME_LENGTH = 128;
	
	/**
	 * No longer used as filenames are now just ROT13-ed, but may be useful for backwards compatibility: 
	 */
	static private final Pattern V1X_OBFUSCATED_MEDIA_FILE_NAME_AND_EXTENSION_FORMAT = Pattern.compile("^([0-9A-F]{32})" + FILENAME_ELEMENT_SEPARATOR + "([0-9A-Z]+)$");
	
	/**
	 * Undoes the obfuscation of the extension on filenames that match the {@link #V1X_OBFUSCATED_MEDIA_FILE_NAME_AND_EXTENSION_FORMAT} pattern.
	 * 
	 * @param filename 
	 * @return the deobfuscated extension if the input matched the pattern, else the input filename
	 */
	static public String undoHistoricExtensionObfuscation(String filename)
	{
		Matcher matcher = V1X_OBFUSCATED_MEDIA_FILE_NAME_AND_EXTENSION_FORMAT.matcher(filename);
		if(matcher.find() && matcher.groupCount() == 2)
		{	// Got match!
			/*
			 * System.out.println("Found value: " + matcher.group(0)); // entire expression
			 * System.out.println("Found value: " + matcher.group(1)); // hash part
			 * System.out.println("Found value: " + matcher.group(2)); // ROT13-ed and uppercased extension
			 */
			return matcher.group(1) /*remains MD5 hash (cannot be undone)*/ + '.' + ROT13.rot13NumRot5(matcher.group(2)).toLowerCase();
		}
		else
			// No match, return filename as-is:
			return filename;
	}
	
	// DYNAMIC ------------------------------------------------------
	//protected int min;
	protected int max;
	protected boolean useNativeApp;
	protected boolean showReview;

	protected String approveButtonImageRelativePath;
	protected String discardButtonImageRelativePath;

	/**
	 * @param form
	 * @param id
	 * @param caption
	 * @param useNativeApp default {@link #useNativeApp} value
	 */
	public MediaField(Form form, String id, String caption, boolean useNativeApp)
	{
		super(form, GetID(id, form, ID_PREFIX, caption), caption);
		setMax(DEFAULT_MAX); //setMinMax(DEFAULT_MIN, DEFAULT_MAX);
		setShowReview(DEFAULT_SHOW_REVIEW);
		this.useNativeApp = useNativeApp;
	}
	
	public abstract String getMediaType();
	
	public String getFileExtension()
	{
		return getFileExtension(getMediaType()).toLowerCase();
	}
	
	protected abstract String getFileExtension(String mediaType);
	
	public String getFileMimeType()
	{
		return getFileMimeType(getMediaType());
	}
	
	protected abstract String getFileMimeType(String mediaType);
	
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
		IntegerListColumn intListCol = new IntegerListColumn(name, new IntegerColumn("creationTimeOffset", false, 0, MAX_ATTACHMENT_CREATION_TIME_OFFSET), colOptional, (colOptional ? 0 : 1), max);
		
		// Add virtual column which will contain the filenames of all attachments:
		intListCol.addVirtualVersion(	GetVirtualFileNamesColumn(colOptional, max),
										new FileNameGeneratorV2X(id, form.isObfuscateMediaFiles(), form.isObfuscateMediaFiles(), getFileExtension()));
		
		return intListCol;
	}
	
	public IntegerColumn createV1XColumn()
	{
		boolean colOptional = form.getColumnOptionalityAdvisor().getColumnOptionality(this);
		IntegerColumn intCol = new IntegerColumn(getColumn().name, colOptional, (colOptional ? 0 : 1), max);

		// Add virtual column which will contain the filenames of all attachments:
		intCol.addVirtualVersion(	GetVirtualFileNamesColumn(colOptional, max),
									new FileNameGeneratorV1X(id, form.isObfuscateMediaFiles(), form.isObfuscateMediaFiles(), getFileExtension()));
		
		return intCol;
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
		if(record == null)
			return 0; // just to be sure (see https://github.com/ExCiteS/Sapelli/issues/40)
		List<Long> currentOffsets = getCurrentAttachmentOffsets(record);
		return currentOffsets != null ? currentOffsets.size() : 0;
	}
	
	/**
	 * Checks whether or not there is at least 1 attachment corresponding to to this field in the provided record.
	 * 
	 * @param record
	 * @return at least 1 attachment
	 */
	public boolean hasAttachements(Record record)
	{
		return (getAttachmentCount(record) > 0);
	}
	
	/**
	 * Checks whether or not the maximum number of attachments corresponding to to this field has been reached in the provided record.
	 * 
	 * @param record
	 * @return max attachments reached
	 */
	public boolean isMaxAttachmentsReached(Record record)
	{
		return (getAttachmentCount(record) >= max);
	}
	
	/**
	 * Add the provided attachment file to the column corresponding to this field in the provided record.
	 * <br>
	 * Note: does not actually alter the file system, only the record contents. The file is added to the file system as soon as it is requested through the
	 * {@link #getNewAttachmentFile(FileStorageProvider, Record)} method.
	 * 
	 * @param attachment - the MediaFile to attach to the record
	 * @param record - the record to attach the file to
	 */
	public void addAttachmentToRecord(MediaFile attachment, Record record)
	{
		// check if we have a record
		if(record == null)
			return;
		// check if adding would exceed max no attachments for this field
		if(isMaxAttachmentsReached(record))
			throw new IllegalStateException("Maximum # of attachments (" + max + ") reached.");
		// add creationTimeOffset to column
		List<Long> offsets = getCurrentAttachmentOffsets(record);
		if(offsets == null)
			offsets = new ArrayList<Long>();
		offsets.add(attachment.creationTimeOffset);
		getColumn().storeValue(record, offsets);
	}

	/**
	 * Remove the provided attachment file from the column corresponding to this field in the provided record.
	 * <br>
	 * Note: does not actually alter the file system, only the record contents. Files are actually deleted from the 
	 * file system once the user's deletions are "confirmed" by them saving the record at the end.
	 * 
	 * @param attachment - the MediaFile to delete
	 * @param record - the record to delete the attachment from
	 */
	public void removeAttachmentFromRecord(MediaFile attachment, Record record)
	{
		// check if we have a record:
		if(record == null)
			return;
		// remove creationTimeOffset from column
		List<Long> offsets = getCurrentAttachmentOffsets(record);
		if(offsets != null)
		{
			if(offsets.remove(attachment.creationTimeOffset))
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
	 * 
	 * @param file
	 * @return the creation time offset
	 */
	protected long getCreationTimeOffsetFromFile(File file)
	{
		String deobfuscatedName = file.getName();
		if(form.isObfuscateMediaFiles()) // deobfuscate if necessary by rotating
			deobfuscatedName = ROT13.rot13NumRot5(file.getName());
		else
		{
			// remove file extension before extracting creationTimeOffset (if obfuscated, name/ext separator is _ anyway)
			int pos = deobfuscatedName.lastIndexOf(".");
			if(pos > 0)
				deobfuscatedName = deobfuscatedName.substring(0, pos);
		}

		// creationTimeOffset is the fourth part of the filename:
		String[] parts = deobfuscatedName.split(Character.toString(FILENAME_ELEMENT_SEPARATOR), -1); // -1: allow empty Strings
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
	 * Creates a new MediaFile (with new File contained in it) in which to store media with a filename determined by the field ID,
	 * record start time and time offset at which this file was requested from that start time.
	 * 
	 * @param fileStorageProvider - an object that provides information on the location of the attachments
	 * @param record - the record with which the new file should be associated
	 * @return the new file
	 * @throws FileStorageException
	 */
	public MediaFile getNewAttachmentFile(FileStorageProvider fileStorageProvider, Record record) throws FileStorageException
	{
		long creationTimeOffset = System.currentTimeMillis() - Form.GetStartTime(record).getMsSinceEpoch();
		return new MediaFile(this, record, creationTimeOffset, getFile(fileStorageProvider, record, creationTimeOffset, true));
	}
	
	/**
	 * Returns a list of the files attached to this field and record.
	 * @param fileStorageProvider
	 * @param record
	 * @return the list of attachments
	 */
	public List<MediaFile> getAttachments(FileStorageProvider fileStorageProvider, Record record)
	{
		// Check if we have a record:
		if(record == null)
			return Collections.<MediaFile> emptyList();
		
		// Get offsets:
		List<Long> offsets = getCurrentAttachmentOffsets(record);
		if(offsets == null || offsets.isEmpty())
			return Collections.<MediaFile> emptyList(); // return an empty list
		
		// Construct list of files:	
		List<MediaFile> files = new ArrayList<MediaFile>();
		for(Long offset : offsets)
			CollectionUtils.addIgnoreNull(files, getAttachmentFromOffset(fileStorageProvider, record, offset));
		return files;
	}
	
	public MediaFile getAttachment(FileStorageProvider fileStorageProvider, Record record, int index)
	{
		if(record == null)
			return null;
		return getAttachmentFromOffset(fileStorageProvider, record, getCreationTimeOffset(record, index));
	}
	
	protected Long getCreationTimeOffset(Record record, int index)
	{
		List<Long> offsets = getCurrentAttachmentOffsets(record);
		if(offsets != null && index >= 0 && index < offsets.size())
			return offsets.get(index);
		else
			return null;
	}
	
	/**
	 * @param fileStorageProvider
	 * @param record
	 * @param creationTimeOffset
	 * @return a MediaFile instance or null if no such file exists
	 */
	public MediaFile getAttachmentFromOffset(final FileStorageProvider fileStorageProvider, final Record record, final Long creationTimeOffset)
	{
		if(creationTimeOffset == null)
			return null;
		// Locate corresponding file:
		File file = getFile(fileStorageProvider, record, creationTimeOffset, false);
		if(file.exists())
			return new MediaFile(this, record, creationTimeOffset, file);
		else
			return null;
	}
	
	/**
	 * Returns the most recently attached file.
	 * @param fileStorageProvider
	 * @param record
	 * @return
	 */
	public MediaFile getLastAttachment(final FileStorageProvider fileStorageProvider, final Record record)
	{
		if(record == null)
			return null;
		List<Long> offsets = getCurrentAttachmentOffsets(record);
		if(offsets == null || offsets.isEmpty())
			return null;
		final long creationTimeOffset = offsets.get(offsets.size() - 1); // get final offset from list
		return new MediaFile(this, record, creationTimeOffset, getFile(fileStorageProvider, record, creationTimeOffset, false));
	}
	
	
	/**
	 * Creates new File instance (to be wrapped in a MediaFile).
	 * 
	 * @param fileStorageProvider
	 * @param record
	 * @param creationTimeOffset
	 * @param createFolder
	 * @return
	 */
	protected File getFile(FileStorageProvider fileStorageProvider, Record record, long creationTimeOffset, boolean createFolder)
	{
		return new File(fileStorageProvider.getProjectAttachmentFolder(form.getProject(), createFolder), generateFilename(record, creationTimeOffset));
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
	 * @param creationTimeOffset
	 * @param obfuscate
	 * @return the generated filename
	 */
	public String generateFilename(Record record, long creationTimeOffset, boolean obfuscate)
	{	
		return new FileNameGeneratorV2X(id, obfuscate, obfuscate, getFileExtension()).generateFilename(record, creationTimeOffset);
	}
	
	/**
	 * Generates a new filename for the next media attachment for this field. If obfuscation is enabled, the entire filename is obfuscated.
	 * 
	 * @param record
	 * @param attachmentNumber
	 * @return the generated filename
	 */
	public String generateFilenameV1X(Record record, int attachmentNumber)
	{
		return generateFilenameV1X(record, attachmentNumber, form.isObfuscateMediaFiles());
	}
	
	/**
	 * Generates a filename for a new attachment for this {@link MediaField} and the provided {@code record}.
	 * The filename will be obfuscated if {@code obfuscate} is {@code true}.
	 * 
	 * @param record
	 * @param attachmentNumber
	 * @param obfuscate
	 * @return the generated filename
	 */
	public String generateFilenameV1X(Record record, int attachmentNumber, boolean obfuscate)
	{	
		return new FileNameGeneratorV1X(id, obfuscate, obfuscate, getFileExtension()).generateFilename(record, attachmentNumber);
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
	
	/**
	 * @param v1XRecord
	 * @param findAndRenameFiles
	 * @param fileStorageProvider
	 * @return
	 */
	public List<Long> convertV1XColumnValue(Record v1XRecord, boolean findAndRenameFiles, FileStorageProvider fileStorageProvider)	
	{
		File attachmentFolder = findAndRenameFiles ? fileStorageProvider.getProjectAttachmentFolder(form.project, false) : null;
		IntegerColumn v1XColumn = createV1XColumn();
		if(!v1XColumn.isValuePresent(v1XRecord))
			return null;
		int attachmentCount = v1XColumn.retrieveValue(v1XRecord).intValue();
		List<Long> creationTimeOffsets = new ArrayList<Long>(attachmentCount);
		for(int attachmentNumber = 0; attachmentNumber < attachmentCount; attachmentNumber++)
		{
			Long fakeCreationTimeOffset = Long.valueOf((attachmentNumber + 1) * 100); // 100ms apart and 1st one 100ms after record StartTime 
			creationTimeOffsets.add(fakeCreationTimeOffset);
			if(findAndRenameFiles)
			{
				File v1XFile = new File(attachmentFolder, generateFilenameV1X(v1XRecord, attachmentNumber));
				if(v1XFile.exists())
					v1XFile.renameTo(new File(attachmentFolder, generateFilename(v1XRecord, fakeCreationTimeOffset)));
			}
		}
		return creationTimeOffsets;
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
					Objects.equals(this.approveButtonImageRelativePath, that.approveButtonImageRelativePath) &&
					Objects.equals(this.discardButtonImageRelativePath, that.discardButtonImageRelativePath);
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
		hash = 31 * hash + (approveButtonImageRelativePath == null ? 0 : approveButtonImageRelativePath.hashCode());
		hash = 31 * hash + (discardButtonImageRelativePath == null ? 0 : discardButtonImageRelativePath.hashCode());
		return hash;
	}
	
	/**
	 * @author mstevens
	 *
	 * @param <T> type of the MediaField Column (v1x: Long; v2x: List<Long>)
	 * @param <N> type of the attachmentIdentifier (v1x: Integer; v2x: Long)
	 */
	static protected abstract class FileNameGenerator<T, N extends Number> extends VirtualColumn.ValueMapper<List<String>, T> implements Serializable
	{
		
		private static final long serialVersionUID = 2L;
		
		protected final String fieldID;
		protected final boolean obfuscateFilename;
		protected final boolean obfuscateExtension;
		protected final String fileExtension;
		
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
		 * Generates a filename for the attachment with the given identifier for this {@link MediaField} and the provided {@code record}.
		 * The filename will be obfuscated if {@code obfuscate} is {@code true}.
		 * 
		 * @param record
		 * @param attachmentIdentifier
		 * @param obfuscateFilename
		 * @param obfuscateExtension
		 * @return
		 */
		public abstract String generateFilename(ValueSet<?> record, N attachmentIdentifier);

		@Override
		public boolean equals(Object obj)
		{
			if(this == obj)
				return true;
			if(!getClass().isInstance(obj))
				return false;
			FileNameGenerator<?, ?> that = (FileNameGenerator<?, ?>) obj;
			return	this.fieldID.equals(that.fieldID) &&
					this.obfuscateFilename == that.obfuscateFilename &&
					this.obfuscateExtension == that.obfuscateExtension &&
					this.fileExtension.equals(that.fileExtension);
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
	
	/**
	 * @author mstevens
	 */
	static protected class FileNameGeneratorV2X extends FileNameGenerator<List<Long>, Long>
	{
		
		private static final long serialVersionUID = 2L;
		
		public FileNameGeneratorV2X(String fieldID, boolean obfuscateFilename, boolean obfuscateExtension, String fileExtension)
		{
			super(fieldID, obfuscateFilename, obfuscateExtension, fileExtension);
		}

		/**
		 * Generates a filename for the attachment with the given creationTimeOffset for this {@link MediaField} and the provided {@code record}.
		 * The filename will be obfuscated if {@code obfuscate} is {@code true}.
		 * 
		 * @param record
		 * @param attachmentIdentifier
		 * @param obfuscateFilename
		 * @param obfuscateExtension
		 * @return
		 */
		@Override
		public String generateFilename(ValueSet<?> record, Long creationTimeOffset)
		{
			// Elements:
			String dateTime = TimeStampUtils.getTimestampForFileName(Form.GetStartTime(record));
			long deviceID = Form.GetDeviceID(record);
			
			// Assemble base filename
			// Format: "FieldID_DeviceID_StartTime_CreationTimeOffset"
			String filename = FileHelpers.makeValidFileName(fieldID) + FILENAME_ELEMENT_SEPARATOR + Long.toString(deviceID) + FILENAME_ELEMENT_SEPARATOR + dateTime + FILENAME_ELEMENT_SEPARATOR + creationTimeOffset;
			String ext = fileExtension;
			char extensionSeparator = '.';
			
			// Obfuscate filename if necessary:
			if(obfuscateFilename)
				filename = ROT13.rot13NumRot5(filename);
			if(obfuscateExtension)
			{
				extensionSeparator = FILENAME_ELEMENT_SEPARATOR; 	// '_' instead of '.'
				ext = ROT13.rot13NumRot5(ext).toUpperCase(); 		// Format: UPPERCASE(ROT13(extension))
			}
			
			// Return fully assembled filename:
			return filename + extensionSeparator + ext;
		}

		@Override
		public List<String> mapValue(List<Long> creationTimeOffsets, UnmodifiableValueSet<?> valueSet)
		{
			List<String> filenames = new ArrayList<String>(creationTimeOffsets.size());
			for(Long creationTimeOffset : creationTimeOffsets)
				filenames.add(generateFilename(valueSet, creationTimeOffset));
			return Collections.unmodifiableList(filenames);
		}

	}
	
	/**
	 * @author mstevens
	 */
	static protected class FileNameGeneratorV1X extends FileNameGenerator<Long, Integer>
	{
		
		private static final long serialVersionUID = 2L;
		
		/**
		 * This is the "V1X" version of the Form start time column (minus the virtual versions).
		 * We needs this because since v2.0 Beta 17 primary key columns can no longer be lossy.
		 */
		private static final TimeStampColumn COLUMN_TIMESTAMP_START_V1X = TimeStampColumn.Century21NoMS(Form.COLUMN_TIMESTAMP_START_NAME, false, false);
		
		public FileNameGeneratorV1X(String fieldID, boolean obfuscateFilename, boolean obfuscateExtension, String fileExtension)
		{
			super(fieldID, obfuscateFilename, obfuscateExtension, fileExtension);
		}

		/**
		 * Generates a filename for the {@code attachmentNumber}-th attachment for this {@link MediaField} and the provided {@code record}.
		 * The filename will be obfuscated if {@code obfuscate} is {@code true}.
		 * 
		 * @param record
		 * @param attachmentIdentifier
		 * @param obfuscateFilename
		 * @param obfuscateExtension
		 * @return
		 */
		@Override
		public String generateFilename(ValueSet<?> record, Integer attachmentNumber)
		{
			// Elements:
			String dateTime = TimeStampUtils.getTimestampForFileName(COLUMN_TIMESTAMP_START_V1X.toLossy(Form.GetStartTime(record)));
			long deviceID = Form.GetDeviceID(record);
			
			// Assemble base filename
			//	Format: "FieldID_DeviceID_StartTime_#AttachmentNumber"
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
		public List<String> mapValue(Long nonNullValue, UnmodifiableValueSet<?> valueSet)
		{
			int attachmentCount = nonNullValue.intValue();
			List<String> filenames = new ArrayList<String>(attachmentCount);
			for(int attachmentNumber = 0; attachmentNumber < attachmentCount; attachmentNumber++)
				filenames.add(generateFilename(valueSet, attachmentNumber));
			return Collections.unmodifiableList(filenames);
		}
		
	}
	
}
