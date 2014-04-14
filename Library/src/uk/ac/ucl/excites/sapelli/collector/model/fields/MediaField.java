/**
 * 
 */
package uk.ac.ucl.excites.sapelli.collector.model.fields;

import java.io.File;
import java.io.IOException;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import uk.ac.ucl.excites.sapelli.collector.control.Controller;
import uk.ac.ucl.excites.sapelli.collector.model.Field;
import uk.ac.ucl.excites.sapelli.collector.model.FieldParameters;
import uk.ac.ucl.excites.sapelli.collector.model.Form;
import uk.ac.ucl.excites.sapelli.collector.xml.FormParser;
import uk.ac.ucl.excites.sapelli.shared.util.BinaryHelpers;
import uk.ac.ucl.excites.sapelli.shared.util.ROT13;
import uk.ac.ucl.excites.sapelli.shared.util.TimeUtils;
import uk.ac.ucl.excites.sapelli.storage.model.Record;
import uk.ac.ucl.excites.sapelli.storage.model.columns.IntegerColumn;
import uk.ac.ucl.excites.sapelli.transmission.crypto.Hashing;

/**
 * @author mstevens, Michalis Vitos
 *
 */
public abstract class MediaField extends Field
{

	//static public final int DEFAULT_MIN = 0;
	static public final int DEFAULT_MAX = 255; //column will use 1 byte (up to 255 items)
	static public final char FILENAME_ELEMENT_SEPARATOR = '_';
	
	static private final Pattern OBFUSCATED_MEDIA_FILE_NAME_AND_EXTENSION_FORMAT = Pattern.compile("^([0-9A-F]{32})" + FILENAME_ELEMENT_SEPARATOR + "([0-9A-Z]+)$");
	
	//protected int min;
	protected boolean useNativeApp;
	protected int max;
	protected ChoiceField disableChoice;

	public MediaField(Form form, String id, String caption)
	{
		super(form, id, caption);
		setMax(DEFAULT_MAX); //setMinMax(DEFAULT_MIN, DEFAULT_MAX);		
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
		return optional == Optionalness.ALWAYS ? 0 : 1; 
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
	protected IntegerColumn createColumn()
	{
		return new IntegerColumn(id, (optional != Optionalness.NEVER), (optional != Optionalness.NEVER ? 0 : 1), max);
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

	public File getNewTempFile(Record record) throws IOException
	{
		String filename = generateFilename(record, getCount(record));
		String dataFolderPath = form.getProject().getTempFolder().getAbsolutePath(); //getTempFolder() does the necessary checks (IOException is thrown in case of trouble)
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
		return generateFilename(record, attachmentNumber, form.isObfuscateMediaFiles(), form.isObfuscateMediaFiles());
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
		// Elements:
		String dateTime = TimeUtils.getTimestampForFileName(form.getStartTime(record, true));
		long deviceID = form.getDeviceID(record);
		
		// Assemble base filename
		//	Format: "FieldID_DeviceID_DateTime_AttachmentNumber"
		String filename = this.getID() + FILENAME_ELEMENT_SEPARATOR + Long.toString(deviceID) + FILENAME_ELEMENT_SEPARATOR + dateTime + FILENAME_ELEMENT_SEPARATOR + '#' + Integer.toString(attachmentNumber);
		String extension = getFileExtension();
		char extensionSeparator = '.';
		
		// Obfuscate filename and/or extension if necessary:
		if(obfuscateFilename)
			filename = BinaryHelpers.toHexadecimealString(Hashing.getMD5Hash(filename.getBytes()).toByteArray(), 16, true); // Format: HEX(MD5(filename))
		if(obfuscateExtension)
		{
			extensionSeparator = FILENAME_ELEMENT_SEPARATOR; // '_' instead of '.'
			extension = ROT13.rot13NumRot5(extension).toUpperCase(); // Format: UPPERCASE(ROT13(extension))
		}
		
		return filename + extensionSeparator + extension;
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
	
	@Override
	public boolean enter(Controller controller, FieldParameters arguments, boolean withPage)
	{
		if(!withPage)
			return controller.enterMediaField(this, arguments);
		return true;
	}
	
}
