/**
 * 
 */
package uk.ac.ucl.excites.collector.project.model.fields;

import java.io.File;
import java.io.IOException;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import org.joda.time.DateTime;

import uk.ac.ucl.excites.collector.project.data.FormEntry;
import uk.ac.ucl.excites.collector.project.model.Form;
import uk.ac.ucl.excites.collector.project.ui.Controller;
import uk.ac.ucl.excites.collector.project.ui.FieldUI;
import uk.ac.ucl.excites.storage.model.IntegerColumn;
import uk.ac.ucl.excites.storage.model.Record;
import uk.ac.ucl.excites.transmission.crypto.Hashing;
import uk.ac.ucl.excites.util.BinaryHelpers;
import uk.ac.ucl.excites.util.ROT13;

/**
 * @author mstevens
 *
 */
public abstract class MediaField extends Field
{

	//static public final int DEFAULT_MIN = 0;
	static public final int DEFAULT_MAX = 255; //column will use 1 byte
	
	static private final Pattern REGEX_PATTERN_FOR_MEDIA_FILE_WITH_OBFUSCATED_EXTENSION = Pattern.compile("^([0-9A-F]{32})_([0-9A-Z]+)$");
	
	//protected int min;
	protected int max;
	protected ChoiceField disableChoice;

	public MediaField(Form form, String id)
	{
		this(form, id, /*DEFAULT_MIN,*/ DEFAULT_MAX, null); //Use on byte --> up to 255 items
	}

	/**
	 * @param id
	 * @param max
	 * @param disableChoice
	 */
	public MediaField(Form form, String id, /*int min,*/ int max, ChoiceField disableChoice)
	{
		super(form, id);
		if(id == null)
			throw new NullPointerException("ID of top-level field cannot be null");
		setMax(max); //setMinMax(min, max);
		this.disableChoice = disableChoice;
	}
	
	public abstract String getMediaType();
	
	public String getFileExtension()
	{
		return getFileExtension(getMediaType()).toLowerCase();
	}
	
	protected abstract String getFileExtension(String mediaType);

//	/**
//	 * @return the min
//	 */
//	public int getMin()
//	{
//		return min;
//	}
	
	/**
	 * @return the max
	 */
	public int getMax()
	{
		return max;
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
		((IntegerColumn) form.getColumnFor(this)).storeValue(record, Long.valueOf(++currentCount));
	}

	public File getNewTempFile(Record record) throws IOException
	{
		String filename = generateFilename(record, getCount(record), true);
		String dataFolderPath = form.getProject().getTempFolder().getAbsolutePath(); //getTempFolder() does the necessary checks (IOException is thrown in case of trouble)
		return new File(dataFolderPath + File.separator + filename);
	}
	
	public String generateFilename(Record record, int attachmentNumber, boolean obfuscatedExtension)
	{
		FormEntry entry = new FormEntry(form, record);
		//Elements:
		DateTime dt = entry.getStartTime(true);
		long deviceID = entry.getDeviceID();
		int fieldIdx = form.getFieldIndex(this);
		String mediaType = getMediaType();
		//Assemble:
		String message = dt.toString() + Long.toString(deviceID) + Integer.toString(fieldIdx) + mediaType + Integer.toString(attachmentNumber);
		//Return MD5 hash as hexadecimal String:
		return 	BinaryHelpers.toHexadecimealString(Hashing.getMD5Hash(message.getBytes()).toByteArray(), 16) +
				(obfuscatedExtension ? 	"_" + ROT13.rot13NumRot5(getFileExtension()).toUpperCase() :
										"." + getFileExtension());
	}
	
	public static String getNonObfuscatedFilename(String filename)
	{
		Matcher matcher = REGEX_PATTERN_FOR_MEDIA_FILE_WITH_OBFUSCATED_EXTENSION.matcher(filename);
		if(matcher.find() && matcher.groupCount() == 2)
		{	// Got match!
			/*
			 * System.out.println("Found value: " + matcher.group(0)); //entire expression
			 * System.out.println("Found value: " + matcher.group(1)); //hash part
			 * System.out.println("Found value: " + matcher.group(2)); //ROT13-ed and uppercased extension
			 */
			return matcher.group(1) /*hash*/ + "." + ROT13.rot13NumRot5(matcher.group(2)).toLowerCase();
		}
		else
			// No match, return filename as-is:
			return filename;
	}
	
	@Override
	public boolean enter(Controller controller)
	{
		return controller.enterMediaField(this);
	}
	
	@Override
	public void leave(FieldUI ui, Controller controller)
	{
		controller.leaveMediaField(ui, this);
	}
	
}
