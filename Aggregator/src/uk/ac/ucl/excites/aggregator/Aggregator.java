/**
 * 
 */
package uk.ac.ucl.excites.aggregator;

import java.io.File;
import java.io.IOException;
import java.lang.reflect.Field;
import java.nio.ByteBuffer;
import java.nio.charset.Charset;
import java.nio.file.DirectoryStream;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.Comparator;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.SortedSet;
import java.util.TreeSet;
import java.util.zip.ZipException;

import org.apache.commons.codec.binary.Base64;
import org.apache.commons.io.FileUtils;
import org.apache.commons.io.FilenameUtils;
import org.joda.time.DateTime;
import org.joda.time.format.DateTimeFormatter;
import org.joda.time.format.ISODateTimeFormat;

import uk.ac.ucl.excites.collector.database.DB4OConnector;
import uk.ac.ucl.excites.collector.database.DataAccess;
import uk.ac.ucl.excites.collector.project.ProjectModelProvider;
import uk.ac.ucl.excites.collector.project.data.FormEntry;
import uk.ac.ucl.excites.collector.project.io.ExCiteSFileLoader;
import uk.ac.ucl.excites.collector.project.model.Form;
import uk.ac.ucl.excites.collector.project.model.Project;
import uk.ac.ucl.excites.collector.project.util.DuplicateException;
import uk.ac.ucl.excites.storage.model.Column;
import uk.ac.ucl.excites.storage.model.DateTimeColumn;
import uk.ac.ucl.excites.storage.model.Record;
import uk.ac.ucl.excites.storage.model.Schema;
import uk.ac.ucl.excites.storage.util.IntegerRangeMapping;
import uk.ac.ucl.excites.storage.xml.RecordsExporter;
import uk.ac.ucl.excites.storage.xml.RecordsImporter;
import uk.ac.ucl.excites.transmission.DecodeException;
import uk.ac.ucl.excites.transmission.IncompleteTransmissionException;
import uk.ac.ucl.excites.transmission.Settings;
import uk.ac.ucl.excites.transmission.compression.CompressorFactory.CompressionMode;
import uk.ac.ucl.excites.transmission.sms.Message;
import uk.ac.ucl.excites.transmission.sms.SMSAgent;
import uk.ac.ucl.excites.transmission.sms.SMSTransmission;
import uk.ac.ucl.excites.transmission.sms.binary.BinaryMessage;
import uk.ac.ucl.excites.transmission.sms.binary.BinarySMSTransmission;
import uk.ac.ucl.excites.util.BinaryHelpers;
import uk.ac.ucl.excites.util.FileHelpers;
import uk.ac.ucl.excites.util.FileWriter;
import uk.ac.ucl.excites.util.VersionComparator;

import com.db4o.ObjectContainer;

/**
 * @author mstevens
 * 
 */
public class Aggregator
{
	
	private static final String DEFAULT_PATH_MSTEVENS = "C:\\Users\\mstevens\\Documents\\Werk\\UCL-ExCiteS\\Intelligent Maps\\Data assembly";

	static public final String FOLDER_STORAGE = "storage";
	static public final String DATABASE_NAME = "ExCiteS.db4o";
	static public final String FOLDER_STORAGE_PROJECTS = "projects";
	static public final String FOLDER_STORAGE_TEMP = "temp";

	static public final String FOLDER_INPUT = "input";
	static public final String FOLDER_IN_PROJECTS = "projects";
	static public final String FOLDER_IN_DB4O_DUMPS = "db4o";
	static public final String FOLDER_IN_RECORD_EXPORTS = "exports";
	static public final String FOLDER_IN_SMS = "sms";

	static public final String FOLDER_OUTPUT = "output";

	static private Aggregator aggregator;

	private Path inputFolder;
	private Path storageFolder;
	private Path tempStorageFolder;
	private Path outputFolder;

	private ObjectContainer db;

	private DataAccess dao;
	private Set<Record> allRecords;
	
	private static DateTimeFormatter formatter = ISODateTimeFormat.dateTime();

	/**
	 * @param args
	 */
	public static void main(String[] args)
	{
		if(args.length < 1)
			aggregator = new Aggregator(Paths.get(DEFAULT_PATH_MSTEVENS)); // System.out.println("Please provide a working path");
		else
			aggregator = new Aggregator(Paths.get(args[0]));

		aggregator.loadProjects();
		System.out.println("");
		
		aggregator.printSchemaAndProjectList();
		System.out.println("");
		
		aggregator.loadRecords();

		aggregator.export();
		
		// Close db:
		aggregator.db.close();
	}

	public Aggregator(Path workingPath)
	{
		this.inputFolder = workingPath.resolve(FOLDER_INPUT);
		this.storageFolder = workingPath.resolve(FOLDER_STORAGE);
		this.tempStorageFolder = storageFolder.resolve(FOLDER_STORAGE_TEMP);
		this.outputFolder = workingPath.resolve(FOLDER_OUTPUT);
		
		// Clean storage & output folders (TODO make this optional)
		try
		{
			FileUtils.cleanDirectory(storageFolder.toFile());
			FileUtils.cleanDirectory(outputFolder.toFile());
		}
		catch(IOException ioe)
		{
			ioe.printStackTrace();
			System.exit(1);
		}

		// Setup database
		try
		{
			db = DB4OConnector.open(storageFolder.resolve(DATABASE_NAME).toString());
			dao = new DataAccess(db);
		}
		catch(Exception e)
		{
			System.err.println("Error opening the database: " + e.getMessage());
			e.printStackTrace(System.err);
			return;
		}

		// Data structures:
		allRecords = new HashSet<Record>();
	}

	public void loadRecords()
	{
		Set<Record> dbRecords = loadDatabaseDumps(); //new HashSet<Record>(); 
		System.out.println("");
		
		Set<Record> xmlRecords = loadExportedRecords(); //new HashSet<Record>(); 
		System.out.println("");
		
		Set<Record> smsRecords = loadSMS(); //new HashSet<Record>();
		System.out.println("");

		// List unique records per source:
		System.out.println("Loaded " + dbRecords.size() + " unique records from DB4O dumps.");
		System.out.println("Loaded " + xmlRecords.size() + " unique records from XML exports.");
		System.out.println("Loaded " + smsRecords.size() + " unique records from SMS transmissions.");
		System.out.println("");
		
		// Count duplicates between all pairs:
		Set<Record> dbAndXml = new HashSet<Record>();
		dbAndXml.addAll(dbRecords);
		for(Record xmlRec : xmlRecords) //deal with floored timestamps in XML records
			if(!dbAndXml.contains(xmlRec) && !dbAndXml.contains(shiftOneSecond(xmlRec, true, false)) && !dbAndXml.contains(shiftOneSecond(xmlRec, false, true)) && !dbAndXml.contains(shiftOneSecond(xmlRec, true, true)))
				dbAndXml.add(xmlRec);
		System.out.println("There are " + (dbRecords.size() + xmlRecords.size() - dbAndXml.size()) + " duplicate records loaded both from DB4O dumps and XML exports.");

		Set<Record> dbAndSms = new HashSet<Record>();
		dbAndSms.addAll(dbRecords);
		dbAndSms.addAll(smsRecords);
		System.out.println("There are " + (dbRecords.size() + smsRecords.size() - dbAndSms.size()) + " duplicate records loaded both from DB4O dumps and SMS transmissions.");
		
		Set<Record> smsAndXml = new HashSet<Record>();
		smsAndXml.addAll(smsRecords);
		for(Record xmlRec : xmlRecords) //deal with floored timestamps in XML records
			if(!smsAndXml.contains(xmlRec) && !smsAndXml.contains(shiftOneSecond(xmlRec, true, false)) && !smsAndXml.contains(shiftOneSecond(xmlRec, false, true)) && !smsAndXml.contains(shiftOneSecond(xmlRec, true, true)))
				smsAndXml.add(xmlRec);
		System.out.println("There are " + (xmlRecords.size() + smsRecords.size() - smsAndXml.size()) + " duplicate records loaded both from XML exports and SMS transmissions.");
		
		System.out.println("");
		
		// Assemble unique records (take SMS before XML!):
		allRecords.addAll(dbRecords);
		int dbUnique = allRecords.size();
		allRecords.addAll(smsRecords);
		int smsUnique = allRecords.size() - dbUnique;
		for(Record xmlRec : xmlRecords) //deal with floored timestamps in XML records
			if(!allRecords.contains(xmlRec) && !allRecords.contains(shiftOneSecond(xmlRec, true, false)) && !allRecords.contains(shiftOneSecond(xmlRec, false, true)) && !allRecords.contains(shiftOneSecond(xmlRec, true, true)))
				allRecords.add(xmlRec);
		int xmlUnique = allRecords.size() - dbUnique - smsUnique;
		
		System.out.println("DB4O contributed: " + dbUnique + " unique records");
		System.out.println("SMS contributed: " + smsUnique + " unique records");
		System.out.println("XML contributed: " + xmlUnique + " unique records");
		
		System.out.println("\nTotal unique records: " + allRecords.size() + ".\n");
	}

	private void loadProjects()
	{
		try (DirectoryStream<Path> stream = Files.newDirectoryStream(inputFolder.resolve(FOLDER_IN_PROJECTS), "*.excites"))
		{
			Path projectStorageFolder = storageFolder.resolve(FOLDER_STORAGE_PROJECTS);
			for(Path entry : stream)
			{
				System.out.println("Loading file: " + entry.getFileName());
				ExCiteSFileLoader loader = new ExCiteSFileLoader(projectStorageFolder.toString(), tempStorageFolder.toString());
				Project p = loader.load(entry.toFile());
				dao.store(p);
				p.generateDocumentation();
				System.out.println("\tAdded project: " + p.toString());
			}
		}
		catch(IOException ioe)
		{
			System.err.println("IO exception: " + ioe.getMessage());
			ioe.printStackTrace(System.err);
		}
		catch(DuplicateException e)
		{
			System.err.println("Error storing project: " + e.getMessage());
			e.printStackTrace(System.err);
		}
		catch(Exception e)
		{
			System.err.println("Error: " + e.getMessage());
			e.printStackTrace(System.err);
		}
	}

	public void printSchemaAndProjectList()
	{
		List<Schema> schemas = new ArrayList<Schema>(dao.retrieveSchemata());
		Collections.sort(schemas);
		for(Schema s : schemas)
		{
			System.out.println("Schema ID " + s.getID() + ", version " + s.getVersion() + " [Min size: " + s.getMinimumSize() + " bits; Max size: " + s.getMaximumSize() + " bits] is used by:");
			List<Form> forms = new ArrayList<Form>(dao.retrieveForms(s.getID(), s.getVersion()));
			Collections.sort(forms, new Comparator<Form>()
			{
				private VersionComparator vc = new VersionComparator();

				@Override
				public int compare(Form f1, Form f2)
				{
					return vc.compare(f1.getProject().getVersion(), f2.getProject().getVersion());
				}
			});
			for(Form f : forms)
				System.out.println(" - Project " + f.getProject().toString() + ": Form " + f.getName() + ".");
			System.out.print("\n");
		}
		System.out.println("Projects: " + dao.retrieveProjects().size());
		System.out.println("Schemata: " + schemas.size());
	}

	private Set<Record> loadDatabaseDumps()
	{
		Set<Record> importedRecords = new HashSet<Record>(); 
		int dumps = 0, newTotal = 0, skippedTotal = 0, duplicateTotal = 0;
		Path db4oFolder = inputFolder.resolve(FOLDER_IN_DB4O_DUMPS);
		if(!Files.exists(db4oFolder))
		{
			System.out.println("Input folder " + db4oFolder.toString() + " does not exist.");
			return importedRecords;
		}
		try (DirectoryStream<Path> stream = Files.newDirectoryStream(db4oFolder, "*.db4o"))
		{
			for(Path entry : stream)
			{
				dumps++;
				System.out.println("Reading " + entry.getFileName() + "...");
				
				//Make temp copy
				Path tempCopy = tempStorageFolder.resolve("TEMP_" + System.currentTimeMillis() + ".db4o");
				Files.copy(entry, tempCopy);
				
				ObjectContainer dumpDB = DB4OConnector.open(tempCopy.toString());
				DataAccess dumpAccess = new DataAccess(dumpDB);
				int newCurrent = 0, skippedCurrent = 0, duplicateCurrent = 0;
				
				//Query for records:
				List<Record> records = new ArrayList<Record>(dumpAccess.retrieveRecords()); //copy to new ArrayList to avoid losing objects due to GC

				//Process records:
				for(Record record : records)
				{
					if(!replaceSchema(record)) // fixes records that have been assigned the wrong schema version due to project version in which we forgot to increase schema versions
					{	// schema is not replaced yet... link record to existing schema object:
						Schema s = dao.retrieveSchema(record.getSchema().getID(), record.getSchema().getVersion());
						if(s != null)
						{
							try
							{ // Try fixing problems with dumpRec schema such that the existing one will be accepted as a compatible replacement:
								fixSchema(record.getSchema());
								
								// Try replacing the schema:
								record.setSchema(s);
							}
							catch(IllegalArgumentException iae)
							{
								System.out.println("\tSkipped record because we do not have a compatible schema instance (ID: " + record.getSchema().getID() + "; version: " + record.getSchema().getVersion() + ").");
								skippedCurrent++;
								continue;
							}
							catch(Exception e)
							{
								e.printStackTrace(System.err);
								skippedCurrent++;
								continue;
							}
						}
						else
						{
							System.out.println("Skipped record due to unknown schema (ID: " + record.getSchema().getID() + "; version: " + record.getSchema().getVersion() + ")");
							skippedCurrent++;
							continue;
						}
					}
					if(importedRecords.add(record))
						newCurrent++;
					else
						duplicateCurrent++;
				}
				System.out.println("\t" + newCurrent + " new records added (" + skippedCurrent + " skipped, " + duplicateCurrent + " duplicated records already loaded from other db).");
				newTotal += newCurrent;
				skippedTotal += skippedCurrent;
				duplicateTotal += duplicateCurrent;
				dumpDB.close();
				
				//Delete temp copy:
				Files.delete(tempCopy);
			}
		}
		catch(IOException ioe)
		{
			System.err.println("IO exception: " + ioe.getMessage());
			ioe.printStackTrace(System.err);
		}
		catch(Exception e)
		{
			System.err.println("Error: " + e.getMessage());
			e.printStackTrace(System.err);
		}
		System.out.println("Imported " + newTotal + " unique records (" + skippedTotal + " skipped, " + duplicateTotal + " duplicates) from " + dumps + " DB4O dumps.");
		return importedRecords;
	}

	private Set<Record> loadExportedRecords()
	{
		Set<Record> records = new HashSet<Record>();
		int files = 0, newTotal = 0, skippedTotal = 0, duplicateTotal = 0;
		RecordsImporter importer = new CustomRecordsImporter(dao, this);
		Path exportsFolder = inputFolder.resolve(FOLDER_IN_RECORD_EXPORTS);
		if(!Files.exists(exportsFolder))
		{
			System.out.println("Input folder " + exportsFolder.toString() + " does not exist.");
			return records;
		}
		try (DirectoryStream<Path> stream = Files.newDirectoryStream(exportsFolder, "*.xml"))
		{
			for(Path entry : stream)
			{
				files++;
				System.out.println("Parsing " + entry.getFileName() + "...");
				int newCurrent = 0, skippedCurrent = 0, duplicateCurrent = 0;
				for(Record exportRec : importer.importFrom(entry.toFile()))
				{
					//replacing schema's already happens from within CustomRecordsImporter
					if(records.add(exportRec))
						newCurrent++;
					else
						duplicateCurrent++;
				}
				for(String msg : importer.getWarnings())
				{
					System.out.println("\t" + msg);
					skippedCurrent++;
				}
				System.out.println("\t" + newCurrent + " new records added (" + skippedCurrent + " skipped, " + duplicateCurrent + " duplicates already loaded from other xml files).");
				newTotal += newCurrent;
				skippedTotal += skippedCurrent;
				duplicateTotal += duplicateCurrent;
			}
		}
		catch(IOException ioe)
		{
			System.err.println("IO exception: " + ioe.getMessage());
			ioe.printStackTrace(System.err);
		}
		catch(Exception e)
		{
			System.err.println("Error: " + e.getMessage());
			e.printStackTrace(System.err);
		}
		System.out.println("Imported " + newTotal + " unique records (" + skippedTotal + " skipped, " + duplicateTotal + " duplicates) from " + files + " export files.");
		return records;
	}

	static public final String TEST_SMS_CONTENT_1 = "This is a SMS message # "; // followed by a number
	static public final String TEST_SMS_CONTENT_2 = "This is a test!"; // followed by more text

	private Set<Record> loadSMS()
	{
		int skipped = 0, duplicates = 0;
		Set<Message> smsMessages = new HashSet<Message>();
		Set<Record> records = new HashSet<Record>();
		
		// READING SMS FILES AND FILTERING OUT TEST AND DUPLICATE MESSAGES...
		try (DirectoryStream<Path> stream = Files.newDirectoryStream(inputFolder.resolve(FOLDER_IN_SMS), "*.txt"))
		{
			for(Path entry : stream)
			{
				String[] parts = FilenameUtils.getBaseName(entry.toString()).split("_");
				//int smsID = Integer.parseInt(parts[1]);
				String senderPhoneNumber = parts[2];
				DateTime receivedAtByRelay = new DateTime(Long.parseLong(parts[3]));
				String smsText = readFile(entry.toString(), Charset.defaultCharset());
				// Check for test smses:
				if(smsText.startsWith(TEST_SMS_CONTENT_1))
				{
					// System.out.println("\tThis is a non-binary test SMS (#" + smsText.substring(TEST_SMS_CONTENT.length()) + "), skipping.");
					skipped++;
					continue;
				}

				//System.out.println("\nSMS: " + smsID + "; " + senderPhoneNumber + "; " + formatter.print(receivedAtByRelay));

				// Save Received SMS, add to transmission and try to decode records
				byte[] smsBytes = Base64.decodeBase64(smsText);
				BinaryMessage sms = new BinaryMessage(new SMSAgent(senderPhoneNumber), smsBytes, receivedAtByRelay);

				if(sms.getTransmissionID() == 84 && new String(smsBytes).startsWith(TEST_SMS_CONTENT_2))
				{
					// System.out.println("\tThis is a binary test SMS, skipping.");
					skipped++;
					continue;
				}

				// System.out.println("Received SMS " + sms.getPartNumber() + "/" + sms.getTotalParts() + " of transmission with ID " + transmission.getID() + ".");

				// Store SMS...
				if(!smsMessages.add(sms))
					duplicates++;
			}
		}
		catch(Exception e)
		{
			e.printStackTrace(System.err);
		}

		System.out.println("Total SMS messages received: " + smsMessages.size() + " (skipped " + skipped + " test messages & " + duplicates + " duplicates).");

		// System.out.println("Received messages per sender, transmission ID & #parts:");
		// printSMSMessages(smsMessages);

		// DECODING...
		int decodedTransmissions = 0, decodedMessages = 0, newFromSMS = 0, duplicateFromSMS = 0;
		Map<SMSAgent, Map<Integer, Map<Integer, List<Message>>>> smsBySender = groupMessages(smsMessages);
		for(SMSAgent sender : smsBySender.keySet())
		{
			for(int transmissionID : smsBySender.get(sender).keySet())
			{
				for(int numberOfParts : smsBySender.get(sender).get(transmissionID).keySet())
				{
					List<Message> msgs = smsBySender.get(sender).get(transmissionID).get(numberOfParts);
					Collections.sort(msgs, new MessageReceivalTimeComparator()); // Sort by reception time!
					List<BinarySMSTransmission> transmissions = new ArrayList<BinarySMSTransmission>();
					
					// Deal with different situations...
					if(numberOfParts == 1 && !msgs.isEmpty())
						// Each msg is an individual 1-part transmission
						for(Message msg : msgs)
							transmissions.add(new BinarySMSTransmission(new ProjectModelProvider(dao), Arrays.asList(msg)));
					else if(numberOfParts == msgs.size())
						// We've got exactly as many parts as needed
						transmissions.add(new BinarySMSTransmission(new ProjectModelProvider(dao), msgs));
					else if(msgs.size() % numberOfParts == 0) // we have a multiple of the number of parts
						// "Mixed up" (repeated use of same ID by same sender, for transmissions with equal # of parts)
						for(int i = 0; i < msgs.size() / numberOfParts; i++) // Try subselections
							transmissions.add(new BinarySMSTransmission(new ProjectModelProvider(dao), msgs.subList(i * numberOfParts, (i + 1) * numberOfParts)));
					else if(msgs.size() < numberOfParts)
						// Incomplete transmissions (will be delt with below)
						transmissions.add(new BinarySMSTransmission(new ProjectModelProvider(dao), msgs));
					
					// Try to decode each transmission...
					for(BinarySMSTransmission transmission : transmissions)
					{
						List<Record> tRecords = null;
						System.out.println("\nAttempting decoding of transmission from " + transmission.getSender().getPhoneNumber() + ", with ID " + transmission.getID() + ", covering " + transmission.getCurrentNumberOfParts() + " messages ...");
						try
						{
							tRecords = decodeTransmission(transmission, null, null);
						}
						catch(Exception e)
						{
							System.out.println("\tDecoding failed completely (" + e.getMessage() + ").");
							continue;
						}
						// Process records:
						if(!tRecords.isEmpty()) // what if the transmission was empty? (no errors but also no records, is this possible?)
						{
							System.out.println("\tDecoding succesful, transmission contained " + tRecords.size() + " records (schema ID " + tRecords.get(0).getSchema().getID() + "; version " + tRecords.get(0).getSchema().getVersion() + ").");
							
							decodedTransmissions++;
							// Remove decoded messages & updated decodedMessagesCount:
							for(Message p : transmission.getParts())
								if(smsMessages.remove(p))
									decodedMessages++; //this way we don't count the dummy's that may have been added to incomplete transmissions
							// Store records:
							for(Record r : tRecords)
							{
								// System.out.println("\t- Decoded record: " + r.toString());
								try
								{
									// Add record:
									if(records.add(r))
										newFromSMS++;
									else
										duplicateFromSMS++;
								}
								catch(Throwable t)
								{
									System.out.println("Exction upon adding: " + r.toString());
								}
							}
							
							//Compression test
							//System.out.println("\tBest compression possible for this transmission: " + CompressorFactory.ApplyBestCompression(encodeRecords(tRecords)));
							//CompressorFactory.CompressionTest(encodeRecords(tRecords));
							
						}
						else
						{
							System.out.println("\tTransmission was empty (Schema ID " + transmission.getSchema().getID() + ", version: " + transmission.getSchema().getVersion() + " (minimum size: " + transmission.getSchema().getMinimumSize() + " bits.");
						}
					}
				}
			}
		}

		System.out.println("\nUndecodable messages by sender, transmission ID & #parts:");
		printSMSMessages(smsMessages);		
		System.out.println("\nDecoded " + newFromSMS + " unique records (" + duplicateFromSMS + " duplicates) from " + decodedTransmissions + " SMS transmissions (covering " + decodedMessages + " messages), " + smsMessages.size() + " messages remain undecoded.");

		return records;
	}
	
	private List<Record> decodeTransmission(SMSTransmission transmission, Schema schemaToUse, Settings settingsToUse) throws Exception
	{		
		// Try to decode:
		try
		{
			transmission.read(schemaToUse, settingsToUse);
		}
		catch(IncompleteTransmissionException ite)
		{
			System.out.println("\tTransmission is incomplete, it has " + transmission.getCurrentNumberOfParts() + "/" + transmission.getTotalNumberOfParts() + " parts.");
			System.out.println("\tMessages received so far:");
			for(Message m : transmission.getParts())
				System.out.println("\t\t- message " + m.getPartNumber() + "/" + m.getTotalParts() + ": " + BinaryHelpers.toHexadecimealString(m.getPayload()));
			throw ite;
			
			/*if(!transmission.hasPart(1))
			{
				System.out.println("\tFirst part missing, cannot attempt partial decode.");
				throw ite;				
			}
			else
			{
				System.out.println("\tAdding dummy parts...");
				for(int p = 1; p <= transmission.getTotalNumberOfParts(); p++)
					if(!transmission.hasPart(p))
						transmission.addPart(BinaryMessage.GetDummyPart(transmission.getSender(), transmission.getID(), p, transmission.getTotalNumberOfParts()));				
				
				// Strip GZIP header off of first part
				System.out.println("\tStripping GZIP header off of first part...");
				BinaryMessage firstPart = (BinaryMessage) transmission.getPart(1);
				byte[] deflatePayload = new byte[firstPart.payload.length - 10];
				ByteBuffer bff = ByteBuffer.wrap(deflatePayload);
				int schemaHeaderSizeBytes = (Schema.SCHEMA_ID_SIZE + Schema.SCHEMA_VERSION_SIZE) / 8;
				bff.put(Arrays.copyOfRange(firstPart.payload, 0, schemaHeaderSizeBytes));
				bff.put(Arrays.copyOfRange(firstPart.payload, schemaHeaderSizeBytes + 10, firstPart.payload.length)); //skip GZIP header
				firstPart.payload = deflatePayload;
				
				System.out.println("\tAttempting partial decode...");
				Settings deflateSettings = new Settings();
				deflateSettings.setCompressionMode(CompressionMode.DEFLATE);
				
				return decodeTransmission(transmission, schemaToUse, deflateSettings);
			}*/
		}
		catch(DecodeException de)
		{
			System.out.println("\tError upon decoding of transmission, using schema ID " + de.getSchema().getID() + ", version: " + de.getSchema().getVersion() + ".");
			//System.out.println("\tParts:");
			//for(Message msg : transmission.getParts())
			//	System.out.println("\t- Message " + msg.getPartNumber() + "/" + msg.getTotalParts() + ": " + BinaryHelpers.toHexadecimealString(msg.getPayload()));
			if(!de.getRecords().isEmpty())
			{
				System.out.println("\tRecords (partially) decoded so far: ");
				for(Record r : de.getRecords())
					System.out.println("\t\t- Decoded record: " + r.toString());
				Record first = de.getRecords().get(0);
				congoTimeShift(transmission.getSender(), first);
				//Try if we can replace schema by the correct one...
				Schema correctSchema = getCorrectSchema(first);
				if(correctSchema != first.getSchema())
				{
					System.out.println("\tTrying again with schema ID " + correctSchema.getID() + ", version " + correctSchema.getVersion());
					return decodeTransmission(transmission, correctSchema, settingsToUse);
				}
			}
			else
				System.out.println("\tNo records decoded.");
			
			System.out.println("\tException: " + de.getMessage());
			System.out.println("\tCause: " + de.getCause().getMessage());
			if(de.getCause().getCause() != null)
				System.out.println("\tSecundary cause: " + de.getCause().getCause().getMessage());
			//de.printStackTrace();
			throw de;
		}
		catch(Exception e)
		{
			//GZIP error
			if(e.getCause() instanceof ZipException)
			{
				System.out.println(e.getCause().getMessage());
				System.out.println("\tData not in GZIP format! Trying BZIP2 decompression...");
				Settings bzip2Settings = new Settings(transmission.getSettings());
				bzip2Settings.setCompressionMode(CompressionMode.BZIP2);
				return decodeTransmission(transmission, schemaToUse, bzip2Settings);
			}
			else
			{	// Other exception
				System.out.println("\tUnexpected exception upon processing of transmission from " + transmission.getSender().getPhoneNumber() + ", with ID " + transmission.getID() + ", and parts:");
				for(Message msg : transmission.getParts())
					System.out.println("\t\t- Message " + msg.getPartNumber() + "/" + msg.getTotalParts() + ": " + BinaryHelpers.toHexadecimealString(msg.getPayload()));
				System.out.println("\tException: " + e.getMessage());
				if(e.getCause() != null)
					System.out.println("\tCause: " + e.getCause().getMessage());
				e.printStackTrace(System.err);
				throw e;
			}
		}
		
		//Apply time fix...
		for(Record r : transmission.getRecords())
			congoTimeShift(transmission.getSender(), r);

		//Try if we can replace schema by the correct one...
		if(!transmission.getRecords().isEmpty())
		{
			Record first = transmission.getRecords().get(0);
			Schema correctSchema = getCorrectSchema(first);
			if(correctSchema != first.getSchema())
			{
				System.out.println("\tWrong schema (ID " + transmission.getSchema().getID() + ", version: " + transmission.getSchema().getVersion() + ") was used, trying again with schema ID " + correctSchema.getID() + ", version " + correctSchema.getVersion());
				return decodeTransmission(transmission, correctSchema, settingsToUse);
			}
		}
		
		return transmission.getRecords();
	}

	private Map<SMSAgent, Map<Integer, Map<Integer, List<Message>>>> groupMessages(Set<Message> smsMessages)
	{
		Map<SMSAgent, Map<Integer, Map<Integer, List<Message>>>> smsBySender = new HashMap<SMSAgent, Map<Integer, Map<Integer, List<Message>>>>();

		for(Message sms : smsMessages)
		{
			// sender -> transmissionID -> #parts -> messages
			Map<Integer, Map<Integer, List<Message>>> byTransmissionID = smsBySender.get(sms.getSender());
			if(byTransmissionID == null)
			{
				byTransmissionID = new HashMap<Integer, Map<Integer, List<Message>>>();
				smsBySender.put(sms.getSender(), byTransmissionID);
			}
			Map<Integer, List<Message>> byNumberOfParts = byTransmissionID.get(sms.getTransmissionID());
			if(byNumberOfParts == null)
			{
				byNumberOfParts = new HashMap<Integer, List<Message>>();
				byTransmissionID.put(sms.getTransmissionID(), byNumberOfParts);
			}
			List<Message> messages = byNumberOfParts.get(sms.getTotalParts());
			if(messages == null)
			{
				messages = new ArrayList<Message>();
				byNumberOfParts.put(sms.getTotalParts(), messages);
			}
			messages.add(sms);
		}

		return smsBySender;
	}

	private void printSMSMessages(Set<Message> smsMessages)
	{
		Map<SMSAgent, Map<Integer, Map<Integer, List<Message>>>> smsBySender = groupMessages(smsMessages);
		for(SMSAgent sender : smsBySender.keySet())
		{
			System.out.println("\tSender: " + sender.getPhoneNumber());
			for(int transmissionID : smsBySender.get(sender).keySet())
			{
				System.out.println("\t\tTransmission ID: " + transmissionID);
				for(int numberOfParts : smsBySender.get(sender).get(transmissionID).keySet())
				{
					System.out.println("\t\t\t#parts: " + numberOfParts);
					List<Message> msgs = smsBySender.get(sender).get(transmissionID).get(numberOfParts);
					Collections.sort(msgs);
					for(Message msg : msgs)
						System.out.println("\t\t\t\tMessage " + msg.getPartNumber() + "/" + msg.getTotalParts() + ": "
								+ BinaryHelpers.toHexadecimealString(msg.getPayload()));
				}
			}
		}
	}

	private static final DateTimeColumn century21NoMS = DateTimeColumn.Century21NoMS("For use in fixSchema()", false);
	private static DateTime firstUseOfOIFLEGv142And143 = formatter.withOffsetParsed().parseDateTime("2013-04-24T10:53:00.000+01:00");
	private static DateTime firstUseOfOIFLEGv144 = formatter.withOffsetParsed().parseDateTime("2013-04-29T10:18:57.000+01:00");

	private static DateTime firstUseOfAPv212 = formatter.withOffsetParsed().parseDateTime("2013-04-30T11:03:00.000+01:00");
	private static DateTime firstPossibleUseOfAPv22 = formatter.withOffsetParsed().parseDateTime("2013-04-30T13:30:00.000+01:00");

	public Schema getCorrectSchema(Record record)
	{
		Schema currentSchema = record.getSchema();
		DateTime startTime = ((DateTimeColumn) currentSchema.getColumn(Form.COLUMN_TIMESTAMP_START)).retrieveValue(record);
		
		/*
		 * OIFLEG (schema ID 200) schema version:
		 * 	- v1.4.2 & v1.4.3 have version 4 but it should have been 5 (as in v1.4.2.1 & v1.4.3.1)
		 * 	- v1.4.4 has version 4 but it should have been 6 (as in v1.4.4.1)
		 */
		Schema OIFLEG_200_v5 = dao.retrieveSchema(200, 5);
		Schema OIFLEG_200_v6 = dao.retrieveSchema(200, 6);
		if(currentSchema.getID() == 200 && currentSchema.getVersion() == 4)
		{
			if(startTime.isAfter(firstUseOfOIFLEGv142And143))
			{
				if(startTime.isBefore(firstUseOfOIFLEGv144))
				{
					//System.out.println("OIFLEG v1.4.2/v1.4.3 record (starttime: " + formatter.withZone(startTime.getZone()).print(startTime) + "), fixing schema version (5 instead of 4).");
					return OIFLEG_200_v5;
				}
				else
				{
					//System.out.println("OIFLEG v1.4.4 record (starttime: " + formatter.withZone(startTime.getZone()).print(startTime) + "), fixing schema version (6 instead of 4).");
					return OIFLEG_200_v6;
				}
			}
			else
			{
				//System.out.println("OIFLEG v1.4/v1.4.1 record (starttime: " + formatter.withZone(startTime.getZone()).print(startTime) + "), schema version correct (4).");
			}
		}

		/*
		 * AP (schema ID 100) schema version:
		 * - v2.1.2 has version 2 but it should have been 3 (as in v2.1.2.1)
		 * - v2.2 has version 3 but it should have been 4 (as in v2.2.0.1)
		 */
		Schema AP_100_v3 = dao.retrieveSchema(100, 3);
		Schema AP_100_v4 = dao.retrieveSchema(100, 4);
		if(currentSchema.getID() == 100 && currentSchema.getVersion() == 2)
		{
			if(startTime.isAfter(firstUseOfAPv212))
			{
				//System.out.println("AP v2.1.2 record (starttime: " + formatter.withZone(startTime.getZone()).print(startTime) + "), fixing schema version (3 instead of 2).");
				return AP_100_v3;
			}
		}
		if(currentSchema.getID() == 100 && currentSchema.getVersion() == 3)
		{
			if(startTime.isAfter(firstPossibleUseOfAPv22))
			{
				//System.out.println("AP v2.2 record (starttime: " + formatter.withZone(startTime.getZone()).print(startTime) + "), fixing schema version (4 instead of 2).");
				return AP_100_v4;
			}
		}

		//AP-based Test-project
		Schema AP_Test_v0 = dao.retrieveSchema(1, 0);
		Schema AP_Test_v1 = dao.retrieveSchema(1, 1);
		Schema AP_Test_v2 = dao.retrieveSchema(1, 2);
		if(currentSchema.getID() == 1)
		{
			if(currentSchema.getVersion() == 0 && currentSchema.getColumn("photo") != null)
			{
				if(currentSchema.getColumn("magneticNorth") != null)
					return AP_Test_v2; //use v2 instead of v0 if there is a magneticNorth column
				else
					return AP_Test_v1; //use v1 instead of v0 if there is a photo column but no magneticNorth column
			}
			else
			{
				return AP_Test_v0; //force this version of v0
			}
		}

		return currentSchema;
	}
	
	/**
	 * Replaces schema for records that have been created with project versions that had incorrect (i.e. non-increased) schema versions
	 * 
	 * @param record
	 * @return whether or not schema was replaced
	 */
	public boolean replaceSchema(Record record)
	{
		Schema currentSchema = record.getSchema();
		record.setSchema(getCorrectSchema(record), true);
		return currentSchema != record.getSchema();
	}

	/**
	 * This correct incorrect timestamps due the non-UTC hi/lo-bound on DateTimeColumns bug. 
	 */
	private void congoTimeShift(SMSAgent sender, Record record)
	{
		if(sender.getPhoneNumber().startsWith("+242"))
		{
			DateTimeColumn startTimeCol = ((DateTimeColumn) record.getSchema().getColumn(Form.COLUMN_TIMESTAMP_START));
			DateTimeColumn endTimeCol = ((DateTimeColumn) record.getSchema().getColumn(Form.COLUMN_TIMESTAMP_END));
			
			startTimeCol.storeValue(record, startTimeCol.retrieveValue(record).minusHours(1));
			endTimeCol.storeValue(record, endTimeCol.retrieveValue(record).minusHours(1));
		}
	}
	
	private Record shiftOneSecond(Record record, boolean start, boolean end)
	{
		Record copy = new Record(record); //make copy
		DateTimeColumn startTimeCol = ((DateTimeColumn) record.getSchema().getColumn(Form.COLUMN_TIMESTAMP_START));
		DateTimeColumn endTimeCol = ((DateTimeColumn) record.getSchema().getColumn(Form.COLUMN_TIMESTAMP_END));
		if(start)
			startTimeCol.storeValue(copy, startTimeCol.retrieveValue(copy).plusSeconds(1));
		if(end)
			endTimeCol.storeValue(copy, endTimeCol.retrieveValue(copy).plusSeconds(1));
		return copy;
	}

	/**
	 * Corrects:
	 * 	- renames "Anti Poaching" schemas to "AP",
	 *  - schemas from dumped db4o's that have non-UTC hi/lo-bound on DateTimeColumns,
	 * to allow these schemas to be replaced by a compatible correct one
	 * 
	 * @param schema
	 * @throws NoSuchFieldException
	 * @throws SecurityException
	 * @throws IllegalArgumentException
	 * @throws IllegalAccessException
	 */
	private void fixSchema(Schema schema) throws NoSuchFieldException, SecurityException, IllegalArgumentException, IllegalAccessException
	{
		// Rename "Anti Poaching" to "AP"
		if((schema.getID() == 100 || schema.getID() == 101) && schema.getName().equals("Anti Poaching"))
		{
			Field f = Schema.class.getDeclaredField("name");
			f.setAccessible(true);
			f.set(schema, "AP");
		}

		for(Column<?> col : schema.getColumns())
		{
			// Work-around for incorrect (non-UTC based) hi/lowbound on DateTimeColumns generated (bug was fixed in 5dfad63)
			if(col instanceof DateTimeColumn)
			{
				DateTimeColumn dtCol = (DateTimeColumn) col;
				Field f = DateTimeColumn.class.getDeclaredField("timeMapping");
				f.setAccessible(true);
				IntegerRangeMapping mapping = (IntegerRangeMapping) f.get(dtCol);
				if(mapping.getLowBound() != 946684800) // Correct values (for century21NoMS, UTC): LOW=946684800 & HIGH=4102444800
				{
					f.set(dtCol, new IntegerRangeMapping((IntegerRangeMapping) f.get(century21NoMS)));
					// System.out.println("\tFixed DateTimeColumn in schema (ID: " + schema.getID() + "; version: " + schema.getVersion() + ").");
				}
			}
		}
	}

	private void export()
	{
		// Sorts records by schema:
		Map<Schema, Set<Record>> recordsBySchema = new HashMap<Schema, Set<Record>>();
		for(Record r : allRecords)
		{
			Set<Record> recordSet = recordsBySchema.get(r.getSchema());
			if(recordSet == null)
			{
				recordSet = new HashSet<Record>();
				recordsBySchema.put(r.getSchema(), recordSet);
			}
			recordSet.add(r);
		}
		
		// Export project-by-project:
		for(Project project : dao.retrieveProjects())
		{
			Form form = project.getForms().get(0);
			Schema schema = form.getSchema();
			
			Set<Record> records = recordsBySchema.get(schema);
			if(records != null)
			{
				SortedSet<FormEntry> entries = new TreeSet<FormEntry>();
				for(Record r : records)
					entries.add(new FormEntry(form, r));
				
				List<Record> sortedRecords = new ArrayList<Record>();
				for(FormEntry fe : entries)
					sortedRecords.add(fe.getRecord());
				
				// XML export
				RecordsExporter re = new RecordsExporter(outputFolder.toString() + File.separatorChar, dao);
				try
				{
					re.exportRecords(sortedRecords, project.getName() + "_" + project.getVersion() + "_" + form.getName() + "_(" + schema.getID() + "_v" + schema.getVersion() + ")");
				}
				catch(Exception e)
				{
					e.printStackTrace();
				}
				
				// CSV export
				try
				{	//TODO create & use CSVExporter class (and abstract Exporter class)
					FileWriter writer = new FileWriter(outputFolder.toString() + File.separatorChar + project.getName() + "_" + project.getVersion() + "_" + form.getName() + "_(" + schema.getID() + "_v" + schema.getVersion() + ")" + ".csv");
					writer.open(FileHelpers.FILE_EXISTS_STRATEGY_REPLACE, FileHelpers.FILE_DOES_NOT_EXIST_STRATEGY_CREATE);
					writer.writeLine(schema.toCSVHeader(";") + ";ObjectID");
					int id = 1;
					for(Record record : sortedRecords)
						writer.writeLine(record.toCSVRow(";") + ";" + id++);
					writer.close();
				}
				catch(IOException ioe)
				{
					ioe.printStackTrace();
					return;
				}
				
			}
			
//			if(schema.getID() == 100 && schema.getVersion() == 4)
//			{
//				System.out.println(((ChoiceField) form.getField("Observation")).getDictionary().getChoice(16).getValue());
//			}
			
		}
	
	}
	
	public Form getMostRecentFromUsing(Schema schema)
	{
		List<Form> forms = dao.retrieveForms(schema.getID(), schema.getVersion());
		VersionComparator vc = new VersionComparator();
		Form mostRecent = null;
		for(Form f : forms)
			if(mostRecent == null || vc.compare(mostRecent.getProject().getVersion(), f.getProject().getVersion()) < 0)
				mostRecent = f;
		return mostRecent;
	}

	static String readFile(String path, Charset encoding) throws IOException
	{
		byte[] encoded = Files.readAllBytes(Paths.get(path));
		return encoding.decode(ByteBuffer.wrap(encoded)).toString();
	}
	
	public class MessageReceivalTimeComparator implements Comparator<Message>
	{

		@Override
		public int compare(Message m1, Message m2)
		{
			return m1.getReceivedAt().compareTo(m2.getReceivedAt());
		}

	}
	
}
