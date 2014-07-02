/**
 * 
 */
package uk.ac.ucl.excites.sapelli.collector.xml;

import org.xml.sax.SAXException;

import uk.ac.ucl.excites.sapelli.collector.model.Project;
import uk.ac.ucl.excites.sapelli.collector.model.TransmissionSettings;
import uk.ac.ucl.excites.sapelli.shared.util.xml.SubtreeParser;
import uk.ac.ucl.excites.sapelli.shared.util.xml.XMLAttributes;
import uk.ac.ucl.excites.sapelli.transmission.EncryptionSettings;
import uk.ac.ucl.excites.sapelli.transmission.modes.sms.SMSAgent;

/**
 * @author mstevens
 * 
 */
public class ConfigurationParser extends SubtreeParser
{

	// STATICS--------------------------------------------------------

	// TAGS
	static private final String TAG_CONFIGURATION = "Configuration";
	static private final String TAG_LOGGING = "Logging";
	static private final String TAG_TRANSMISSION = "Transmission";
	static private final String TAG_DROPBOX_UPLOAD = "DropboxUpload";
	static private final String TAG_HTTP_UPLOAD = "HTTPUpload";
	static private final String TAG_SMS_UPLOAD = "SMSUpload";
	static private final String TAG_ENCRYPTION = "Encryption";

	// ATTRIBUTES
	static private final String ATTRIBUTE_ENABLED = "enabled";
	static private final String ATTRIBUTE_MOBILE_DATA = "allowMobileData";
	static private final String ATTRIBUTE_ROAMING = "allowRoaming";

	// DYNAMICS-------------------------------------------------------
	private Project project;
	private TransmissionSettings transmissionSettings;

	public ConfigurationParser(ProjectParser projectParser)
	{
		super(projectParser, TAG_CONFIGURATION);
		this.project = projectParser.getProject();
	}

	@Override
	public void parseStartElement(String uri, String localName, String qName, XMLAttributes attributes) throws SAXException
	{
		// <Configuration>
		if(qName.equals(TAG_CONFIGURATION))
		{
			activate();
		}
		// children of <Configuration>
		else if(isActive())
		{
			// <Logging>
			if(qName.equals(TAG_LOGGING))
			{
				project.setLogging(attributes.getBoolean(ATTRIBUTE_ENABLED, Project.DEFAULT_LOGGING));
			}
			// <Transmission>
			else if(qName.equals(TAG_TRANSMISSION))
			{
				if(project.getTransmissionSettings() != null)
					throw new SAXException("There can be only one <" + TAG_TRANSMISSION + "> tag.");
				transmissionSettings = new TransmissionSettings();
				project.setTransmissionSettings(transmissionSettings);
			}
			// <DropboxUpload>
			else if(qName.equals(TAG_DROPBOX_UPLOAD))
			{
				if(transmissionSettings == null)
					throw new SAXException("<" + TAG_DROPBOX_UPLOAD + "> should only appear in <" + TAG_TRANSMISSION + ">.");
				transmissionSettings.setDropboxUpload(attributes.getBoolean(ATTRIBUTE_ENABLED, TransmissionSettings.DEFAULT_DROPBOX_UPLOAD));
				transmissionSettings.setDropboxAllowMobileData(attributes.getBoolean(ATTRIBUTE_MOBILE_DATA, TransmissionSettings.DEFAULT_DROPBOX_ALLOW_MOBILE_DATA));
				transmissionSettings.setDropboxAllowRoaming(attributes.getBoolean(ATTRIBUTE_ROAMING, TransmissionSettings.DEFAULT_DROPBOX_ALLOW_ROAMING));
			}
			// <HTTPUpload>
			else if(qName.equals(TAG_HTTP_UPLOAD))
			{
				if(transmissionSettings == null)
					throw new SAXException("<" + TAG_HTTP_UPLOAD + "> should only appear in <" + TAG_TRANSMISSION + ">.");
				transmissionSettings.setHTTPUpload(attributes.getBoolean(ATTRIBUTE_ENABLED, TransmissionSettings.DEFAULT_HTTP_UPLOAD));
				String server = attributes.getValue("server");
				if(server != null && !server.isEmpty())
					transmissionSettings.setServerAddress(server);
				transmissionSettings.setHTTPAllowMobileData(attributes.getBoolean(ATTRIBUTE_MOBILE_DATA, TransmissionSettings.DEFAULT_HTTP_ALLOW_MOBILE_DATA));
				transmissionSettings.setHTTPAllowRoaming(attributes.getBoolean(ATTRIBUTE_ROAMING, TransmissionSettings.DEFAULT_HTTP_ALLOW_ROAMING));
			}
			// <SMSUpload>
			else if(qName.equals(TAG_SMS_UPLOAD))
			{
				if(transmissionSettings == null)
					throw new SAXException("<" + TAG_SMS_UPLOAD + "> should only appear in <" + TAG_TRANSMISSION + ">.");
				transmissionSettings.setSMSUpload(attributes.getBoolean(ATTRIBUTE_ENABLED, TransmissionSettings.DEFAULT_SMS_UPLOAD));
				String relay = attributes.getValue("relay");
				if(relay != null && !relay.isEmpty())
					transmissionSettings.setSMSRelay(new SMSAgent(relay));
				transmissionSettings.setSMSAllowRoaming(attributes.getBoolean(ATTRIBUTE_ROAMING, TransmissionSettings.DEFAULT_SMS_ALLOW_ROAMING));
			}
			// <Encryption>
			else if(qName.equals(TAG_ENCRYPTION))
			{
				if(transmissionSettings == null)
					throw new SAXException("<" + TAG_ENCRYPTION + "> should only appear in <" + TAG_TRANSMISSION + ">.");
				EncryptionSettings encryptSettings = new EncryptionSettings();
				encryptSettings.setAllowEncryption(attributes.getBoolean(ATTRIBUTE_ENABLED, EncryptionSettings.DEFAULT_ALLOW_ENCRYPTION));
				// TODO default project passwd?
				transmissionSettings.setEncryptionSettings(encryptSettings);
			}
			// Add future configuration elements here...
			
			// <?> in <Configuration>
			else
			{
				addWarning("Ignored unrecognised or invalidly placed element <" + qName + "> occuring within <" + TAG_CONFIGURATION + ">.");
			}
		}
		// <?> outside of <Configuration> (shouldn't happen)
		else
		{
			throw new IllegalArgumentException("ConfigurationParser only deals with elements that are equal to, or contained within <" + TAG_CONFIGURATION + ">.");
		}
	}

	@Override
	public void parseEndElement(String uri, String localName, String qName) throws SAXException
	{
		// </Transmission>
		if(qName.equals(TAG_TRANSMISSION))
		{
			transmissionSettings = null;
		}
		// </Configuration>
		else if(qName.equals(TAG_CONFIGURATION))
		{
			deactivate();
		}
	}
	
	@Override
	protected void reset()
	{
		// does nothing (this SubTreeParser is single use anyway)
	}

	@Override
	protected boolean isSingleUse()
	{
		return true; //only 1 <Configuration> element per Project
	}

}
