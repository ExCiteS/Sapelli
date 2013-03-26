package uk.ac.uk.excites.server;

import java.io.FileWriter;
import java.io.IOException;
import java.io.PrintWriter;

import javax.servlet.ServletContext;
import javax.servlet.ServletException;
import javax.servlet.http.HttpServlet;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import org.apache.commons.codec.binary.Base64;

import uk.ac.ucl.excites.collector.project.db.DataAccess;
import uk.ac.ucl.excites.collector.project.db.ProjectModelProvider;
import uk.ac.ucl.excites.storage.model.Record;
import uk.ac.ucl.excites.transmission.sms.SMSAgent;
import uk.ac.ucl.excites.transmission.sms.SMSTransmission;
import uk.ac.ucl.excites.transmission.sms.binary.BinaryMessage;
import uk.ac.ucl.excites.transmission.sms.binary.BinarySMSTransmission;
import uk.ac.uk.excites.server.db.DataAccessHelper;

/**
 * Servlet implementation to Receive SMS Transmitions from Relay
 * 
 * @author Michalis Vitos
 */
public class ServerDataReceiver extends HttpServlet
{
	private static final long serialVersionUID = 5655090058815084878L;
	private DataAccess dao;

	/**
	 * @see HttpServlet#HttpServlet()
	 */
	public ServerDataReceiver()
	{
		super();
	}

	/**
	 * @see HttpServlet#doGet(HttpServletRequest request, HttpServletResponse response)
	 */
	protected void doGet(HttpServletRequest request, HttpServletResponse response) throws ServletException, IOException
	{
		// Print a HTTP_CONNECTIVITY_OK
		PrintWriter out = response.getWriter();
		out.println("HTTP_CONNECTIVITY_OK");
		out.close();
	}

	/**
	 * @see HttpServlet#doPost(HttpServletRequest request, HttpServletResponse response)
	 */
	protected void doPost(HttpServletRequest request, HttpServletResponse response) throws ServletException, IOException
	{

		// Get context
		ServletContext context = getServletContext();
		// Get the session container
		dao = DataAccessHelper.getInstance(request);
		// Get a writer
		PrintWriter out = response.getWriter();

		// Logging for errors and SMS
		FileWriter errorLog = new FileWriter(ProjectUpload.getProjectsUploadFolderPath(context) + "errors.csv", true);
		FileWriter smsLog = new FileWriter(ProjectUpload.getProjectsUploadFolderPath(context) + "sms.csv", true);

		String smsID = request.getParameter("smsID");
		// Set smsID to -1 if it is null
		smsID = (smsID == null) ? "-1" : smsID;
		String smsPhoneNumber = request.getParameter("smsPhoneNumber");
		String smsTimestamp = request.getParameter("smsTimestamp");
		byte[] smsData = Base64.decodeBase64(request.getParameter("smsData"));

		// TODO Log SMS
		logSMStoCsv(smsLog, smsID, smsPhoneNumber, smsTimestamp, new String(smsData));
		
		// Save Received SMS, add to transmission and try to decode records
		try
		{
			BinaryMessage sms = new BinaryMessage(new SMSAgent(smsPhoneNumber), smsData);
			SMSTransmission transmission = dao.retrieveSMSTransmission(sms.getTransmissionID());
			if(transmission == null)
				transmission = new BinarySMSTransmission(new ProjectModelProvider(dao));
			transmission.addPart(sms);
			
			//Try to decode: //TODO put this in a separate thread
			if(transmission.isComplete())
			{
				transmission.receive();
				
				//store the records:
				for(Record r : transmission.getRecords())
				{
					logToCsvLine(errorLog, r.toString());
					dao.store(r);
				}
			}
			dao.store(transmission); //!!!
		}
		catch(Exception e)
		{
			logToCsvLine(errorLog, e.getStackTrace().toString());
		}


		// TODO Print ok or error
		out.println("OK:" + smsID);
		out.close();
	}

	private static void logSMStoCsv(FileWriter writer, String smsID, String smsPhoneNumber, String smsTimestamp, String smsData)
	{
		String line = smsID + "," + smsPhoneNumber + "," + smsTimestamp + "," + smsData + '\n';
		logToCsvLine(writer, line);
	}

	private static void logToCsvLine(FileWriter writer, String msg)
	{
		try
		{
			writer.append(msg);
			writer.flush();
			writer.close();
		}
		catch(IOException e)
		{
			e.printStackTrace(System.err);
		}
	}

}
