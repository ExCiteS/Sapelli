package uk.ac.ucl.excites.sapelli.server;

import java.io.FileWriter;
import java.io.IOException;
import java.io.PrintWriter;
import java.io.StringWriter;

import javax.servlet.ServletContext;
import javax.servlet.ServletException;
import javax.servlet.http.HttpServlet;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import org.apache.commons.codec.binary.Base64;

import uk.ac.ucl.excites.collector.project.db.DataAccess;
import uk.ac.ucl.excites.collector.project.db.ProjectModelProvider;
import uk.ac.ucl.excites.sapelli.server.db.DataAccessHelper;
import uk.ac.ucl.excites.storage.model.Record;
import uk.ac.ucl.excites.transmission.crypto.Hashing;
import uk.ac.ucl.excites.transmission.sms.Message;
import uk.ac.ucl.excites.transmission.sms.SMSAgent;
import uk.ac.ucl.excites.transmission.sms.SMSTransmission;
import uk.ac.ucl.excites.transmission.sms.binary.BinaryMessage;
import uk.ac.ucl.excites.transmission.sms.binary.BinarySMSTransmission;
import uk.ac.ucl.excites.util.BinaryHelpers;

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
		//FileWriter recordLog = new FileWriter(ProjectUpload.getProjectsUploadFolderPath(context) + "record.csv", true);
		
		String smsID = request.getParameter("smsID");
		// Set smsID to -1 if it is null
		smsID = (smsID == null) ? "-1" : smsID;
		String smsPhoneNumber = request.getParameter("smsPhoneNumber");
		String smsTimestamp = request.getParameter("smsTimestamp");
		byte[] smsData = Base64.decodeBase64(request.getParameter("smsData"));
		
		FileWriter smsBackupLog = new FileWriter(ProjectUpload.getProjectsUploadFolderPath(context) + "SMS_" + smsID + "_" + smsPhoneNumber + "_" + smsTimestamp + ".txt", true);
		smsBackupLog.append(request.getParameter("smsData"));
		smsBackupLog.flush();
		smsBackupLog.close();

		// TODO Log SMS
		logSMStoCsv(smsLog, smsID, smsPhoneNumber, smsTimestamp, BinaryHelpers.toHexadecimealString(Hashing.getMD5Hash(smsData).toByteArray()));
		//logToCsvLine(errorLog, "Test error log");
		
		try
		{
			BinaryMessage sms = new BinaryMessage(new SMSAgent(smsPhoneNumber), smsData);
			logToCsvLine(smsLog, "Received SMS " + sms.getPartNumber() + "/" + sms.getTotalParts() + " of transmission with ID " + sms.getTransmissionID() + ".");
		}
		catch(Exception e)
		{
			StringWriter sw = new StringWriter();
			PrintWriter pw = new PrintWriter(sw);
			e.printStackTrace(pw);
			logToCsvLine(errorLog, sw.toString());
		}
		
//		// Save Received SMS, add to transmission and try to decode records
//		try
//		{
//			BinaryMessage sms = new BinaryMessage(new SMSAgent(smsPhoneNumber), smsData);
//			SMSTransmission transmission = dao.retrieveSMSTransmission(sms.getTransmissionID());
//			if(transmission == null)
//			{
//				logToCsvLine(smsLog, "No transmisison with ID " + sms.getTransmissionID() + " found, creating new one.");
//				transmission = new BinarySMSTransmission(new ProjectModelProvider(dao));
//			}
//			else
//				logToCsvLine(smsLog, "Found existing transmisison with ID " + sms.getTransmissionID() + ".");
//				
//			transmission.addPart(sms);
//			sms.setTransmission(transmission);
//		
//			logToCsvLine(smsLog, "Received SMS " + sms.getPartNumber() + "/" + sms.getTotalParts() + " of transmission with ID " + transmission.getID() + ".");
//			
//			//Try to decode: //TODO put this in a separate thread
//			if(transmission.isComplete())
//			{
//				logToCsvLine(smsLog, "Transmission is complete");
//				transmission.receive();
//				
//				//store the records:
//				for(Record r : transmission.getRecords())
//				{
//					logToCsvLine(smsLog, "Decoded record: " + r.toString());
//					dao.store(r);
//				}
//			}
//			else
//			{
//				logToCsvLine(smsLog, "Transmission is incomplete, it has " + transmission.getParts().size() + "/" + transmission.getTotalParts() + " parts.");
//				logToCsvLine(smsLog, "  Messages received so far:");
//				for(Message m : transmission.getParts())
//					logToCsvLine(smsLog, "    - message " + m.getPartNumber() + "/" + m.getTotalParts());
//			}
//			dao.store(transmission); //!!!
//			dao.commit();
//			
//			if(dao.retrieveSMSTransmission(transmission.getID()) != null)
//				logToCsvLine(smsLog, "Transmission successfully stored");
//			else
//				logToCsvLine(smsLog, "Transmission not successfully stored");
//		}
//		catch(Exception e)
//		{
//			StringWriter sw = new StringWriter();
//			PrintWriter pw = new PrintWriter(sw);
//			e.printStackTrace(pw);
//			logToCsvLine(errorLog, sw.toString());
//		}

		// TODO Print ok or error
		out.println("OK:" + smsID);
		out.close();
		
		errorLog.close();
		smsLog.close();
	}

	private static void logSMStoCsv(FileWriter writer, String smsID, String smsPhoneNumber, String smsTimestamp, String smsData)
	{
		String line = smsID + "," + smsPhoneNumber + "," + smsTimestamp + "," + smsData;
		logToCsvLine(writer, line);
	}

	private static void logToCsvLine(FileWriter writer, String msg)
	{
		try
		{
			writer.append(msg + "\n");
			writer.flush();
		}
		catch(IOException e)
		{
			e.printStackTrace(System.err);
		}
	}

}
