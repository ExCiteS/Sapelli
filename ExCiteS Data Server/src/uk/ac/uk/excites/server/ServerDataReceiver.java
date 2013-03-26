package uk.ac.uk.excites.server;

import java.io.FileWriter;
import java.io.IOException;
import java.io.PrintWriter;
import java.math.BigInteger;
import java.security.MessageDigest;
import java.security.NoSuchAlgorithmException;

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

		String smsID = request.getParameter("smsID");
		// Set smsID to -1 if it is null
		smsID = (smsID == null) ? "-1" : smsID;
		String smsPhoneNumber = request.getParameter("smsPhoneNumber");
		String smsTimestamp = request.getParameter("smsTimestamp");
		byte[] smsData = Base64.decodeBase64(request.getParameter("smsData"));
		
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
					dao.store(r);
			}
			dao.store(transmission); //!!!
		}
		catch(Exception e)
		{
			e.printStackTrace(System.err);
		}

		// TODO
		logToCsvFile(context, response, smsID, smsPhoneNumber, smsTimestamp, new String(smsData));

		// TODO Print ok or error
		out.println("OK:" + smsID);
		out.close();
	}

	private static void logToCsvFile(ServletContext context, HttpServletResponse response, String smsID, String smsPhoneNumber, String smsTimestamp,
			String smsData)
			throws IOException
	{
		try
		{
			// TODO
			String filePath = ProjectUpload.getProjectsUploadFolderPath(context) + "logging.csv";
			FileWriter writer = new FileWriter(filePath, true);

			writer.append(smsID + ",");
			writer.append(smsPhoneNumber + ",");
			writer.append(smsTimestamp + ",");
			writer.append(smsData);
			writer.append('\n');

			writer.flush();
			writer.close();
		}
		catch(IOException e)
		{
			// Debug Code
			PrintWriter out = response.getWriter();
			out.println("In the CSV Writer, error: " + e.toString());
		}
	}

	// TODO is needed?
	public static byte[] getSHA256Hash(byte[] data)
	{
		MessageDigest digest = null;
		try
		{
			digest = MessageDigest.getInstance("SHA-256");
		}
		catch(NoSuchAlgorithmException ex)
		{
			// Log.e(TAG, "Cannot get hash algorithm", ex);
		}
		digest.reset();
		return digest.digest(data);
	}

	// TODO is needed?
	public static String bin2Hex(byte[] data)
	{
		return String.format("%0" + (data.length * 2) + "X", new BigInteger(1, data));
	}

	// TODO is needed?
	public static String toBinaryString(byte b)
	{
		String str = "";
		for(int i = 7; i >= 0; i--)
			str += ((b & (1 << i)) != 0) ? "1" : "0";
		return str;
	}
}
