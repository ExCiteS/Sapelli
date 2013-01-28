package uk.ac.uk.excites.serverdatareceiver;

import java.io.FileWriter;
import java.io.IOException;
import java.io.PrintWriter;
import java.math.BigInteger;
import java.security.MessageDigest;
import java.security.NoSuchAlgorithmException;

import javax.servlet.ServletException;
import javax.servlet.http.HttpServlet;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

/**
 * Servlet implementation class DataReceiver
 */
public class ServerDataReceiver extends HttpServlet
{
	// private static final String SENDER_PHONE_NUMBER = "SenderPhoneNumber";
	private static final long serialVersionUID = 5655090058815084878L;

	/**
	 * @see HttpServlet#HttpServlet()
	 */
	public ServerDataReceiver()
	{
		super();
		// TODO Auto-generated constructor stub
	}

	/**
	 * @see HttpServlet#doGet(HttpServletRequest request, HttpServletResponse response)
	 */
	protected void doGet(HttpServletRequest request, HttpServletResponse response) throws ServletException, IOException
	{
		// String senderPhoneNumber = request.getParameter("senderPhoneNumber");
		// String messageDigest = request.getParameter("messageDigest");
		// request.setAttribute("phone", senderPhoneNumber); // This will be available as ${phone}
		// request.setAttribute("message", messageDigest); // This will be available as ${message}
		// request.getRequestDispatcher("DataReceiver.jsp").forward(request, response);

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
		String smsID = request.getParameter("smsID");
		// Set smsID to -1 if it is null
		smsID = (smsID == null) ? "-1" : smsID;
		String smsPhoneNumber = request.getParameter("smsPhoneNumber");
		String smsTimestamp = request.getParameter("smsTimestamp");
		String smsData = request.getParameter("smsData");

		// TODO Save Received SMS

		generateCsvFile(response, smsID, smsPhoneNumber, smsTimestamp, smsData);

		// TODO Print ok or error
		// then get the writer and write the response data
		PrintWriter out = response.getWriter();
		out.println("OK:" + smsID);
		out.close();

	}

	private static void generateCsvFile(HttpServletResponse response, String smsID, String smsPhoneNumber, String smsTimestamp, String smsData) throws IOException
	{
		try
		{
			FileWriter writer = new FileWriter("/var/lib/tomcat6/webapps/ServerDataReceiver/test.csv", true);
			
			writer.append(smsID + ",");
			writer.append(smsPhoneNumber + ",");
			writer.append(smsTimestamp + ",");
			writer.append(smsData);
			writer.append('\n');

			writer.flush();
			writer.close();
		} catch (IOException e)
		{
			// Debug Code
			PrintWriter out = response.getWriter();
			out.println("In the CSV Writer, error: " + e.toString());
		}
	}

	public static byte[] getSHA256Hash(byte[] data)
	{
		MessageDigest digest = null;
		try
		{
			digest = MessageDigest.getInstance("SHA-256");
		} catch (NoSuchAlgorithmException ex)
		{
			// Log.e(TAG, "Cannot get hash algorithm", ex);
		}
		digest.reset();
		return digest.digest(data);
	}

	public static String bin2Hex(byte[] data)
	{
		return String.format("%0" + (data.length * 2) + "X", new BigInteger(1, data));
	}

	public static String toBinaryString(byte b)
	{
		String str = "";
		for (int i = 7; i >= 0; i--)
			str += ((b & (1 << i)) != 0) ? "1" : "0";
		return str;
	}

}
