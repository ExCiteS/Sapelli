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

package uk.ac.ucl.excites.sapelli.relay.sms;

import java.io.File;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Date;
import java.util.List;
import java.util.Locale;

import uk.ac.ucl.excites.sapelli.relay.util.Debug;
import uk.ac.ucl.excites.sapelli.relay.util.FileHelpers;
import android.content.ContentValues;
import android.content.Context;
import android.database.Cursor;
import android.database.sqlite.SQLiteDatabase;
import android.database.sqlite.SQLiteOpenHelper;
import android.os.Environment;
import android.widget.Toast;

/**
 * Class for creating and maintaining the database
 * 
 * @author Michalis Vitos
 * 
 */
public class SmsDatabaseSQLite extends SQLiteOpenHelper
{
	// Define Database Parameters
	private static final int DATABASE_VERSION = 2;
	private static final String DATABASE_NAME = "ExCiteS_Relay_SQLite";
	public static final String TABLE_SMS = "Sms";
	private static final String TABLE_SMS_TMP = "Sms_tmp";

	// Sms Table Columns names
	private static final String KEY_ID = "id";
	private static final String KEY_NUMBER = "number";
	private static final String KEY_TIME = "timestamp";
	private static final String KEY_MESSAGE = "message";
	private static final String KEY_RECEIVED = "dateReceived";
	private static final String KEY_SENT = "dateSent";

	public static final String DATE_FORMAT = "KK:mm:ss a dd-MM-yyyy";

	public SmsDatabaseSQLite(Context context)
	{
		super(context, DATABASE_NAME, null, DATABASE_VERSION);
	}

	// Creating Tables
	@Override
	public void onCreate(SQLiteDatabase db)
	{
		String CREATE_SMS_TABLE = "CREATE TABLE " + TABLE_SMS + "(" + KEY_ID + " INTEGER PRIMARY KEY AUTOINCREMENT, " + KEY_NUMBER + " TEXT, " + KEY_TIME
				+ " INTEGER, " + KEY_MESSAGE + " TEXT, " + KEY_RECEIVED + " TEXT, " + KEY_SENT + " TEXT" + ") ";
		Debug.d("SQL: " + CREATE_SMS_TABLE);
		db.execSQL(CREATE_SMS_TABLE);
	}

	// Updating Tables
	@Override
	public void onUpgrade(SQLiteDatabase db, int oldVersion, int newVersion)
	{
		Debug.d("Old is: " + oldVersion + " New is: " + newVersion);

		// Change the name of the old table
		db.execSQL("ALTER TABLE " + TABLE_SMS + " RENAME TO " + TABLE_SMS_TMP + "_" + oldVersion);

		// Create tables again
		onCreate(db);

		// Copy files from to Sms_tmp_1 to the current Sms table
		// This is used to from version 1 to 2
		String COPY_SMS_TABLE = "INSERT INTO " + TABLE_SMS + " SELECT NULL, " + KEY_NUMBER + ", " + KEY_TIME + ", " + KEY_MESSAGE + ", NULL, NULL FROM "
				+ TABLE_SMS_TMP + "_1";
		Debug.d("SQL: " + COPY_SMS_TABLE);
		db.execSQL(COPY_SMS_TABLE);
		// TODO Make sure that everything is copied correctly before deleting the old table
		// Drop the Old Table
		// db.execSQL("DROP TABLE IF EXISTS " + TABLE_SMS_TMP + "_1");

		db.close();
	}

	/**
	 * All CRUD(Create, Read, Update, Delete) Operations
	 */

	/**
	 * Stores an SmsObject to the db
	 * 
	 * @param sms
	 */
	public void storeSms(SmsObject sms)
	{
		SQLiteDatabase db = this.getWritableDatabase();

		ContentValues values = new ContentValues();
		values.put(KEY_NUMBER, sms.getTelephoneNumber());
		values.put(KEY_TIME, sms.getMessageTimestamp());
		String data = sms.getMessageData();
		values.put(KEY_MESSAGE, data);
		values.put(KEY_RECEIVED, System.currentTimeMillis());

		// Inserting Row
		long id = db.insert(TABLE_SMS, null, values);
		db.close();

		sms.setId(id);

		Debug.d("Stored: " + sms.toString());
	}

	/**
	 * Update the Sent info
	 * 
	 * @param sms
	 */
	public void updateSent(SmsObject sms)
	{
		long id = sms.getId();

		SQLiteDatabase db = this.getWritableDatabase();
		db.execSQL("UPDATE " + TABLE_SMS + " SET " + KEY_SENT + "=" + System.currentTimeMillis() + " WHERE " + KEY_ID + "=" + id);
		db.close();
	}

	/**
	 * Retrieve all SmsObjects
	 * 
	 * @return
	 */
	public List<SmsObject> getUnsentSms()
	{
		List<SmsObject> smsList = new ArrayList<SmsObject>();

		String selectQuery = "SELECT * FROM " + TABLE_SMS + " WHERE " + KEY_SENT + " IS NULL";

		SQLiteDatabase db = this.getWritableDatabase();
		Cursor cursor = db.rawQuery(selectQuery, null);

		// looping through all rows
		while(cursor.moveToNext())
		{
			SmsObject sms = new SmsObject();
			sms.setId(cursor.getLong(0));
			sms.setTelephoneNumber(cursor.getString(1));
			sms.setMessageTimestamp(cursor.getLong(2));
			sms.setMessageData(cursor.getString(3));
			smsList.add(sms);
		}

		db.close();
		Debug.d("Found: " + smsList.size());

		return smsList;
	}

	protected void deleteSms(SmsObject sms)
	{
		long id = sms.getId();

		SQLiteDatabase db = this.getWritableDatabase();
		db.delete(TABLE_SMS, KEY_ID + " = ?", new String[] { String.valueOf(id) });
		db.close();
	}

	/**
	 * Delete a list of Sms Objects
	 * 
	 * @param smsList
	 */
	protected void deleteSms(List<SmsObject> smsList)
	{
		for(SmsObject sms : smsList)
			deleteSms(sms);
	}

	/**
	 * ATTENTION: Delete all Sms Objects
	 * 
	 * @param smsList
	 */
	protected void deleteAllSms()
	{

		SQLiteDatabase db = this.getWritableDatabase();

		db.delete(TABLE_SMS, null, null);
		// print the size
		getUnsentSms();
	}

	/**
	 * Add some SMS messages to the db
	 * 
	 * @param number
	 *            of SMS messages to add
	 */
	protected void populateDb(int number)
	{
		for(int i = 0; i < number; i++)
		{
			String telephoneNumber = "+44000000" + i;
			long messageTimestamp = System.currentTimeMillis();
			String messageData = "This is a SMS message # " + i;

			SmsObject sms = new SmsObject(telephoneNumber, messageTimestamp, messageData);
			storeSms(sms);
		}
	}

	/**
	 * Get the total number of SMS received by the Relay
	 * 
	 * @return
	 */
	public int getTotal()
	{
		SQLiteDatabase db = this.getWritableDatabase();

		String query = "SELECT * FROM " + TABLE_SMS;
		Cursor cursor = db.rawQuery(query, null);

		cursor.moveToNext();

		return cursor.getCount();
	}

	/**
	 * Get the total number of SMS sent by the Relay
	 * 
	 * @return
	 */
	public int getSent()
	{
		SQLiteDatabase db = this.getWritableDatabase();

		String query = "SELECT * FROM " + TABLE_SMS + " WHERE " + KEY_SENT + " IS NOT NULL";
		Cursor cursor = db.rawQuery(query, null);

		cursor.moveToNext();
		cursor.getCount();

		return cursor.getCount();
	}

	/**
	 * Get the timestamp of the last received message
	 * 
	 * @return
	 */
	public long getLastReceived()
	{
		SQLiteDatabase db = this.getWritableDatabase();

		long lastReceived = 0;

		String query = "SELECT " + KEY_RECEIVED + " FROM " + TABLE_SMS + " WHERE " + KEY_ID + " = (SELECT MAX(" + KEY_ID + ") FROM " + TABLE_SMS + ");";
		Cursor cursor = db.rawQuery(query, null);

		while(cursor.moveToNext())
		{
			lastReceived = cursor.getLong(cursor.getColumnIndex(KEY_RECEIVED));
		}

		return lastReceived;
	}

	/**
	 * Get the timestamp of the last sent message
	 * 
	 * @return
	 */
	public long getLastSent()
	{
		SQLiteDatabase db = this.getWritableDatabase();

		long lastSent = 0;

		String query = "SELECT " + KEY_SENT + " FROM " + TABLE_SMS + " WHERE " + KEY_ID + " = (SELECT MAX(" + KEY_ID + ") FROM " + TABLE_SMS + " WHERE "
				+ KEY_SENT + " IS NOT NULL);";
		Cursor cursor = db.rawQuery(query, null);

		while(cursor.moveToNext())
		{
			lastSent = cursor.getLong(cursor.getColumnIndex(KEY_SENT));
		}

		return lastSent;
	}

	/**
	 * Get all the Tables of the db
	 * 
	 * @return
	 */
	public List<String> getAllTables()
	{
		final ArrayList<String> tables = new ArrayList<String>();

		SQLiteDatabase db = this.getWritableDatabase();
		Cursor cursor = db.rawQuery("SELECT name FROM sqlite_master WHERE type='table'", null);

		while(cursor.moveToNext())
		{
			String tmpTable = cursor.getString(0);
			if(tmpTable.equalsIgnoreCase("android_metadata") || tmpTable.equalsIgnoreCase("sqlite_sequence"))
				continue;
			else
				tables.add(tmpTable);
		}

		db.close();

		return tables;
	}

	/**
	 * Dump a table
	 * 
	 * @param table
	 * @param rowLength
	 */
	public String dumpHtmlTable(String table, int limit)
	{
		SQLiteDatabase db = this.getWritableDatabase();
		String selectQuery = null;
		
		// Print table header
		if(table.equals(TABLE_SMS))
			selectQuery = "SELECT " + KEY_ID + " as 'SMS #', " + KEY_NUMBER + " as 'Phone Number', " + KEY_TIME + " as 'Time Sent', " + KEY_RECEIVED
					+ " as 'Time Received at Relay', " + KEY_SENT + " as 'Time Sent to Server' FROM " + table + " ORDER BY "
					+ KEY_ID + " DESC LIMIT " + limit;
		else
			selectQuery = "SELECT * FROM " + table + " ORDER BY " + KEY_ID + " DESC LIMIT " + limit;
			
		String output = "<h3>Log Result for table: " + table + "</h3>";
		output += "<table border=1 style='font-size:10pt; white-space: nowrap;'>";

		Cursor cursor = db.rawQuery(selectQuery, null);

		// Print column names
		String[] columnNames = cursor.getColumnNames();

		output += "<tr>";
		for(String column : columnNames)
		{
			// Don't print the message as is binary
			if(column.equals(KEY_MESSAGE))
				continue;

			output += "<th>";
			output += column;
			output += "</th>";
		}
		output += "</tr>";

		// looping through all rows and print the content
		while(cursor.moveToNext())
		{
			output += "<tr>";
			for(String column : columnNames)
			{
				// Don't print the message as is binary
				if(column.equals(KEY_MESSAGE))
					continue;

				output += "<td>";
				SimpleDateFormat dateFormat = new SimpleDateFormat(DATE_FORMAT, Locale.ENGLISH);

				if(column.contains("Time"))
				{
					final long date = cursor.getLong(cursor.getColumnIndex(column));
					output += (date != 0) ? dateFormat.format(new Date(date)) : "-";
					continue;
				}

				output += cursor.getString(cursor.getColumnIndex(column));
				output += "</td>";
			}
			output += "</tr>";
		}

		output += "</table>";
		db.close();

		return output;
	}

	/**
	 * Dump all tables
	 */
	public void dumpDb()
	{
		List<String> tables = getAllTables();

		for(String table : tables)
		{
			dumpHtmlTable(table, 2000);
		}
	}

	/**
	 * Return the string with a padding
	 * 
	 * @param string
	 * @param pad
	 * @param left
	 * @return
	 */
	public static String addPad(String text, String pad, int length, String side)
	{
		for(int i = text.length(); i < length; i++)
		{
			if(side.equalsIgnoreCase("right"))
				text = text + pad;
			else if(side.equalsIgnoreCase("left"))
				text = pad + text;
		}

		return text;
	}

	/**
	 * Copy Database File to the destination
	 * 
	 * @param dstFilePath
	 */
	public static void copyDBtoSD(Context context, String dstFilePath)
	{
		String currentDb = context.getDatabasePath(DATABASE_NAME).getAbsolutePath();
		FileHelpers.copyFile(currentDb, dstFilePath);
		Toast.makeText(context, "Database was copied to folder: " + dstFilePath, Toast.LENGTH_LONG).show();
	}

	/**
	 * Copy Database File to the SD Card
	 * 
	 * @param dstFilePath
	 */
	public static void copyDBtoSD(Context context)
	{
		String dstFilePath = Environment.getExternalStorageDirectory().getAbsolutePath() + File.separator + DATABASE_NAME;
		copyDBtoSD(context, dstFilePath);
	}

}
