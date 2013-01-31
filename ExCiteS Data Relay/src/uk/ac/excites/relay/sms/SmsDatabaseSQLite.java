package uk.ac.excites.relay.sms;

import java.io.UnsupportedEncodingException;
import java.util.ArrayList;
import java.util.List;

import uk.ac.excites.relay.Constants;
import android.content.ContentValues;
import android.content.Context;
import android.database.Cursor;
import android.database.sqlite.SQLiteDatabase;
import android.database.sqlite.SQLiteOpenHelper;
import android.util.Log;

public class SmsDatabaseSQLite extends SQLiteOpenHelper
{
	// Define Database Parameters
	private static final int DATABASE_VERSION = 1;
	private static final String DATABASE_NAME = "ExCiteS_Relay_SQLite";
	private static final String TABLE_SMS = "Sms";

	// Contacts Table Columns names
	private static final String KEY_ID = "id";
	private static final String KEY_NUMBER = "number";
	private static final String KEY_TIME = "timestamp";
	private static final String KEY_MESSAGE = "message";

	public SmsDatabaseSQLite(Context context)
	{
		super(context, DATABASE_NAME, null, DATABASE_VERSION);
	}

	// Creating Tables
	@Override
	public void onCreate(SQLiteDatabase db)
	{
		String CREATE_SMS_TABLE = "CREATE TABLE " + TABLE_SMS + "(" + KEY_ID + " INTEGER PRIMARY KEY AUTOINCREMENT, " + KEY_NUMBER + " TEXT, " + KEY_TIME + " INTEGER, " + KEY_MESSAGE + " TEXT" + ") ";
		if (Constants.DEBUG_LOG)
			Log.i(Constants.TAG, "SQL: " + CREATE_SMS_TABLE);
		db.execSQL(CREATE_SMS_TABLE);
	}

	// Updating Tables
	@Override
	public void onUpgrade(SQLiteDatabase db, int oldVersion, int newVersion)
	{
		// Drop older table if existed
		db.execSQL("DROP TABLE IF EXISTS " + TABLE_SMS);

		// Create tables again
		onCreate(db);
	}

	/**
	 * All CRUD(Create, Read, Update, Delete) Operations
	 */

	// Store a SMS Object
	public void storeSmsObject(SmsObject sms)
	{
		SQLiteDatabase db = this.getWritableDatabase();

		ContentValues values = new ContentValues();
		values.put(KEY_NUMBER, sms.getTelephoneNumber());
		values.put(KEY_TIME, sms.getMessageTimestamp());
		try
		{
			String data = new String(sms.getMessageData(), "UTF-8");
			values.put(KEY_MESSAGE, data);
		} catch (UnsupportedEncodingException e)
		{
			e.printStackTrace();
		}

		// Inserting Row
		long id = db.insert(TABLE_SMS, null, values);
		db.close();

		sms.setId(id);

		if (Constants.DEBUG_LOG)
			Log.i(Constants.TAG, "-- Stored SMSObject to DB: " + sms.toString().substring(0, 60) + "... ---");
	}

	// Retrieve all SMS
	public List<SmsObject> retrieveSmsObjects()
	{
		List<SmsObject> smsList = new ArrayList<SmsObject>();

		String selectQuery = "SELECT * FROM " + TABLE_SMS;

		SQLiteDatabase db = this.getWritableDatabase();
		Cursor cursor = db.rawQuery(selectQuery, null);

		// looping through all rows and adding to list
		if (cursor.moveToFirst())
		{
			do
			{
				SmsObject sms = new SmsObject();
				sms.setId(cursor.getLong(0));
				sms.setTelephoneNumber(cursor.getString(1));
				sms.setMessageTimestamp(cursor.getLong(2));
				try
				{
					sms.setMessageData(cursor.getString(3).getBytes("UTF-8"));
				} catch (UnsupportedEncodingException e)
				{
				}
				smsList.add(sms);

			} while (cursor.moveToNext());
		}

		if (Constants.DEBUG_LOG)
			Log.i(Constants.TAG, "-- Retrieving SMSObjects from the DB: found: " + smsList.size());

		return smsList;
	}

	public void deleteSmsObject(SmsObject sms)
	{
		long id = sms.getId();

		SQLiteDatabase db = this.getWritableDatabase();
		db.delete(TABLE_SMS, KEY_ID + " = ?", new String[]
		{ String.valueOf(id) });
		db.close();
	}

	public void deleteSmsObjects(List<SmsObject> smsList)
	{
		for (SmsObject sms : smsList)
			deleteSmsObject(sms);
	}

	public void deleteAllSmsObjects()
	{

		SQLiteDatabase db = this.getWritableDatabase();

		db.delete(TABLE_SMS, null, null);
		// print the size
		retrieveSmsObjects();
	}

	/**
	 * Add some SMS messages to the db
	 * 
	 * @param number
	 *            of SMS messages to add
	 */
	public void populateDb(int number)
	{
		for (int i = 0; i < number; i++)
		{
			String telephoneNumber = "+44000000" + i;
			long messageTimestamp = System.currentTimeMillis();
			byte[] messageData = null;
			try
			{
				messageData = ("Hello world " + i).getBytes("UTF-8");
			} catch (UnsupportedEncodingException e)
			{
			}

			SmsObject sms = new SmsObject(telephoneNumber, messageTimestamp, messageData);
			storeSmsObject(sms);
		}
	}
}
