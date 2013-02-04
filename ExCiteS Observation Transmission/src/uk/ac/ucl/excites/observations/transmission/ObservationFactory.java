package uk.ac.ucl.excites.observations.transmission;

import java.util.List;

import android.util.Log;

import com.db4o.Db4oEmbedded;
import com.db4o.ObjectContainer;
import com.db4o.query.Predicate;

public class ObservationFactory {

	private final static String TAG = "EVENTS";
	private ObjectContainer db;

	// Opening the database
	public void db() {
		
		try {
			if (db == null || db.ext().isClosed()) {
				this.db = Db4oEmbedded.openFile(
						Db4oEmbedded.newConfiguration(),
						"/sdcard/external_sd/oberservation.db4o"); // "/sdcard/external_sd/oberservation.db4o",
																	// dbPath()
				Log.d(TAG, "opened new database connection");
			}

		} catch (Exception e) {
			Log.e(TAG, "unable to open database");
		}
	}

	// private String dbPath() {
	// return Environment.getExternalStorageDirectory().getPath() +
	// "/observation.db4o";
	// }

	// store new observation
	public void storeObservation(Observation obs) {
		db.store(obs);
	}

	// store new transaction
	public void storeTransaction(Transmission trans) {
		db.store(trans);
	}

	// retrieve all observations
	public List<Observation> retrieveObservations() {
		List<Observation> result = db.query(Observation.class);
		return result;
	}

	// retrieve all transactions
	public List<Transmission> retrieveTransactions() {
		List<Transmission> result = db.query(Transmission.class);
		return result;
	}

	// retrieve transactions that have not yet been sent
	public List<Transmission> transNotSent() {
		return db.query(new Predicate<Transmission>() {

			private static final long serialVersionUID = 4796848447268681614L;

			public boolean match(Transmission transaction) {
				return transaction.isSent() == false;
			}
		});
	}

	// retrieve transactions that have not yet been received
	public List<Transmission> transNotReceived() {
		return db.query(new Predicate<Transmission>() {

			private static final long serialVersionUID = -8293981698181882202L;

			public boolean match(Transmission transaction) {
				return transaction.isReceived() == false;
			}
		});
	}

	// close the database file and release all resources associated with it
	public void close() {
		db.close();
	}

}
