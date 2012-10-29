package com.example.observations;

import java.util.ArrayList;
import java.util.List;
import java.util.Random;

import android.os.Bundle;
import android.app.Activity;
import android.view.Menu;
import android.widget.TextView;

public class MainActivity extends Activity {

	private TextView textField;
	private List<Observation> ObsInTrans;
	private List<Observation> ObsNotInTrans;
	private Observation[] observations;
	private Transaction[] transactions;

	@Override
	public void onCreate(Bundle savedInstanceState) {
		super.onCreate(savedInstanceState);
		setContentView(R.layout.activity_main);

		// call method to generate 100 observations
		this.observations = new Observation[100];
		this.observations = generateObservations();

		// call method to generate 5 transactions
		this.transactions = new Transaction[5];
		this.transactions = generateTransactions();

		// create db
		ObservationFactory myObsFac = new ObservationFactory();
		myObsFac.db();
		
		// store generated observations in db
		for (int i = 0; i < observations.length; i++) {
			myObsFac.storeObservation(observations[i]);
		}
		
		// store generated transactions in db
		for (int i = 0; i < transactions.length; i++) {
			myObsFac.storeTransaction(transactions[i]);
		}

		// call methods to check with observations/transaction have not yet been sent/retrieved
		List<Transaction> notSent = myObsFac.transNotSent();
		List<Transaction> notReceived = myObsFac.transNotReceived();
		
		// locally store list containing all observations retrieved from the db
		List<Observation> allObs = myObsFac.retrieveObservations();
		
		// locally store list containing all transactions retrieved from the db
		List<Transaction> allTrans = myObsFac.retrieveTransactions();

		// create list containing all observations that are contained in transactions
		ObsInTrans = new ArrayList<Observation>();
		for (int i = 0; i < allTrans.size(); i++)
			for(Observation o : allTrans.get(i).getObservations())
				ObsInTrans.add(o);

		// create list containing all observations
		ObsNotInTrans = new ArrayList<Observation>();
		for (int i = 0; i < allObs.size(); i++) {
			ObsNotInTrans.add(allObs.get(i));
		}

		// delete observations that are already contained in any transaction
		ObsNotInTrans.retainAll(ObsInTrans); 


		// initialise textfield
		textField = (TextView) findViewById(R.id.Textfield);

		// print transactions that have not yet been sent/received
		String printNotSent = "";
		for (int i = 0; i < notSent.size(); i++) {
			printNotSent += "Not sent: " + notSent.get(i).getID() + "\n";
		}
		String printNotReceived = "";
		for (int i = 0; i < notReceived.size(); i++) {
			printNotReceived += "Not received: " + notReceived.get(i).getID()
					+ "\n";
		}
		textField.setText(printNotSent + printNotReceived);
		
		// call method to close the database file and release all resources associated with it
		myObsFac.close();

	}

	@Override
	public boolean onCreateOptionsMenu(Menu menu) {
		getMenuInflater().inflate(R.menu.activity_main, menu);
		return true;
	}

	// generate random observations
	public Observation[] generateObservations() {

		for (int i = 0; i < observations.length; i++) {

			String decision = "desc" + new Random().nextInt(10);
			Float lat = new Random().nextFloat() * (90 - (-90)) + (-90);
			Float lng = new Random().nextFloat() * (180 - (-180)) + (-180);
			Float alt = new Random().nextFloat() * (1000 - (0)) + (0);
			Float gpsAcc = new Random().nextFloat() * (500 - (0)) + (0);
			Float bearing = new Random().nextFloat() * (360 - (0)) + (0);
			long timestamp = System.currentTimeMillis();

			observations[i] = new Observation(decision, lat, lng, alt,
					gpsAcc, bearing, timestamp);
		}
		return observations;
	}

	// generate transactions consisting of 10 observations stored in the db
	public Transaction[] generateTransactions() {

		for (int i = 0; i < transactions.length; i++) {

			Observation[] obs10;
			obs10 = new Observation[10];
			System.arraycopy(observations, i * 10, obs10, 0, 10);
			Integer ID = i + 1;

			boolean sent = false;
			if (new Random().nextInt() % 2 == 0) {
				sent = true;
			} else {
				sent = false;
			}

			boolean received = false;
			if (sent == true) {
				if (new Random().nextInt() % 2 == 0) {
					received = true;
				}
			} else {
				received = false;
			}

			transactions[i] = new Transaction(ID, sent, received);
			for(Observation o : obs10)
				transactions[i].addObservation(o);
		}
		return transactions;
	}
}
