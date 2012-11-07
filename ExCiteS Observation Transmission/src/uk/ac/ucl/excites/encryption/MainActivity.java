package uk.ac.ucl.excites.encryption;

import android.app.Activity;
import android.os.Bundle;
import android.view.Menu;
import android.widget.TextView;

public class MainActivity extends Activity {
	String passwordDec;
	byte[] passwordEnc;

	private TextView textField;

	@Override
	public void onCreate(Bundle savedInstanceState) {
		super.onCreate(savedInstanceState);
//		setContentView(R.layout.activity_main);

		String content = "Text to be encrypted";

		try {
			passwordEnc = AESencrp.encrypt(content);
		} catch (Exception e) {
			e.printStackTrace();
		}

		try {
			passwordDec = AESencrp.decrypt(passwordEnc);
		} catch (Exception e) {
			e.printStackTrace();
		}

//		textField = (TextView) findViewById(R.id.textView);
//		textField.setText("Plain Text : " + content + "\n"
//				+ "Encrypted Text : " + passwordEnc + "\n"
//				+ "Decrypted Text : " + passwordDec);

	}

	@Override
	public boolean onCreateOptionsMenu(Menu menu) {
//		getMenuInflater().inflate(R.menu.activity_main, menu);
		return true;
	}
}
