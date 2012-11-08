package uk.ac.ucl.excites.encryption;

import android.app.Activity;
import android.os.Bundle;
import android.view.Menu;
import android.widget.TextView;

public class MainActivity extends Activity {
	byte[] decoded;
	byte[] encoded;

//	private TextView textField;

	@Override
	public void onCreate(Bundle savedInstanceState) {
		super.onCreate(savedInstanceState);
//		setContentView(R.layout.activity_main);

		byte[] content = "Test".getBytes();
		AESencrp aes = new AESencrp();
		aes.getSHA256Hash("password");

		try {
			encoded = aes.encrypt(content);
		} catch (Exception e) {
			e.printStackTrace();
		}

		try {
			decoded = aes.decrypt(encoded);
		} catch (Exception e) {
			e.printStackTrace();
		}

//		textField = (TextView) findViewById(R.id.textView);
//		textField.setText("Plain : " + Arrays.toString(content) + "\n\n"
//				+ "Encrypted : " + Arrays.toString(encoded) + "\n\n"
//				+ "Decrypted : " + Arrays.toString(decoded));

	}

	@Override
	public boolean onCreateOptionsMenu(Menu menu) {
//		getMenuInflater().inflate(R.menu.activity_main, menu);
		return true;
	}
}
