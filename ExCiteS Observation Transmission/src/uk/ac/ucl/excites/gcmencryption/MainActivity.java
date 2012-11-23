package uk.ac.ucl.excites.gcmencryption;

import java.util.Arrays;

import org.spongycastle.crypto.InvalidCipherTextException;

import android.app.Activity;
import android.os.Bundle;
import android.view.Menu;
import android.widget.TextView;

public class MainActivity extends Activity {
	byte[] plain;
	byte[] encrypted;
	byte[] decrypted;
	String fail;
	private TextView textField;
	

	
	@Override
	public void onCreate(Bundle savedInstanceState) {
		super.onCreate(savedInstanceState);
//		setContentView(R.layout.activity_main);

		GCMencryp gcm = new GCMencryp();
		byte[] pwHash = gcm.getSHA256Hash("password");
		byte[] wrongPW = gcm.getSHA256Hash("paswoord");
		plain = "Tesdfghjkst".getBytes();

		try {
			encrypted = gcm.encrypt(pwHash, plain);
		} catch (InvalidCipherTextException e) {
			e.printStackTrace();
		}
		
		test(gcm, encrypted, pwHash);
		test(gcm, encrypted, wrongPW);
		encrypted[3] = (new Integer(147)).byteValue();
		test(gcm, encrypted, pwHash);
		test(gcm, encrypted, wrongPW);


/*		textField = (TextView) findViewById(R.id.textView);
		if (fail != null) {
			textField.setText(fail);
		} else {

			textField.setText("Plain : " + Arrays.toString(plain) + "\n\n"
					+ "Encrypted : " + Arrays.toString(encrypted) + "\n\n"
					+ "Decrypted : " + Arrays.toString(decrypted));
		}
*/		
		
	}
	
	public void test(GCMencryp gcm, byte[] encrypted, byte[] key)
	{
		try {
			decrypted = gcm.decrypt(key, encrypted);
		} catch (InvalidCipherTextException e) {
			System.out.println(e.getMessage());
			return;
		}
		System.out.println("Success");
	}

	@Override
	public boolean onCreateOptionsMenu(Menu menu) {
//		getMenuInflater().inflate(R.menu.activity_main, menu);
		return true;
	}
}
