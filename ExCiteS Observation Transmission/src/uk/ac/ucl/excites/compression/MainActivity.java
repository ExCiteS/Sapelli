package uk.ac.ucl.excites.compression;

import java.io.UnsupportedEncodingException;
import java.nio.charset.Charset;

import android.app.Activity;
import android.os.Bundle;
import android.widget.TextView;

public class MainActivity extends Activity {

	private static byte[] uncompressed;
	private static byte[] compressed;
	private static byte[] decompressed;

	private TextView textField;
	private TextView uncompressedLength;
	private TextView compressedLength;

	@Override
	public void onCreate(Bundle savedInstanceState) {
		super.onCreate(savedInstanceState);
//		setContentView(R.layout.activity_main);

		String lipsum = "Lorem ipsum dolor sit amet, consectetur adipisicing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat. Duis aute irure dolor in reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla pariatur. Excepteur sint occaecat cupidatat non proident, sunt in culpa qui officia deserunt mollit anim id est laborum.";
		uncompressed = lipsum.getBytes(Charset.forName("UTF-8"));

		compressed = Compressor.compress(uncompressed);

		decompressed = Compressor.decompress(compressed);

//		String check;
//		try {
//			check = new String(decompressed, "UTF-8");
//			textField = (TextView) findViewById(R.id.textfield);
//			textField.setText(check);
//			uncompressedLength = (TextView) findViewById(R.id.uncompressedLength);
//			compressedLength = (TextView) findViewById(R.id.compressedLength);
//			uncompressedLength.setText("uncompressed: " + uncompressed.length + " bytes");
//			compressedLength.setText("compressed: " + compressed.length + " bytes");
//
//		} catch (UnsupportedEncodingException e) {
//			e.printStackTrace();
//
//		}

	}

}
