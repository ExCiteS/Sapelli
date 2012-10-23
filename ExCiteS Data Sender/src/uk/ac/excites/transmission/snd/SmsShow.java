package uk.ac.excites.transmission.snd;

import android.app.Activity;
import android.os.Bundle;
import android.widget.TextView;

/**
 * @author Michalis Vitos
 *
 */
public class SmsShow extends Activity 
{

	private TextView txtShowMessage;

	@Override
	protected void onCreate(Bundle savedInstanceState) 
	{
		// TODO Auto-generated method stub
		super.onCreate(savedInstanceState);
		setContentView(R.layout.sms_show);
		txtShowMessage = (TextView) findViewById(R.id.txtSmsShow);


		Bundle extras = getIntent().getExtras();
		String sms = extras.getString("sms");
		txtShowMessage.setText(sms);
	}
}
