package uk.ac.ucl.excites.sapelli.relay;

import uk.ac.ucl.excites.sapelli.relay.R;
import uk.ac.ucl.excites.sapelli.relay.sms.SmsDatabaseSQLite;
import android.app.Activity;
import android.os.Bundle;
import android.webkit.WebView;

/**
 * Activity that shows the log
 * 
 * @author Michalis Vitos
 * 
 */
public class LogActivity extends Activity
{
	private static SmsDatabaseSQLite dao;

	// UI
	private WebView logArea;

	@Override
	public void onCreate(Bundle savedInstanceState)
	{
		super.onCreate(savedInstanceState);
		dao = new SmsDatabaseSQLite(this);

		// UI Set up
		setContentView(R.layout.activity_log);
		logArea = (WebView) findViewById(R.id.logArea);

		String logTest = dao.dumpHtmlTable(SmsDatabaseSQLite.TABLE_SMS, 150);
		logArea.loadDataWithBaseURL(null, logTest, "text/html", "UTF-8", null);
	}
}
