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
